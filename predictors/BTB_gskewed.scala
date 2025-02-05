// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.
//61~69, 80~196 참고
package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.util._

case class BHTParams(
  nEntries: Int = 8192,
  counterLength: Int = 2,
  historyLength: Int = 13,
  historyBits: Int = 4)

case class BTBParams(
  nEntries: Int = 28,
  nMatchBits: Int = 14,
  nPages: Int = 6,
  nRAS: Int = 6,
  bhtParams: Option[BHTParams] = Some(BHTParams()),
  updatesOutOfOrder: Boolean = false)

trait HasBtbParameters extends HasCoreParameters { this: InstanceId =>
  val btbParams = tileParams.btb.getOrElse(BTBParams(nEntries = 0))
  val matchBits = btbParams.nMatchBits max log2Ceil(p(CacheBlockBytes) * tileParams.icache.get.nSets)
  val entries = btbParams.nEntries
  val updatesOutOfOrder = btbParams.updatesOutOfOrder
  val nPages = (btbParams.nPages + 1) / 2 * 2 // control logic assumes 2 divides pages
}

abstract class BtbModule(implicit val p: Parameters) extends Module with HasBtbParameters {
  Annotated.params(this, btbParams)
}

abstract class BtbBundle(implicit val p: Parameters) extends Bundle with HasBtbParameters

class RAS(nras: Int) {
  def push(addr: UInt): Unit = {
    when (count < nras.U) { count := count + 1.U }
    val nextPos = Mux((isPow2(nras)).B || pos < (nras-1).U, pos+1.U, 0.U)
    stack(nextPos) := addr
    pos := nextPos
  }
  def peek: UInt = stack(pos)
  def pop(): Unit = when (!isEmpty) {
    count := count - 1.U
    pos := Mux((isPow2(nras)).B || pos > 0.U, pos-1.U, (nras-1).U)
  }
  def clear(): Unit = count := 0.U
  def isEmpty: Bool = count === 0.U

  private val count = RegInit(0.U(log2Up(nras+1).W))
  private val pos = RegInit(0.U(log2Up(nras).W))
  private val stack = Reg(Vec(nras, UInt()))
}

class BHTResp(implicit p: Parameters) extends BtbBundle()(p) {
  val history = UInt(btbParams.bhtParams.map(_.historyLength).getOrElse(1).W)
  val value = UInt(btbParams.bhtParams.map(_.counterLength).getOrElse(1).W)
  val value00 = UInt(btbParams.bhtParams.map(_.counterLength).getOrElse(1).W) //각 PHT0, PHT1, PHT2에 대한 prediction 값
  val value11 = UInt(btbParams.bhtParams.map(_.counterLength).getOrElse(1).W)
  val value22 = UInt(btbParams.bhtParams.map(_.counterLength).getOrElse(1).W)
  def taken = value(0)
  def strongly_taken = value === 1.U
}

// BHT contains table of 2-bit counters and a global history register.
// The BHT only predicts and updates when there is a BTB hit.
// The global history:
//    - updated speculatively in fetch (if there's a BTB hit).
//    - on a mispredict, the history register is reset (again, only if BTB hit).
// The counter table:
//    - each counter corresponds with the address of the fetch packet ("fetch pc").
//    - updated when a branch resolves (and BTB was a hit for that branch).
//      The updating branch must provide its "fetch pc".
class BHT(params: BHTParams)(implicit val p: Parameters) extends HasCoreParameters {
  //Hash function
  def H(x: UInt) = {
    val n = x(log2Up(params.nEntries)-1)^x(0) // x(n) xor x(0)
    val s = x(log2Up(params.nEntries)-1,1) // x>>1
    val h = Cat(n,s) // result
    h
  }
  //inverse of Hash function
  def Hin(y: UInt) = {
    val n = y(log2Up(params.nEntries)-1)^y(log2Up(params.nEntries)-2)
    val s = y(log2Up(params.nEntries)-2,0)
    val hin = Cat(s,n)
    hin
  }
  //hashing function for PHT0
  def index0(addr: UInt, history: UInt) = {
    val hi = addr >> (log2Ceil(fetchBytes))
    val i = H(history)^Hin(hi(log2Up(params.nEntries)-1,0))
    i^hi(log2Up(params.nEntries)-1,0)
  }
  //hashing function for PHT1
  def index1(addr: UInt, history: UInt) = {
    val hi = addr >> (log2Ceil(fetchBytes))
    val i = H(history)^Hin(hi(log2Up(params.nEntries)-1,0))
    i^history
  }
  //hashing function for PHT2
  def index2(addr: UInt, history: UInt) = {
    val hi = addr >> (log2Ceil(fetchBytes))
    val i = Hin(history)^H(hi(log2Up(params.nEntries)-1,0))
    i^hi(log2Up(params.nEntries)-1,0)
  }

  def get(addr: UInt): BHTResp = {
    val res = Wire(new BHTResp)
    val value0 = table0(index0(addr, history))
    val value1 = table1(index1(addr, history))
    val value2 = table2(index2(addr, history))
    res.value00 := Mux(resetting, 0.U, table0(index0(addr, history)))
    res.value11 := Mux(resetting, 0.U, table1(index1(addr, history)))
    res.value22 := Mux(resetting, 0.U, table2(index2(addr, history)))

    val a = ((value0(1)&value1(1))|value1(1)&value2(1)|value2(1)&value0(1)) //majority
    val b = ((value0(0)&value1(0))|value1(0)&value2(0)|value2(0)&value0(0))
    val ab = Cat(a,b)
    res.value := Mux(resetting, 0.U, ab)
    res.history := history
    res
  }
  def updateTable(addr: UInt, d: BHTResp, taken: Bool, misprediction: Bool): Unit = {
    wen0 := true.B
    wen1 := true.B
    wen2 := true.B
    when (!resetting) {
      //when correct
      when(!misprediction){
        //예측과 결과가 다른 경우 해당 table update하지않음 (partial update), 또한, correct인 경우, 무조건 3개 중 2개가 branch 결과와 동일하기에 하단의 code 진행 시, wen = false가 되는 것은 1개의 PHT밖에 없음
        when(d.value00(0)=/=taken){wen0 := false.B}
        when(d.value11(0)=/=taken){wen1 := false.B}
        when(d.value22(0)=/=taken){wen2 := false.B}
      }
      when(wen0){
        waddr0 := index0(addr, d.history)
        wdata0 := (params.counterLength match {
          case 1 => taken
          case 2 => Cat(taken ^ d.value00(0), d.value00 === 1.U || d.value00(1) && taken)
        })
      }
      when(wen1){
        waddr1 := index1(addr, d.history)
        wdata1 := (params.counterLength match {
          case 1 => taken
          case 2 => Cat(taken ^ d.value11(0), d.value11 === 1.U || d.value11(1) && taken)
        })
      }
      when(wen2){
        waddr2 := index2(addr, d.history)
        wdata2 := (params.counterLength match {
          case 1 => taken
          case 2 => Cat(taken ^ d.value22(0), d.value22 === 1.U || d.value22(1) && taken)
        })
      }
    }
  }
  def resetHistory(d: BHTResp): Unit = {
    history := d.history
  }
  def updateHistory(addr: UInt, d: BHTResp, taken: Bool): Unit = {
    history := Cat(taken, d.history >> 1)
  }
  def advanceHistory(taken: Bool): Unit = {
    history := Cat(taken, history >> 1)
  }

  private val table0 = Mem(params.nEntries, UInt(params.counterLength.W)) //PHT table
  private val table1 = Mem(params.nEntries, UInt(params.counterLength.W))
  private val table2 = Mem(params.nEntries, UInt(params.counterLength.W))
  val history = RegInit(0.U(params.historyLength.W))

  private val reset_waddr = RegInit(0.U((params.nEntries.log2+1).W))
  private val resetting = !reset_waddr(params.nEntries.log2)
  private val wen0 = WireInit(resetting)
  private val wen1 = WireInit(resetting)
  private val wen2 = WireInit(resetting)
  private val waddr0 = WireInit(reset_waddr)
  private val waddr1 = WireInit(reset_waddr)
  private val waddr2 = WireInit(reset_waddr)
  private val wdata0 = WireInit(0.U)
  private val wdata1 = WireInit(0.U)
  private val wdata2 = WireInit(0.U)
  when (resetting) { reset_waddr := reset_waddr + 1.U }
  when (wen0) { table0(waddr0) := wdata0}
  when (wen1) { table1(waddr1) := wdata1}
  when (wen2) { table2(waddr2) := wdata2}
}

object CFIType {
  def SZ = 2
  def apply() = UInt(SZ.W)
  def branch = 0.U
  def jump = 1.U
  def call = 2.U
  def ret = 3.U
}

// BTB update occurs during branch resolution (and only on a mispredict).
//  - "pc" is what future fetch PCs will tag match against.
//  - "br_pc" is the PC of the branch instruction.
class BTBUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val prediction = new BTBResp
  val pc = UInt(vaddrBits.W)
  val target = UInt(vaddrBits.W)
  val taken = Bool()
  val isValid = Bool()
  val br_pc = UInt(vaddrBits.W)
  val cfiType = CFIType()
}

// BHT update occurs during branch resolution on all conditional branches.
//  - "pc" is what future fetch PCs will tag match against.
class BHTUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val prediction = new BHTResp
  val pc = UInt(vaddrBits.W)
  val branch = Bool()
  val taken = Bool()
  val mispredict = Bool()
}

class RASUpdate(implicit p: Parameters) extends BtbBundle()(p) {
  val cfiType = CFIType()
  val returnAddr = UInt(vaddrBits.W)
}

//  - "bridx" is the low-order PC bits of the predicted branch (after
//     shifting off the lowest log(inst_bytes) bits off).
//  - "mask" provides a mask of valid instructions (instructions are
//     masked off by the predicted taken branch from the BTB).
class BTBResp(implicit p: Parameters) extends BtbBundle()(p) {
  val cfiType = CFIType()
  val taken = Bool()
  val mask = Bits(fetchWidth.W)
  val bridx = Bits(log2Up(fetchWidth).W)
  val target = UInt(vaddrBits.W)
  val entry = UInt(log2Up(entries + 1).W)
  val bht = new BHTResp
}

class BTBReq(implicit p: Parameters) extends BtbBundle()(p) {
   val addr = UInt(vaddrBits.W)
}

// fully-associative branch target buffer
// Higher-performance processors may cause BTB updates to occur out-of-order,
// which requires an extra CAM port for updates (to ensure no duplicates get
// placed in BTB).
class BTB(implicit p: Parameters) extends BtbModule {
  val io = IO(new Bundle {
    val req = Flipped(Valid(new BTBReq))
    val resp = Valid(new BTBResp)
    val btb_update = Flipped(Valid(new BTBUpdate))
    val bht_update = Flipped(Valid(new BHTUpdate))
    val bht_advance = Flipped(Valid(new BTBResp))
    val ras_update = Flipped(Valid(new RASUpdate))
    val ras_head = Valid(UInt(vaddrBits.W))
    val flush = Input(Bool())
  })

  val accessCount = RegInit(0.U(32.W)) //count for Instructions
  val mispredictCount = RegInit(0.U(32.W)) //count for misprediction


  val idxs = Reg(Vec(entries, UInt((matchBits - log2Up(coreInstBytes)).W)))
  val idxPages = Reg(Vec(entries, UInt(log2Up(nPages).W)))
  val tgts = Reg(Vec(entries, UInt((matchBits - log2Up(coreInstBytes)).W)))
  val tgtPages = Reg(Vec(entries, UInt(log2Up(nPages).W)))
  val pages = Reg(Vec(nPages, UInt((vaddrBits - matchBits).W)))
  val pageValid = RegInit(0.U(nPages.W))
  val pagesMasked = (pageValid.asBools zip pages).map { case (v, p) => Mux(v, p, 0.U) }

  val isValid = RegInit(0.U(entries.W))
  val cfiType = Reg(Vec(entries, CFIType()))
  val brIdx = Reg(Vec(entries, UInt(log2Up(fetchWidth).W)))

  private def page(addr: UInt) = addr >> matchBits
  private def pageMatch(addr: UInt) = {
    val p = page(addr)
    pageValid & pages.map(_ === p).asUInt
  }
  private def idxMatch(addr: UInt) = {
    val idx = addr(matchBits-1, log2Up(coreInstBytes))
    idxs.map(_ === idx).asUInt & isValid
  }

  val r_btb_update = Pipe(io.btb_update)
  val update_target = io.req.bits.addr

  val pageHit = pageMatch(io.req.bits.addr)
  val idxHit = idxMatch(io.req.bits.addr)

  val updatePageHit = pageMatch(r_btb_update.bits.pc)
  val (updateHit, updateHitAddr) =
    if (updatesOutOfOrder) {
      val updateHits = (pageHit << 1)(Mux1H(idxMatch(r_btb_update.bits.pc), idxPages))
      (updateHits.orR, OHToUInt(updateHits))
    } else (r_btb_update.bits.prediction.entry < entries.U, r_btb_update.bits.prediction.entry)

  val useUpdatePageHit = updatePageHit.orR
  val usePageHit = pageHit.orR
  val doIdxPageRepl = !useUpdatePageHit
  val nextPageRepl = RegInit(0.U(log2Ceil(nPages).W))
  val idxPageRepl = Cat(pageHit(nPages-2,0), pageHit(nPages-1)) | Mux(usePageHit, 0.U, UIntToOH(nextPageRepl))
  val idxPageUpdateOH = Mux(useUpdatePageHit, updatePageHit, idxPageRepl)
  val idxPageUpdate = OHToUInt(idxPageUpdateOH)
  val idxPageReplEn = Mux(doIdxPageRepl, idxPageRepl, 0.U)

  val samePage = page(r_btb_update.bits.pc) === page(update_target)
  val doTgtPageRepl = !samePage && !usePageHit
  val tgtPageRepl = Mux(samePage, idxPageUpdateOH, Cat(idxPageUpdateOH(nPages-2,0), idxPageUpdateOH(nPages-1)))
  val tgtPageUpdate = OHToUInt(pageHit | Mux(usePageHit, 0.U, tgtPageRepl))
  val tgtPageReplEn = Mux(doTgtPageRepl, tgtPageRepl, 0.U)

  when (r_btb_update.valid && (doIdxPageRepl || doTgtPageRepl)) {
    val both = doIdxPageRepl && doTgtPageRepl
    val next = nextPageRepl + Mux[UInt](both, 2.U, 1.U)
    nextPageRepl := Mux(next >= nPages.U, next(0), next)
  }

  val repl = new PseudoLRU(entries)
  val waddr = Mux(updateHit, updateHitAddr, repl.way)
  val r_resp = Pipe(io.resp)
  when (r_resp.valid && r_resp.bits.taken || r_btb_update.valid) {
    repl.access(Mux(r_btb_update.valid, waddr, r_resp.bits.entry))
  }

  when (r_btb_update.valid) {
    val mask = UIntToOH(waddr)
    idxs(waddr) := r_btb_update.bits.pc(matchBits-1, log2Up(coreInstBytes))
    tgts(waddr) := update_target(matchBits-1, log2Up(coreInstBytes))
    idxPages(waddr) := idxPageUpdate +& 1.U // the +1 corresponds to the <<1 on io.resp.valid
    tgtPages(waddr) := tgtPageUpdate
    cfiType(waddr) := r_btb_update.bits.cfiType
    isValid := Mux(r_btb_update.bits.isValid, isValid | mask, isValid & ~mask)
    if (fetchWidth > 1)
      brIdx(waddr) := r_btb_update.bits.br_pc >> log2Up(coreInstBytes)

    require(nPages % 2 == 0)
    val idxWritesEven = !idxPageUpdate(0)

    def writeBank(i: Int, mod: Int, en: UInt, data: UInt) =
      for (i <- i until nPages by mod)
        when (en(i)) { pages(i) := data }

    writeBank(0, 2, Mux(idxWritesEven, idxPageReplEn, tgtPageReplEn),
      Mux(idxWritesEven, page(r_btb_update.bits.pc), page(update_target)))
    writeBank(1, 2, Mux(idxWritesEven, tgtPageReplEn, idxPageReplEn),
      Mux(idxWritesEven, page(update_target), page(r_btb_update.bits.pc)))
    pageValid := pageValid | tgtPageReplEn | idxPageReplEn
  }

  io.resp.valid := (pageHit << 1)(Mux1H(idxHit, idxPages))
  io.resp.bits.taken := true.B
  io.resp.bits.target := Cat(pagesMasked(Mux1H(idxHit, tgtPages)), Mux1H(idxHit, tgts) << log2Up(coreInstBytes))
  io.resp.bits.entry := OHToUInt(idxHit)
  io.resp.bits.bridx := (if (fetchWidth > 1) Mux1H(idxHit, brIdx) else 0.U)
  io.resp.bits.mask := Cat((1.U << ~Mux(io.resp.bits.taken, ~io.resp.bits.bridx, 0.U))-1.U, 1.U)
  io.resp.bits.cfiType := Mux1H(idxHit, cfiType)

  // if multiple entries for same PC land in BTB, zap them
  when (PopCountAtLeast(idxHit, 2)) {
    isValid := isValid & ~idxHit
  }
  when (io.flush) {
    isValid := 0.U
  }

  if (btbParams.bhtParams.nonEmpty) {
    val bht = new BHT(Annotated.params(this, btbParams.bhtParams.get))
    val isBranch = (idxHit & cfiType.map(_ === CFIType.branch).asUInt).orR
    val res = bht.get(io.req.bits.addr)
    when (io.bht_advance.valid) {
      bht.advanceHistory(io.bht_advance.bits.bht.taken)
    }
    when (io.bht_update.valid) {
      when (io.bht_update.bits.branch) {
        bht.updateTable(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken,io.bht_update.bits.mispredict)
        accessCount := accessCount + 1.U
        printf(cf"access count = $accessCount\n")
        when (io.bht_update.bits.mispredict) {
          mispredictCount := mispredictCount + 1.U
          printf(cf"misprediction count = $mispredictCount\n")
          bht.updateHistory(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken)
        }
      }.elsewhen (io.bht_update.bits.mispredict) {
        bht.resetHistory(io.bht_update.bits.prediction)
      }
    }
    when (!res.taken && isBranch) { io.resp.bits.taken := false.B }
    io.resp.bits.bht := res
  }

  if (btbParams.nRAS > 0) {
    val ras = new RAS(btbParams.nRAS)
    val doPeek = (idxHit & cfiType.map(_ === CFIType.ret).asUInt).orR
    io.ras_head.valid := !ras.isEmpty
    io.ras_head.bits := ras.peek
    when (!ras.isEmpty && doPeek) {
      io.resp.bits.target := ras.peek
    }
    when (io.ras_update.valid) {
      when (io.ras_update.bits.cfiType === CFIType.call) {
        ras.push(io.ras_update.bits.returnAddr)
      }.elsewhen (io.ras_update.bits.cfiType === CFIType.ret) {
        ras.pop()
      }
    }
  }
}
