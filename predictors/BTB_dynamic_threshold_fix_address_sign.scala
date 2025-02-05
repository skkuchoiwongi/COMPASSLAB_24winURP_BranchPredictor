// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.

package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.util._

case class BHTParams(
  nEntries: Int = 64,
  counterLength: Int = 8,
  historyLength: Int = 30,
  historyBits: Int = 13)

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
  val value = UInt(1.W) //value의 경우, taken/not taken을 나타내는 지표이므로, UInt 1bit로 설정하였으며, 이는 get function에서 threshold 비교를 통해 값이 정해짐
  val dot = SInt(btbParams.bhtParams.map(_.historyBits).getOrElse(1).W) //dot은 prediction에서 dot product 결과를 저장하기 위하여 넣은 것으로, get function에서 저장되며, update 시 사용
  //n-bc가 아닌, threshold로 taken이냐 not taken이냐가 정해지기에 taken, strongly_taken을 같은 것으로 함.
  def taken = value(0)
  def strongly_taken = taken
}

// BHT contains table of 2-bit counters and a global history register.
// The BHT only predicts and updates when there is a BTB hit.
// The global history:
//    - updated speculatively in fetch (if there'SInt a BTB hit).
//    - on a mispredict, the history register is reset (again, only if BTB hit).
// The counter table:
//    - each counter corresponds with the address of the fetch packet ("fetch pc").
//    - updated when a branch resolves (and BTB was a hit for that branch).
//      The updating branch must provide its "fetch pc".
class BHT(params: BHTParams)(implicit val p: Parameters) extends HasCoreParameters {

  def index(addr: UInt) = {
    (addr >> log2Ceil(fetchBytes)) (params.nEntries.log2 - 1,0) ^
    (addr >> log2Ceil(fetchBytes)) (params.nEntries.log2*2 - 1,params.nEntries.log2)
  }

  def dot(hist: UInt, weight: Vec[SInt], bias: SInt): SInt = { //내적을 진행하는 함수
    val weight_bit = params.counterLength.W
    val dot_product =  (0 until params.historyLength).foldLeft(0.S(params.historyBits.W)) { (acc, i) =>
    acc + Mux(hist(i), 1.S(weight_bit), -1.S(weight_bit)) * weight(i)
  }
    dot_product + bias //마지막에 bias를 더해줌
  }
  def get(addr: UInt): BHTResp = {
    val res = Wire(new BHTResp)
    val dot_result = dot(history,table(index(addr)),bias(index(addr)))
    //mux를 이용하여 양수 음수로 prediction 판별
    res.value := Mux(resetting, 0.U, Mux(dot_result(params.historyBits-1),0.U,1.U))
    res.history := history
    res.dot := dot_result
    res
  }
  def updateTable(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {
    val weight = table(index(addr))
    val weight_bit = params.counterLength.W
    val weight_i = VecInit(Seq.fill(params.historyLength)(0.S(params.counterLength.W)))
    when (!resetting) {
      when(mispredict||(!mispredict)&&(Mux(d.dot>=0.S, d.dot, -d.dot)<threshold)) { //update 조건을 1. misprediction 2. dot product의 크기가 threshold보다 작은 경우로 설정
        wen := true.B
        for(i <- 0 until params.historyLength){
          weight_i(i) := weight(i) + Mux(taken, 1.S(weight_bit), -1.S(weight_bit))*Mux(d.history(i),1.S(weight_bit),-1.S(weight_bit)) //가중치 업데이트
        }
        wdata_bias := bias(index(addr)) + Mux(taken, 1.S(weight_bit), -1.S(weight_bit))
        wdata := weight_i
        waddr := index(addr)
        when(mispredict){
          when(TC_counter === tc_max){
            TC_counter := pos_zero
            threshold := threshold + 1.S
            //printf(cf"threshold: $threshold\n")
          }.otherwise{
            TC_counter := TC_counter + 1.U
          }
        }.elsewhen((!mispredict)&&(Mux(d.dot>=0.S, d.dot, -d.dot)<threshold)){
          when(TC_counter === 0.U){
            TC_counter := neg_zero
            val update_threshold = WireInit(0.S(8.W))
            update_threshold := Mux(threshold===0.S, 0.S, threshold - 1.S)
            threshold := update_threshold
            //printf(cf"threshold: $threshold\n")
          }
          TC_counter := TC_counter - 1.U
        }
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
  private val table = Mem((params.nEntries), Vec(params.historyLength, SInt(params.counterLength.W))) //전체 weight table
  private val bias = RegInit(VecInit(Seq.fill((params.nEntries))(0.S(params.counterLength.W))))
  val history = RegInit(0.U(params.historyLength.W))
  private val reset_waddr = RegInit(0.U((params.nEntries.log2+1).W))
  private val resetting = !reset_waddr(params.nEntries.log2)
  private val wen = WireInit(resetting)
  private val waddr = WireInit(reset_waddr)
  private val wdata = VecInit(Seq.fill(params.historyLength)(0.S(params.counterLength.W))) //Weight값 업데이트 시 사용
  private val wdata_bias = WireInit(0.S(params.counterLength.W)) //bias값 업데이트 시 사용
  private val threshold = RegInit(15.S(8.W)) //threshold value, in real, it is always positive, but for compare weight sum(its type is SInt), make type to be SInt
  private val TC_counter = RegInit(64.U(7.W)) //TC counter: count number of updates at correct and miss
  private val tc_max = (1.U << 7) - 1.U
  private val pos_zero = (1.U << 6)
  private val neg_zero = (1.U << 6) - 1.U
  when (resetting) { reset_waddr := reset_waddr + 1.U }
  when (wen) { table(waddr) := wdata }
  when (wen) { bias(waddr) := wdata_bias }
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
  val accessCount = RegInit(0.U(32.W))
  val mispredictCount = RegInit(0.U(32.W))
  
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
        accessCount := accessCount + 1.U
        printf(cf"access count = $accessCount\n")
        bht.updateTable(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken,io.bht_update.bits.mispredict)
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
