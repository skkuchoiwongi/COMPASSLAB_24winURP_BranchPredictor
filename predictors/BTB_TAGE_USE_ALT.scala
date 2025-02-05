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
  nEntries: Int = 2048,
  counterLength: Int = 3,
  historyLength: Int = 80,
  historyBits: Int = 5,
  component_nEntries: Int = 1024,
  tagbits: Int = 8,
  ubits: Int = 2,
  use_alt_confidence_counter_length: Int = 4
  )

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
  val alt = UInt(btbParams.bhtParams.map(_.counterLength).getOrElse(1).W)
  val prov = UInt(btbParams.bhtParams.map(_.counterLength).getOrElse(1).W)
  val provider_u = UInt(btbParams.bhtParams.map(_.ubits).getOrElse(1).W)
  val prov_table = UInt(3.W)
  def taken = value(2)
  def strongly_taken = value > 4.U
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
  def index(addr: UInt, history: UInt) = {
    def hashAddr(addr: UInt) = {
      val hi = addr >> log2Ceil(fetchBytes)
      hi(log2Ceil(params.nEntries)-1, 0) ^ (hi >> log2Ceil(params.nEntries))(1, 0)
    }
    hashAddr(addr)
  }
  def index_confidence(addr: UInt) = {
    def hashAddr(addr: UInt) = {
      val hi = addr >> log2Ceil(fetchBytes)
      hi(log2Ceil(params.component_nEntries)-1, 0) ^ (hi >> log2Ceil(params.component_nEntries))(1, 0)
    }
    hashAddr(addr)
    //(addr>>log2Ceil(fetchBytes)) (9,0)
  }
  def index1(addr: UInt, history: UInt) = {
    val hi = addr >> log2Ceil(fetchBytes)
    hi(9,0) ^ history(79,70)
  }
  def index2(addr: UInt, history: UInt) = {
    val hi = addr >> log2Ceil(fetchBytes)
    hi(9,0) ^ history(79,70) ^ history(69,60)
  }
  def index3(addr: UInt, history: UInt) = {
    val hi = addr >> log2Ceil(fetchBytes)
    hi(9,0) ^ history(79,70) ^ history(69,60) ^ history(59,50) ^ history(49,40)
  }
  def index4(addr: UInt, history: UInt) = {
    val hi = addr >> log2Ceil(fetchBytes)
    hi(9,0) ^ history(79,70) ^ history(69,60) ^ history(59,50) ^ history(49,40) ^ history(39,30) ^ history(29,20) ^ history(19,10) ^ history(9,0)
  }

  def tag1(addr:UInt, history:UInt) = {
    val hi = addr >> log2Ceil(fetchBytes)
    val hii = hi >> 10
    hii(7,0) ^ history(79,72)
  }

  def tag2(addr:UInt, history:UInt) = {
    val hi = addr >> log2Ceil(fetchBytes)
    val hii = hi >> 10
    hii(7,0) ^ history(79,72) ^ history(69,62)
  }

  def tag3(addr:UInt, history:UInt) = {
    val hi = addr >> log2Ceil(fetchBytes)
    val hii = hi >> 10
    hii(7,0) ^ history(79,72) ^ history(69,62) ^ history(59,52) ^ history(49,42)
  }  

  def tag4(addr:UInt, history:UInt) = {
    val hi = addr >> log2Ceil(fetchBytes)
    val hii = hi >> 10
    hii(7,0) ^ history(79,72) ^ history(69,62) ^ history(59,52) ^ history(49,42) ^ history(39,32) ^ history(29,22) ^ history(19,12) ^ history(9,2)
  }  
  def get_TAGE(addr: UInt): (UInt, UInt, UInt, UInt, UInt) = {
    val tagMatch1 = Mux((T1_tag(index1(addr,history)) === tag1(addr, history)),1.U,0.U)
    val tagMatch2 = Mux((T2_tag(index2(addr,history)) === tag2(addr, history)),1.U,0.U)
    val tagMatch3 = Mux((T3_tag(index3(addr,history)) === tag3(addr, history)),1.U,0.U)
    val tagMatch4 = Mux((T4_tag(index4(addr,history)) === tag4(addr, history)),1.U,0.U)
    val prov_u = WireInit(0.U((params.ubits + 1).W))
    val prov_pred = WireInit(0.U((params.counterLength).W))
    val alt_pred = WireInit(0.U((params.counterLength).W))
    val final_pred = WireInit(0.U((params.counterLength).W))
    val prov_num = WireInit(0.U(3.W))
    when ((tagMatch4 === 1.U)) {
      prov_pred := T4_pred(index4(addr,history))
      prov_u := T4_u(index4(addr,history))
      prov_num := 4.U
      when(tagMatch3 === 1.U){
        alt_pred := T3_pred(index3(addr,history))
      }.elsewhen ((tagMatch2 === 1.U)) {
        alt_pred := T2_pred(index2(addr,history))
      }.elsewhen ((tagMatch1 === 1.U)) {
        alt_pred := T1_pred(index1(addr,history))
      }.otherwise (alt_pred := Mux(resetting, 0.U, table(index(addr,history))))
    }.elsewhen ((tagMatch3 === 1.U)) {
      prov_pred := T3_pred(index3(addr,history))
      prov_u := T3_u(index3(addr,history))
      prov_num := 3.U
      when ((tagMatch2 === 1.U)) {
        alt_pred := T2_pred(index2(addr,history))
      }.elsewhen ((tagMatch1 === 1.U)) {
        alt_pred := T1_pred(index1(addr,history))
      }.otherwise (alt_pred := Mux(resetting, 0.U, table(index(addr,history))))
    }.elsewhen ((tagMatch2 === 1.U)) {
      prov_pred := T2_pred(index2(addr,history))
      prov_u := T2_u(index2(addr,history))
      prov_num := 2.U
      when ((tagMatch1 === 1.U)) {
        alt_pred := T1_pred(index1(addr,history))
      }.otherwise (alt_pred := Mux(resetting, 0.U, table(index(addr,history))))
    }.elsewhen ((tagMatch1 === 1.U)) {
      prov_pred := T1_pred(index1(addr,history))
      prov_u := T1_u(index1(addr,history))
      alt_pred := table(index(addr,history))
      prov_num := 1.U
    }.otherwise {
      prov_num := 0.U
      prov_pred := Mux(resetting, 0.U, table(index(addr,history))) //T0이면 prov, alt를 같은 것으로 취급
      alt_pred := prov_pred
      prov_u := 1.U << params.ubits //ubit+1번째 bit가 1이라는 것은 prov_predictor가 T0라는 것을 의미
    }
    when((prov_pred===4.U | prov_pred === 3.U)&&(prov_u===0.U)){ //use_alt
      final_pred := Mux(use_alt_confidence_counter(index_confidence(addr)) >= use_alt_threshold, alt_pred, prov_pred)
      use_pred := Mux(use_alt_confidence_counter(index_confidence(addr)) >= use_alt_threshold, 0.U, 1.U)
    }.otherwise{ //or use provider prediction
      final_pred := prov_pred
      use_pred := 1.U
    }
    (final_pred, prov_pred, alt_pred, prov_u, prov_num)
  }
  def get(addr: UInt): BHTResp = {

    val res = Wire(new BHTResp)
    val (final_pred, prov_pred, alt_pred, prov_u, prov_num) = get_TAGE(addr)
    res.value := final_pred
    res.prov := prov_pred
    res.alt := alt_pred
    res.provider_u := prov_u
    res.prov_table := prov_num
    res.history := history
    res
  }
  def update_use_alt_confidence_counter(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {
    val value_update = WireInit(0.U(params.use_alt_confidence_counter_length.W))
    val use_alt_max = (1.U << params.use_alt_confidence_counter_length) - 1.U
    when((d.alt=/=d.prov)&&(d.prov===3.U|d.prov===4.U)&&(d.provider_u === 0.U)){//prev are different, and prev is weak, unuseful
      when(mispredict){
        when(use_pred===1.U){ //when used prov but miss, but alt is correct
          value_update := use_alt_confidence_counter(index_confidence(addr)) + 1.U //in this case, confidence is low, so impossible to be saturated. so just add + 1.U
          when(TC_counter === 0.U){
            TC_counter := neg_zero
            //use_alt_threshold := Mux(use_alt_threshold===0.U, 0.U, use_alt_threshold - 1.U)
            //printf(cf"use_alt_threshold: $use_alt_threshold\n")
          }.otherwise{
            TC_counter := TC_counter - 1.U
          }
        }.otherwise{ //when used alt but miss, but prov is correct
          value_update := use_alt_confidence_counter(index_confidence(addr)) - 1.U
          when(TC_counter === tc_max){
            TC_counter := pos_zero
            //use_alt_threshold := Mux(use_alt_threshold===use_alt_threshold_max, use_alt_threshold_max, use_alt_threshold + 1.U)
            //printf(cf"use_alt_threshold: $use_alt_threshold\n")
          }.otherwise{
            TC_counter := TC_counter + 1.U
          }
        }
      }.otherwise{
        when(use_pred===1.U){ //when used prov is correct
          value_update := Mux(use_alt_confidence_counter(index_confidence(addr)) === 0.U, 0.U, use_alt_confidence_counter(index_confidence(addr)) - 1.U)
          when(TC_counter === tc_max){
            TC_counter := pos_zero
            //use_alt_threshold := Mux(use_alt_threshold===use_alt_threshold_max, use_alt_threshold_max, use_alt_threshold + 1.U)
            //printf(cf"use_alt_threshold: $use_alt_threshold\n")
          }.otherwise{
            TC_counter := TC_counter + 1.U
          }
        }.otherwise{ //when used alt is correct
          value_update := Mux(use_alt_confidence_counter(index_confidence(addr)) === use_alt_max, use_alt_max, use_alt_confidence_counter(index_confidence(addr)) + 1.U)
          when(TC_counter === 0.U){
            TC_counter := neg_zero
            //use_alt_threshold := Mux(use_alt_threshold===0.U, 0.U, use_alt_threshold - 1.U)
            //printf(cf"use_alt_threshold: $use_alt_threshold\n")
          }.otherwise{
            TC_counter := TC_counter - 1.U
          }
        }
      }
      use_alt_confidence_counter(index_confidence(addr)) := value_update
    }
  }
  def update_Tage(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {

    update_use_alt_confidence_counter(addr, d, taken, mispredict) //update use_alt confidence

    val tagMatch11 = Mux((T1_tag(index1(addr, d.history)) === tag1(addr, d.history)),1.U,0.U)
    val tagMatch22 = Mux((T2_tag(index2(addr, d.history)) === tag2(addr, d.history)),1.U,0.U)
    val tagMatch33 = Mux((T3_tag(index3(addr, d.history)) === tag3(addr, d.history)),1.U,0.U)
    val tagMatch44 = Mux((T4_tag(index4(addr, d.history)) === tag4(addr, d.history)),1.U,0.U)

    val u1 = T1_u(index1(addr, d.history))
    val u2 = T2_u(index2(addr, d.history))
    val u3 = T3_u(index3(addr, d.history))
    val u4 = T4_u(index4(addr, d.history))
    
    when((tagMatch44===1.U)){ //provider가 T4인 경우
      wen_T4_pred := true.B
      waddr_T4 := index4(addr,d.history)
      wdata_T4_pred := Mux(!taken, Mux(d.value === 0.U, 0.U, d.value - 1.U), Mux(d.value === 7.U, 7.U, d.value + 1.U)) //ctr 업데이트
      when (tagMatch33===1.U) { //alternative가 T3인 경우
        when(T3_pred(index3(addr, d.history))(2) =/= T4_pred(index4(addr, d.history))(2)){ //예측이 서로 다른 경우
          wen_T4_u := true.B
          wdata_T4_u := Mux(mispredict, Mux(u4 === 0.U, 0.U, u4 -1.U),Mux(u4===u_max,u_max,u4+1.U)) //mispredict에 따라 T4의 u 증감함
        }
      }.elsewhen(tagMatch22===1.U) { //alternative가 T2인 경우로 작동방식 동일
        when(T2_pred(index2(addr, d.history))(2) =/= T4_pred(index4(addr, d.history))(2)){
          wen_T4_u := true.B
          wdata_T4_u := Mux(mispredict, Mux(u4 === 0.U, 0.U, u4 -1.U),Mux(u4===u_max,u_max,u4+1.U))
        }
      }.elsewhen(tagMatch11===1.U) { //alternative가 T1인 경우로 작동방식 동일
        when(T1_pred(index1(addr, d.history))(2) =/= T4_pred(index4(addr, d.history))(2)){
          wen_T4_u := true.B
          wdata_T4_u := Mux(mispredict, Mux(u4 === 0.U, 0.U, u4 -1.U),Mux(u4===u_max,u_max,u4+1.U))
        }
      }.otherwise { //alternative가 T0인 경우로 작동방식 동일
        when(table(index(addr, d.history))(2) =/= T4_pred(index4(addr, d.history))(2)){
          wen_T4_u := true.B
          wdata_T4_u := Mux(mispredict, Mux(u4 === 0.U, 0.U, u4 -1.U),Mux(u4===u_max,u_max,u4+1.U))
        }
      }
    }.elsewhen(tagMatch33===1.U){ //T3가 provider인 경우로 위와 동일한 로직이나, mispredict인 경우, tag 교체정책이 있기에 밑에 코드가 따로 추가됨
      wen_T3_pred := true.B
      waddr_T3 := index3(addr,d.history)
      wdata_T3_pred := Mux(!taken, Mux(d.value === 0.U, 0.U, d.value - 1.U), Mux(d.value === 7.U, 7.U, d.value + 1.U)) //ctr 업데이트
      when(tagMatch22===1.U) {
        when(T2_pred(index2(addr, d.history))(2) =/= T3_pred(index3(addr, d.history))(2)){
          wen_T3_u := true.B
          wdata_T3_u := Mux(mispredict, Mux(u3 === 0.U, 0.U, u3 -1.U),Mux(u3===u_max,u_max,u3+1.U))
        }
      }.elsewhen(tagMatch11===1.U) {
        when(T1_pred(index1(addr, d.history))(2) =/= T3_pred(index3(addr, d.history))(2)){
          wen_T3_u := true.B
          wdata_T3_u := Mux(mispredict, Mux(u3 === 0.U, 0.U, u3 -1.U),Mux(u3===u_max,u_max,u3+1.U))
        }
      }.otherwise {
        when(table(index(addr, d.history))(2) =/= T3_pred(index3(addr, d.history))(2)){
          wen_T3_u := true.B
          wdata_T3_u := Mux(mispredict, Mux(u3 === 0.U, 0.U, u3 -1.U),Mux(u3===u_max,u_max,u3+1.U))
        }
      }
      when(mispredict) { //mispredict인 경우
        when(u4===0.U){ //T4의 u = 0인 경우
          wen_T4_tag := true.B
          waddr_T4 := index4(addr, d.history)
          wdata_T4_tag := tag4(addr, d.history) //새로 할당

          wen_T4_pred := true.B
          wdata_T4_pred := Mux(taken, 4.U, 3.U)
        }.otherwise{ //u4 != 0인 경우, u4 감소
          wen_T4_u := true.B
          waddr_T4 := index4(addr, d.history)
          wdata_T4_u := u4-1.U
        }
      }
    }.elsewhen(tagMatch22===1.U){ //T2가 provider인 경우로 T3와 동일한 로직으로 작동함
      wen_T2_pred := true.B
      waddr_T2 := index2(addr,d.history)
      wdata_T2_pred := Mux(!taken, Mux(d.value === 0.U, 0.U, d.value - 1.U), Mux(d.value === 7.U, 7.U, d.value + 1.U)) //ctr 업데이트
      when(tagMatch11===1.U) {
        when(T1_pred(index1(addr, d.history))(2) =/= T2_pred(index2(addr, d.history))(2)){
          wen_T2_u := true.B
          wdata_T2_u := Mux(mispredict, Mux(u2 === 0.U, 0.U, u2 -1.U),Mux(u2===u_max,u_max,u2+1.U))
        }
      }.otherwise {
        when(table(index(addr, d.history))(2) =/= T2_pred(index2(addr, d.history))(2)){
          wen_T2_u := true.B
          wdata_T2_u := Mux(mispredict, Mux(u2 === 0.U, 0.U, u2 -1.U),Mux(u2===u_max,u_max,u2+1.U))
        }
      }
      when(mispredict) {
        when(u3===0.U){ //u3 = 0인 경우, u4 확인 안하고 T3에 바로 할당
          wen_T3_tag := true.B
          waddr_T3 := index3(addr, d.history)
          wdata_T3_tag := tag3(addr, d.history)

          wen_T3_pred := true.B
          wdata_T3_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u4===0.U){ //u3 !=0이고 u4 = 0인 경우 T4에 할당
          wen_T4_tag := true.B
          waddr_T4 := index4(addr, d.history)
          wdata_T4_tag := tag4(addr, d.history)

          wen_T4_pred := true.B
          wdata_T4_pred := Mux(taken, 4.U, 3.U)
        }.otherwise{ //u4 != 0, u3 != 0이면 u3, u4 감소
          wen_T4_u := true.B
          wen_T3_u := true.B
          waddr_T4 := index4(addr, d.history)
          waddr_T3 := index3(addr, d.history)
          wdata_T4_u := u4-1.U
          wdata_T3_u := u3-1.U
        }
      }
    }.elsewhen(tagMatch11===1.U){
      wen_T1_pred := true.B
      waddr_T1 := index1(addr,d.history)
      wdata_T1_pred := Mux(!taken, Mux(d.value === 0.U, 0.U, d.value - 1.U), Mux(d.value === 7.U, 7.U, d.value + 1.U)) //ctr 업데이트
      when(table(index(addr, d.history))(2) =/= T1_pred(index1(addr, d.history))(2)){
        wen_T1_u := true.B
        wdata_T1_u := Mux(mispredict, Mux(u1 === 0.U, 0.U, u1 -1.U),Mux(u1===u_max,u_max,u1+1.U))
      }
      when(mispredict) {
        when(u2===0.U){ //u2 = 0인 경우, u3, u4 확인 안하고 T2에 바로 할당
          wen_T2_tag := true.B
          waddr_T2 := index2(addr, d.history)
          wdata_T2_tag := tag2(addr, d.history)

          wen_T2_pred := true.B
          wdata_T2_pred :=Mux(taken, 4.U, 3.U)
        }.elsewhen(u3===0.U){ //u2 !=0이고 u3 = 0인 경우 T3에 할당
          wen_T3_tag := true.B
          waddr_T3 := index3(addr, d.history)
          wdata_T3_tag := tag3(addr, d.history)

          wen_T3_pred := true.B
          wdata_T3_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u4===0.U){ //u3 !=0이고 u4 = 0인 경우 T4에 할당
          wen_T4_tag := true.B
          waddr_T4 := index4(addr, d.history)
          wdata_T4_tag := tag4(addr, d.history)

          wen_T4_pred := true.B
          wdata_T4_pred := Mux(taken, 4.U, 3.U)
        }.otherwise{ //u4 != 0, u3 != 0, u2 != 0이면 u2, u3, u4 감소
          wen_T4_u := true.B
          wen_T3_u := true.B
          wen_T2_u := true.B
          waddr_T4 := index4(addr, d.history)
          waddr_T3 := index3(addr, d.history)
          waddr_T2 := index2(addr, d.history)
          wdata_T4_u := u4-1.U
          wdata_T3_u := u3-1.U
          wdata_T2_u := u2-1.U
        }
      }
    }.otherwise{
      wen := true.B
      waddr := index(addr, d.history)
      wdata := Mux(!taken, Mux(table(waddr) === 0.U, 0.U, table(waddr) - 1.U), Mux(table(waddr) === 7.U, 7.U, table(waddr) + 1.U))
      when(mispredict) {
        when(u1===0.U){
          wen_T1_tag := true.B
          waddr_T1 := index1(addr, d.history)
          wdata_T1_tag := tag1(addr, d.history)

          wen_T1_pred := true.B
          wdata_T1_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u2===0.U){
          wen_T2_tag := true.B
          waddr_T2 := index2(addr, d.history)
          wdata_T2_tag := tag2(addr, d.history)

          wen_T2_pred := true.B
          wdata_T2_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u3===0.U){
          wen_T3_tag := true.B
          waddr_T3 := index3(addr, d.history)
          wdata_T3_tag := tag3(addr, d.history)

          wen_T3_pred := true.B
          wdata_T3_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u4===0.U){
          wen_T4_tag := true.B
          waddr_T4 := index4(addr, d.history)
          wdata_T4_tag := tag4(addr, d.history)

          wen_T4_pred := true.B
          wdata_T4_pred := Mux(taken, 4.U, 3.U)
        }.otherwise{
          wen_T4_u := true.B
          wen_T3_u := true.B
          wen_T2_u := true.B
          wen_T1_u := true.B
          waddr_T4 := index4(addr, d.history)
          waddr_T3 := index3(addr, d.history)
          waddr_T2 := index2(addr, d.history)
          waddr_T1 := index1(addr, d.history)
          wdata_T4_u := u4-1.U
          wdata_T3_u := u3-1.U
          wdata_T2_u := u2-1.U
          wdata_T1_u := u1-1.U
        }
      }
    }
  }
  def updateTable(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {
    update_Tage(addr, d, taken, mispredict)
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

  private val table = Mem(params.nEntries, UInt(params.counterLength.W))
  val history = RegInit(0.U(params.historyLength.W))

  private val u_max = (1.U << params.ubits) - 1.U
  private val use_alt_confidence_counter = RegInit(VecInit(Seq.fill(params.component_nEntries)(8.U((params.use_alt_confidence_counter_length).W))))
  private val use_pred = RegInit(0.U(1.W)) //1이면 prov, 0이면 alt 사용
  private val use_alt_threshold = RegInit(14.U(4.W))
  private val use_alt_threshold_max = (1.U << 4) -1.U
  private val TC_counter = RegInit(8.U(4.W)) //TC counter: count number of updates at correct and miss
  private val tc_max = (1.U << 4) - 1.U
  private val pos_zero = (1.U << 3)
  private val neg_zero = (1.U << 3) - 1.U

  private val T1_pred = Mem(params.component_nEntries,UInt(params.counterLength.W))
  private val T2_pred = Mem(params.component_nEntries,UInt(params.counterLength.W))
  private val T3_pred = Mem(params.component_nEntries,UInt(params.counterLength.W))
  private val T4_pred = Mem(params.component_nEntries,UInt(params.counterLength.W))

  private val T1_tag = Mem(params.component_nEntries,UInt(params.tagbits.W))
  private val T2_tag = Mem(params.component_nEntries,UInt(params.tagbits.W))
  private val T3_tag = Mem(params.component_nEntries,UInt(params.tagbits.W))
  private val T4_tag = Mem(params.component_nEntries,UInt(params.tagbits.W))

  private val T1_u = Reg(Vec(params.component_nEntries, UInt(params.ubits.W)))
  private val T2_u = Reg(Vec(params.component_nEntries, UInt(params.ubits.W)))
  private val T3_u = Reg(Vec(params.component_nEntries, UInt(params.ubits.W)))
  private val T4_u = Reg(Vec(params.component_nEntries, UInt(params.ubits.W)))

  
  
  private val reset_waddr = RegInit(0.U((params.nEntries.log2+1).W))
  private val resetting = !reset_waddr(params.nEntries.log2)
  private val wen = WireInit(resetting)
  private val waddr = WireInit(reset_waddr)
  private val wdata = WireInit(0.U)

  when (resetting) { reset_waddr := reset_waddr + 1.U }
  when (wen) { table(waddr) := wdata }

  private val reset_waddr_T1 = RegInit(0.U((params.component_nEntries.log2+1).W))
  private val resetting_T1 = !reset_waddr(params.component_nEntries.log2)
  private val wen_T1_pred = WireInit(resetting)
  private val wen_T1_tag = WireInit(resetting)
  private val wen_T1_u = WireInit(resetting)
  private val waddr_T1 = WireInit(reset_waddr)
  private val wdata_T1_pred = WireInit(0.U)
  private val wdata_T1_tag = WireInit(0.U)
  private val wdata_T1_u = WireInit(0.U)
  when (resetting_T1) { reset_waddr_T1 := reset_waddr_T1 + 1.U }
  when (wen_T1_pred) { T1_pred(waddr_T1) := wdata_T1_pred }
  when (wen_T1_tag) { T1_tag(waddr_T1) := wdata_T1_tag }
  when (wen_T1_u) { T1_u(waddr_T1) := wdata_T1_u }

  private val reset_waddr_T2 = RegInit(0.U((params.component_nEntries.log2 + 1).W))
  private val resetting_T2 = !reset_waddr(params.component_nEntries.log2)
  private val wen_T2_pred = WireInit(resetting)
  private val wen_T2_tag = WireInit(resetting)
  private val wen_T2_u = WireInit(resetting)
  private val waddr_T2 = WireInit(reset_waddr)
  private val wdata_T2_pred = WireInit(0.U)
  private val wdata_T2_tag = WireInit(0.U)
  private val wdata_T2_u = WireInit(0.U)
  when (resetting_T2) { reset_waddr_T2 := reset_waddr_T2 + 1.U }
  when (wen_T2_pred) { T2_pred(waddr_T2) := wdata_T2_pred }
  when (wen_T2_tag) { T2_tag(waddr_T2) := wdata_T2_tag }
  when (wen_T2_u) { T2_u(waddr_T2) := wdata_T2_u }

  private val reset_waddr_T3 = RegInit(0.U((params.component_nEntries.log2 + 1).W))
  private val resetting_T3 = !reset_waddr(params.component_nEntries.log2)
  private val wen_T3_pred = WireInit(resetting)
  private val wen_T3_tag = WireInit(resetting)
  private val wen_T3_u = WireInit(resetting)
  private val waddr_T3 = WireInit(reset_waddr)
  private val wdata_T3_pred = WireInit(0.U)
  private val wdata_T3_tag = WireInit(0.U)
  private val wdata_T3_u = WireInit(0.U)
  when (resetting_T3) { reset_waddr_T3 := reset_waddr_T3 + 1.U }
  when (wen_T3_pred) { T3_pred(waddr_T3) := wdata_T3_pred }
  when (wen_T3_tag) { T3_tag(waddr_T3) := wdata_T3_tag }
  when (wen_T3_u) { T3_u(waddr_T3) := wdata_T3_u }

  private val reset_waddr_T4 = RegInit(0.U((params.component_nEntries.log2 + 1).W))
  private val resetting_T4 = !reset_waddr(params.component_nEntries.log2)
  private val wen_T4_pred = WireInit(resetting)
  private val wen_T4_tag = WireInit(resetting)
  private val wen_T4_u = WireInit(resetting)
  private val waddr_T4 = WireInit(reset_waddr)
  private val wdata_T4_pred = WireInit(0.U)
  private val wdata_T4_tag = WireInit(0.U)
  private val wdata_T4_u = WireInit(0.U)
  when (resetting_T4) { reset_waddr_T4 := reset_waddr_T4 + 1.U }
  when (wen_T4_pred) { T4_pred(waddr_T4) := wdata_T4_pred }
  when (wen_T4_tag) { T4_tag(waddr_T4) := wdata_T4_tag }
  when (wen_T4_u) { T4_u(waddr_T4) := wdata_T4_u }
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
        bht.updateTable(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken, io.bht_update.bits.mispredict)
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
