// See LICENSE.Berkeley for license details.
// See LICENSE.SiFive for license details.
// TAGE(USE_ALT) + perceptron + loop version
package freechips.rocketchip.rocket

import chisel3._
import chisel3.util._
import org.chipsalliance.cde.config.Parameters
import freechips.rocketchip.subsystem.CacheBlockBytes
import freechips.rocketchip.tile.HasCoreParameters
import freechips.rocketchip.util._

case class BHTParams(
  // TAGE
  tage_nEntries: Int = 1024, //tage's base predictor's table's entry
  tage_counterLength: Int = 3, //tage's pred bits, for all components including base predictor
  tage_historyLength: Int = 72, //tage's global history length, L(1) = 10, L(2) = 20, ... L(4) = 80, here, number of component is 5 (fixed)
  component_nEntries: Int = 512, //tage's components' entry except for base predictor
  tagbits: Int = 8, //tage's tag bit
  ubits: Int = 2, //tage's u bit
  use_alt_confidence_counter_length: Int = 4, //for USE_ALT's counter's length(used for tage), threshold is fixed, so if change bit, also need to change threshold. here, it is not for global, managed by PC, so it has table
  
  // Perceptron
  perceptron_nEntries: Int = 128, //perceptron's table's entry (weight table)
  perceptron_counterLength: Int = 8, //perceptron's weight's bit. use dynamic threshold fitting whose initial size is managed at BHT class, not here
  perceptron_historyLength: Int = 31, //perceptron's global history bit except bias, here, it is also same as the number of weight per entry
  perceptron_historyBits: Int = 13, //just means maximum bit of dot product

  // Meta selector
  meta_nEntries: Int = 256, //meta selector's entry
  meta_ctr: Int = 2, //meta selector's value's bit.

  // Loop
  loop_nEntries: Int = 64, // loop predictor table's entry
  loop_tagLength: Int = 10, // partial tag bitwidth
  loop_iterLength: Int = 10, // (past/retire) iteration count bitwidth
  loop_confLength: Int = 5, // confidence bitwidth
  loop_ageLength: Int = 4, // age bitwidth
  loop_direction: Int = 1, // branch direction
  loop_ageTickLength: Int = 6, // age tick, shorter: fast age increase, longer: slow age increase
  loop_confThreshold: UInt = 31.U, // confidence threshold
  loop_ageThreshold: UInt = 3.U // age threshold
  )

class LoopEntry(params: BHTParams) extends Bundle {
  val tag = UInt(params.loop_tagLength.W) // partial tag
  val conf = UInt(params.loop_confLength.W) // confidence
  val age = UInt(params.loop_ageLength.W) // age
  val pastIter = UInt(params.loop_iterLength.W) // past iteration count
  val retrIter = UInt(params.loop_iterLength.W) // retire iteration count
  val dir = UInt(params.loop_direction.W) // branch direction 
}

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
  val tage_history = UInt(btbParams.bhtParams.map(_.tage_historyLength).getOrElse(1).W) //global history for tage
  val tage_value = UInt(btbParams.bhtParams.map(_.tage_counterLength).getOrElse(1).W) //prediction value from tage
  val alt = UInt(btbParams.bhtParams.map(_.tage_counterLength).getOrElse(1).W) //alternative prediction from tage
  val prov = UInt(btbParams.bhtParams.map(_.tage_counterLength).getOrElse(1).W) //provider prediction from tage
  val provider_u = UInt(btbParams.bhtParams.map(_.ubits).getOrElse(1).W) //provider's u bit from tage to use USE_ALT
  val prov_table = UInt(3.W) //provider predictor's component number, so if provider predictor is T4, prov_table = 4
  
  val perceptron_history = UInt(btbParams.bhtParams.map(_.perceptron_historyLength).getOrElse(1).W) //global history for perceptron
  val perceptron_value = UInt(1.W) //prediction from perceptron
  val dot = SInt(btbParams.bhtParams.map(_.perceptron_historyBits).getOrElse(1).W) //dot product from perceptron
  
  val meta_value = UInt(btbParams.bhtParams.map(_.meta_ctr).getOrElse(1).W) //meta selector's value to select predictor

  val loop_value = UInt(1.W) // loop predictor's prediction value
  val loop_hit = UInt(1.W) // loop predictor hit (if false, do not use loop predictior)
  val loop_index = UInt(log2Ceil(btbParams.bhtParams.map(_.loop_nEntries).getOrElse(2)).W) // index for loop predictor update

  val loop_last = UInt(1.W) // whether current iteration is the last, for loop predictor update
  val loop_conf = UInt(btbParams.bhtParams.map(_.loop_confLength).getOrElse(1).W) // loop confidence
  val loop_iter = UInt(btbParams.bhtParams.map(_.loop_iterLength).getOrElse(1).W) // current loop length

  def loop_conf_threshold: UInt = {
    btbParams.bhtParams.map(_.loop_confThreshold).getOrElse(1.U)
    // Adaptive confidence threshold
    // + Mux((loop_iter > 10.U), 0.U, (10.U - loop_iter))
  }

  def loop_hybrid = // do we use loop predictor or not
    Mux(
      (
        (loop_iter > 18.U) && // Loop Length Limit
        loop_hit.asBool && // isHit?
        (loop_conf >= loop_conf_threshold) // confidence > threshold?
      ),
      0.U, 1.U
    )
  def taken =
    Mux(
      (
        (loop_iter > 18.U) &&
        loop_hit.asBool &&
        (loop_conf >= loop_conf_threshold)
      ),
      loop_value(0),
      Mux(meta_value(1) === 0.U, tage_value(2), perceptron_value)
    ).asBool //final prediction
  
  def strongly_taken = 
    Mux(
      (
        (loop_iter > 18.U) &&
        loop_hit.asBool &&
        (loop_conf >= loop_conf_threshold)
      ),
      loop_value(0),
      Mux(
        meta_value(1) === 0.U,
        (tage_value > 4.U).asUInt,
        perceptron_value
      )
    ).asBool //strongly taken
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

  // **********  TAGE Predictor ****************
  def index_tage(addr: UInt, history: UInt) = { //index function for base predictor
    def hashAddr(addr: UInt) = {
      val hi = addr >> log2Ceil(fetchBytes)
      hi(log2Ceil(params.tage_nEntries)-1, 0) ^ (hi >> log2Ceil(params.tage_nEntries))(1, 0)
    }
    hashAddr(addr)
  }
  def index1(addr: UInt, history: UInt) = { //index function for T1
    val hi = addr >> log2Ceil(fetchBytes)
    val idx = (0 until math.pow(2,0).toInt).foldLeft(0.U(params.component_nEntries.log2.W)) { (acc, i) =>
      acc ^ history(
        params.tage_historyLength - params.component_nEntries.log2 * i - 1,
        params.tage_historyLength - params.component_nEntries.log2 * (i + 1)
      )
    }
    idx ^ hi(9,0)
  }
  def index2(addr: UInt, history: UInt) = { //index function for T2
    val hi = addr >> log2Ceil(fetchBytes)
    val idx = (0 until math.pow(2,1).toInt).foldLeft(0.U(params.component_nEntries.log2.W)) { (acc, i) =>
      acc ^ history(
        params.tage_historyLength - params.component_nEntries.log2 * i - 1,
        params.tage_historyLength - params.component_nEntries.log2 * (i + 1)
      )
    }
    idx ^ hi(9,0)
  }
  def index3(addr: UInt, history: UInt) = { //index function for T3
    val hi = addr >> log2Ceil(fetchBytes)
    val idx = (0 until math.pow(2,2).toInt).foldLeft(0.U(params.component_nEntries.log2.W)) { (acc, i) =>
      acc ^ history(
        params.tage_historyLength - params.component_nEntries.log2 * i - 1,
        params.tage_historyLength - params.component_nEntries.log2 * (i + 1)
      )
    }
    idx ^ hi(9,0)
  }
  def index4(addr: UInt, history: UInt) = { //index function for T4
    val hi = addr >> log2Ceil(fetchBytes)
    val idx = (0 until math.pow(2,3).toInt).foldLeft(0.U(params.component_nEntries.log2.W)) { (acc, i) =>
      acc ^ history(
        params.tage_historyLength - params.component_nEntries.log2 * i - 1,
        params.tage_historyLength - params.component_nEntries.log2 * (i + 1)
      )
    }
    idx ^ hi(9,0)
  }
  def index_confidence(addr: UInt) = { //index function for confidence table(USE_ALT)
    def hashAddr(addr: UInt) = {
      val hi = addr >> log2Ceil(fetchBytes)
      hi(log2Ceil(params.component_nEntries)-1, 0) ^ (hi >> log2Ceil(params.component_nEntries))(1, 0)
    }
    hashAddr(addr)
  }
  def tag1(addr:UInt, history:UInt) = { //tag function for T1
    val hi = addr >> log2Ceil(fetchBytes)
    val hii = hi >> 10
    val idx = (0 until math.pow(2,0).toInt).foldLeft(0.U(params.component_nEntries.log2.W)) { (acc, i) =>
      acc ^ history(
        params.tage_historyLength - params.tagbits * i - 1,
        params.tage_historyLength - params.tagbits * (i + 1)
      )
    }
    idx ^ hii(9,0)
  }
  def tag2(addr:UInt, history:UInt) = { //tag function for T2
    val hi = addr >> log2Ceil(fetchBytes)
    val hii = hi >> 10
    val idx = (0 until math.pow(2,1).toInt).foldLeft(0.U(params.component_nEntries.log2.W)) { (acc, i) =>
      acc ^ history(
        params.tage_historyLength - params.tagbits * i - 1,
        params.tage_historyLength - params.tagbits * (i + 1)
      )
    }
    idx ^ hii(9,0)
  }
  def tag3(addr:UInt, history:UInt) = { //tag function for T3
    val hi = addr >> log2Ceil(fetchBytes)
    val hii = hi >> 10
    val idx = (0 until math.pow(2,2).toInt).foldLeft(0.U(params.component_nEntries.log2.W)) { (acc, i) =>
      acc ^ history(
        params.tage_historyLength - params.tagbits * i - 1,
        params.tage_historyLength - params.tagbits * (i + 1)
      )
    }
    idx ^ hii(9,0)
  }  
  def tag4(addr:UInt, history:UInt) = { //tag function for T4
    val hi = addr >> log2Ceil(fetchBytes)
    val hii = hi >> 10
    val idx = (0 until math.pow(2,3).toInt).foldLeft(0.U(params.component_nEntries.log2.W)) { (acc, i) =>
      acc ^ history(
        params.tage_historyLength - params.tagbits * i - 1,
        params.tage_historyLength - params.tagbits * (i + 1)
      )
    }
    idx ^ hii(9,0)
  }
  def get_TAGE(addr: UInt): (UInt, UInt, UInt, UInt, UInt) = { //prediction from tage
    val tagMatch1 = Mux((T1_tag(index1(addr,tage_history)) === tag1(addr, tage_history)),1.U,0.U)
    val tagMatch2 = Mux((T2_tag(index2(addr,tage_history)) === tag2(addr, tage_history)),1.U,0.U)
    val tagMatch3 = Mux((T3_tag(index3(addr,tage_history)) === tag3(addr, tage_history)),1.U,0.U)
    val tagMatch4 = Mux((T4_tag(index4(addr,tage_history)) === tag4(addr, tage_history)),1.U,0.U)
    val prov_u = WireInit(0.U((params.ubits + 1).W))
    val prov_pred = WireInit(0.U((params.tage_counterLength).W))
    val alt_pred = WireInit(0.U((params.tage_counterLength).W))
    val final_pred = WireInit(0.U((params.tage_counterLength).W))
    val prov_table_num = WireInit(0.U(3.W))
    when ((tagMatch4 === 1.U)) {
      prov_pred := T4_pred(index4(addr,tage_history))
      prov_u := T4_u(index4(addr,tage_history))
      prov_table_num := 4.U
      when(tagMatch3 === 1.U){
        alt_pred := T3_pred(index3(addr,tage_history))
      }.elsewhen ((tagMatch2 === 1.U)) {
        alt_pred := T2_pred(index2(addr,tage_history))
      }.elsewhen ((tagMatch1 === 1.U)) {
        alt_pred := T1_pred(index1(addr,tage_history))
      }.otherwise (alt_pred := Mux(tage_resetting, 0.U, tage_table(index_tage(addr,tage_history))))
    }.elsewhen ((tagMatch3 === 1.U)) {
      prov_pred := T3_pred(index3(addr,tage_history))
      prov_u := T3_u(index3(addr,tage_history))
      prov_table_num := 3.U
      when ((tagMatch2 === 1.U)) {
        alt_pred := T2_pred(index2(addr,tage_history))
      }.elsewhen ((tagMatch1 === 1.U)) {
        alt_pred := T1_pred(index1(addr,tage_history))
      }.otherwise (alt_pred := Mux(tage_resetting, 0.U, tage_table(index_tage(addr,tage_history))))
    }.elsewhen ((tagMatch2 === 1.U)) {
      prov_pred := T2_pred(index2(addr,tage_history))
      prov_u := T2_u(index2(addr,tage_history))
      prov_table_num := 2.U
      when ((tagMatch1 === 1.U)) {
        alt_pred := T1_pred(index1(addr,tage_history))
      }.otherwise (alt_pred := Mux(tage_resetting, 0.U, tage_table(index_tage(addr,tage_history))))
    }.elsewhen ((tagMatch1 === 1.U)) {
      prov_pred := T1_pred(index1(addr,tage_history))
      prov_u := T1_u(index1(addr,tage_history))
      alt_pred := tage_table(index_tage(addr,tage_history))
      prov_table_num := 1.U
    }.otherwise {
      prov_table_num := 0.U
      prov_pred := Mux(tage_resetting, 0.U, tage_table(index_tage(addr,tage_history))) //if provider predictor is T0, alt is also T0
      alt_pred := prov_pred
      prov_u := 1.U << params.ubits //MSB = 1 means provider predictor is T0. In result, it stored as 0 in provider_u(BHTResp) because of length limitation.
    }
    when((prov_pred===4.U | prov_pred === 3.U)&&(prov_u===0.U)){ //use_alt
      final_pred := Mux(use_alt_confidence_counter(index_confidence(addr)) >= use_alt_threshold, alt_pred, prov_pred)
      use_pred := Mux(use_alt_confidence_counter(index_confidence(addr)) >= use_alt_threshold, 0.U, 1.U)
    }.otherwise{ //or use provider prediction
      final_pred := prov_pred
      use_pred := 1.U
    }
    (final_pred, prov_pred, alt_pred, prov_u, prov_table_num)
  }
  def update_use_alt_confidence_counter(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = { //update function for USE_ALT table
    val value_update = WireInit(0.U(params.use_alt_confidence_counter_length.W))
    val use_alt_max = (1.U << params.use_alt_confidence_counter_length) - 1.U
    when((d.alt=/=d.prov)&&(d.prov===3.U|d.prov===4.U)&&(d.provider_u === 0.U)){//prev are different, and prev is weak, unuseful
      when(mispredict){
        when(use_pred===1.U){ //when used prov but miss, but alt is correct
          value_update := use_alt_confidence_counter(index_confidence(addr)) + 1.U //in this case, confidence is low, so impossible to be saturated. so just add + 1.U
        }.otherwise{ //when used alt but miss, but prov is correct
          value_update := use_alt_confidence_counter(index_confidence(addr)) - 1.U
        }
      }.otherwise{
        when(use_pred===1.U){ //when used prov is correct
          value_update := Mux(use_alt_confidence_counter(index_confidence(addr)) === 0.U, 0.U, use_alt_confidence_counter(index_confidence(addr)) - 1.U)
        }.otherwise{ //when used alt is correct
          value_update := Mux(use_alt_confidence_counter(index_confidence(addr)) === use_alt_max, use_alt_max, use_alt_confidence_counter(index_confidence(addr)) + 1.U)
        }
      }
      use_alt_confidence_counter(index_confidence(addr)) := value_update
    }
  }
  def update_Tage(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = { //update function for TAGE, and in here, also include 'update_use_alt_confidence_counter'

    update_use_alt_confidence_counter(addr, d, taken, mispredict) //update use_alt confidence

    val tagMatch11 = Mux((T1_tag(index1(addr, d.tage_history)) === tag1(addr, d.tage_history)),1.U,0.U)
    val tagMatch22 = Mux((T2_tag(index2(addr, d.tage_history)) === tag2(addr, d.tage_history)),1.U,0.U)
    val tagMatch33 = Mux((T3_tag(index3(addr, d.tage_history)) === tag3(addr, d.tage_history)),1.U,0.U)
    val tagMatch44 = Mux((T4_tag(index4(addr, d.tage_history)) === tag4(addr, d.tage_history)),1.U,0.U)

    val u1 = T1_u(index1(addr, d.tage_history))
    val u2 = T2_u(index2(addr, d.tage_history))
    val u3 = T3_u(index3(addr, d.tage_history))
    val u4 = T4_u(index4(addr, d.tage_history))
    
    when((tagMatch44===1.U)){ //provider = T4
      wen_T4_pred := true.B
      waddr_T4 := index4(addr,d.tage_history)
      wdata_T4_pred := Mux(!taken, Mux(d.tage_value === 0.U, 0.U, d.tage_value - 1.U), Mux(d.tage_value === 7.U, 7.U, d.tage_value + 1.U)) //T4's pred update
      when (tagMatch33===1.U) { //alternative = T3
        when(T3_pred(index3(addr, d.tage_history))(2) =/= T4_pred(index4(addr, d.tage_history))(2)){ //alt pred =/= prov pred
          wen_T4_u := true.B
          wdata_T4_u := Mux(mispredict, Mux(u4 === 0.U, 0.U, u4 -1.U),Mux(u4===u_max,u_max,u4+1.U)) //T4's u update
        }
      }.elsewhen(tagMatch22===1.U) { //alternative = T2
        when(T2_pred(index2(addr, d.tage_history))(2) =/= T4_pred(index4(addr, d.tage_history))(2)){
          wen_T4_u := true.B
          wdata_T4_u := Mux(mispredict, Mux(u4 === 0.U, 0.U, u4 -1.U),Mux(u4===u_max,u_max,u4+1.U))
        }
      }.elsewhen(tagMatch11===1.U) { //alternative = T1
        when(T1_pred(index1(addr, d.tage_history))(2) =/= T4_pred(index4(addr, d.tage_history))(2)){
          wen_T4_u := true.B
          wdata_T4_u := Mux(mispredict, Mux(u4 === 0.U, 0.U, u4 -1.U),Mux(u4===u_max,u_max,u4+1.U))
        }
      }.otherwise { //alternative = T0
        when(tage_table(index_tage(addr, d.tage_history))(2) =/= T4_pred(index4(addr, d.tage_history))(2)){
          wen_T4_u := true.B
          wdata_T4_u := Mux(mispredict, Mux(u4 === 0.U, 0.U, u4 -1.U),Mux(u4===u_max,u_max,u4+1.U))
        }
      }
    }.elsewhen(tagMatch33===1.U){ //provider = T3, update policy of pred is same as above, but here, include entry allocation logic (for T0~T2 are also same)
      wen_T3_pred := true.B
      waddr_T3 := index3(addr,d.tage_history)
      wdata_T3_pred := Mux(!taken, Mux(d.tage_value === 0.U, 0.U, d.tage_value - 1.U), Mux(d.tage_value === 7.U, 7.U, d.tage_value + 1.U))
      when(tagMatch22===1.U) {
        when(T2_pred(index2(addr, d.tage_history))(2) =/= T3_pred(index3(addr, d.tage_history))(2)){
          wen_T3_u := true.B
          wdata_T3_u := Mux(mispredict, Mux(u3 === 0.U, 0.U, u3 -1.U),Mux(u3===u_max,u_max,u3+1.U))
        }
      }.elsewhen(tagMatch11===1.U) {
        when(T1_pred(index1(addr, d.tage_history))(2) =/= T3_pred(index3(addr, d.tage_history))(2)){
          wen_T3_u := true.B
          wdata_T3_u := Mux(mispredict, Mux(u3 === 0.U, 0.U, u3 -1.U),Mux(u3===u_max,u_max,u3+1.U))
        }
      }.otherwise {
        when(tage_table(index_tage(addr, d.tage_history))(2) =/= T3_pred(index3(addr, d.tage_history))(2)){
          wen_T3_u := true.B
          wdata_T3_u := Mux(mispredict, Mux(u3 === 0.U, 0.U, u3 -1.U),Mux(u3===u_max,u_max,u3+1.U))
        }
      }
      when(mispredict) { //entry allocation
        when(u4===0.U){ //T4's u = 0
          wen_T4_tag := true.B
          waddr_T4 := index4(addr, d.tage_history)
          wdata_T4_tag := tag4(addr, d.tage_history) //entry allocation

          wen_T4_pred := true.B
          wdata_T4_pred := Mux(taken, 4.U, 3.U)
        }.otherwise{ //u4 != 0, decrement
          wen_T4_u := true.B
          waddr_T4 := index4(addr, d.tage_history)
          wdata_T4_u := u4-1.U
        }
      }
    }.elsewhen(tagMatch22===1.U){ //provider = T2
      wen_T2_pred := true.B
      waddr_T2 := index2(addr,d.tage_history)
      wdata_T2_pred := Mux(!taken, Mux(d.tage_value === 0.U, 0.U, d.tage_value - 1.U), Mux(d.tage_value === 7.U, 7.U, d.tage_value + 1.U))
      when(tagMatch11===1.U) {
        when(T1_pred(index1(addr, d.tage_history))(2) =/= T2_pred(index2(addr, d.tage_history))(2)){
          wen_T2_u := true.B
          wdata_T2_u := Mux(mispredict, Mux(u2 === 0.U, 0.U, u2 -1.U),Mux(u2===u_max,u_max,u2+1.U))
        }
      }.otherwise {
        when(tage_table(index_tage(addr, d.tage_history))(2) =/= T2_pred(index2(addr, d.tage_history))(2)){
          wen_T2_u := true.B
          wdata_T2_u := Mux(mispredict, Mux(u2 === 0.U, 0.U, u2 -1.U),Mux(u2===u_max,u_max,u2+1.U))
        }
      }
      when(mispredict) {
        when(u3===0.U){
          wen_T3_tag := true.B
          waddr_T3 := index3(addr, d.tage_history)
          wdata_T3_tag := tag3(addr, d.tage_history)

          wen_T3_pred := true.B
          wdata_T3_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u4===0.U){
          wen_T4_tag := true.B
          waddr_T4 := index4(addr, d.tage_history)
          wdata_T4_tag := tag4(addr, d.tage_history)

          wen_T4_pred := true.B
          wdata_T4_pred := Mux(taken, 4.U, 3.U)
        }.otherwise{
          wen_T4_u := true.B
          wen_T3_u := true.B
          waddr_T4 := index4(addr, d.tage_history)
          waddr_T3 := index3(addr, d.tage_history)
          wdata_T4_u := u4-1.U
          wdata_T3_u := u3-1.U
        }
      }
    }.elsewhen(tagMatch11===1.U){
      wen_T1_pred := true.B
      waddr_T1 := index1(addr,d.tage_history)
      wdata_T1_pred := Mux(!taken, Mux(d.tage_value === 0.U, 0.U, d.tage_value - 1.U), Mux(d.tage_value === 7.U, 7.U, d.tage_value + 1.U))
      when(tage_table(index_tage(addr, d.tage_history))(2) =/= T1_pred(index1(addr, d.tage_history))(2)){
        wen_T1_u := true.B
        wdata_T1_u := Mux(mispredict, Mux(u1 === 0.U, 0.U, u1 -1.U),Mux(u1===u_max,u_max,u1+1.U))
      }
      when(mispredict) {
        when(u2===0.U){ 
          wen_T2_tag := true.B
          waddr_T2 := index2(addr, d.tage_history)
          wdata_T2_tag := tag2(addr, d.tage_history)

          wen_T2_pred := true.B
          wdata_T2_pred :=Mux(taken, 4.U, 3.U)
        }.elsewhen(u3===0.U){
          wen_T3_tag := true.B
          waddr_T3 := index3(addr, d.tage_history)
          wdata_T3_tag := tag3(addr, d.tage_history)

          wen_T3_pred := true.B
          wdata_T3_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u4===0.U){ 
          wen_T4_tag := true.B
          waddr_T4 := index4(addr, d.tage_history)
          wdata_T4_tag := tag4(addr, d.tage_history)

          wen_T4_pred := true.B
          wdata_T4_pred := Mux(taken, 4.U, 3.U)
        }.otherwise{ 
          wen_T4_u := true.B
          wen_T3_u := true.B
          wen_T2_u := true.B
          waddr_T4 := index4(addr, d.tage_history)
          waddr_T3 := index3(addr, d.tage_history)
          waddr_T2 := index2(addr, d.tage_history)
          wdata_T4_u := u4-1.U
          wdata_T3_u := u3-1.U
          wdata_T2_u := u2-1.U
        }
      }
    }.otherwise{
      tage_wen := true.B
      tage_waddr := index_tage(addr, d.tage_history)
      tage_wdata := Mux(!taken, Mux(tage_table(tage_waddr) === 0.U, 0.U, tage_table(tage_waddr) - 1.U), Mux(tage_table(tage_waddr) === 7.U, 7.U, tage_table(tage_waddr) + 1.U))
      when(mispredict) {
        when(u1===0.U){
          wen_T1_tag := true.B
          waddr_T1 := index1(addr, d.tage_history)
          wdata_T1_tag := tag1(addr, d.tage_history)

          wen_T1_pred := true.B
          wdata_T1_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u2===0.U){
          wen_T2_tag := true.B
          waddr_T2 := index2(addr, d.tage_history)
          wdata_T2_tag := tag2(addr, d.tage_history)

          wen_T2_pred := true.B
          wdata_T2_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u3===0.U){
          wen_T3_tag := true.B
          waddr_T3 := index3(addr, d.tage_history)
          wdata_T3_tag := tag3(addr, d.tage_history)

          wen_T3_pred := true.B
          wdata_T3_pred := Mux(taken, 4.U, 3.U)
        }.elsewhen(u4===0.U){
          wen_T4_tag := true.B
          waddr_T4 := index4(addr, d.tage_history)
          wdata_T4_tag := tag4(addr, d.tage_history)

          wen_T4_pred := true.B
          wdata_T4_pred := Mux(taken, 4.U, 3.U)
        }.otherwise{
          wen_T4_u := true.B
          wen_T3_u := true.B
          wen_T2_u := true.B
          wen_T1_u := true.B
          waddr_T4 := index4(addr, d.tage_history)
          waddr_T3 := index3(addr, d.tage_history)
          waddr_T2 := index2(addr, d.tage_history)
          waddr_T1 := index1(addr, d.tage_history)
          wdata_T4_u := u4-1.U
          wdata_T3_u := u3-1.U
          wdata_T2_u := u2-1.U
          wdata_T1_u := u1-1.U
        }
      }
    }
  }
  // *******************************************


  // **********  Perceptron Predictor **********
  def perceptron_index(addr: UInt) = { //index function for perceptron
    (addr >> log2Ceil(fetchBytes)) (params.perceptron_nEntries.log2 - 1,0) ^
    (addr >> log2Ceil(fetchBytes)) (params.perceptron_nEntries.log2*2 - 1,params.perceptron_nEntries.log2)
  }
  def dot(hist: UInt, weight: Vec[SInt], bias: SInt): SInt = { //function for perceptron's dot product
    val weight_bit = params.perceptron_counterLength.W
    val dot_product =  (0 until params.perceptron_historyLength).foldLeft(0.S(params.perceptron_historyBits.W)) { (acc, i) =>
    acc + Mux(hist(i), 1.S(weight_bit), -1.S(weight_bit)) * weight(i)
    }
    dot_product + bias
  }
  def get_perceptron(addr: UInt): (UInt, SInt) = { //prediction from perceptron
    val dot_result = dot(perceptron_history,perceptron_table(perceptron_index(addr)),bias(perceptron_index(addr)))
    val final_pred = Mux(perceptron_resetting, 0.U, Mux(dot_result(params.perceptron_historyBits-1),0.U,1.U))
    (final_pred, dot_result)
  }
  def perceptron_updateTable(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = { //update function for perceptron, also threshold
    val weight = perceptron_table(perceptron_index(addr))
    val weight_bit = params.perceptron_counterLength.W
    val weight_i = VecInit(Seq.fill(params.perceptron_historyLength)(0.S(params.perceptron_counterLength.W)))
    when (!perceptron_resetting) {
      when(mispredict||(!mispredict)&&(Mux(d.dot>=0.S, d.dot, -d.dot)<perceptron_threshold)) { //update conditions: 1. misprediction 2. size of dot product < perceptron_threshold
        perceptron_wen := true.B
        for(i <- 0 until params.perceptron_historyLength){
          weight_i(i) := weight(i) + Mux(taken, 1.S(weight_bit), -1.S(weight_bit))*Mux(d.perceptron_history(i),1.S(weight_bit),-1.S(weight_bit)) //가중치 업데이트
        }
        perceptron_wdata_bias := bias(perceptron_index(addr)) + Mux(taken, 1.S(weight_bit), -1.S(weight_bit))
        perceptron_wdata := weight_i
        perceptron_waddr := perceptron_index(addr)
        when(mispredict){
          when(perceptron_TC_counter === perceptron_tc_max){
            perceptron_TC_counter := perceptron_pos_zero
            perceptron_threshold := perceptron_threshold + 1.S
          }.otherwise{
            perceptron_TC_counter := perceptron_TC_counter + 1.U
          }
        }.elsewhen((!mispredict)&&(Mux(d.dot>=0.S, d.dot, -d.dot)<perceptron_threshold)){
          when(perceptron_TC_counter === 0.U){
            perceptron_TC_counter := perceptron_neg_zero
            val update_perceptron_threshold = WireInit(0.S(8.W))
            update_perceptron_threshold := Mux(perceptron_threshold===0.S, 0.S, perceptron_threshold - 1.S) //learning rate is always Int, in here, lr = 1
            perceptron_threshold := update_perceptron_threshold
          }
          perceptron_TC_counter := perceptron_TC_counter - 1.U
        }
      }
    }
  }
  // *************************************************


  // ********** Meta Selector ************************
  def meta_index(addr: UInt) = { //index function for meta selector
    (addr >> log2Ceil(fetchBytes))(params.meta_nEntries.log2 - 1, 0)
  }
  def get_meta(addr: UInt): (UInt) = { //selection from meta selector
    meta_table(meta_index(addr))
  }
  def meta_updateTable(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = { //update function for meta selector
    //update only for both predictor's prediction are not same
    when(d.tage_value(2) === taken){
      when(d.perceptron_value =/= taken){ 
        meta_wen := true.B
        meta_waddr := meta_index(addr)
        meta_wdata := Mux(d.meta_value === 0.U, 0.U, d.meta_value - 1.U)
      }
    }.elsewhen(d.perceptron_value === taken){
      when(d.tage_value(2) =/= taken){
        meta_wen := true.B
        meta_waddr := meta_index(addr)
        meta_wdata := Mux(d.meta_value === 3.U, 3.U, d.meta_value + 1.U)
      }
    }
  }
  // *************************************************

  // ************ Loop Predictor *********************
  // Age Tick Counter Settings
  def updateAge(): Unit = {
    ageTickCounter := ageTickCounter + 1.U
    when(ageTickCounter === ((1 << params.loop_ageTickLength) - 1).U) {
      // Reset Age Tick Counter
      ageTickCounter := 0.U

      for (i <- 0 until params.loop_nEntries) {
        // if valid entry ...
        when ((ltable(i).tag =/= 0.U) || (ltable(i).conf =/= 0.U)) {
          val oldAge = Wire(UInt(params.loop_ageLength.W))
          oldAge := ltable(i).age
          val maxAge = ((1 << params.loop_ageLength) - 1).U

          // Increment Age
          ltable(i).age := Mux((oldAge === maxAge), maxAge, oldAge + 1.U)
        }
      }
    }
  }

  // Index and tag generation for each way
  def lh0(addr: UInt) = ((addr >> log2Ceil(fetchBytes)) ^ (addr >> (log2Ceil(fetchBytes) + 2)))(params.loop_nEntries.log2 - 1, 0)
  def lh1(addr: UInt) = ((addr >> log2Ceil(fetchBytes)) ^ (addr >> (log2Ceil(fetchBytes) + 3)))(params.loop_nEntries.log2 - 1, 0)
  def lh2(addr: UInt) = ((addr >> log2Ceil(fetchBytes)) ^ (addr >> (log2Ceil(fetchBytes) + 5)))(params.loop_nEntries.log2 - 1, 0)
  def lh3(addr: UInt) = ((addr >> log2Ceil(fetchBytes)) ^ (addr >> (log2Ceil(fetchBytes) + 7)))(params.loop_nEntries.log2 - 1, 0)
  def ltag(addr: UInt) = {
    (addr >> log2Ceil(fetchBytes))(params.loop_tagLength - 1, 0) ^ (addr >> log2Ceil(fetchBytes))(params.loop_tagLength * 2 - 1, params.loop_tagLength)
  }

  // Priority Generation for comparing multiple ways
  def makePriority(hit: Bool, conf: UInt, tag: UInt, age: UInt, way: UInt): UInt = {
    val isEmpty = (conf === 0.U) && (tag === 0.U)
    Cat(hit, isEmpty, conf, age, ~way)
  }

  // Loop table entry print
  def printltable(i: UInt): Unit = {
    val tagi = ltable(i).tag
    val confi = ltable(i).conf
    val agei = ltable(i).age
    val piteri = ltable(i).pastIter
    val riteri = ltable(i).retrIter
    val diri = ltable(i).dir

    printf(cf"t:$tagi c:$confi a:$agei p:$piteri r:$riteri d:$diri\n")
  }

  // get function
  def lget(addr: UInt) = {
    val indices = Seq(lh0(addr), lh1(addr), lh2(addr), lh3(addr))
    val tables = indices.map(ltable(_))
    val tag = ltag(addr)

    // calculate the priority of multiple ways
    val ways = tables.zipWithIndex.map { case (table, wayIndex) =>
      val hit = table.tag === tag
      val last = table.pastIter === table.retrIter
      val value = Mux(last, ~table.dir, table.dir)
      val priority = makePriority(hit, table.conf, table.tag, table.age, wayIndex.U)
      (priority, hit, table.conf, indices(wayIndex), last, value, table.pastIter)
    }

    // select the highest priority entry
    val bestWay = ways.reduceLeft { (a, b) =>
      val (aPrio, aHit, aConf, aIdx, aLast, aVal, aIter) = a
      val (bPrio, bHit, bConf, bIdx, bLast, bVal, bIter) = b

      (
        Mux(aPrio > bPrio, aPrio, bPrio),
        Mux(aPrio > bPrio, aHit, bHit),
        Mux(aPrio > bPrio, aConf, bConf),
        Mux(aPrio > bPrio, aIdx, bIdx),
        Mux(aPrio > bPrio, aLast, bLast),
        Mux(aPrio > bPrio, aVal, bVal),
        Mux(aPrio > bPrio, aIter, bIter)
      )
    }

    // pop the highest priority entry
    val (_, loop_hit, loop_conf, loop_index, loop_last, loop_value, loop_past) = bestWay

    (loop_value, loop_hit, loop_index, loop_last, loop_conf, loop_past)
  }
  
  // New entry value for ways (initialization)
  def createNewEntry(tag: UInt, dir: Bool): LoopEntry = {
    val newEntry = Wire(new LoopEntry(params))

    newEntry.tag := tag
    newEntry.dir := dir
    newEntry.conf := 0.U
    newEntry.age := 0.U
    newEntry.pastIter := 0.U
    newEntry.retrIter := 1.U

    newEntry
  }

  def lupdateTable(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {
    val targetIndex = d.loop_index
    val entry = ltable(targetIndex)
    val correct = (d.taken === d.loop_value(0))
    val tag = ltag(addr)
    val iter = entry.retrIter
    // val maxIter = ((1 << params.loop_iterLength) - 1).U
    val oldConf = entry.conf
    val maxConf = ((1 << params.loop_confLength) - 1).U


    when (d.loop_hit.asBool) {
      // Tag Hit
      entry.age := 0.U // Refresh age (pseudo-LRU)
      when (entry.pastIter === 0.U) {
        // does not have loop out record or reset
        when (entry.dir === taken) {
          // Same Direction (in loop)
          entry.retrIter := iter + 1.U
        }.otherwise {
          // Diff. Direction (loop out)
          entry.retrIter := 0.U
          entry.pastIter := iter
        }
      }.otherwise {
        // has loop out record
        when (correct && d.loop_last.asBool) {
          // Correct, last (Conf. Increase, Age Reset)
          entry.retrIter := 0.U(params.loop_iterLength.W)
          entry.conf := Mux((oldConf === maxConf), oldConf, oldConf + 1.U)
        }.elsewhen (~correct && d.loop_last.asBool) {
          // Last prediction Fail (Conf. Decrease, new Past iter)
          entry.retrIter := iter + 1.U

          // Method 1: conf -= 1
          // entry.conf := Mux((oldConf === 0.U), oldConf, oldConf - 1.U)
          // Method 2: conf -= 2
          // entry.conf := Mux((oldConf >= 2.U), oldConf - 2.U, 0.U)
          // Method 3: Reset conf
          entry.conf := 0.U

          entry.pastIter := 0.U
        }.elsewhen (correct && ~(d.loop_last.asBool)) {
          // Not last, prediction success (Age Reset)
          entry.retrIter := iter + 1.U
        }.otherwise {
          // Not last, prediction fail
          // New past iter or direction change, age increase?
          entry.pastIter := iter
          entry.retrIter := 0.U
        }
      }
    }.otherwise {
      // Tag Miss
      val newEntry = createNewEntry(tag, taken)
      when (entry.tag === 0.U && entry.conf === 0.U) {
        // Tag Empty
        // Allocate tag, direction, iter ...
        entry := newEntry
      }.elsewhen (entry.age >= params.loop_ageThreshold) {
        // Has Old Entry
        // Allocate tag, direction, iter ...
        entry := newEntry
      }.elsewhen (entry.pastIter === 1.U) {
        entry := newEntry
      }
    }
  }
  // *************************************************



  // Unified Get
  def get(addr: UInt): BHTResp = { //store values to res(BHTResp)
    val res = Wire(new BHTResp)

    val (tage_pred, prov_pred, alt_pred, prov_u, prov_table_num) = get_TAGE(addr)
    val (perceptron_pred, dot_result) = get_perceptron(addr)
    val meta_sel = get_meta(addr)
    val (lvalue, lhit, lindex, llast, lconf, liter) = lget(addr)

    res.tage_history := tage_history
    res.tage_value := tage_pred
    res.alt := alt_pred
    res.prov := prov_pred
    res.provider_u := prov_u
    res.prov_table := prov_table_num

    res.dot := dot_result
    res.perceptron_value := perceptron_pred
    res.perceptron_history := perceptron_history
    
    res.meta_value := meta_sel

    res.loop_value := lvalue
    res.loop_hit := lhit
    res.loop_index := lindex
    res.loop_last := llast
    res.loop_conf := lconf
    res.loop_iter := liter

    res
  }

  // Unified Update
  def updateTable(addr: UInt, d: BHTResp, taken: Bool, mispredict: Bool): Unit = {
    when (d.loop_hybrid === 1.U || mispredict) {
      perceptron_updateTable(addr, d, taken, mispredict)
      update_Tage(addr, d, taken, mispredict)
      meta_updateTable(addr, d, taken, mispredict)
    }
    updateAge()
    lupdateTable(addr, d, taken, mispredict)
  }


  def resetHistory(d: BHTResp): Unit = {
    perceptron_history := d.perceptron_history
    tage_history := d.tage_history
  }
  def updateHistory(addr: UInt, d: BHTResp, taken: Bool): Unit = {
    tage_history := Cat(taken, d.tage_history >> 1)
    perceptron_history := Cat(taken, d.perceptron_history >> 1)
  }
  def advanceHistory(taken: Bool): Unit = {
    tage_history := Cat(taken, tage_history >> 1)
    perceptron_history := Cat(taken, perceptron_history >> 1)
  }

  // ************ Storage Settings ***************
  //TAGE predictor

  //base predictor
  private val tage_table = Mem(params.tage_nEntries, UInt(params.tage_counterLength.W))
  val tage_history = RegInit(0.U(params.tage_historyLength.W))

  private val tage_reset_waddr = RegInit(0.U((params.tage_nEntries.log2+1).W))
  private val tage_resetting = !tage_reset_waddr(params.tage_nEntries.log2)
  private val tage_wen = WireInit(tage_resetting)
  private val tage_waddr = WireInit(tage_reset_waddr)
  private val tage_wdata = WireInit(0.U)

  when (tage_resetting) { tage_reset_waddr := tage_reset_waddr + 1.U }
  when (tage_wen) { tage_table(tage_waddr) := tage_wdata }

  //USE_ALT table, here, use_alt_confidence_counter is updated at the function of 'update_use_alt_confidence_counter'
  private val u_max = (1.U << params.ubits) - 1.U
  private val use_alt_confidence_counter = RegInit(VecInit(Seq.fill(params.component_nEntries)(8.U((params.use_alt_confidence_counter_length).W))))
  private val use_pred = RegInit(0.U(1.W)) //1 means prov, 0 means alt for prediction
  private val use_alt_threshold = RegInit(14.U(4.W)) //USE_ALT's threshold
  private val use_alt_threshold_max = (1.U << 4) -1.U

  //T1 ~ T4
  private val T1_pred = Mem(params.component_nEntries,UInt(params.tage_counterLength.W))
  private val T2_pred = Mem(params.component_nEntries,UInt(params.tage_counterLength.W))
  private val T3_pred = Mem(params.component_nEntries,UInt(params.tage_counterLength.W))
  private val T4_pred = Mem(params.component_nEntries,UInt(params.tage_counterLength.W))

  private val T1_tag = Mem(params.component_nEntries,UInt(params.tagbits.W))
  private val T2_tag = Mem(params.component_nEntries,UInt(params.tagbits.W))
  private val T3_tag = Mem(params.component_nEntries,UInt(params.tagbits.W))
  private val T4_tag = Mem(params.component_nEntries,UInt(params.tagbits.W))

  private val T1_u = Mem(params.component_nEntries, UInt(params.ubits.W))
  private val T2_u = Mem(params.component_nEntries, UInt(params.ubits.W))
  private val T3_u = Mem(params.component_nEntries, UInt(params.ubits.W))
  private val T4_u = Mem(params.component_nEntries, UInt(params.ubits.W))

  private val reset_waddr_T1 = RegInit(0.U((params.component_nEntries.log2+1).W))
  private val resetting_T1 = !reset_waddr_T1(params.component_nEntries.log2)
  private val wen_T1_pred = WireInit(resetting_T1)
  private val wen_T1_tag = WireInit(resetting_T1)
  private val wen_T1_u = WireInit(resetting_T1)
  private val waddr_T1 = WireInit(reset_waddr_T1)
  private val wdata_T1_pred = WireInit(0.U)
  private val wdata_T1_tag = WireInit(0.U)
  private val wdata_T1_u = WireInit(0.U)
  when (resetting_T1) { reset_waddr_T1 := reset_waddr_T1 + 1.U }
  when (wen_T1_pred) { T1_pred(waddr_T1) := wdata_T1_pred }
  when (wen_T1_tag) { T1_tag(waddr_T1) := wdata_T1_tag }
  when (wen_T1_u) { T1_u(waddr_T1) := wdata_T1_u }

  private val reset_waddr_T2 = RegInit(0.U((params.component_nEntries.log2 + 1).W))
  private val resetting_T2 = !reset_waddr_T2(params.component_nEntries.log2)
  private val wen_T2_pred = WireInit(resetting_T2)
  private val wen_T2_tag = WireInit(resetting_T2)
  private val wen_T2_u = WireInit(resetting_T2)
  private val waddr_T2 = WireInit(reset_waddr_T2)
  private val wdata_T2_pred = WireInit(0.U)
  private val wdata_T2_tag = WireInit(0.U)
  private val wdata_T2_u = WireInit(0.U)
  when (resetting_T2) { reset_waddr_T2 := reset_waddr_T2 + 1.U }
  when (wen_T2_pred) { T2_pred(waddr_T2) := wdata_T2_pred }
  when (wen_T2_tag) { T2_tag(waddr_T2) := wdata_T2_tag }
  when (wen_T2_u) { T2_u(waddr_T2) := wdata_T2_u }

  private val reset_waddr_T3 = RegInit(0.U((params.component_nEntries.log2 + 1).W))
  private val resetting_T3 = !reset_waddr_T3(params.component_nEntries.log2)
  private val wen_T3_pred = WireInit(resetting_T3)
  private val wen_T3_tag = WireInit(resetting_T3)
  private val wen_T3_u = WireInit(resetting_T3)
  private val waddr_T3 = WireInit(reset_waddr_T3)
  private val wdata_T3_pred = WireInit(0.U)
  private val wdata_T3_tag = WireInit(0.U)
  private val wdata_T3_u = WireInit(0.U)
  when (resetting_T3) { reset_waddr_T3 := reset_waddr_T3 + 1.U }
  when (wen_T3_pred) { T3_pred(waddr_T3) := wdata_T3_pred }
  when (wen_T3_tag) { T3_tag(waddr_T3) := wdata_T3_tag }
  when (wen_T3_u) { T3_u(waddr_T3) := wdata_T3_u }

  private val reset_waddr_T4 = RegInit(0.U((params.component_nEntries.log2 + 1).W))
  private val resetting_T4 = !reset_waddr_T4(params.component_nEntries.log2)
  private val wen_T4_pred = WireInit(resetting_T4)
  private val wen_T4_tag = WireInit(resetting_T4)
  private val wen_T4_u = WireInit(resetting_T4)
  private val waddr_T4 = WireInit(reset_waddr_T4)
  private val wdata_T4_pred = WireInit(0.U)
  private val wdata_T4_tag = WireInit(0.U)
  private val wdata_T4_u = WireInit(0.U)
  when (resetting_T4) { reset_waddr_T4 := reset_waddr_T4 + 1.U }
  when (wen_T4_pred) { T4_pred(waddr_T4) := wdata_T4_pred }
  when (wen_T4_tag) { T4_tag(waddr_T4) := wdata_T4_tag }
  when (wen_T4_u) { T4_u(waddr_T4) := wdata_T4_u }

  //perceptron predictor
  private val perceptron_table = Mem((params.perceptron_nEntries), Vec(params.perceptron_historyLength, SInt(params.perceptron_counterLength.W))) //weigth table
  private val bias = RegInit(VecInit(Seq.fill((params.perceptron_nEntries))(0.S(params.perceptron_counterLength.W)))) //bias table
  val perceptron_history = RegInit(0.U(params.perceptron_historyLength.W))
  private val perceptron_reset_waddr = RegInit(0.U((params.perceptron_nEntries.log2+1).W))
  private val perceptron_resetting = !perceptron_reset_waddr(params.perceptron_nEntries.log2)
  private val perceptron_wen = WireInit(perceptron_resetting)
  private val perceptron_waddr = WireInit(perceptron_reset_waddr)
  private val perceptron_wdata = VecInit(Seq.fill(params.perceptron_historyLength)(0.S(params.perceptron_counterLength.W)))
  private val perceptron_wdata_bias = WireInit(0.S(params.perceptron_counterLength.W))
  private val perceptron_threshold = RegInit(15.S(8.W)) //perceptron_threshold value, in real, it is always positive, but for compare weight sum(its type is SInt), make type to be SInt
  private val perceptron_TC_counter = RegInit(64.U(7.W)) //TC counter: count the number of updates at correct and miss for dynamic threshold fitting
  private val perceptron_tc_max = (1.U << 7) - 1.U
  private val perceptron_pos_zero = (1.U << 6)
  private val perceptron_neg_zero = (1.U << 6) - 1.U
  when (perceptron_resetting) { perceptron_reset_waddr := perceptron_reset_waddr + 1.U }
  when (perceptron_wen) { perceptron_table(perceptron_waddr) := perceptron_wdata }
  when (perceptron_wen) { bias(perceptron_waddr) := perceptron_wdata_bias }

  //meta selector: MSB = 0 -> select TAGE, MSB = 1 -> select Perceptron
  private val meta_table = Mem(params.meta_nEntries,UInt(params.meta_ctr.W))
  private val meta_reset_waddr = RegInit(0.U((params.meta_nEntries.log2 + 1).W))
  private val meta_resetting = !meta_reset_waddr(params.meta_nEntries.log2)
  private val meta_wen = WireInit(meta_resetting)
  private val meta_waddr = WireInit(meta_reset_waddr)
  private val meta_wdata = WireInit(1.U)
  private val meta_value_max = (1.U << params.meta_ctr) - 1.U
  when (meta_resetting) { meta_reset_waddr := meta_reset_waddr + 1.U}
  when (meta_wen) { meta_table(meta_waddr) := meta_wdata}

  // loop predictor
  // Loop table definition
  // private val ltable = Reg(Vec(params.loop_nEntries, new LoopEntry(params)))
  private val ltable = RegInit(VecInit(Seq.fill(params.loop_nEntries)(0.U.asTypeOf(new LoopEntry(params)))))
  private val ageTickCounter = RegInit(0.U(params.loop_ageTickLength.W))

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
  val loopCount = RegInit(0.U(32.W))
  val loopMissCount = RegInit(0.U(32.W))

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
        when(res.loop_hybrid === 0.U){
          val PC_address = io.bht_update.bits.pc
          loopCount := loopCount + 1.U
          printf(cf"used Loop, PC: $PC_address, lcount: $loopCount\n")
          bht.printltable(res.loop_index)
        }/*.otherwise{
          printf(cf"used hybrid\n")
        }*/
        bht.updateTable(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken, io.bht_update.bits.mispredict)
        
        when (io.bht_update.bits.mispredict) {
          mispredictCount := mispredictCount + 1.U
          printf(cf"misprediction count = $mispredictCount\n")
          val PC_address = io.bht_update.bits.pc
          printf(cf"miss PC address : $PC_address\n")
          bht.updateHistory(io.bht_update.bits.pc, io.bht_update.bits.prediction, io.bht_update.bits.taken)
          
          when(res.loop_hybrid === 0.U){
            loopMissCount := loopMissCount + 1.U
            printf(cf"Loop Miss, lmisscount: $loopMissCount\n")
            bht.printltable(res.loop_index)
          }
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
