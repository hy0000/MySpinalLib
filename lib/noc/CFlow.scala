package noc

import spinal.core._
import spinal.lib._

class CFlowFragmentFactory extends MSFactory {
  def apply[T <: Data](dataType: HardType[T]): CFlow[Fragment[T]] = {
    val ret = new CFlow(Fragment(dataType))
    postApply(ret)
    ret
  }
}

class CFlowFactory extends MSFactory {
  object Fragment extends CFlowFragmentFactory

  def apply[T <: Data](hardType: HardType[T]): CFlow[T] = {
    val ret = new CFlow(hardType)
    postApply(ret)
    ret
  }

  def apply[T <: Data](hardType: => T): CFlow[T] = apply(HardType(hardType))
}

object CFlow extends CFlowFactory

class CFlow[T <: Data](val payloadType: HardType[T]) extends Bundle with IMasterSlave with DataCarrier[T] {

  val valid   = Bool()
  val payload = payloadType()
  val credit  = Bool()

  override def clone: CFlow[T] = CFlow(payloadType).asInstanceOf[this.type]

  override def asMaster(): Unit = {
    out(valid, payload)
    in(credit)
  }

  override def fire: Bool = valid

  override type RefOwnerType = this.type

  override def freeRun(): this.type = this

  def <<(that: CFlow[T]): CFlow[T] = {
    this.valid   := that.valid
    that.credit  := this.credit
    this.payload := that.payload
    that
  }
  
  def >>(into: CFlow[T]): CFlow[T] = {
    into << this
    into
  }
}
