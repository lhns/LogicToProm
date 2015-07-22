package com.dafttech.logic

import com.dafttech.logic.Signal.Ref

/**
 * Created by LolHens on 21.07.2015.
 */
abstract class Signal {
  def value: Boolean

  def ref = Ref(this)


  def &&(signal: Signal) = Signal(value && signal.value)

  def ||(signal: Signal) = Signal(value || signal.value)

  def ^(signal: Signal) = Signal(value ^ signal.value)

  def AND(signal: Signal) = &&(signal)

  def OR(signal: Signal) = ||(signal)

  def XOR(signal: Signal) = ^(signal)

  def unary_! = Signal(!value)


  def ==(signal: Signal) = value == signal.value

  def !=(signal: Signal) = value != signal.value


  def ==(boolean: Boolean) = value == boolean

  def !=(boolean: Boolean) = value != boolean
}

object Signal {
  def apply(_value: => Boolean) = new Signal {
    override def value: Boolean = _value
  }

  def apply(signal: => Signal)(implicit dummyImplicit: DummyImplicit) = new Signal {
    override def value: Boolean = signal.value
  }


  implicit def booleanToSignal(boolean: Boolean): Signal = Signal(boolean)


  class Ref private(var signal: Signal) extends Signal {
    override def value: Boolean = signal match {
      case null => false
      case field => signal.value
    }
  }

  object Ref {
    def apply(signal: Signal) = new Ref(signal)

    def apply(): Ref = apply(null)
  }

}