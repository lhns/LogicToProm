package com.dafttech.logic

import com.dafttech.logic.Signal.Ref

import scala.language.implicitConversions

/**
 * Created by LolHens on 21.07.2015.
 */
abstract class Signal {
  def value: Boolean

  def ref: Ref = Ref(this)


  def &&(signal: Signal): Signal = Signal(value && signal.value)

  def ||(signal: Signal): Signal = Signal(value || signal.value)

  def ^(signal: Signal): Signal = Signal(value ^ signal.value)

  def AND(signal: Signal): Signal = &&(signal)

  def OR(signal: Signal): Signal = ||(signal)

  def XOR(signal: Signal): Signal = ^(signal)

  def unary_! : Signal = Signal(!value)


  def ==(signal: Signal): Signal = Signal(value == signal.value)

  def !=(signal: Signal): Signal = Signal(value != signal.value)


  def ==(boolean: Boolean): Signal = Signal(value == boolean)

  def !=(boolean: Boolean): Signal = Signal(value != boolean)
}

object Signal {
  def apply(_value: => Boolean): Signal = new Signal {
    override def value: Boolean = _value
  }

  def apply(signal: => Signal)(implicit dummyImplicit: DummyImplicit): Signal = new Signal {
    override def value: Boolean = signal.value
  }


  implicit def booleanToSignal(boolean: Boolean): Signal = Signal(boolean)


  class Ref private(var signal: Signal) extends Signal {
    override def value: Boolean = Option(signal).exists(_.value)
  }

  object Ref {
    def apply(signal: Signal): Ref = new Ref(signal)

    def apply(): Ref = apply(null)
  }

}