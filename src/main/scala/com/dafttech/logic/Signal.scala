package com.dafttech.logic

/**
 * Created by LolHens on 21.07.2015.
 */
abstract class Signal {
  def value: Boolean

  def &&(signal: Signal) = Signal(value && signal.value)

  def ||(signal: Signal) = Signal(value || signal.value)

  def ^(signal: Signal) = Signal(value ^ signal.value)

  def AND(signal: Signal) = &&(signal)

  def OR(signal: Signal) = ||(signal)

  def XOR(signal: Signal) = ^(signal)

  def unary_! = Signal(!value)


  def ==(signal: Signal) = value == signal.value

  def !=(signal: Signal) = value != signal.value
}

object Signal {
  def apply(_value: => Boolean) = new Signal {
    override def value: Boolean = _value
  }

  def apply(signal: => Signal) = new Signal {
    override def value: Boolean = signal.value
  }

  implicit def booleanToSignal(boolean: Boolean): Signal = Signal(boolean)
}