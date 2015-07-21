package com.dafttech.logic

/**
 * Created by LolHens on 21.07.2015.
 */
abstract class Signal {
  def value: Boolean

  def &&(signal: Signal) = Signal(value && signal.value)

  def ||(signal: Signal) = Signal(value || signal.value)

  def ^(signal: Signal) = Signal(value ^ signal.value)

  def unary_! = Signal(!value)
}

object Signal {
  def apply(_value: => Boolean) = new Signal {
    override def value: Boolean = _value
  }
}