package com.dafttech.logic

/**
 * Created by LolHens on 22.07.2015.
 */
abstract class Field {
  def value: Int

  object signal {
    def apply(i: Int) = Signal(((value >>> i) & 1) != 0)
  }

  def +(field: Field) = Field(value + field.value)
}

object Field {
  def apply(_value: => Int) = new Field {
    override def value: Int = _value
  }

  def apply(signals: Signal*): Field = Field(signalsToField(signals))

  private[this] def signalsToField(signals: Seq[Signal]) = {
    var int = 0

    for (i <- 0 until signals.size)
      int |= (if (signals(i).value) 1 else 0) << i

    int
  }

  implicit def intToField(int: Int): Field = Field(int)
}