package com.dafttech.logic

/**
 * Created by LolHens on 22.07.2015.
 */
abstract class Field {
  def value: Int

  object signal {
    private[this] var cache: Option[Array[Signal]] = None

    def apply(i: Int) = {
      val signals = cache match {
        case None =>
          val signals = new Array[Signal](32)
          cache = Some(signals)
          signals
        case Some(cache) => cache
      }
      signals(i) match {
        case null =>
          val signal = Signal(((value >>> i) & 1) != 0)
          signals(i) = signal
          signal
        case signal => signal
      }
    }
  }

  def +(field: Field) = Field(value + field.value)

  def -(field: Field) = Field(value - field.value)

  def *(field: Field) = Field(value * field.value)

  def /(field: Field) = Field(value / field.value)


  def ==(field: Field) = value == field.value

  def !=(field: Field) = value != field.value

  def >(field: Field) = value > field.value

  def <(field: Field) = value < field.value

  def >=(field: Field) = value >= field.value

  def <=(field: Field) = value <= field.value
}

object Field {
  def apply(_value: => Int) = new Field {
    override def value: Int = _value
  }

  def apply(field: => Field) = new Field {
    override def value: Int = field.value
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