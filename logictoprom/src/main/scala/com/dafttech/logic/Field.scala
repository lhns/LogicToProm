package com.dafttech.logic

import com.dafttech.logic.Field.Ref

import scala.language.implicitConversions

/**
  * Created by LolHens on 22.07.2015.
  */
abstract class Field {
  def value: Int

  def ref = Ref(this)

  object signal {
    private[this] var cache: Option[Array[Signal]] = None

    def apply(i: Int): Signal = {
      require(i >= 0)

      val signals = cache.getOrElse {
        val signals = new Array[Signal](32)
        cache = Some(signals)
        signals
      }

      Option(signals(i)).getOrElse {
        val signal = Signal(((value >>> i) & 1) != 0)
        signals(i) = signal
        signal
      }
    }
  }


  def +(field: Field) = Field(value + field.value)

  def -(field: Field) = Field(value - field.value)

  def *(field: Field) = Field(value * field.value)

  def /(field: Field) = Field {
    val thatValue = field.value
    if (thatValue == 0) 0 else value / thatValue
  }

  def %(field: Field) = Field {
    val thatValue = field.value
    if (thatValue == 0) 0 else value % thatValue
  }

  def &(field: Field) = Field(value & field.value)

  def |(field: Field) = Field(value | field.value)

  def ^(field: Field) = Field(value ^ field.value)

  def >>(field: Field) = Field(value >> field.value)

  def <<(field: Field) = Field(value << field.value)

  def unary_~() = Field(~value)


  def ==(field: Field): Signal = Signal(value == field.value)

  def !=(field: Field): Signal = Signal(value != field.value)

  def >(field: Field): Signal = Signal(value > field.value)

  def <(field: Field): Signal = Signal(value < field.value)

  def >=(field: Field): Signal = Signal(value >= field.value)

  def <=(field: Field): Signal = Signal(value <= field.value)


  def ==(int: Int): Signal = Signal(value == int)

  def !=(int: Int): Signal = Signal(value != int)
}

object Field {
  def apply(_value: => Int): Field = new Field {
    override def value: Int = _value
  }

  def apply(field: => Field)(implicit dummyImplicit: DummyImplicit): Field = new Field {
    override def value: Int = field.value
  }

  def apply(signals: Signal*): Field = Field(signalsToField(signals))

  private[this] def signalsToField(signals: Seq[Signal]) =
    signals.indices.foldLeft(0)((last, i) =>
      last | (if (signals(i).value) 1 else 0) << i
    )


  implicit def intToField(int: Int): Field = Field(int)


  class Ref private(var field: Field) extends Field {
    override def value: Int = Option(field).map(_.value).getOrElse(0)
  }

  object Ref {
    def apply(field: Field): Ref = new Ref(field)

    def apply(): Ref = apply(null)
  }

}