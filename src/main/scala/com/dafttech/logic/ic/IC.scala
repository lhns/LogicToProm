package com.dafttech.logic.ic

import com.dafttech.logic.ic.IC.Pins
import com.dafttech.logic.{Signal, Utils}

/**
 * Created by LolHens on 21.07.2015.
 */
class IC protected(inPins: Int, outPins: Int) {

  object in extends Pins(inPins)

  object out extends Pins(outPins)

  private def row(inArray: Array[Boolean]): Array[Boolean] = {
    for (i <- 0 until inArray.size) in(i) = Signal(inArray(i))

    val outArray = new Array[Boolean](out.size)
    for (i <- 0 until outArray.size) outArray(i) = out(i).value

    outArray
  }

  def table: Map[Array[Boolean], Array[Boolean]] = {
    var map = Map[Array[Boolean], Array[Boolean]]()

    for (i <- 0 until Math.pow(2, in.size).toInt) {
      val array = Utils.intToBooleanArray(i, in.size)
      map += array -> row(array)
    }

    map
  }
}

object IC {
  def apply(inPins: Int, outPins: Int): IC = new IC(inPins, outPins)

  abstract class Pins(val size: Int) {
    private val arrayIn = new Array[Signal](size)

    private val arrayOut = new Array[Signal](size)

    for (i <- 0 until size) arrayOut(i) = Signal(arrayIn(i) match {
      case null => false
      case signal => signal.value
    })

    def update(i: Int, signalProvider: Signal) = i match {
      case _ if (i < 0 || i >= size) => throw new IndexOutOfBoundsException()
      case _ => arrayIn(i) = signalProvider
    }

    def apply(i: Int): Signal = i match {
      case _ if (i < 0 || i >= size) => throw new IndexOutOfBoundsException()
      case _ => arrayOut(i)
    }
  }

}
