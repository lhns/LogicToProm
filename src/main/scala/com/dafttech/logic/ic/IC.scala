package com.dafttech.logic.ic

import com.dafttech.logic.Signal
import com.dafttech.logic.ic.IC.Pins

/**
 * Created by LolHens on 21.07.2015.
 */
class IC protected(inPins: Int, outPins: Int) {

  object in extends Pins(inPins)

  object out extends Pins(outPins)

  private def row(inArray: Array[Boolean]): Array[Boolean] = {
    for (i <- 0 until inArray.length) in(i) = Signal(inArray(i))

    val outArray = new Array[Boolean](out.length)
    for (i <- 0 until outArray.length) outArray(i) = out(i).value

    outArray
  }

  def table: Map[Array[Boolean], Array[Boolean]] = {
    var map = Map[Array[Boolean], Array[Boolean]]()

    for (combination <- 0 until Math.pow(2, in.length).toInt) {
      val inArray = new Array[Boolean](in.length)
      for (bitPos <- 0 until inArray.length) inArray(bitPos) = ((combination >>> bitPos) & 1) != 0

      map += inArray -> row(inArray)
    }

    map
  }
}

object IC {
  def apply(inPins: Int, outPins: Int): IC = new IC(inPins, outPins)

  abstract class Pins(val length: Int) {
    private val arrayIn = new Array[Signal](length)

    private val arrayOut = new Array[Signal](length)

    for (i <- 0 until length) arrayOut(i) = Signal(arrayIn(i) match {
      case null => false
      case signal => signal.value
    })

    def update(i: Int, signalProvider: Signal) = i match {
      case _ if (i < 0 || i >= length) => throw new IndexOutOfBoundsException()
      case _ => arrayIn(i) = signalProvider
    }

    def apply(i: Int): Signal = i match {
      case _ if (i < 0 || i >= length) => throw new IndexOutOfBoundsException()
      case _ => arrayOut(i)
    }
  }

}
