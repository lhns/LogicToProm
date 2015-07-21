package com.dafttech.logic

/**
 * Created by LolHens on 21.07.2015.
 */
class IC private(inPins: Int, outPins: Int) {

  object in {
    val length = inPins

    def apply(i: Int): Signal = i match {
      case _ if (i < 0 || i >= length) => throw new IndexOutOfBoundsException()
      case _ => array(i)
    }

    private[IC] class InPin extends Signal {
      var value: Boolean = false
    }

    private[IC] val array = new Array[InPin](length)

    for (i <- 0 until length) array(i) = new InPin()
  }

  object out {
    val length = outPins

    def update(i: Int, signalProvider: Signal) = i match {
      case _ if (i < 0 || i >= length) => throw new IndexOutOfBoundsException()
      case _ => array(i) = signalProvider
    }

    def apply(i: Int): Boolean = i match {
      case _ if (i < 0 || i >= length) => throw new IndexOutOfBoundsException()
      case _ => array(i).value
    }

    private[IC] val array = new Array[Signal](length)
  }

  private def row(inArray: Array[Boolean]): Array[Boolean] = {
    for (i <- 0 until inArray.length) in.array(i).value = inArray(i)

    val outArray = new Array[Boolean](out.length)
    for (i <- 0 until outArray.length) outArray(i) = out(i)

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
}
