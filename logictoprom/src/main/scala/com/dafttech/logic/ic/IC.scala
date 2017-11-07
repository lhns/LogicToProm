package com.dafttech.logic.ic

import com.dafttech.logic.ic.IC.Pins
import com.dafttech.logic.{Signal, Utils}

import scala.collection.mutable.ListBuffer

/**
  * Created by LolHens on 21.07.2015.
  */
class IC protected(inPins: Int, outPins: Int) {

  object in extends Pins(inPins)

  object out extends Pins(outPins)

  private def row(inList: List[Boolean]): List[Boolean] = {
    for (i <- 0 until inList.size) in(i) = Signal(inList(i))

    val outList = ListBuffer[Boolean]()
    for (i <- 0 until out.size) outList += out(i).value

    outList.toList
  }

  def table: Map[List[Boolean], List[Boolean]] = {
    var map = Map[List[Boolean], List[Boolean]]()

    for (i <- 0 until Math.pow(2, in.size).toInt) {
      val list = Utils.intToBooleanList(i, in.size)
      map += list -> row(list)
    }

    map
  }
}

object IC {
  def apply(inPins: Int, outPins: Int): IC = new IC(inPins, outPins)

  abstract class Pins(val size: Int) {
    private[this] val array = new Array[Signal.Ref](size)

    for (i <- 0 until size) array(i) = Signal.Ref()


    def update(i: Int, signal: Signal): Unit = i match {
      case _ if i < 0 || i >= size => throw new IndexOutOfBoundsException()
      case _ => array(i).signal = signal
    }

    def apply(i: Int): Signal.Ref = i match {
      case _ if i < 0 || i >= size => throw new IndexOutOfBoundsException()
      case _ => array(i)
    }
  }

}