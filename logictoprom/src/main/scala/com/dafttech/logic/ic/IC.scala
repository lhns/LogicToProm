package com.dafttech.logic.ic

import com.dafttech.logic.ic.IC.Pins
import com.dafttech.logic.{Signal, Utils}

/**
 * Created by LolHens on 21.07.2015.
 */
class IC protected(inPins: Int, outPins: Int) {

  object in extends Pins(inPins)

  object out extends Pins(outPins)

  private def row(inList: List[Boolean]): List[Boolean] = {
    for (i <- inList.indices) in(i) = Signal(inList(i))
    (0 until out.size).map(i => out(i).value).toList
  }

  def table: Seq[(List[Boolean], List[Boolean])] =
    (0 until Math.pow(2, in.size).toInt).map { i =>
      val list: List[Boolean] = Utils.intToBooleanList(i, in.size)
      list -> row(list)
    }
}

object IC {
  def apply(inPins: Int, outPins: Int): IC = new IC(inPins, outPins)

  abstract class Pins(val size: Int) {
    private[this] val array = new Array[Signal.Ref](size)

    for (i <- 0 until size) array(i) = Signal.Ref()


    def update(i: Int, signal: Signal): Unit = {
      require(i >= 0 && i < size)
      array(i).signal = signal
    }

    def apply(i: Int): Signal.Ref = {
      require(i >= 0 && i < size)
      array(i)
    }
  }

}
