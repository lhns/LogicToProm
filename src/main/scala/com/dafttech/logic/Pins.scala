package com.dafttech.logic

/**
 * Created by LolHens on 21.07.2015.
 */
abstract class Pins(val length: Int) {
  def update(i: Int, signalProvider: Signal) = i match {
    case _ if (i < 0 || i >= length) => throw new IndexOutOfBoundsException()
    case _ => arrayIn(i) = signalProvider
  }

  def apply(i: Int): Signal = i match {
    case _ if (i < 0 || i >= length) => throw new IndexOutOfBoundsException()
    case _ => arrayOut(i)
  }

  private val arrayIn = new Array[Signal](length)

  private val arrayOut = new Array[Signal](length)

  for (i <- 0 until length) arrayOut(i) = Signal(arrayIn(i) match {
    case null => false
    case signal => signal.value
  })
}
