package com.dafttech.logic.ic

/**
 * Created by LolHens on 21.07.2015.
 */
class AndGate private() extends IC(2, 1) {
  val a = in(0)

  val b = in(1)

  val output = out(0)
  
  out(0) = a && b
}

object AndGate {
  def apply() = new AndGate()
}