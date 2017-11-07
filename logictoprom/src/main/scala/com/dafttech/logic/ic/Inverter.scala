package com.dafttech.logic.ic

/**
  * Created by LolHens on 22.07.2015.
  */
class Inverter private(size: Int) extends IC(size, size) {
  for (i <- 0 until size) out(i) = !in(i)
}

object Inverter {
  def apply(size: Int) = new Inverter(size)
}