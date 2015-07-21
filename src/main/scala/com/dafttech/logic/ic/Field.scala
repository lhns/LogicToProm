package com.dafttech.logic.ic

import com.dafttech.logic.Signal

/**
 * Created by LolHens on 22.07.2015.
 */
class Field(size: Int) extends IC(size, size) {
  def value: Int = {
    var int = 0

    for (i <- 0 until in.size) int |= (if (in(i).value) 1 else 0) << i

    int
  }

  def value_=(int: Int): Unit = {
    for (i <- 0 until out.size) out(i) = Signal(((int >>> i) & 1) != 0)
  }
}
