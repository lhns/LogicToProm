package com.dafttech.logic

import com.dafttech.logic.ic.IC

/**
 * Created by LolHens on 21.07.2015.
 */
object Utils {
  def intToBooleanArray(int: Int, size: Int): Array[Boolean] = {
    val array = new Array[Boolean](size)

    for (i <- 0 until array.size)
      array(i) = ((int >>> i) & 1) != 0

    array
  }

  def booleanArrayToInt(array: Array[Boolean]): Int = {
    var int: Int = 0

    for (i <- 0 until array.size)
      int |= (if (array(i)) 1 else 0) << i

    int
  }

  def toBin(ic: IC): Array[Byte] = {
    val table = ic.table

    val array = new Array[Byte](table.size)

    for (i <- 0 until array.size)
      array(i) = booleanArrayToInt(table(intToBooleanArray(i, ic.in.size))).toByte

    array
  }
}
