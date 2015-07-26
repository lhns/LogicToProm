package com.dafttech.logic

import java.nio.file.{Files, Path}

import com.dafttech.logic.ic.IC

import scala.collection.mutable.ListBuffer

/**
 * Created by LolHens on 21.07.2015.
 */
object Utils {
  def intToBooleanList(int: Int, size: Int): List[Boolean] = {
    var list = ListBuffer[Boolean]()

    for (i <- 0 until size)
      list += (((int >>> i) & 1) != 0)

    list.toList
  }

  def booleanListToInt(list: List[Boolean]): Int = {
    var int: Int = 0

    for (i <- 0 until list.size)
      int |= (if (list(i)) 1 else 0) << i

    int
  }

  def toBin(ic: IC): Array[Byte] = {
    val table = ic.table

    val bytesPerIn: Int = Math.ceil(ic.out.size / 8.0).toInt

    val array = new Array[Byte](table.size * bytesPerIn)

    for (entry <- 0 until table.size)
      for (byteNum <- 0 until bytesPerIn)
        array(entry * bytesPerIn + byteNum) = ((booleanListToInt(table(intToBooleanList(entry, ic.in.size))) >>> (byteNum * 8)) & 0xFF).toByte

    array
  }

  def writeBin(ic: IC, path: Path) = Files.write(path, toBin(ic))
}
