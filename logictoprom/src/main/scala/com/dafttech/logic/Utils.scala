package com.dafttech.logic

import com.dafttech.logic.ic.IC

import java.nio.file.{Files, Path}

/**
 * Created by LolHens on 21.07.2015.
 */
object Utils {
  def intToBooleanList(int: Int, size: Int): List[Boolean] =
    (0 until size)
      .map(i => ((int >>> i) & 1) != 0)
      .toList

  def booleanListToInt(list: List[Boolean]): Int =
    list.indices.foldLeft(0)((last, i) =>
      last | (if (list(i)) 1 else 0) << i
    )

  def toBin(ic: IC): Array[Byte] = {
    val table = ic.table.toMap

    val bytesPerIn: Int = (ic.out.size + (8 - 1)) / 8

    val array = new Array[Byte](table.size * bytesPerIn)

    for {
      entry <- 0 until table.size
      byteNum <- 0 until bytesPerIn
      result = booleanListToInt(table(intToBooleanList(entry, ic.in.size)))
    } array(entry * bytesPerIn + byteNum) = ((result >>> (byteNum * 8)) & 0xFF).toByte

    array
  }

  def writeBin(ic: IC, path: Path): Unit = Files.write(path, toBin(ic))

  def tabSeparatedLogicTable(ic: IC): String = {
    ic.table
      .sortBy(_._1.map(if (_) 1 else 0).mkString)
      .map {
        case (in, out) =>
          (in ++ out).map(if (_) 1 else 0).mkString("\t")
      }
      .mkString("\n")
  }
}
