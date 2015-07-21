package com.dafttech.workspace

import com.dafttech.logic.{IC, Signal}

/**
 * Created by LolHens on 21.07.2015.
 */
class Workspace {
  def test1 = {
    val on = Signal(true)
    println(on.value)
    println((!on).value)
  }

  def testTable = {
    val ic = IC(2, 1)

    val variable = ic.in(0) && ic.in(1)

    ic.out(0) = !variable

    ic.table.foreach(e => {
      println(e._1.mkString(",\t") + "\t=> " + e._2.mkString(",\t"))
    })
  }

  testTable

}
