package com.dafttech.workspace

import com.dafttech.logic.Signal
import com.dafttech.logic.ic.{AndGate, IC, OrGate, XOrGate}

/**
 * Created by LolHens on 21.07.2015.
 */
class Workspace {
  def test1 = {
    val on = Signal(true)
    println(on.value)
    println((!on).value)
  }

  def testTable(ic: IC) = {
    ic.table.foreach(e => {
      println(e._1.mkString(",\t") + "\t=> " + e._2.mkString(",\t"))
    })
  }

  def testIC = {
    val ic = IC(2, 1)

    val variable = ic.in(0) && ic.in(1)

    ic.out(0) = !variable

    testTable(ic)
  }

  def testICOutDelegate = {
    val ic = IC(1, 1)

    val out = ic.out(0)

    ic.out(0) = ic.in(0)

    ic.in(0) = Signal(true)

    println(ic.out(0).value)
  }

  def testGates = {
    println("AND:")
    testTable(AndGate())
    println()

    println("OR:")
    testTable(OrGate())
    println()

    println("XOR:")
    testTable(XOrGate())
  }

  testGates
}
