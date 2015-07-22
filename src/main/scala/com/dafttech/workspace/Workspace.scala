package com.dafttech.workspace

import java.nio.file.Paths

import com.dafttech.logic.Signal._
import com.dafttech.logic.ic.{AndGate, IC, OrGate, XOrGate}
import com.dafttech.logic.{Field, Signal, Utils}

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
      println((e._1.mkString(",\t") + "\t=> " + e._2.mkString(",\t")).replaceAll("true", "1").replaceAll("false", "0"))
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

  def S(a: Signal, b: Signal, c: Signal) = (a XOR b) XOR c

  def Co(a: Signal, b: Signal, c: Signal) = ((a XOR b) AND c) OR (a AND b)

  def adder = {

    //in0..2 = 3bit input; in3 = OutEnable; in4 = invert
    val ic = IC(13, 8)

    val a0 = ic.in(0)
    val a1 = ic.in(1)
    val a2 = ic.in(2)
    val a3 = ic.in(3)

    val b0 = ic.in(4)
    val b1 = ic.in(5)
    val b2 = ic.in(6)
    val b3 = ic.in(7)

    val c0 = ic.in(8)

    val c1 = Co(a0, b0, c0)
    val c2 = Co(a1, b1, c1)
    val c3 = Co(a2, b2, c2)
    val c4 = Co(a3, b3, c3)

    ic.out(0) = S(a0, b0, c0)
    ic.out(1) = S(a1, b1, c1)
    ic.out(2) = S(a2, b2, c2)
    ic.out(3) = S(a3, b3, c3)

    ic.out(4) = c4

    Utils.writeBin(ic, Paths.get("output.bin"))

  }

  def STATE_PROM = {
    val ic = IC(13, 8)

    val STATE = Field(ic.in(0), ic.in(1), ic.in(2), ic.in(3))
    val IS_NOP = ic.in(4)
    val IS_JMP = ic.in(5)

    /*  DO_TEND */ ic.out(0) = !Signal((((STATE.value == 0) AND IS_NOP) OR ((STATE.value == 2) AND IS_JMP)).value)
    /* !LOAD_CP */ ic.out(1) = true
    /* !CTEN_CP */ ic.out(2) = !Signal(STATE.value < 4)

    Utils.writeBin(ic, Paths.get("state_prom.bin"))
  }

  STATE_PROM

  /*val f = Field(3)

  val f2 = Field(f.signal(0), f.signal(1), f.signal(2))

  println(f2.value)*/
}
