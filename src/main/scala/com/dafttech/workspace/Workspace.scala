package com.dafttech.workspace

import java.nio.file.Paths

import com.dafttech.logic.Signal._
import com.dafttech.logic.ic._
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
    val INSTRUCTION = Field(ic.in(4), ic.in(5), ic.in(6), ic.in(7), ic.in(8), ic.in(9), ic.in(10), ic.in(11))

    val READ_REG = Field(ic.in(4), ic.in(5), ic.in(6))
    val WRITE_REG = Field(ic.in(7), ic.in(8), ic.in(9))
    val JMP = Field(ic.in(10), ic.in(11))

    val DO_TEND = ic.out(0)
    val LOAD_IP = ic.out(1)
    val CTEN_CP = ic.out(2)
    val FTCH_RA = ic.out(3)
    val FTCH_RB = ic.out(4)
    val SEL_CPA = ic.out(5)
    val MBUS_RDEN = ic.out(6)
    val RBUS_WREN = ic.out(7)

    DO_TEND.signal = !Signal(((STATE == 1) AND (INSTRUCTION == 0))
      OR ((STATE == 3) AND (INSTRUCTION == 1))
      OR (STATE == 5))
    CTEN_CP.signal = !Signal((STATE == 1) OR ((STATE == 3) AND (READ_REG == 4)))
    FTCH_RA.signal = Signal(STATE == 0)
    FTCH_RB.signal = Signal((STATE == 2) AND (READ_REG == 4))
    SEL_CPA.signal = Signal(STATE < Field(3))
    LOAD_IP.signal = !Signal(JMP != 0)

    MBUS_RDEN.signal = !Signal((STATE < Field(4)) OR (READ_REG == 7))
    RBUS_WREN.signal = Signal(STATE == 4)


    Utils.writeBin(ic, Paths.get("state_prom.bin"))
  }

  def SWITCH_NOP_NOTNOP = {
    val ic = IC(13, 8)

    val STATE = Field(ic.in(1), ic.in(2), ic.in(3))

    for (i <- 0 until 8) {
      ic.out(i) = Signal(STATE == i)
    }

    Utils.writeBin(ic, Paths.get("switch.bin"))
  }

  def ADD_SUB = {
    val adder = IC(16, 16)

    val in1 = Field(
      adder.in(0),
      adder.in(1),
      adder.in(2),
      adder.in(3),
      adder.in(4),
      adder.in(5),
      adder.in(6),
      adder.in(7))

    val in2 = Field(
      adder.in(8),
      adder.in(9),
      adder.in(10),
      adder.in(11),
      adder.in(12),
      adder.in(13),
      adder.in(14),
      adder.in(15))

    val out1 = Field.Ref()
    adder.out(0).signal = out1.signal(0)
    adder.out(1).signal = out1.signal(1)
    adder.out(2).signal = out1.signal(2)
    adder.out(3).signal = out1.signal(3)
    adder.out(4).signal = out1.signal(4)
    adder.out(5).signal = out1.signal(5)
    adder.out(6).signal = out1.signal(6)
    adder.out(7).signal = out1.signal(7)

    val out2 = Field.Ref()
    adder.out(8).signal = out2.signal(0)
    adder.out(9).signal = out2.signal(1)
    adder.out(10).signal = out2.signal(2)
    adder.out(11).signal = out2.signal(3)
    adder.out(12).signal = out2.signal(4)
    adder.out(13).signal = out2.signal(5)
    adder.out(14).signal = out2.signal(6)
    adder.out(15).signal = out2.signal(7)

    out1.field = in1 + in2
    out2.field = in1 - in2

    adder
  }

  def NAND_XOR = {
    val adder = IC(16, 16)

    val in1 = Field(
      adder.in(0),
      adder.in(1),
      adder.in(2),
      adder.in(3),
      adder.in(4),
      adder.in(5),
      adder.in(6),
      adder.in(7))

    val in2 = Field(
      adder.in(8),
      adder.in(9),
      adder.in(10),
      adder.in(11),
      adder.in(12),
      adder.in(13),
      adder.in(14),
      adder.in(15))

    val out1 = Field.Ref()
    adder.out(0).signal = out1.signal(0)
    adder.out(1).signal = out1.signal(1)
    adder.out(2).signal = out1.signal(2)
    adder.out(3).signal = out1.signal(3)
    adder.out(4).signal = out1.signal(4)
    adder.out(5).signal = out1.signal(5)
    adder.out(6).signal = out1.signal(6)
    adder.out(7).signal = out1.signal(7)

    val out2 = Field.Ref()
    adder.out(8).signal = out2.signal(0)
    adder.out(9).signal = out2.signal(1)
    adder.out(10).signal = out2.signal(2)
    adder.out(11).signal = out2.signal(3)
    adder.out(12).signal = out2.signal(4)
    adder.out(13).signal = out2.signal(5)
    adder.out(14).signal = out2.signal(6)
    adder.out(15).signal = out2.signal(7)

    out1.field = ~(in1 & in2)
    out2.field = in1 ^ in2

    adder
  }

  Utils.writeBin(ADD_SUB, Paths.get("addsub.bin"))
  Utils.writeBin(NAND_XOR, Paths.get("nandxor.bin"))

  //STATE_PROM
  //SWITCH_NOP_NOTNOP

  /*val f = Field(3)

  val f2 = Field(f.signal(0), f.signal(1), f.signal(2))

  println(f2.value)*/
}
