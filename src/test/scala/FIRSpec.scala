// See LICENSE for license details.
package fir

import dsptools.numbers.implicits._
import dsptools.{DspContext, Grow}
import spire.algebra.{Field, Ring}
import breeze.math.{Complex}
import breeze.linalg._
import chisel3._
import chisel3.util._
import chisel3.iotesters._
import firrtl_interpreter.InterpreterOptions
import dsptools.numbers.{DspReal, SIntOrder, SIntRing}
import dsptools.{DspContext, DspTester, Grow}
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers.implicits._
import dsptools.numbers.{DspComplex, Real}
import scala.util.Random
import scala.math._
import org.scalatest.Tag

import cde._
import junctions._
import uncore.tilelink._
import uncore.coherence._

import dsptools._

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

class FIRWrapperTester[T <: Data](c: FIRWrapper[T])(implicit p: Parameters) extends DspBlockTester(c) {
  val config = p(FIRKey)(p)
  val test_length = 10
  
  // define input datasets here, a Seq of Seqs
  def rawStreamIn = (0 until test_length).map(x => (x*config.numberOfTaps until (x+1)*config.numberOfTaps).toList)

  def doublesToBigInt(in: List[Int]): BigInt = {
    in.reverse.foldLeft(BigInt(0)) {case (bi, dbl) =>
      val new_bi = BigInt(java.lang.Double.doubleToLongBits(dbl))
      (bi << 64) | new_bi
    }
  }
  def streamIn = rawStreamIn.map(doublesToBigInt)

  pauseStream
  axiWrite(0, 1)
  println(peek(c.io.out.sync).toString)
  step(10)
  axiWrite(8, 0)
  println(peek(c.io.out.sync).toString)
  playStream
  step(10)
  println(peek(c.io.out.sync).toString)
  println("Input:")
  rawStreamIn.foreach{ x => println(x.toString) }
  println("Output:")
  streamOut.foreach { x => (0 until 16).foreach { idx => {
    val y = (x >> (64 * idx)) & 0xFFFFFFFFFFFFFFFFL
    print(java.lang.Double.longBitsToDouble(y.toLong).toString + " ") }}
    println()
  }
}

class FIRWrapperSpec extends FlatSpec with Matchers {
  behavior of "FIRWrapper"
  val manager = new TesterOptionsManager {
    testerOptions = TesterOptions(backendName = "verilator", testerSeed = 7L)
    interpreterOptions = InterpreterOptions(setVerbose = false, writeVCD = true)
  }

  it should "work with DspBlockTester" in {
    implicit val p: Parameters = Parameters.root(new DspConfig().toInstance)
    val dut = () => new FIRWrapper[DspReal]()
    chisel3.iotesters.Driver.execute(dut, manager) { c => new FIRWrapperTester(c) } should be (true)
  }
}
