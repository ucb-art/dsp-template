// See LICENSE for license details.
package fir

import dsptools.numbers.implicits._
import dsptools.Utilities._
import dsptools.{DspContext, Grow}
import spire.algebra.{Field, Ring}
import breeze.math.{Complex}
import breeze.linalg._
import breeze.signal._
import breeze.signal.support._
import breeze.signal.support.CanFilter._
import chisel3._
import chisel3.util._
import chisel3.iotesters._
import firrtl_interpreter.InterpreterOptions
import dsptools.numbers.{DspReal, SIntOrder, SIntRing}
import dsptools.{DspContext, DspTester, Grow}
import org.scalatest.{FlatSpec, Matchers}
import dsptools.numbers.implicits._
import dsptools.numbers.{DspComplex, Real}
import chisel3.testers.BasicTester
import org.scalatest._
import scala.util.Random
import scala.math._

import cde._
import junctions._
import uncore.tilelink._
import uncore.coherence._

import dsptools._

object LocalTest extends Tag("edu.berkeley.tags.LocalTest")

class FIRWrapperTester[T <: Data](c: FIRWrapper[T])(implicit p: Parameters) extends DspBlockTester(c)(p) {
  val config = p(FIRKey)(p)
  val gk = p(GenKey)
  val test_length = 10
  
  // define input datasets here
  val input = Seq.fill(test_length)(Seq.fill(gk.lanesIn)(Random.nextDouble*2-1))
  val filter_coeffs = Array.fill(config.numberOfTaps)(Random.nextDouble*2-1)

  def streamIn = packInputStream(input, gk.genIn)

  // use Breeze FIR filter, but trim (it zero pads the input) and decimate output
  val expected_output = filter(DenseVector(input.toArray.flatten), DenseVector(filter_coeffs)).toArray.drop(config.numberOfTaps-2).dropRight(config.numberOfTaps-2).grouped(gk.lanesIn/gk.lanesOut).map(_.head).toArray

  // reset 5 cycles
  reset(5)

  pauseStream
  println("Addr Map:")
  println(testchipip.SCRAddressMap("FIRWrapper").get.map(_.toString).toString)
  // assumes coefficients are first addresses
  filter_coeffs.zipWithIndex.foreach { case(x, i) => axiWriteAs(i*8, x, gk.genIn) }
  step(10)
  playStream
  step(test_length)
  val output = unpackOutputStream(gk.genOut[T], gk.lanesOut)

  println("Input")
  println(input.toArray.flatten.deep.mkString(","))
  println("Coefficients")
  println(filter_coeffs.deep.mkString(","))
  println("Chisel Output")
  println(output.toArray.deep.mkString(","))
  println("Reference Output")
  println(expected_output.deep.mkString(","))

  // as an example, though still need to convert from BigInt in bits to double
  val tap0 = axiRead(0)

  // check within 5%
  compareOutput(output, expected_output, 5e-2)
}

class FIRWrapperSpec extends FlatSpec with Matchers {
  behavior of "FIRWrapper"
  val manager = new TesterOptionsManager {
    testerOptions = TesterOptions(backendName = "firrtl", testerSeed = 7L)
    interpreterOptions = InterpreterOptions(setVerbose = false, writeVCD = true)
  }

  it should "work with DspBlockTester" in {
    implicit val p: Parameters = Parameters.root(new DspConfig().toInstance)
    val dut = () => new FIRWrapper[FixedPoint]()
    chisel3.iotesters.Driver.execute(dut, manager) { c => new FIRWrapperTester(c) } should be (true)
  }
}
