package fir

import chisel3.util._
import chisel3._
import dsptools.junctions._
import scala.math._
import dsptools.numbers.{Real, DspComplex}
import dsptools.numbers.implicits._
import dsptools._
import cde.Parameters

class FIRIO[T<:Data:Real]()(implicit val p: Parameters) extends Bundle with HasFIRGenParameters[T] {
  val config = p(FIRKey)(p)
  val in = Input(ValidWithSync(Vec(lanesIn, genIn())))
  val out = Output(ValidWithSync(Vec(lanesOut, genOut())))
  val taps = Input(Vec(config.numberOfTaps, genTap.getOrElse(genIn()))) // default to input or output?
}

class FIR[T<:Data:Real]()(implicit val p: Parameters) extends Module with HasFIRGenParameters[T] {
  val io = IO(new FIRIO[T])
  val config = p(FIRKey)(p)

  // TODO
  io.out.sync := io.in.sync
  io.out.valid := io.in.valid

  val products = io.taps.reverse.map { tap => io.in.bits.map { in => in * tap }}

  def rotate(l: Seq[T], i: Int): Seq[T] = if(i >= 0) { l.drop(i%l.size) ++ l.take(i%l.size) } else { l.takeRight(-i%l.size) ++ l.dropRight(-i%l.size) }

  val last = products.reduceLeft { (left: Seq[T], right: Seq[T]) =>
    val reg = Reg(left.last.cloneType)
    reg := left.last
    right.zip(rotate(left.dropRight(1) :+ reg, -1)).map{case(a, b) => a+b}
  }

  io.out.bits := Vec(last.grouped(lanesIn/lanesOut).map(_.head).toSeq)
}

class FIRWrapper[T<:Data:Real]()(implicit p: Parameters) extends GenDspBlock[T, T]()(p) {
  val baseAddr = BigInt(0)
  val fir = Module(new FIR[T])

  addControl("FIR Coeff", 0.U)
  addStatus("fftStatus")

  fir.io.in <> unpackInput(lanesIn, genIn())
  fir.io.in.sync := control("fftControl")(0)

  unpackOutput(lanesOut, genOut()) <> fir.io.out
  status("fftStatus") := fir.io.out.sync
}
