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

  // define the latency as the slowest output
  val latency = ceil(config.numberOfTaps/lanesIn).toInt + config.pipelineDepth
  io.out.sync := ShiftRegister(io.in.sync, latency)
  io.out.valid := ShiftRegister(io.in.valid, latency)

  // feed in zeros when invalid
  val in = Wire(Vec(lanesIn, genIn()))
  when (io.in.valid) {
    in := io.in.bits
  } .otherwise {
    in := Wire(Vec(lanesIn, implicitly[Real[T]].zero))
  }
  val products: Seq[Seq[T]] = io.taps.reverse.map { tap => in.map { i => 
    i * tap
  }}

  // rotates a Seq by i terms, wraps around and can be negative for reverse rotation
  // e.g. (1,2,3) rotate by 1 = (2,3,1)
  def rotate(l: Seq[T], i: Int): Seq[T] = if(i >= 0) { l.drop(i%l.size) ++ l.take(i%l.size) } else { l.takeRight(-i%l.size) ++ l.dropRight(-i%l.size) }

  val last = products.reduceLeft { (left: Seq[T], right: Seq[T]) =>
    val reg = Reg(left.last.cloneType)
    reg := left.last
    right.zip(rotate(left.dropRight(1) :+ reg, -1)).map{case(a, b) => a+b}
  }

  // all pipeline registers tacked onto end, hopefully synthesis tools handle correctly
  io.out.bits := ShiftRegister(Vec(last.grouped(lanesIn/lanesOut).map(_.head).toSeq), config.pipelineDepth)
}

class FIRWrapper[T<:Data:Real]()(implicit p: Parameters) extends GenDspBlock[T, T]()(p) with HasFIRGenParameters[T] {
  val baseAddr = BigInt(0)
  val fir = Module(new FIR[T])
  val config = p(FIRKey)(p)

  (0 until config.numberOfTaps).map( i =>
    addControl(s"firCoeff$i", 0.U)
  )
  addStatus("firStatus")

  fir.io.in <> unpackInput(lanesIn, genIn())
  val taps = Wire(Vec(config.numberOfTaps, genTap.getOrElse(genIn())))
  val w = taps.zipWithIndex.map{case (x, i) => x.fromBits(control(s"firCoeff$i"))}
  fir.io.taps := w

  unpackOutput(lanesOut, genOut()) <> fir.io.out
  status("firStatus") := fir.io.out.sync
}
