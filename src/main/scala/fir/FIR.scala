package fir

import chisel3.util._
import chisel3._
import dspjunctions._
import dspblocks._
import scala.math._
import dsptools.numbers.{Real, DspComplex}
import dsptools.numbers.implicits._
import dsptools.counters._
import dsptools._
import cde.Parameters
import internal.firrtl.{Width, BinaryPoint}

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
  io.out.sync := ShiftRegisterWithReset(io.in.sync, latency, 0.U)
  io.out.valid := ShiftRegisterWithReset(io.in.valid, latency, 0.U)

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
