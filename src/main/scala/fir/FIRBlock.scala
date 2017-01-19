// See LICENSE for license details.

package fir

import cde.Parameters
import chisel3._
import dsptools._
import dsptools.numbers._
import dspjunctions._
import dspblocks._

class LazyFIRBlock[T <: Data : Real]()(implicit p: Parameters) extends LazyDspBlock()(p) 
  with HasFIRGenParameters[T] {

  def controls = Seq()
  def statuses = Seq()

  lazy val module = new FIRBlock[T](this)
  val config = p(FIRKey)(p)
  val mio = module.io.asInstanceOf[FIRIO[T]]

  (0 until config.numberOfTaps).map( i =>
    addControl(s"firCoeff$i", 0.U)
  )
  addStatus("firStatus")

  val taps = Wire(Vec(config.numberOfTaps, genTap.getOrElse(genIn()).cloneType))
  val w = taps.zipWithIndex.map{case (x, i) => {
    x.fromBits(controls(s"firCoeff$i"))
  }}
  mio.taps := w
  statuses("firStatus") := mio.out.sync

}

class FIRBlock[T <: Data : Real](outer: LazyDspBlock)(implicit p: Parameters)
  extends GenDspBlock[T, T](outer)(p) {

  val baseAddr = BigInt(0)
  val module = Module(new FIR[T])

  module.io.in <> unpackInput(lanesIn, genIn())
  unpackOutput(lanesOut, genOut()) <> module.io.out
}
