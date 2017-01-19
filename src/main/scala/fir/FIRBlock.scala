// See LICENSE for license details.

package fir

import cde.Parameters
import chisel3._
import dsptools._
import dsptools.numbers._
import dspjunctions._
import dspblocks._

class LazyFIRBlock[T <: Data : Real]()(implicit p: Parameters) extends LazyDspBlock()(p) {
  def controls = Seq()
  def statuses = Seq()

  lazy val module = new FIRBlock[T](this)
  val config = p(FIRKey)(p)

  (0 until config.numberOfTaps).map( i =>
    addControl(s"firCoeff$i", 0.U)
  )
  addStatus("firStatus")
}

class FIRBlock[T <: Data : Real](outer: LazyDspBlock)(implicit p: Parameters)
  extends GenDspBlock[T, T](outer)(p) with HasFIRGenParameters[T] {

  val baseAddr = BigInt(0)
  val module = Module(new FIR[T])
  val config = p(FIRKey)(p)
  val mio = module.io.asInstanceOf[FIRIO[T]]

  module.io.in <> unpackInput(lanesIn, genIn())
  unpackOutput(lanesOut, genOut()) <> module.io.out

  val taps = Wire(Vec(config.numberOfTaps, genTap.getOrElse(genIn()).cloneType))
  val w = taps.zipWithIndex.map{case (x, i) => {
    x.fromBits(control(s"firCoeff$i"))
  }}
  mio.taps := w
  status("firStatus") := mio.out.sync

}
