package fir

import chisel3._
import cde.{Parameters, Field}
import dsptools._
import diplomacy.{LazyModule, LazyModuleImp}
import dspjunctions._
import dspblocks._


class DspTop(p: Parameters) extends LazyModule {
  override lazy val module = Module(new DspTopModule(p, this, new DspTopBundle(p)))
}

class DspTopBundle(p: Parameters) extends BasicDspBlockIO()(p) {}

class DspTopModule[+L <: DspTop, +B <: DspTopBundle](val p: Parameters, l: L, b: => B)
  extends LazyModuleImp(l) with DspModule {
    val io = IO(b)
    io <> module.io
  }

case object BuildDSP extends Field[(Parameters) => LazyDspBlock]

trait DspModule {
  val p: Parameters
  val module = Module(LazyModule(p(BuildDSP)(p)).module)
}

class DspBareTop(val p: Parameters) extends Module with DspModule {
  val io = IO(module.io.cloneType)
  io <> module.io
}
