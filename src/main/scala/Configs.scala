package fir

import breeze.math.{Complex}
import breeze.signal.{fourierTr}
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

// create a new DSP Configuration
class DspConfig extends Config(
  (pname, site, here) => pname match {
    case BuildDSP => { (q: Parameters) => {
      implicit val p = q
      Module(new FIRWrapper[DspReal])
    }}
    case FIRKey => { (q: Parameters) => { 
      implicit val p = q
      FIRConfig[DspReal](numberOfTaps = 4)
    }}
	  case NastiKey => NastiParameters(64, 32, 1)
    case PAddrBits => 32
    case CacheBlockOffsetBits => 6
    case AmoAluOperandBits => 64
    case TLId => "FIR"
    case TLKey("FIR") =>
        TileLinkParameters(
          coherencePolicy = new MICoherence(
            new NullRepresentation(1)),
          nManagers = 1,
          nCachingClients = 0,
          nCachelessClients = 1,
          maxClientXacts = 4,
          maxClientsPerPort = 1,
          maxManagerXacts = 1,
          dataBeats = 1,
          dataBits = 64)
    case DspBlockKey => DspBlockParameters(1024, 1024)
    case GenKey => new GenParameters {
      def getReal(): DspReal = DspReal(0.0).cloneType
      def genIn [T <: Data] = getReal().asInstanceOf[T]
      override def genOut[T <: Data] = getReal().asInstanceOf[T]
      val lanesIn = 2
      override val lanesOut = 2
    }
    case _ => throw new CDEMatchError
  })

case object FIRKey extends Field[(Parameters) => FIRConfig[DspReal]]

trait HasFIRGenParameters[T <: Data] extends HasGenParameters[T, T] {
   def genTap: Option[T] = None
}

case class FIRConfig[T<:Data:Real](val numberOfTaps: Int)(implicit val p: Parameters) extends HasFIRGenParameters[T] {
  // sanity checks
  require(lanesIn%lanesOut == 0, "Decimation amount must be an integer.")
  require(lanesOut <= lanesIn, "Cannot have more output lanes than input lanes.")
}

