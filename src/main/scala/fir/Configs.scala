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
import fir.Generator.params

import dsptools._
import scala.collection.mutable.Map

trait HasIPXACTParameters {
  def getIPXACTParameters: Map[String, String]
}

// create a new DSP Configuration
class DspConfig extends Config(
  (pname, site, here) => pname match {
    case BuildDSP => { (q: Parameters) => {
      implicit val p = q
      Module(new FIRWrapper[DspReal])
    }}
    case FIRKey => { (q: Parameters) => { 
      implicit val p = q
      FIRConfig[DspReal](numberOfTaps = 4, pipelineDepth = 0)
    }}
	  case NastiKey => NastiParameters(64, 32, 1)
    case PAddrBits => 32
    case CacheBlockOffsetBits => 6
    case AmoAluOperandBits => 64
    case TLId => "FIR"
    case TLKey("FIR") =>
      TileLinkParameters(
        coherencePolicy = new MEICoherence(
          new NullRepresentation(2)),
        nManagers = 1,
        nCachingClients = 0,
        nCachelessClients = 1,
        maxClientXacts = 4,
        maxClientsPerPort = 1,
        maxManagerXacts = 1,
        dataBeats = 8,
        dataBits = 64 * 8)
    case DspBlockKey => DspBlockParameters(128, 128)
    case GenKey => new GenParameters {
      //def getReal(): FixedPoint = FixedPoint(width = 32, binaryPoint = 16)
      def getReal(): DspReal = DspReal(0.0).cloneType
      def genIn [T <: Data] = getReal().asInstanceOf[T]
      override def genOut[T <: Data] = getReal().asInstanceOf[T]
      val lanesIn = 2
      override val lanesOut = 2
    }
    case _ => throw new CDEMatchError
  }) with HasIPXACTParameters {
  def getIPXACTParameters: Map[String, String] = {
    // Get unadulterated, top level parameters.
    val parameterList = List[Field[_]](TLId, PAddrBits)
    val parameterMap = parameterList.foldLeft(Map[String, String]()) { (m, s) => m(s.toString) = params(s).toString; m }

    // Conjure up some IPXACT synthsized parameters.
    parameterMap ++= List(("InputLanes", params(GenKey).lanesIn.toString), ("OutputLanes", params(GenKey).lanesOut.toString))
    parameterMap ++= List(("InputTotalBits", params(DspBlockKey).inputWidth.toString))

    parameterMap ++= List(("NumberOfTaps", params(FIRKey)(params).numberOfTaps.toString), ("PipelineDepth", params(FIRKey)(params).pipelineDepth.toString))

    parameterMap
  }
}

case object FIRKey extends Field[(Parameters) => FIRConfig[DspReal]]

trait HasFIRGenParameters[T <: Data] extends HasGenParameters[T, T] {
   def genTap: Option[T] = None
}

case class FIRConfig[T<:Data:Real](val numberOfTaps: Int, val pipelineDepth: Int)(implicit val p: Parameters) extends HasFIRGenParameters[T] {
  // sanity checks
  require(lanesIn%lanesOut == 0, "Decimation amount must be an integer.")
  require(lanesOut <= lanesIn, "Cannot have more output lanes than input lanes.")
  require(pipelineDepth >= 0, "Must have positive pipelining")
}

