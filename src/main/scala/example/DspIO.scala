// See LICENSE for license details.

package example

import chisel3._
import dsptools.junctions.ValidWithSync
import dsptools.numbers.Real

class DspIO[T<:Data:Real](
    genIn: => T,
    genOut: => Option[T] = None,
    controlBitsGen: => Option[Bundle] = None,
    numberOfIns: Int,
    numberOfOuts: Int)
  extends Bundle {

  val in = Input(ValidWithSync(Vec(numberOfIns, genIn)))
  val out = Output(ValidWithSync(Vec(numberOfOuts, genOut.getOrElse(genIn))))
  val control = controlBitsGen
}
