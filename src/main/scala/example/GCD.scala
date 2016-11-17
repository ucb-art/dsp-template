// See LICENSE for license details.

package example

import chisel3._

class GCD extends Module {
  val io = IO(new Bundle {
    val a  = Input(UInt.width(32))
    val b  = Input(UInt.width(32))
    val e  = Input(Bool())
    val z  = Output(UInt.width(32))
    val v  = Output(Bool())
  })
  val x = Reg(UInt.width( 32))
  val y = Reg(UInt.width( 32))
  when (x > y)   { x := x -% y }
  .otherwise     { y := y -% x }
  when (io.e) { x := io.a; y := io.b }
  io.z := x
  io.v := y === 0.U
}
