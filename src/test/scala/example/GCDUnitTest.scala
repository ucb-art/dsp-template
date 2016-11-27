// See LICENSE for license details.

package example

import chisel3._
import chisel3.iotesters.PeekPokeTester
import dsptools.DspTester
import org.scalatest.{FlatSpec, Matchers}

class GCDTester(c: GCD, a: Int, b: Int, z: Int) extends DspTester(c) {
  poke(c.io.a, a)
  poke(c.io.b, b)
  poke(c.io.e, 1)
  step(1)
  poke(c.io.e, 0) // always enable?
  while (peek(c.io.v) == 0) { step(1) }
  expect(c.io.z, z)
}

class GCDSpec extends FlatSpec with Matchers {

  //TODO: use generators and this function to make z's
  def gcd(a: Int, b: Int): Int = if(b == 0) a else gcd(b, a % b)

  val gcds = Seq((64, 48), (12, 9), (48, 64))

  behavior of "GCD"
  it should "greatly find the denominator in common" in {
    for ((a, b) <- gcds) {
      val z = gcd(a, b)
      chisel3.iotesters.Driver(() => new GCD) { c => new GCDTester(c, a, b, z) } should be (true)
    }
  }
}
