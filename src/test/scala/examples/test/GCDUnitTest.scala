// See LICENSE for license details.

package examples.test

import Chisel.iotesters.{ChiselFlatSpec, Driver, PeekPokeTester}
import example.GCD

class GCDUnitTester(c: GCD) extends PeekPokeTester(c) {
  /**
    * compute the gcd and the number of steps it should take to do it
    *
    * @param a positive integer
    * @param b positive integer
    * @return the GCD of a and b
    */
  def computeGcd(a: Int, b: Int): (Int, Int) = {
    var x = a
    var y = b
    var depth = 1
    while(y > 0 ) {
      if (x > y) {
        x -= y
      }
      else {
        y -= x
      }
      depth += 1
    }
    (x, depth)
  }

  val gcd = c

  for(i <- 1 to 10) {
    for (j <- 1 to 10) {
      val (a, b, z) = (64, 48, 16)

      poke(gcd.io.a, a)
      poke(gcd.io.b, b)
      poke(gcd.io.e, 1)
      step(1)
      poke(gcd.io.e, 0)

      val (expected_gcd, steps) = computeGcd(a, b)

      step(steps - 1) // -1 is because we step(1) already to toggle the enable
      expect(gcd.io.z, expected_gcd)
      expect(gcd.io.v, 1)
    }
  }
}

class GCDTester extends ChiselFlatSpec {
  val backendNames = Array[String]("firrtl", "verilator")
  for ( backendName <- backendNames ) {
    "GCD" should s"calculate proper greatest common denominator (with ${backendName})" in {
      Driver(() => new GCD, backendName) {
        c => new GCDUnitTester(c)
      } should be (true)
    }
  }
}
