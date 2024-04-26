package core.decode

import chiseltest._
import core.decode.ControlUnit
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class ControlUnitTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ALU"

  it should "correctly do sth" in {
    test(new ControlUnit) {controlUnit =>

    }
  }
}