package core.execute

import chiseltest._
import core.execute.ALU
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ALU"

  it should "correctly do sth" in {
    test(new ALUTest) {ALUTest =>

    }
  }
}