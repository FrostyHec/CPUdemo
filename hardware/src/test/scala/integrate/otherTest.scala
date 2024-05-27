package integrate

import Generate.Top
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils.InsUtils._
import chisel3._

class otherTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Other Test"

  it should "pass lui" in {
    load_instructions("test_lui.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      checkRegsInTop(total, 1, "h_30_00".U) // led
    }
  }

  it should "pass auipc" in {
    load_instructions("test_auipc.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      checkRegsInTop(total, 1, "h_10_10".U) // led
    }
  }

}
