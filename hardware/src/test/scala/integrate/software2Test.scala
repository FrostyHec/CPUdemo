package integrate

import Generate.Top
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils.InsUtils._
import chisel3._

class software2Test extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Software1 Test"
  /* the following tests are for the software1, assembly code can be found in software */

  it should "correctly initialize the data address" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 20)
      checkRegsInTop(total, 10, "h_ff_ff_ff_00".U) // led
      checkRegsInTop(total, 11, "h_ff_ff_ff_04".U) // btn
      checkRegsInTop(total, 12, "h_ff_ff_ff_08".U) // swi
      checkRegsInTop(total, 13, "h_ff_ff_ff_0c".U) // 7seg
      checkRegsInTop(total, 16, "h_00_01_ff_ff".U) // stack
      checkRegsInTop(total, 8, "h_00_01_04_00".U) // push_stack
      checkRegsInTop(total, 9, "h_00_01_08_00".U) // pop_stack
      checkRegsInTop(total, 14, "h_70_00_00".U) // mask1 -> switch for test cases
      checkRegsInTop(total, 17, "h_01_00_00".U) // mask2 -> switch for fib
    }
  }
}
