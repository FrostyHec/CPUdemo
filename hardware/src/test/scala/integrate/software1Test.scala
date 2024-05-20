package integrate

import Generate.Top
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils.InsUtils._
import chisel3._

class software1Test extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Software1 Test"
  /* the following tests are for the software1, assembly code can be found in software */

  it should "correctly initialize the data address" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 6)
      checkRegsInTop(total, 10, "h_ff_ff_ff_00".U) // led
      checkRegsInTop(total, 11, "h_ff_ff_ff_04".U) // btn
      checkRegsInTop(total, 12, "h_ff_ff_ff_08".U) // swi
      checkRegsInTop(total, 13, "h_ff_ff_ff_0c".U) // 7seg
    }
  }

  it should "jump to the correct place1 : stay in the cycle if no correct input in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }


}
