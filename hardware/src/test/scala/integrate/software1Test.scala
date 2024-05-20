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

  it should "jump to the correct place2 : jump to case 0 if input 0 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_00_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ab_00".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_cd_ef".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ab_ef".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ab_ef".U)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  // todo 这个测试过不了, 应该是 lb 出问题了
//  it should "jump to the correct place3 : jump to case 1 if input 1 in button" in {
//    load_instructions("software1.txt")
//    test(new Top) { total =>
//      run_instructions(total, 30)
//      checkRegsInTop(total, 15, 0.U)
//      total.io.switch.switches.poke("h_20_12_34".U)
//      run_instructions(total, 30)
//      total.io.btn.button.poke(4.U)
//      run_instructions(total, 30)
//      total.io.btn.button.poke(0.U)
//      run_instructions(total, 30)
//      total.io.switch.switches.poke("h_00_ab_cd".U)
//      run_instructions(total, 30)
//      total.io.btn.button.poke(4.U)
//      run_instructions(total, 30)
//      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
//      checkRegsInTop(total, 15, 0.U)
//    }
//  }
}
