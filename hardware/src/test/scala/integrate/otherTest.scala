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
      total.io.led.led.expect("h_30_00".U)
    }
  }

  it should "pass auipc" in {
    load_instructions("test_auipc.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      checkRegsInTop(total, 1, "h_64000".U) // led
    }
  }

  it should "ecall" in {
    load_instructions("test_ecall.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      total.io.switch.switches.poke("h_00_12_34".U)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 40)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 40)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 40)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 40)
      total.io.led.led.expect("h_00_12_34".U)
    }
  }

  it should "ebreak" in {
    load_instructions("test_ebreak.txt")
    test(new Top) { total =>
      run_instructions(total, 3) // initialize sp
      checkRegsInTop(total, 2, "h0001_fffc".U)

      run_instructions(total, 11)
      checkRegsInTop(total, 14, 20.U)
      checkRegsInTop(total, 5, 1.U)
      checkRegsInTop(total, 6, 2.U)
      checkRegsInTop(total, 7, 3.U)
      checkRegsInTop(total, 28, 4.U)
      checkRegsInTop(total, 29, 5.U)

      // begin sys_ebreak
      run_instructions(total, 6)
      checkRegsInTop(total, 14, 20.U)
      run_instructions(total, 4)
      checkRegsInTop(total, 11, "hffff_ff00".U)
      checkRegsInTop(total, 12, "hffff_ff04".U)
      checkRegsInTop(total, 13, "hffff_ff08".U)
      checkRegsInTop(total, 14, "hffff_ff0c".U)
      run_instructions(total, 5)

      // begin sys_break_cases
      total.io.btn.button.poke("b00100".U) //  sys_ebreak_case1
      run_instructions(total, 6)

      // begin sys_ebreak_case1
      total.io.switch.switches.poke("h_00_00_1F".U) // sys_ebreak_x31
      run_instructions(total, 4)
      checkRegsInTop(total, 5, 0.U);

      // begin sys_ebreak_x31
      run_instructions(total, 4)
      checkRegsInTop(total, 31, 10.U)

      // back to sys_ebreak_cases
      total.io.btn.button.poke("b00100".U) //  sys_ebreak_case1
      run_instructions(total, 6)

      // begin sys_ebreak_case1
      total.io.switch.switches.poke("h_00_00_0E".U) // sys_ebreak_x14
      run_instructions(total, 38)
      checkRegsInTop(total, 5, 0.U)

      // begin sys_ebreak_x14
      run_instructions(total, 3)
      checkRegsInTop(total, 5, 20.U)

      // back to sys_ebreak_cases
      total.io.btn.button.poke("b01000".U)
      run_instructions(total, 5)

      // begin sys_ebreak_cases2
      run_instructions(total, 5)
      checkRegsInTop(total, 5, 852.U)
      run_instructions(total, 7)
      checkRegsInTop(total, 14, 20.U)
    }
  }

  it should "pass other exception" in {
    load_instructions("test_otherFault.txt")
    test(new Top) { total =>
      run_instructions(total, 3) // initial sp

      run_instructions(total, 4)
      checkRegsInTop(total, 1, 0.U)

      // begin app
      run_instructions(total, 2) // load wrong address
      checkRegsInTop(total, 1, 9.U)

      // begin sys_other_cause
      run_instructions(total, 8)
      checkRegsInTop(total, 31, 100.U)
    }
  }

}
