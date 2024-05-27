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

  it should "pass other exception" in {
    load_instructions("test_otherFault.txt")
    test(new Top) { total =>
      run_instructions(total, 40)
      total.io.led.led.expect("h_9".U)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 40)

    }
  }

}
