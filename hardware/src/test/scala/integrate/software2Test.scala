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

  it should "pass test case 0 _ v1.0.1" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_00_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("h_00_12_34".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(2.U)
//      // todo 七段数码管可能有点问题？
//      total.io.seg7.seg7.expect(2.U)
    }
  }

  it should "pass test case 0 _ v1.t1.1" in {
    // 用于确认是否正确进入某一个case
    load_instructions("software2_t1.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_00_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      checkRegsInTop(total, 27, 1.U)
      checkRegsInTop(total, 26, 10.U)
      // 拨码开关输入
      total.io.switch.switches.poke("h_00_12_34".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      run_instructions(total, 50)
    }
  }

  it should "pass test case 0 _ v1.t2.1" in {
    // 用于确认是否正确进入某一个case
    load_instructions("s2_t_c0.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_00_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      checkRegsInTop(total, 27, 1.U)
      checkRegsInTop(total, 26, 10.U)
      // 拨码开关输入
      total.io.switch.switches.poke("h_55_12_34".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      run_instructions(total, 50)
      run_instructions(total, 50)
      checkRegsInTop(total, 25, "h_34".U)
      checkRegsInTop(total, 27, 7.U)
      total.io.led.led.expect(2.U)
//      total.io.seg7.seg7.expect(2.U)
    }
  }

  it should "pass test case 1 _ v1.0.1" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_10001_00100_00000".U) // 4.5
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(5.U)
    }
  }

  it should "pass test case 1 _ v1.0.2" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case1
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_01110_00000_00000".U) // 0.5
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(1.U)
    }
  }

  it should "pass test case 1 _ v1.0.3" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_10001_00000_00000".U) // 4.0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(4.U)
    }
  }

  it should "pass test case 1 _ v1.0.4" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_10001_00110_00000".U) // 4.75
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(5.U)
    }
  }

  it should "pass test case 1 _ v1.0.5" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_10001_00010_00000".U) // 4.25
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(5.U)
    }
  }

  it should "pass test case 1 _ v1.0.6 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_00000_00010_00000".U) // very small
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(1.U)
    }
  }

  it should "pass test case 1 _ v1.0.7 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      //todo 这种情况没有考虑到
      total.io.switch.switches.poke("b0_00000_00000_00000".U) // 0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }
  it should "pass test case 1 _ v1.0.8 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      //todo 这种情况没有考虑到
      total.io.switch.switches.poke("b1_00000_00000_00000".U) // 0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }

  it should "pass test case 1 _ v1.0.9 : corner case - very large" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      //todo 这种情况没有考虑到
      total.io.switch.switches.poke("b0_11111_00000_00000".U) // inf
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("h_1_00000_000000".U)
    }
  }

  it should "pass test case 1 _ v1.0.10  : corner case - very large" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      //todo 这里 led 会报错
      total.io.switch.switches.poke("b1_11111_00000_00000".U) // -inf
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("h_1_00000_000000".U)
    }
  }

  it should "pass test case 1 _ v1.0.11  : corner case - very large" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      //todo 这里没有考虑到
      total.io.switch.switches.poke("b0_11111_00001_00000".U) // NaN
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("h_1_00000_000000".U)
    }
  }

  it should "pass test case 1 _ v1.t1.2" in {
    load_instructions("s2_t_c1.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case1
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_01110_00000_00000".U) // 0.5
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(1.U)
    }
  }
}
