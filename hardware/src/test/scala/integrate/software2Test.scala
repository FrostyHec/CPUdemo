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
      checkRegsInTop(total, 16, "h_00_01_ff_fc".U) // stack
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
    }
  }

  /*
//  it should "pass test case 0 _ v1.t1.1" in {
//    // 用于确认是否正确进入某一个case
//    load_instructions("software2_t1.txt")
//    test(new Top) { total =>
//      run_instructions(total, 50)
//      // 选择 case0
//      total.io.switch.switches.poke("h_00_12_00".U)
//      run_instructions(total, 50)
//      total.io.btn.button.poke(4.U)
//      run_instructions(total, 20)
//      total.io.btn.button.poke(0.U)
//      run_instructions(total, 50)
//      checkRegsInTop(total, 27, 1.U)
//      checkRegsInTop(total, 26, 10.U)
//      // 拨码开关输入
//      total.io.switch.switches.poke("h_00_12_34".U)
//      run_instructions(total, 50)
//      total.io.btn.button.poke(4.U)
//      run_instructions(total, 20)
//      run_instructions(total, 50)
//    }
//  }
//
//  it should "pass test case 0 _ v1.t2.1" in {
//    // 用于确认是否正确进入某一个case
//    load_instructions("s2_t_c0.txt")
//    test(new Top) { total =>
//      run_instructions(total, 50)
//      // 选择 case0
//      total.io.switch.switches.poke("h_00_12_00".U)
//      run_instructions(total, 50)
//      total.io.btn.button.poke(4.U)
//      run_instructions(total, 20)
//      total.io.btn.button.poke(0.U)
//      run_instructions(total, 50)
//      checkRegsInTop(total, 27, 1.U)
//      checkRegsInTop(total, 26, 10.U)
//      // 拨码开关输入
//      total.io.switch.switches.poke("h_55_12_34".U)
//      run_instructions(total, 50)
//      total.io.btn.button.poke(4.U)
//      run_instructions(total, 20)
//      run_instructions(total, 50)
//      run_instructions(total, 50)
//      checkRegsInTop(total, 25, "h_34".U)
//      checkRegsInTop(total, 27, 7.U)
//      total.io.led.led.expect(2.U)
////      total.io.seg7.seg7.expect(2.U)
//    }
//  }
  */


  // case1 : FP16 向上取整
  it should "pass test case 1 _ v1.0.1" in {
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
      // 选择 case1
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
      // 选择 case
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
      // 选择 case
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
      // 选择 case
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_00000_00010_00000".U) // a very small number
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
      // 选择 case
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_00000_00010_00000".U) // a very small number
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
      // 选择 case
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_00000_00000_00000".U) // 0.0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }
  it should "pass test case 1 _ v1.0.9 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_00000_00000_00000".U) // - 0.0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }

  it should "pass test case 1 _ v1.0.10 : corner case - very large (inf)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_11111_00000_00000".U) // inf
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_100_000_000_000_000_000_000_000".U)
    }
  }
  it should "pass test case 1 _ v1.0.11  : corner case - very large (-inf)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_11111_00000_00000".U) // -inf
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_100_000_000_000_000_000_000_000".U)
    }
  }

  it should "pass test case 1 _ v1.0.12  : corner case - very large (NaN)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_11111_00001_00000".U) // NaN
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_110_000_000_000_000_000_000_000".U)
    }
  }
  it should "pass test case 1 _ v1.0.13  : corner case - very large (-NaN)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_10_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_11111_00001_00000".U) // -NaN
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_110_000_000_000_000_000_000_000".U)
    }
  }

  /*
//  it should "pass test case 1 _ v1.t1.2" in {
//    load_instructions("s2_t_c1.txt")
//    test(new Top) { total =>
//      run_instructions(total, 50)
//      // 选择 case1
//      total.io.switch.switches.poke("h_10_12_00".U)
//      run_instructions(total, 50)
//      total.io.btn.button.poke(4.U)
//      run_instructions(total, 20)
//      total.io.btn.button.poke(0.U)
//      run_instructions(total, 50)
//      // 拨码开关输入
//      total.io.switch.switches.poke("b0_01110_00000_00000".U) // 0.5
//      run_instructions(total, 50)
//      total.io.btn.button.poke(4.U)
//      run_instructions(total, 20)
//      total.io.btn.button.poke(0.U)
//      run_instructions(total, 50)
//      total.io.led.led.expect(1.U)
//    }
//  }
   */

  // case2 : FP16 向下取整
  it should "pass test case 2 _ v1.0.1" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case2
      total.io.switch.switches.poke("h_20_12_00".U)
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
      total.io.led.led.expect(4.U)
    }
  }

  it should "pass test case 2 _ v1.0.2" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case2
      total.io.switch.switches.poke("h_20_12_00".U)
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
      total.io.led.led.expect(0.U)
    }
  }

  it should "pass test case 2 _ v1.0.3" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case2
      total.io.switch.switches.poke("h_20_12_00".U)
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

  it should "pass test case 2 _ v1.0.4" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case2
      total.io.switch.switches.poke("h_20_12_00".U)
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
      total.io.led.led.expect(4.U)
    }
  }

  it should "pass test case 2 _ v1.0.5" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case0
      total.io.switch.switches.poke("h_20_12_00".U)
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
      total.io.led.led.expect(4.U)
    }
  }

  it should "pass test case 2 _ v1.0.6 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_20_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_00000_00010_00000".U) // a very small number
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }
  it should "pass test case 2 _ v1.0.7 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_20_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_00000_00010_00000".U) // - a very small number
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("h_ff_ff_ff".U)
    }
  }

  it should "pass test case 2 _ v1.0.8 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_20_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_00000_00000_00000".U) // 0.0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }
  it should "pass test case 2 _ v1.0.9 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_20_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_00000_00000_00000".U) // - 0.0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }

  it should "pass test case 2 _ v1.0.10 : corner case - very large (inf)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_20_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_11111_00000_00000".U) // inf
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_100_000_000_000_000_000_000_000".U)
    }
  }
  it should "pass test case 2 _ v1.0.11  : corner case - very large (-inf)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_20_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_11111_00000_00000".U) // -inf
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_100_000_000_000_000_000_000_000".U)
    }
  }

  it should "pass test case 2 _ v1.0.12  : corner case - very large (NaN)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_20_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_11111_00001_00000".U) // NaN
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_110_000_000_000_000_000_000_000".U)
    }
  }
  it should "pass test case 2 _ v1.0.13  : corner case - very large (-NaN)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_20_12_cd".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_11111_00001_00000".U) // -NaN
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_110_000_000_000_000_000_000_000".U)
    }
  }

  // case3 : FP16 四舍五入取整
  it should "pass test case 3 _ v1.0.1" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
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

  it should "pass test case 3 _ v1.0.2" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
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

  it should "pass test case 3 _ v1.0.3" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
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

  it should "pass test case 3 _ v1.0.4" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
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

  it should "pass test case 3 _ v1.0.5" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
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
      total.io.led.led.expect(4.U)
    }
  }

  // todo 这四个非常小的有问题
  it should "pass test case 3 _ v1.0.6 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_00000_00010_00000".U) // a very small number
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }
  it should "pass test case 3 _ v1.0.7 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_00000_00010_00000".U) // - a very small number
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }

  it should "pass test case 3 _ v1.0.8 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_00000_00000_00000".U) // 0.0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }
  it should "pass test case 3 _ v1.0.9 : corner case - very small" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_00000_00000_00000".U) // - 0.0
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }

  it should "pass test case 3 _ v1.0.10 : corner case - very large (inf)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_11111_00000_00000".U) // inf
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_100_000_000_000_000_000_000_000".U)
    }
  }
  it should "pass test case 3 _ v1.0.11  : corner case - very large (-inf)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_11111_00000_00000".U) // -inf
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_100_000_000_000_000_000_000_000".U)
    }
  }

  it should "pass test case 3 _ v1.0.12  : corner case - very large (NaN)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b0_11111_00001_00000".U) // NaN
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_110_000_000_000_000_000_000_000".U)
    }
  }
  it should "pass test case 3 _ v1.0.13  : corner case - very large (-NaN)" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_30_12_cd".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("b1_11111_00001_00000".U) // -NaN
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("b_110_000_000_000_000_000_000_000".U)
    }
  }

  //todo add some corner cases

  // case4: add two numbers
  it should "pass test case 4 _ v1.0.1" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_40_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入 a
      total.io.switch.switches.poke("h_23_24_25".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入 b
      total.io.switch.switches.poke("h_23_24_32".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("h_57".U)
    }
  }

  it should "pass test case 4 _ v1.0.2" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_40_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入 a
      total.io.switch.switches.poke("h_23_24_ff".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入 b
      total.io.switch.switches.poke("h_23_24_32".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("h_cd".U)
    }
  }

  // case5: little endian - big endian
  it should "pass test case 5 _ v1.0.1" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_50_12_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("h_04_25".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("h_250".U)
    }
  }

  it should "pass test case 5 _ v1.0.2" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_50_cd_34".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("h_03_ac".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect("h_ac0".U)
    }
  }

  // case6: is power of 2
  it should "pass test case 6 _ v1.0.1" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_60_cd_34".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("h_00_00".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }

  it should "pass test case 6 _ v1.0.2" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_60_cd_34".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("h_00_80".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(1.U)
    }
  }

  it should "pass test case 6 _ v1.0.3" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_60_cd_34".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("h_ba_04".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(1.U)
    }
  }

  it should "pass test case 6 _ v1.0.4" in {
    load_instructions("software2.txt")
    test(new Top) { total =>
      run_instructions(total, 50)
      // 选择 case
      total.io.switch.switches.poke("h_60_cd_34".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      // 拨码开关输入
      total.io.switch.switches.poke("h_ba_a4".U)
      run_instructions(total, 50)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 20)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 50)
      total.io.led.led.expect(0.U)
    }
  }
}
