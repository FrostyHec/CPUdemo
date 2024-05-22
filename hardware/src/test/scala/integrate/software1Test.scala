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
      run_instructions(total, 10)
      checkRegsInTop(total, 10, "h_ff_ff_ff_00".U) // led
      checkRegsInTop(total, 11, "h_ff_ff_ff_04".U) // btn
      checkRegsInTop(total, 12, "h_ff_ff_ff_08".U) // swi
      checkRegsInTop(total, 13, "h_ff_ff_ff_0c".U) // 7seg
      checkRegsInTop(total, 14, "h_e0_00_00".U) // mask
    }
  }

  it should "correctly initialize the data address : t1" in {
    load_instructions("software1_t1.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      checkRegsInTop(total, 10, "h_ff_ff_ff_00".U) // led
      checkRegsInTop(total, 11, "h_ff_ff_ff_04".U) // btn
      checkRegsInTop(total, 12, "h_ff_ff_ff_08".U) // swi
      checkRegsInTop(total, 13, "h_ff_ff_ff_0c".U) // 7seg
      checkRegsInTop(total, 14, "h_e0_00_00".U) // mask
    }
  }

  it should "correctly initialize the data address : t2" in {
    load_instructions("software1_t2.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      checkRegsInTop(total, 10, "h_ff_ff_ff_00".U) // led
      checkRegsInTop(total, 11, "h_ff_ff_ff_04".U) // btn
      checkRegsInTop(total, 12, "h_ff_ff_ff_08".U) // swi
      checkRegsInTop(total, 13, "h_ff_ff_ff_0c".U) // 7seg
      checkRegsInTop(total, 14, "h_e0_00_00".U) // mask
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

  it should "jump to the correct place3 : jump to case 1 if input 1 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place4 : jump to case 2 if input 2 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place5 : jump to case 3 if input 3 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_60_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      checkRegsInTop(total, 31, 0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place5 : jump to case 3 if input 3 in button _ test jump" in {
    load_instructions("software1_t1.txt") // 这个和原先的的区别是 case3 条件的 beq 变成了 bne
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 50)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_60_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      checkRegsInTop(total, 31, 1.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place5.1 : jump to case 3 if input 3 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_05_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      // todo 这里似乎有lb导致的问题
      checkRegsInTop(total, 28, "h_05".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_05".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_05".U)
      checkRegsInTop(total, 29, "h_05".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_60_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_05".U)
      checkRegsInTop(total, 29, "h_05".U)
      checkRegsInTop(total, 31, 1.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place6 : jump to case 4 if input 4 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_80_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      run_instructions(total, 30)
      // todo 这里是 0 ， 这个测试过不了，原因未知
      checkRegsInTop(total, 31, 1.U)
      total.io.led.led.expect(1.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place6.2 : jump to case 4 if input 4 in button" in {
    load_instructions("software1_t2.txt") // 区别是可以通过s0（x8）看到（最后）进了哪一个分支
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_80_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 19, "h_80_12_34".U)
      checkRegsInTop(total, 18, "h_80_00_00".U)
      checkRegsInTop(total, 9, 4.U) // todo 这里实际跑出来结果是 2
      checkRegsInTop(total, 8, 4.U)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      run_instructions(total, 30)
      // todo 这里是 0 ， 这个测试过不了，原因未知
      checkRegsInTop(total, 31, 1.U)
      total.io.led.led.expect(1.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place7 : jump to case 5 if input 5 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_a0_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      checkRegsInTop(total, 31, 0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place7.1 : jump to case 5 if input 5 in button" in {
    load_instructions("software1_t2.txt") // 区别是可以通过s0（x8）看到（最后）进了哪一个分支
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_a0_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 19, "h_a0_12_34".U)
      checkRegsInTop(total, 18, "h_a0_00_00".U)
      checkRegsInTop(total, 9, 5.U) // todo 这里实际跑出来结果是 2
      checkRegsInTop(total, 8, 4.U)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      run_instructions(total, 30)
      // todo 这里是 0 ， 这个测试过不了，原因未知
      checkRegsInTop(total, 31, 1.U)
      total.io.led.led.expect(1.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place7.2 : jump to case 5 if input 5 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_12_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      // todo 这里有lb导致的错误
      checkRegsInTop(total, 28, "h_12".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_2".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 29, "h_2".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_a0_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_12".U)
      checkRegsInTop(total, 29, "h_2".U)
      checkRegsInTop(total, 31, 1.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place8 : jump to case 6 if input 6 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_c0_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      checkRegsInTop(total, 31, 0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }

  it should "jump to the correct place9 : jump to case 7 if input 7 in button" in {
    load_instructions("software1.txt")
    test(new Top) { total =>
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_20_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_40_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      total.io.switch.switches.poke("h_00_ab_cd".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 29, "h_cd".U)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)

      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
      total.io.switch.switches.poke("h_e0_12_34".U)
      run_instructions(total, 30)
      total.io.btn.button.poke(4.U)
      run_instructions(total, 30)
      total.io.btn.button.poke(0.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 28, "h_ff_ff_ff_ab".U)
      checkRegsInTop(total, 29, "h_cd".U)
      // bgeu
      // todo 这里也出问题了，感觉像是就跳不到了，但也不是，前面那个可以跳到
      checkRegsInTop(total, 31, 1.U)
      run_instructions(total, 30)
      checkRegsInTop(total, 15, 0.U)
    }
  }
}
