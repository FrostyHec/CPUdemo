package integrate

import Generate.Top
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils.InsUtils._
import chisel3._
import core.utils.UARTTest
import utils.UARTUtils

class TotalTopTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Total Generate.Top Test"

  it should "read data from switch" in {
    load_instructions("switchRead1.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke(1.U)
      run_instructions(total, 2)
      checkRegsInTop(total, 6, 1.U)
    }
  }

  //TODO testing any wanted io operations

  it should "load instructions correctly from UART" in {
    //TODO 如果切换了时钟，这个要改
    //TODO 如果修改了signal的来源，这个也要改
    test(new Top) { top =>
      UARTUtils.initFromTop(top)
      top.io.switch.switches.poke("b1000_0000_0000_0000_0000_0000".U)
      top.clock.step(10)
      UARTUtils.sendWordFromTop(top, "h00a0_0093".U)
      top.io.switch.switches.poke("b0000_0000_0000_0000_0000_0000".U)
      run_instructions(top)
      checkRegsInTop(top, 1, 10.U)
    }
  }

  it should "load instructions correctly from UART ——complex version" in {
    //TODO 如果切换了时钟，这个要改
    //TODO 如果修改了signal的来源，这个也要改
    test(new Top) { top =>
      UARTUtils.initFromTop(top)
      top.io.switch.switches.poke("b1000_0000_0000_0000_0000_0000".U)
      top.clock.step(10)
      UARTUtils.sendWordFromTop(top, "h00a0_0093".U)
      UARTUtils.sendWordFromTop(top, "h00a0_8093".U)
      UARTUtils.sendWordFromTop(top, "h0010_8133".U)
      top.io.switch.switches.poke("b0000_0000_0000_0000_0000_0000".U)
      run_instructions(top,3)
      checkRegsInTop(top, 1, 20.U)
      checkRegsInTop(top, 2, 40.U)
    }
  }


  it should "read data from button" in {
    /*
    addi x1,x0,-252 // x1 = 0xffffff04 -> btn
    lw x2, 0(x1)
    */
    load_instructions("buttonRead1.txt")
    test(new Top) { total =>
      total.io.btn.button.poke(4.U)
      run_instructions(total,2)
      checkRegsInTop(total, 2, 4.U)
    }
  }

  it should "write data to led" in {
    /*
    addi x1,x0,-256 // x1 = 0xffffff00 -> led
    addi x2,x0,15
    sw x2, 0(x1)
    */
    load_instructions("ledWrite1.txt")
    test(new Top) { total =>
      total.io.led.led.expect(0.U)
      run_instructions(total,3)
      total.io.led.led.expect(15.U)
    }
  }

  // todo seg7 not implemented yet
  it should "write data to seg7" in {
    /*
    addi x1,x0,-260 // x1 = 0xfffffefc -> seg7
    addi x2,x0,15
    sw x2, 0(x1)
    */
    load_instructions("seg7Write1.txt")
    test(new Top) { total =>
      total.io.seg7.seg7_high.expect(0.U)
      total.io.seg7.seg7_low.expect(0.U)
      run_instructions(total,3)
      total.io.seg7.seg7_high.expect(0.U)
      total.io.seg7.seg7_low.expect(15.U)
    }
  }


}
