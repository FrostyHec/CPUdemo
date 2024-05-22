package integrate

import Generate.Top
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils.InsUtils._
import chisel3._
import core.utils.UARTWrapperTest
import utils.UARTUtils

class TotalTopTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Total Generate.Top Test"

  it should "load instructions correctly from UART" in {
//    //TODO 如果切换了时钟，这个要改
//    //TODO 如果修改了signal的来源，这个也要改
//    test(new Top) { top =>
//      UARTUtils.initFromTop(top)
//      top.io.switch.switches.poke("b1000_0000_0000_0000_0000_0000".U)
//      top.clock.step(10)
//      UARTUtils.sendWordFromTop(top, "h00a0_0093".U)
//      top.io.switch.switches.poke("b0000_0000_0000_0000_0000_0000".U)
//      run_instructions(top)
//      checkRegsInTop(top, 1, 10.U)
//    }
  }

  it should "load instructions correctly from UART ——complex version" in {
//    //TODO 如果切换了时钟，这个要改
//    //TODO 如果修改了signal的来源，这个也要改
//    test(new Top){ top =>
//      UARTUtils.initFromTop(top)
//      top.io.switch.switches.poke("b1000_0000_0000_0000_0000_0000".U)
//      top.clock.step(10)
//      UARTUtils.sendWordFromTop(top, "h00a0_0093".U)
//      UARTUtils.sendWordFromTop(top, "h00a0_8093".U)
//      UARTUtils.sendWordFromTop(top, "h0010_8133".U)
//      top.io.switch.switches.poke("b0000_0000_0000_0000_0000_0000".U)
//      run_instructions(top,3)
//      checkRegsInTop(top, 1, 20.U)
//      checkRegsInTop(top, 2, 40.U)
//    }
  }

  it should "read data from switch 1.0" in {
    load_instructions("switchRead1.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke(1.U)
      run_instructions(total, 2)
      checkRegsInTop(total, 6, 1.U)
    }
  }

  it should "read data from switch 1.1" in {
    load_instructions("switchRead1.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke("h_7f_ff_ff".U)
      run_instructions(total, 2)
      checkRegsInTop(total, 6, "h_7f_ff_ff".U)
    }
  }

  it should "read data from switch 3.1" in {
    /*
      addi x1,x0,-248 // x1 = 0xffffff08 -> switch
      lw x2, 0(x1)
     */
    load_instructions("switchRead3.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke("h_00_00_ff".U)
      run_instructions(total, 2)
      checkRegsInTop(total, 2, "h_00_00_ff".U)
    }
  }

  it should "read data from switch 3.2" in {
    /*
      addi x1,x0,-248 // x1 = 0xffffff08 -> switch
      lw x2, 0(x1)
     */
    load_instructions("switchRead3.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke("h_00_ff_ff".U)
      run_instructions(total, 2)
      checkRegsInTop(total, 2, "h_00_ff_ff".U)
    }
  }

  it should "read data from switch 3.3" in {
    /*
      addi x1,x0,-248 // x1 = 0xffffff08 -> switch
      lw x2, 0(x1)
     */
    load_instructions("switchRead3.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke("h_3f_ff_ff".U)
      run_instructions(total, 2)
      checkRegsInTop(total, 2, "h_3f_ff_ff".U)
    }
  }

  it should "read data from switch 3.4" in {
    /*
      addi x1,x0,-248 // x1 = 0xffffff08 -> switch
      lw x2, 0(x1)
     */
    load_instructions("switchRead3.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke("h_7f_ff_ff".U)
      run_instructions(total, 2)
      checkRegsInTop(total, 2, "h_7f_ff_ff".U)
    }
  }

  it should "read data from switch 3.5" in {
    /*
      addi x1,x0,-248 // x1 = 0xffffff08 -> switch
      lw x2, 0(x1)
     */
    load_instructions("switchRead3.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke("h_70_00_00".U)
      run_instructions(total, 2)
      checkRegsInTop(total, 2, "h_70_00_00".U)
    }
  }

  it should "read data from button 1.0" in {
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

  it should "read data from button 1.1" in {
    /*
    addi x1,x0,-252 // x1 = 0xffffff04 -> btn
    lw x2, 0(x1)
    */
    load_instructions("buttonRead1.txt")
    test(new Top) { total =>
      total.io.btn.button.poke(7.U)
      run_instructions(total,2)
      checkRegsInTop(total, 2, 7.U)
    }
  }

  it should "read data from button 1.2" in {
    /*
    addi x1,x0,-252 // x1 = 0xffffff04 -> btn
    lw x2, 0(x1)
    */
    load_instructions("buttonRead1.txt")
    test(new Top) { total =>
      total.io.btn.button.poke("b_10_100".U)
      run_instructions(total,2)
      checkRegsInTop(total, 2, "b_10_100".U)
    }
  }

  it should "read data from button 1.3" in {
    /*
    addi x1,x0,-252 // x1 = 0xffffff04 -> btn
    lw x2, 0(x1)
    */
    load_instructions("buttonRead1.txt")
    test(new Top) { total =>
      total.io.btn.button.poke("b_11111".U)
      run_instructions(total,2)
      checkRegsInTop(total, 2, "b_11111".U)
    }
  }


  it should "write data to led 1.0" in {
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

  it should "write data to led 2.0" in {
    /*
    addi x1,x0,-256 // x1 = 0xffffff00 -> led
    addi x2, x0, 255
    slli x2, x2, 24
    sw x2, 0(x1)
    */
    load_instructions("ledWrite2.txt")
    test(new Top) { total =>
      total.io.led.led.expect(0.U)
      run_instructions(total,4)
      total.io.led.led.expect(0.U)
    }
  }

  it should "write data to led 3.0" in {
    /*
    addi x1,x0,-256 // x1 = 0xffffff00 -> led
    addi x2, x0, 255
    slli x2, x2, 16
    sw x2, 0(x1)
    */
    load_instructions("ledWrite3.txt")
    test(new Top) { total =>
      total.io.led.led.expect(0.U)
      run_instructions(total,4)
      total.io.led.led.expect("h_ff_00_00".U)
    }
  }

  it should "write data to led 4.0" in {
    /*
    addi x1,x0,-256 // x1 = 0xffffff00 -> led
    addi x2, x0, 255
    slli x2, x2, 8
    addi x2, x2, 255
    slli x2, x2, 8
    addi x2, x2, 255
    sw x2, 0(x1)
    */
    load_instructions("ledWrite4.txt")
    test(new Top) { total =>
      total.io.led.led.expect(0.U)
      run_instructions(total,7)
      total.io.led.led.expect("h_ff_ff_ff".U)
    }
  }


}
