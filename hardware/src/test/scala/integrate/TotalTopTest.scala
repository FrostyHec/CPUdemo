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
    load_instructions("switchRead.txt")
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
}
