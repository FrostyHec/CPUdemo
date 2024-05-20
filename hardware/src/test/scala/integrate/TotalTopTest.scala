package integrate

import Generate.Top
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils.InsUtils._
import chisel3._

class TotalTopTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Total Generate.Top Test"

  it should "read data from switch" in {
    load_instructions("switchRead1.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke(1.U)
      run_instructions(total,2)
      checkRegsInTop(total, 6, 1.U)
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
//  it should "write data to seg7" in {
//    /*
//    addi x1,x0,-260 // x1 = 0xfffffefc -> seg7
//    addi x2,x0,15
//    sw x2, 0(x1)
//    */
//    load_instructions("seg7Write1.txt")
//    test(new Top) { total =>
//      total.io.seg7.seg7.expect(0.U)
//      run_instructions(total,3)
//      total.io.seg7.seg7.expect(15.U)
//    }
//  }

}
