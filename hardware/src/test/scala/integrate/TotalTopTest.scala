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
    load_instructions("switchRead.txt")
    test(new Top) { total =>
      total.io.switch.switches.poke(1.U)
      run_instructions(total,2)
      checkRegsInTop(total, 6, 1.U)
    }
  }
  //TODO testing any wanted io operations

  it should "load data correctly from UART" in{
    //TODO UART需要一个较慢的时钟
  }
}
