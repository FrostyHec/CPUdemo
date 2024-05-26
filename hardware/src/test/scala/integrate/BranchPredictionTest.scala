package integrate

import chisel3._
import chiseltest._
import core.CoreTop
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils.InsUtils._

class BranchPredictionTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "branch prediction"
  it should "not branch" in {
    load_instructions("branch/branch_predict1.txt")
    test(new CoreTop) { cpu =>
      //  initial:
      //        addi x1,x0,1
      //        beq x0,x1,next
      //        addi x2,x0,1
      //        j initial
      //  next:
      //         addi x3,x0,1
      run_instructions(cpu,13)
      checkRegsInCPU(cpu,2,1.U)
      checkRegsInCPU(cpu,3,0.U)
    }
  }
}
