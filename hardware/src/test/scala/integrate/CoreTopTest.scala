package integrate

import chisel3._
import chiseltest._
import core.CoreTop
import utils.InsUtils._
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class CoreTopTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CPU Core"
  it should "addi x1,x0,1 -> x1=1 " in {
    load_instructions("simpleAdd1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu)
      checkRegsInCPU(cpu, 1, 1.U)
    }
  }

  it should "addi x1,x0,-1 x1=>-1" in {
    load_instructions("simpleAdd2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu)
      checkRegsInCPU(cpu, 1, "hffffffff".U)
    }
  }

  it should "beq1" in {
    //    addi x1,x0,1
    //    addi x2,x0,1
    //    beq x2,x1,NEXT
    //    addi x3,x0,1
    //    addi x4,x0,1
    //    addi x5,x0,1
    //    NEXT:
    //      addi x3,x0,10
    //    -> x3=10,x4=0,x5=0

    load_instructions("jump1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 7)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 0.U)
      checkRegsInCPU(cpu, 5, 0.U)
    }
  }
  it should "not beq" in {
    //            addi x1,x0,1
    //            addi x2,x1,1
    //            beq x2,x1,NEXT
    //            addi x3,x0,1
    //            addi x4,x0,1
    //            addi x5,x0,1
    //      NEXT:
    //            addi x3,x0,10
    //            -> x3=10,x4=1,x5=1
    load_instructions("jump2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 7)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 1.U)
      checkRegsInCPU(cpu, 5, 1.U)
    }
  }
  //TODO MINIMUM TESTING INSTRUCTIONS:
  // lb,sb,lw,sw
  // bgeu,blt
  // jalr,jal,lui,auipc

}
