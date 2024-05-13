package integrate

import chisel3._
import chiseltest._
import configs.GenConfig
import core.CoreTop
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class CoreTopTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CPU Core"
  private val file_root_path = "src/test/scala/integrate/inst_file/"

  //外部要访问
  def load_instructions(file_path: String): Unit = {
    GenConfig.s.initInsFile = Option(file_root_path + file_path)
  }

  def checkRegs(cpu: CoreTop, targetReg: Int, targetValue: UInt): Unit = {
    require(GenConfig.s.debugMode, "Debug mode is not enabled.")

    cpu.debug_io match {
      case Some(debugIO) =>
        //        printf(debugIO.reg_vals.reg_vals.peek().litValue.toString())
        debugIO.reg_vals.reg_vals(targetReg).expect(targetValue)
      case None =>
        println("Debug IO is not available. Ensure that debug mode is enabled.")
    }
  }

  def run_instructions(cpu: CoreTop, times: Int = 1): Unit = {
    for (i <- 1 to times) {
      println("============================Time " + i + "============================\n")
      cpu.clock.step(2)
    }
  }

  it should "addi x1,x0,1 -> x1=1 " in {
    load_instructions("simpleAdd1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu)
      checkRegs(cpu, 1, 1.U)
    }
  }

  it should "addi x1,x0,-1 x1=>-1" in {
    load_instructions("simpleAdd2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu)
      checkRegs(cpu, 1, "hffffffff".U)
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
      checkRegs(cpu, 3, 10.U)
      checkRegs(cpu, 4, 0.U)
      checkRegs(cpu, 5, 0.U)
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
      checkRegs(cpu, 3, 10.U)
      checkRegs(cpu, 4, 1.U)
      checkRegs(cpu, 5, 1.U)
    }
  }
}
