package integrate

import Generate.Top
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils.InsUtils._
import chisel3._


class PipelineTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Software1 Test"

  it should "perform good in test 1" in {
    /*
      addi x1,x0,5
      addi x3,x0,2
      add x5,x0,0
      sub x2,x1,x3
      and x12,x2,x5
     */
    load_instructions("pipeline/test1.txt")
    test(new Top) { total =>
      run_instructions(total, 5)
      checkRegsInTop(total, 12, 0.U)
    }
  }

  it should "perform good in test 2" in {
    /*
      addi x1,x0,5
      addi x3,x0,2
      add x5,x0,1
      sub x2,x1,x3
      and x12,x2,x5
     */
    load_instructions("pipeline/test2.txt")
    test(new Top) { total =>
      run_instructions(total, 5)
      checkRegsInTop(total, 12, 1.U)
    }
  }

  it should "perform good in test 3" in {
    /*
      addi x1,x0,5
      addi x3,x0,2
      add x5,x0,1
      sub x2,x1,x3
      and x12,x2,x5
     */
    load_instructions("pipeline/test3.txt")
    test(new Top) { total =>
      run_instructions(total, 5)
      checkRegsInTop(total, 13, 3.U)
    }
  }

  it should "perform good in test 4" in {
    /*
      addi x2, x0, 0
      begin:
      addi x1, x0, 1
      beq x0, x1, label
      beq x0, x0, begin
      label2:
      addi x2, x0, 10
      label:
      beq x0,x0,label2
     */
    load_instructions("pipeline/test4.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      checkRegsInTop(total, 2, 0.U)
    }
  }

  it should "perform good in test 5" in {
    /*
      addi x2, x0, 100
      addi x3, x0, 200
      sw x2, 0(x2)
      sw x3, 0(x2)
      lw x4, 0(x2)
     */
    load_instructions("pipeline/test5.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      checkRegsInTop(total, 4, 200.U)
    }
  }

  it should "perform good in test 6" in {
    /*
      addi x2, x0, 100
      addi x3, x0, 200
      sw x2, 0(x2)
      sw x3, 0(x2)
      lw x4, 0(x2)
      addi x4, x4, 100
     */
    load_instructions("pipeline/test6.txt")
    test(new Top) { total =>
      run_instructions(total, 10)
      checkRegsInTop(total, 4, 300.U)
    }
  }
}
