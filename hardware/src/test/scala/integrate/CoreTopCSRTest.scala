package integrate

import chisel3._
import chiseltest._
import core.CoreTop
import utils.InsUtils._
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class CoreTopCSRTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CPU Core on CSR"
  it should "behave correctly on csr instructions " in {
    //addi x1,x0,1
    //csrrw x2,mtval,x1
    //csrrs x2,mtval,x0
    // -> x2=1,mtval=1
    load_instructions("csr/csrIns1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 3)
      checkRegsInCPU(cpu, 2, 1.U)
      getCSRInCPU(cpu).mtval.expect(1.U)
    }
    //请帮我生成一些合适地样例用于测试csrrw,csrrs,csrrc,csrrwi,csrrsi,csrrci六条指令的正确性
    //我对chat说:你首先像上面一样生成汇编代码注释，然后类似于上面的操作调库写测试代码即可，.txt由我来生成，请开始
    //addi x1,x0,1
    //csrrw x2,mtval,x1
    //csrrc x2,mtval,x1
    // -> x2=1,mtval=0
    load_instructions("csr/csrIns2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 3)
      checkRegsInCPU(cpu, 2, 1.U)
      getCSRInCPU(cpu).mtval.expect(0.U)
    }

    //addi x1,x0,1
    //csrrwi x2,mtval,1
    //csrrsi x2,mtval,1
    // -> x2=1,mtval=1
    load_instructions("csr/csrIns3.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 3)
      checkRegsInCPU(cpu, 2, 1.U)
      getCSRInCPU(cpu).mtval.expect(1.U)
    }
  }

  it should "correctly interrupt on trap instructions" in {
    //    addi x1,x0,16
    //    csrrw x0,mtvec,t0
    //    beqz x0, test
    //
    //interrupt:
    //    addi x3,x0,20
    //    csrrs x4,mepc,x0
    //    addi x4,x4,4
    //    csrrw x0,mepc,x4
    //    mret
    //    beqz x0, interrupt // should be useless
    //
    //test:
    //    ecall
    //    addi x2,x0,10
    // 3->20.U, 2->10.U
    load_instructions("fault/faultIns1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 10)
      checkRegsInCPU(cpu, 3, 20.U)
      checkRegsInCPU(cpu, 2, 10.U)
    }
  }
  it should "avoid illegal mem access" in {
    load_instructions("fault/memFault.txt")
    test(new CoreTop){cpu=>
      run_instructions(cpu,12)
      checkRegsInCPU(cpu,4,100.U)
      checkRegsInCPU(cpu,3,20.U)
      checkRegsInCPU(cpu,6,1000.U)
    }

  }
}
