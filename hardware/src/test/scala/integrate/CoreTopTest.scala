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
    //      addi x1,x0,1
    //      addi x2,x1,1
    //      beq x2,x1,NEXT
    //      addi x3,x0,1
    //      addi x4,x0,1
    //      addi x5,x0,1
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

  it should "lw and sw" in {
    //    addi x1, x0, 108
    //    sw x1, (x2)
    //    add x1, x0, x0
    //    lw x1, (x2)
    load_instructions("lsw1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 1, 108.U)
    }
  }

  it should "lw 1" in {
    /*
        addi x1, x0, 128
        slli x1, x1, 8
        sw x1, 1(x2)
        add x1, x0, x0
        lw x1, 1(x2)
     */
    load_instructions("lw1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 5)
      checkRegsInCPU(cpu, 1, "h_80_00".U)
    }
  }

  it should "lw 2" in {
    /*
        addi x1, x0, 255
        sw x1, 0(x2)
        lb x1, 0(x2)
        sw x1, 0(x2)
        addi x1, x0, 0
        lw x1, 0(x2)
     */
    load_instructions("lw2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 6)
      checkRegsInCPU(cpu, 1, "h_ff_ff_ff_ff".U)
    }
  }

  it should "lb and sb (1)" in {
    //    addi x1, x0, 108
    //    sb x1, (x2)
    //    add x1, x0, x0
    //    lb x1, (x2)
    load_instructions("lsb1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 1, 108.U)
    }
  }

  it should "lb and sb (2)" in {
    //    addi x1, x0, 128
    //    sb x1, (x2)
    //    add x1, x0, x0
    //    lb x1, (x2)
    load_instructions("lsb2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 1, "h_ff_ff_ff_80".U)
    }
  }

  it should "lb and sb (3)" in {
    //    addi x1, x0, 128
    //    sb x1, (x2)
    //    add x1, x0, x0
    //    lbu x1, (x2)
    load_instructions("lsb3.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 1, "h_80".U)
    }
  }

  it should "lb (1)" in {
    //    addi x1, x0, 108
    //    slli x1, x1, 8
    //    sw x1, (x2)
    //    add x1, x0, x0
    //    lb x1, 1(x2)
    load_instructions("lb1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 5)
      checkRegsInCPU(cpu, 1, 108.U)
    }
  }

  it should "lb (2)" in {
    /*
        addi x1, x0, 128
        slli x1, x1, 8
        sw x1, (x2)
        add x1, x0, x0
        lb x1, 1(x2)
     */
    load_instructions("lb2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 5)
      checkRegsInCPU(cpu, 1, "h_ff_ff_ff_80".U)
    }
  }

  it should "lb (3)" in {
    /*
        addi x1, x0, 128
        slli x1, x1, 8
        sw x1, (x2)
        add x1, x0, x0
        lbu x1, 1(x2)
     */
    load_instructions("lb3.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 5)
      checkRegsInCPU(cpu, 1, 128.U)
    }
  }

  it should "not bgeu" in {
    //      addi x1, x0, 1
    //      addi x2, x0, 2
    //      bgeu x1, x2, NEXT
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //      addi x5, x0, 1
    //      NEXT:
    //      addi x3, x0, 10
    //    -> x3=10,x4=1,x5=1
    load_instructions("bgeu1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 7)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 1.U)
      checkRegsInCPU(cpu, 5, 1.U)
    }
  }

  it should "bgeu" in {
    //      addi x1, x0, 2
    //      addi x2, x0, 1
    //      bgeu x1, x2, NEXT
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //      addi x5, x0, 1
    //      NEXT:
    //      addi x3, x0, 10
    //    -> x3=10,x4=0,x5=0
    load_instructions("bgeu2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 0.U)
      checkRegsInCPU(cpu, 5, 0.U)
    }
  }

  it should "blt" in {
    //      addi x1, x0, 1
    //      addi x2, x0, 2
    //      blt x1, x2, NEXT
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //      addi x5, x0, 1
    //      NEXT:
    //      addi x3, x0, 10
    //    -> x3=10,x4=0,x5=0
    load_instructions("blt1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 0.U)
      checkRegsInCPU(cpu, 5, 0.U)
    }
  }

  it should "not blt" in {
    //      addi x1, x0, 2
    //      addi x2, x0, 1
    //      blt x1, x2, NEXT
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //      addi x5, x0, 1
    //      NEXT:
    //      addi x3, x0, 10
    //    -> x3=10,x4=1,x5=1
    load_instructions("blt2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 7)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 1.U)
      checkRegsInCPU(cpu, 5, 1.U)
    }
  }

  it should "blt with sign" in {
    /*
        addi x1, x0, -1
        addi x2, x0, 2
        blt x1, x2, NEXT
        addi x3, x0, 1
        addi x4, x0, 1
        addi x5, x0, 1
        NEXT:
        addi x3, x0, 10
      -> x3=10,x4=0,x5=0
     */
    load_instructions("blt3.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 0.U)
      checkRegsInCPU(cpu, 5, 0.U)
    }
  }

  it should "blt with sign 2" in {
    /*
        addi x28, x0, 255
        slli x28, x28, 8
        addi x28, x28, 255
        slli x28, x28, 8
        addi x28, x28, 255
        slli x28, x28, 8
        addi x28, x28, 171
        addi x2, x0, 2
        blt x28, x2, NEXT
        addi x3, x0, 1
        addi x4, x0, 1
        addi x5, x0, 1
        NEXT:
        addi x3, x0, 10
        add x1, x0, x0
        add x1, x0, x0
        add x1, x0, x0
        add x1, x0, x0
      -> x3=10,x4=0,x5=0
     */
    load_instructions("blt4.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 15)
      checkRegsInCPU(cpu, 28, "h_ff_ff_ff_ab".U)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 0.U)
      checkRegsInCPU(cpu, 5, 0.U)
    }
  }

  it should "jalr" in {
    //      addi x1, x0, 8
    //      jalr x2, x1, 4
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //      addi x5, x0, 1
    //    -> x2=8,x3=0,x4=1,x5=1
    load_instructions("jalr1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 2, 8.U)
      checkRegsInCPU(cpu, 3, 0.U)
      checkRegsInCPU(cpu, 4, 1.U)
      checkRegsInCPU(cpu, 5, 1.U)
    }
  }

  it should "not bne" in {
    /*
        addi x1, x0, 1
        addi x2, x0, 1
        bne x1, x2, NEXT
        addi x3, x0, 1
        addi x4, x0, 1
        addi x5, x0, 1
        NEXT:
        addi x3, x0, 10
        -> x3=10,x4=1,x5=1
     */
    load_instructions("bne1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 7)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 1.U)
      checkRegsInCPU(cpu, 5, 1.U)
    }
  }

  it should "bne" in {
    /*
        addi x1, x0, 1
        addi x2, x0, 2
        bne x1, x2, NEXT
        addi x3, x0, 1
        addi x4, x0, 1
        addi x5, x0, 1
        NEXT:
        addi x3, x0, 10
        -> x3=10,x4=0,x5=0
     */
    load_instructions("bne2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 7)
      checkRegsInCPU(cpu, 3, 10.U)
      checkRegsInCPU(cpu, 4, 0.U)
      checkRegsInCPU(cpu, 5, 0.U)
    }
  }

  it should "not jalr" in {
    //      addi x1, x0, 8
    //      jalr x2, x1, 0
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //      addi x5, x0, 1
    //    -> x2=8,x3=1,x4=1,x5=1
    load_instructions("jalr2.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 5)
      checkRegsInCPU(cpu, 2, 8.U)
      checkRegsInCPU(cpu, 3, 1.U)
      checkRegsInCPU(cpu, 4, 1.U)
      checkRegsInCPU(cpu, 5, 1.U)
    }
  }

  it should "jal" in {
    //      jal x1, 8
    //      addi x2, x0, 1
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //    -> x1=4,x2=0,x3=1,x4=1
    load_instructions("jal1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 3)
      checkRegsInCPU(cpu, 1, 4.U)
      checkRegsInCPU(cpu, 2, 0.U)
      checkRegsInCPU(cpu, 3, 1.U)
      checkRegsInCPU(cpu, 4, 1.U)
    }
  }

  it should "lui" in {
    //      lui x1, 0x12345
    //      addi x2, x0, 1
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //    -> x1=0x12345000,x2=1,x3=1,x4=1
    load_instructions("lui1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 1, "h12345000".U)
      checkRegsInCPU(cpu, 2, 1.U)
      checkRegsInCPU(cpu, 3, 1.U)
      checkRegsInCPU(cpu, 4, 1.U)
    }
  }

  it should "auipc" in {
    //      auipc x1, 0x12345
    //      addi x2, x0, 1
    //      addi x3, x0, 1
    //      addi x4, x0, 1
    //    -> x1=0x12345000,x2=1,x3=1,x4=1
    load_instructions("auipc1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 4)
      checkRegsInCPU(cpu, 1, "h12345000".U)
      checkRegsInCPU(cpu, 2, 1.U)
      checkRegsInCPU(cpu, 3, 1.U)
      checkRegsInCPU(cpu, 4, 1.U)
    }
  }

  it should "be able to self write" in {
    //            addi x1,x0,1
    //            addi x1,x1,1
    //            ->x1==2
    load_instructions("selfwrite.txt")
    test(new CoreTop){cpu=>
      run_instructions(cpu,2)
      checkRegsInCPU(cpu,1,2.U)
    }
  }

  it should "load and save" in {
    /*
        addi x1, x0, 128
        sw x1, 0(x2)
        add x1, x0, 234
        sw x1, 4(x2)
        lw x1, 0(x2)
        lw x3, 4(x2)
     */
    load_instructions("loadSave1.txt")
    test(new CoreTop) { cpu =>
      run_instructions(cpu, 6)
      checkRegsInCPU(cpu, 1, 128.U)
      checkRegsInCPU(cpu, 3, 234.U)
    }
  }

}
