package core.execute

import chisel3._
import chiseltest._
import core.config._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import testutils._
import testutils.ValueUtils._
class ALUTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "ALU"

  def inputs(dut: ALU,
             operand1: UInt,
             operand2: UInt,
             alu_op: UInt,
             unsigned: Bool): Unit = {
    dut.io.operand1.poke(operand1)
    dut.io.operand2.poke(operand2)
    dut.io.alu_op.poke(alu_op)
    dut.io.unsigned.poke(unsigned)
  }
  def outputs(dut: ALU,
              result: UInt): Unit = {
    dut.io.result.expect(result)
  }

  it should "correctly do sth" in {
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        10.U,
        12.U,
        ALUType.ADD.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        22.U
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        15.U,
        2.U,
        ALUType.SUB.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        13.U
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        5.U,
        20.U,
        ALUType.SUB.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        "h_ff_ff_ff_f1".U //-15
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        10.U,
        15.U,
        ALUType.XOR.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        5.U
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        12.U,
        20.U,
        ALUType.OR.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        28.U
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        11.U,
        14.U,
        ALUType.AND.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        10.U
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        3.U,
        2.U,
        ALUType.SLL.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        12.U
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        10.U,
        2.U,
        ALUType.SRA.getUInt,
        true.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        2.U
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        "h_ff_ff_ff_f6".U, //-10
        2.U,
        ALUType.SRA.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        "h_ff_ff_ff_fd".U //-3
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        1.U,
        2.U,
        ALUType.SRL.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        0.U
      )
    }
  }

  it should "pass corner cases" in {
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        "h_ff_ff_ff_f1".U, //-15
        "h_ff_ff_ff_f6".U, //-10
        ALUType.ADD.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        "h_ff_ff_ff_e7".U //-25
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        "h_ff_ff_ff_f6".U, //-10
        "h_ff_ff_ff_f1".U, //-15
        ALUType.SUB.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        5.U //15-10
      )
    }
    test(new ALU) {ALUTest =>
      inputs(
        ALUTest,
        "h_ff_ff_ff_f1".U,
        "h_ff_ff_00_00".U,
        ALUType.SLL.getUInt,
        false.B
      )
      ALUTest.clock.step()
      outputs(
        ALUTest,
        "h_ff_ff_ff_f1".U
      )
    }
  }
}