package core.decode

import chisel3._
import chiseltest._
import core.config._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import testutils._

class InstructionDecoderTest extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "InstructionDecoder"

  def inputs(dut: InstructionDecoder,
             instruction: UInt): Unit = {
    dut.io.instruction.poke(instruction)
  }
  def outputs(dut: InstructionDecoder,
              opcode: UInt,
              rs1: UInt,
              rs2: UInt,
              rd: UInt,
              func3: UInt,
              func7: UInt,
              raw_imm: UInt): Unit = {
    dut.io.opcode.expect(opcode)
    dut.io.rs1.expect(rs1)
    dut.io.rs2.expect(rs2)
    dut.io.rd.expect(rd)
    dut.io.func3.expect(func3)
    dut.io.func7.expect(func7)
    dut.io.raw_imm.expect(raw_imm)
  }

  it should "correctly decode R-type instructions" in {
    test(new InstructionDecoder) { dut =>
      inputs(dut, "b0000_0000_0001_0000_0000_0001_1011_0011".U) // R-type example
      dut.clock.step()
      outputs(
        dut,
        "b0110011".U, // opcode
        "b00000".U,  // rs1
        "b00001".U,  // rs2
        "b00011".U,  // rd
        "b000".U,    // func3
        "b0000000".U,// func7
        0.U          // raw_imm
      )
    }
  }

  it should "correctly decode I-type instructions" in {
    test(new InstructionDecoder) { dut =>
      inputs(dut, "b0011_0000_0010_0000_1000_0001_0001_0011".U) // I-type example
      dut.clock.step()
      outputs(
        dut,
        "b0010011".U, // opcode
        "b00001".U,  // rs1
        "b00010".U,  // rs2 (not used in I-type but may still be decoded)
        "b00010".U,  // rd
        "b000".U,    // func3
        "b0011000".U,// func7 (not used in I-type but may still be decoded)
        "b001100000010".U // raw_imm (12-bit)
      )
    }
    // Remember to add tests that check for incorrect decodings or illegal instructions as those are also important 'corner cases'.
  }

  it should "correctly decode U-type instructions with zero extension" in {
    test(new InstructionDecoder) { dut =>
      inputs(dut, "b1000_0000_0010_0000_0000_0000_1011_0111".U) // U-type example
      dut.clock.step()
      outputs(
        dut,
        "b0110111".U, // opcode
        "b00000".U,  // rs1 (not used in U-type but may still be decoded)
        "b00010".U,  // rs2 (not used in U-type but may still be decoded)
        "b00001".U,  // rd
        "b000".U,    // func3 (not used in U-type but may still be decoded)
        "b1000000".U,// func7 (not used in U-type but may still be decoded)
        "b10000000001000000000".U // raw_imm (20-bit, with zero extension)
      )
    }
  }
}
