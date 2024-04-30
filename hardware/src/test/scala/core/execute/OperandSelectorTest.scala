package core.execute

import chisel3._
import chiseltest._
import core.config._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import testutils._
import testutils.ValueUtils._
class OperandSelectorTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "OperandSelector"

  it should "correctly select the Operand" in {
    test(new OperandSelector) {dut =>
      dut.io.rs1_val.poke(1.U)
      dut.io.rs2_val.poke(2.U)
      dut.io.real_imm.poke(3.U)
      dut.io.operand2Type.poke(Operand2Type.Imm.getUInt)
      dut.clock.step()
      dut.io.operand1.expect(1.U)
      dut.io.operand2.expect(3.U)
    }
    test(new OperandSelector) {dut =>
      dut.io.rs1_val.poke(1.U)
      dut.io.rs2_val.poke(2.U)
      dut.io.real_imm.poke(3.U)
      dut.io.operand2Type.poke(Operand2Type.Reg2.getUInt)
      dut.clock.step()
      dut.io.operand1.expect(1.U)
      dut.io.operand2.expect(2.U)
    }
  }

  it should "perform robustly under corner cases" in {
    test(new OperandSelector) {dut =>
      dut.io.rs1_val.poke(2345.U)
      dut.io.rs2_val.poke("h_ff_ff_f0_00".U)
      dut.io.real_imm.poke(1234567.U)
      dut.io.operand2Type.poke(Operand2Type.Imm.getUInt)
      dut.clock.step()
      dut.io.operand1.expect(2345.U)
      dut.io.operand2.expect(1234567.U)
    }
    test(new OperandSelector) {dut =>
      dut.io.rs1_val.poke(1234.U)
      dut.io.rs2_val.poke("h_ff_ff_f0_00".U)
      dut.io.real_imm.poke(0.U)
      dut.io.operand2Type.poke(Operand2Type.Reg2.getUInt)
      dut.clock.step()
      dut.io.operand1.expect(1234.U)
      dut.io.operand2.expect("h_ff_ff_f0_00".U)
    }
  }

}
