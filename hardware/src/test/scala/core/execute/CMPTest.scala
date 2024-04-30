package core.execute

import chisel3._
import chiseltest._
import core.config._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import testutils._
import testutils.ValueUtils._
class CMPTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "CMP"

  def inputs(dut: CMP,
             operand1: UInt,
             operand2: UInt,
             cmp_op: UInt,
             unsigned: Bool): Unit = {
    dut.io.operand1.poke(operand1)
    dut.io.operand2.poke(operand2)
    dut.io.cmp_op.poke(cmp_op)
    dut.io.unsigned.poke(unsigned)
  }
  def outputs(dut: CMP,
              result: Bool): Unit = {
    dut.io.result.expect(result)
  }

  it should "compare correctly in less than(LT) mode" in {
    // signed
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        10.U,
        12.U,
        CMPType.LT.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        15.U,
        2.U,
        CMPType.LT.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        5.U,
        5.U,
        CMPType.LT.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f1".U,
        "h_ff_ff_ff_f1".U,
        CMPType.LT.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_ff_ff_ff_f1".U,
        CMPType.LT.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_ff_ff_ff_ff".U,
        CMPType.LT.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_0f_ff_ff_f2".U,
        "h_ff_ff_ff_f1".U,
        CMPType.LT.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_0f_ff_ff_f1".U,
        CMPType.LT.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    // unsigned
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        15.U,
        2.U,
        CMPType.LT.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        5.U,
        5.U,
        CMPType.LT.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f1".U,
        "h_ff_ff_ff_f1".U,
        CMPType.LT.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_ff_ff_ff_f1".U,
        CMPType.LT.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_ff_ff_ff_ff".U,
        CMPType.LT.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_0f_ff_ff_f2".U,
        "h_ff_ff_ff_f1".U,
        CMPType.LT.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_0f_ff_ff_f1".U,
        CMPType.LT.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
  }

  it should "compare correctly in greater or equal(GE) mode" in {
    // signed
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        10.U,
        12.U,
        CMPType.GE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        15.U,
        2.U,
        CMPType.GE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        5.U,
        5.U,
        CMPType.GE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f1".U,
        "h_ff_ff_ff_f1".U,
        CMPType.GE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_ff_ff_ff_f1".U,
        CMPType.GE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_ff_ff_ff_ff".U,
        CMPType.GE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_0f_ff_ff_f2".U,
        "h_ff_ff_ff_f1".U,
        CMPType.GE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_0f_ff_ff_f1".U,
        CMPType.GE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    // unsigned
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        15.U,
        2.U,
        CMPType.GE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        5.U,
        5.U,
        CMPType.GE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f1".U,
        "h_ff_ff_ff_f1".U,
        CMPType.GE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_ff_ff_ff_f1".U,
        CMPType.GE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_ff_ff_ff_ff".U,
        CMPType.GE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_0f_ff_ff_f2".U,
        "h_ff_ff_ff_f1".U,
        CMPType.GE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f2".U,
        "h_0f_ff_ff_f1".U,
        CMPType.GE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
  }

  it should "compare correctly in equal(EQ) mode" in {
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        1.U,
        2.U,
        CMPType.EQ.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        1.U,
        2.U,
        CMPType.EQ.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        100.U,
        100.U,
        CMPType.EQ.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        100.U,
        100.U,
        CMPType.EQ.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f0".U,
        "h_ff_ff_ff_f1".U,
        CMPType.EQ.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f1".U,
        "h_ff_ff_ff_f1".U,
        CMPType.EQ.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_0f_ff_ff_f1".U,
        "h_ff_ff_ff_f1".U,
        CMPType.EQ.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f1".U,
        "h_0f_ff_ff_f1".U,
        CMPType.EQ.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
  }

  it should "compare correctly in not equal(NE) mode" in {
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        1.U,
        2.U,
        CMPType.NE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        1.U,
        2.U,
        CMPType.NE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        100.U,
        100.U,
        CMPType.NE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) {CMPT =>
      inputs(
        CMPT,
        100.U,
        100.U,
        CMPType.NE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f0".U,
        "h_ff_ff_ff_f1".U,
        CMPType.NE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f1".U,
        "h_ff_ff_ff_f1".U,
        CMPType.NE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        false.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_0f_ff_ff_f1".U,
        "h_ff_ff_ff_f1".U,
        CMPType.NE.getUInt,
        false.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
    test(new CMP) { CMPT =>
      inputs(
        CMPT,
        "h_ff_ff_ff_f1".U,
        "h_0f_ff_ff_f1".U,
        CMPType.NE.getUInt,
        true.B
      )
      CMPT.clock.step()
      outputs(
        CMPT,
        true.B
      )
    }
  }
}