package core.execute

import chisel3._
import chiseltest._
import core.config._
import core.insFetch.NextPCGen
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import testutils._
import testutils.ValueUtils._
class NextPCGenTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "NextPCGen"

  def inputs(dut: NextPCGen,
              nextPC_type: UInt,
              cmp_result: Bool,
              alu_result: UInt,
              imm: UInt,
              pc: UInt): Unit = {
      dut.io.nextPC_type.poke(nextPC_type)
      dut.io.cmp_result.poke(cmp_result)
      dut.io.alu_result.poke(alu_result)
      dut.io.imm.poke(imm)
      dut.io.pc.poke(pc)
    }
  def outputs(dut: NextPCGen,
              nextPC: UInt,
              pc4: UInt,
              pcImm: UInt): Unit = {
      dut.io.nextPC.expect(nextPC)
      dut.io.pc4.expect(pc4)
      dut.io.pcImm.expect(pcImm)
    }

  it should "correctly perform pc + 4" in {
    test(new NextPCGen) {dut =>
      inputs(
        dut,
        NextPCType.PC4.getUInt,
        false.B,
        0.U,
        0.U,
        0.U
      )
      dut.clock.step()
      outputs(
        dut,
        4.U,
        4.U,
        0.U
      )
    }
    test(new NextPCGen) {dut =>
      inputs(
        dut,
        NextPCType.PC4.getUInt,
        false.B,
        "h_0f_ff_ff_f0".U,
        "h_0f_ff_ff_f1".U,
        "h_0f_ff_ff_f2".U
      )
      dut.clock.step()
      outputs(
        dut,
        "h_0f_ff_ff_f6".U,
        "h_0f_ff_ff_f6".U,
        0.U
      )
    }
  }

  it should "correctly perform branch" in {
    test(new NextPCGen) {dut =>
      inputs(
        dut,
        NextPCType.Branch.getUInt,
        true.B,
        0.U,
        0.U,
        0.U
      )
      dut.clock.step()
      outputs(
        dut,
        0.U,
        4.U,
        4.U
      )
    }
    test(new NextPCGen) {dut =>
      inputs(
        dut,
        NextPCType.Branch.getUInt,
        false.B,
        0.U,
        0.U,
        0.U
      )
      dut.clock.step()
      outputs(
        dut,
        4.U,
        4.U,
        4.U
      )
    }
    test(new NextPCGen) {dut =>
      dut.io.pc.poke("h_0f_ff_ff_f0".U)
      dut.io.imm.poke("h_0f_ff_ff_f1".U)
      dut.io.cmp_result.poke(true.B)
      dut.io.nextPC_type.poke(NextPCType.Branch.getUInt)
      dut.clock.step()
      dut.io.nextPC.expect("h_1f_ff_ff_e1".U)
      dut.io.pc4.expect("h_0f_ff_ff_f4".U)
      dut.io.pcImm.expect("h_0f_ff_ff_f4".U)
    }
    test(new NextPCGen) {dut =>
      dut.io.pc.poke("h_0f_ff_ff_f0".U)
      dut.io.imm.poke("h_ff_ff_ff_f1".U)
      dut.io.cmp_result.poke(true.B)
      dut.io.nextPC_type.poke(NextPCType.Branch.getUInt)
      dut.clock.step()
      dut.io.nextPC.expect("h_0f_ff_ff_e1".U)
      dut.io.pc4.expect("h_0f_ff_ff_f4".U)
      dut.io.pcImm.expect("h_0f_ff_ff_f4".U)
    }
  }

  // todo 这个测试需要更新： imm在12位以下不会有值
  it should "correctly perform branch from ALU" in {
    test(new NextPCGen) {dut =>
      inputs(
        dut,
        NextPCType.BranchFromALU.getUInt,
        false.B,
        0.U,
        0.U,
        4.U
      )
      dut.clock.step()
      outputs(
        dut,
        8.U,
        8.U,
        4.U
      )
    }
    test(new NextPCGen) {dut =>
      inputs(
        dut,
        NextPCType.BranchFromALU.getUInt,
        false.B,
        0.U,
        0.U,
        "h_0f_ff_ff_f2".U
      )
      dut.clock.step()
      outputs(
        dut,
        "h_0f_ff_ff_f6".U,
        "h_0f_ff_ff_f6".U,
        "h_0f_ff_ff_f2".U
      )
    }
    test(new NextPCGen) {dut =>
      inputs(
        dut,
        NextPCType.BranchFromALU.getUInt,
        false.B,
        "h_ff_ff_00_00".U,
        "h_ff_ff_00_00".U,
        "h_0f_ff_ff_f1".U
      )
      dut.clock.step()
      outputs(
        dut,
        "h_0f_ff_ff_f5".U,
        "h_0f_ff_ff_f5".U,
        "h_0f_fe_ff_f1".U
      )
    }
    test(new NextPCGen) {dut =>
      inputs(
        dut,
        NextPCType.BranchFromALU.getUInt,
        false.B,
        "h_00_ff_00_00".U,
        "h_ff_ff_00_00".U,
        "h_0f_ff_ff_f1".U
      )
      dut.clock.step()
      outputs(
        dut,
        "h_0f_ff_ff_f5".U,
        "h_0f_ff_ff_f5".U,
        "h_10_fe_ff_f1".U
      )
    }
  }

}
