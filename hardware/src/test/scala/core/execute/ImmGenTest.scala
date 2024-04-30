package core.execute

import chisel3._
import chiseltest._
import core.config._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import testutils._
import testutils.ValueUtils._

class ImmGenTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "NextPCGen"

  it should "correctly generate imm when MSB is 11" in {
    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ff_f0".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Eleven.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_ff_ff_f0".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ff_f0".U)
      dut.io.unsigned.poke(true.B)
      dut.io.imm_width.poke(ImmWidthType.Eleven.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_00_00_0f_f0".U)
    }
  }

  it should "correctly generate imm when MSB is 12" in {
    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_00_ff_ff_ff".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Twelve.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_ff_ff_fe".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ff_ff".U)
      dut.io.unsigned.poke(true.B)
      dut.io.imm_width.poke(ImmWidthType.Twelve.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_00_00_1f_fe".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_fa_aa".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Twelve.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_ff_f2_aa".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ea_aa".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Twelve.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_ff_f2_aa".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_e2_aa".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Twelve.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_00_00_02_aa".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_e2_ab".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Twelve.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_00_00_0a_aa".U)
    }
  }

  it should "correctly generate imm when MSB is 20" in {
    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_00_ff_ff_ff".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Twenty.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_ff_ff_fe".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ff_ff".U)
      dut.io.unsigned.poke(true.B)
      dut.io.imm_width.poke(ImmWidthType.Twenty.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_00_1f_ff_fe".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_fa_aa".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Twenty.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_fa_a7_fa".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ea_aa".U)
      dut.io.unsigned.poke(true.B)
      dut.io.imm_width.poke(ImmWidthType.Twenty.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_00_1a_a7_ea".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ea_aa".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.Twenty.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_fa_a7_ea".U)
    }
  }

  it should "correctly generate imm when MSB is 31" in {
    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_00_ff_ff_ff".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.ThirtyOne.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_ff_f0_00".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ff_ff".U)
      dut.io.unsigned.poke(true.B)
      dut.io.imm_width.poke(ImmWidthType.ThirtyOne.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_ff_ff_f0_00".U)
    }

    test(new ImmGen) { dut =>
      dut.io.raw_imm.poke("h_0f_ff_ca_b7".U)
      dut.io.unsigned.poke(false.B)
      dut.io.imm_width.poke(ImmWidthType.ThirtyOne.getUInt)
      dut.clock.step()
      dut.io.real_imm.expect("h_fc_ab_70_00".U)
    }
  }

}
