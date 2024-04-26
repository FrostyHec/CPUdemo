package core.execute

import chisel3._
import chiseltest._
import core.config._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import testutils._
import testutils.ValueUtils._
class RegistersTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Registers"

  def inputs(dut: Registers,
             cpu_state: UInt,
             write: Bool,
             rs1: UInt,
             rs2: UInt,
             rd: UInt,
             write_data: UInt): Unit = {
    dut.io.cpu_state.poke(cpu_state)
    dut.io.write.poke(write)
    dut.io.rs1.poke(rs1)
    dut.io.rs2.poke(rs2)
    dut.io.rd.poke(rd)
    dut.io.write_data.poke(write_data)
  }
  def outputs(dut: Registers,
              rs1_val: UInt,
              rs2_val: UInt): Unit = {
    dut.io.rs1_val.expect(rs1_val)
    dut.io.rs2_val.expect(rs2_val)
  }

  it should "correctly store&read value" in {
    test(new Registers) {registers =>
      inputs(
        registers,
        CPUStateType.sWriteRegs.getUInt,
        true.B,
        3.U,
        3.U,
        3.U,
        100.U
      )
      registers.clock.step()
      outputs(
        registers,
        100.U,
        100.U
      )
    }
  }
  it should "avoid modification on reg0" in {
    test(new Registers) {registers =>
      inputs(
        registers,
        CPUStateType.sWriteRegs.getUInt,
        true.B,
        0.U,
        ignoreVal,
        0.U,
        100.U
      )
      registers.clock.step()
      outputs(
        registers,
        0.U,
        ignoreVal
      )
    }
  }
  it should "avoid write in non-write stage" in {
    test(new Registers) {registers =>
      inputs(
        registers,
        CPUStateType.sWritePC.getUInt,
        true.B,
        3.U,
        ignoreVal,
        3.U,
        100.U
      )
      registers.clock.step()
      outputs(
        registers,
        0.U,
        ignoreVal
      )
    }
  }

  it should "avoid write when write is not enable" in{
    test(new Registers) {registers =>
      inputs(
        registers,
        CPUStateType.sWriteRegs.getUInt,
        false.B,
        3.U,
        ignoreVal,
        3.U,
        100.U
      )
      registers.clock.step()
      outputs(
        registers,
        0.U,
        ignoreVal
      )
    }
  }


  it should "correctly output rs1_val and rs2_val" in {
    test(new Registers) {registers =>
      inputs(
        registers,
        CPUStateType.sWriteRegs.getUInt,
        true.B,
        ignoreVal,
        ignoreVal,
        3.U,
        100.U
      )//3->100
      registers.clock.step()
      inputs(
        registers,
        CPUStateType.sWriteRegs.getUInt,
        true.B,
        ignoreVal,
        ignoreVal,
        31.U,
        10.U
      )//31->10
      registers.clock.step()

      inputs(
        registers,
        CPUStateType.sWritePC.getUInt,
        true.B,
        3.U,
        31.U,
        ignoreVal,
        ignoreVal
      )
      outputs(
        registers,
        100.U,
        10.U
      )
    }
  }
}
