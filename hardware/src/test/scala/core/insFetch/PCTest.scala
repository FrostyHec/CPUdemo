package core.insFetch

import core.config._
import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
class PCTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "PCTest"
  def inputs(dut: PC,
             cpu_state: UInt,
             next_addr: UInt): Unit = {
    dut.io.cpu_state.poke(cpu_state)
    dut.io.next_addr.poke(next_addr)
  }
  def outputs(dut: PC,
              addr:UInt): Unit = {
    dut.io.addr.expect(addr)
  }
  it should "be zero at initial" in{
    test(new PC){ pc=>
      pc.io.addr.expect(0.U)
    }
  }

  it should "correctly store at sWritePC state" in {
    test(new PC) { pc =>
      inputs(
        pc,
        CPUStateType.sWritePC.getUInt,
        100.U)
      pc.clock.step()
      outputs(
        pc,
        100.U
      )
    }
  }
  it should "avoid change value at other state" in {
    test(new PC){ pc=>
      inputs(
        pc,
        CPUStateType.sWriteRegs.getUInt,
        100.U
      )
      pc.clock.step()
      outputs(
        pc,
        0.U
      )
    }
  }
}
