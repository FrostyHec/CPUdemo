package core.utils

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.matchers.should.Matchers
import utils._
import testutils.ValueUtils.ignoreVal

class RAMTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "RAM"

  def inputs(dut: RAM, write: Boolean, read_addr: Int, write_addr: Int, write_data: Int, withStep: Boolean): Unit = {
    dut.io.write.poke(write.B)
    dut.io.read_addr.poke(read_addr.U)
    dut.io.write_addr.poke(write_addr.U)
    dut.io.write_data.poke(write_data.U)
    if (withStep) {
      dut.clock.step()
    }
  }

  def outputs(dut: RAM, read_data: Int): Unit = {
    dut.io.read_data.expect(read_data.U)
  }

  it should "correctly store value" in {
    test(new RAM(
      GenConfig.selected.addressWidth,
      GenConfig.selected.dataWidth,
      GenConfig.selected.insMemSize
    )) { ram =>
      inputs(ram,
        write = true,
        read_addr = ignoreVal,
        write_addr = 0,
        write_data = 1,
        withStep = true
      )
      inputs(ram,
        write = false,
        read_addr = 0,
        write_addr = ignoreVal,
        write_data = ignoreVal,
        withStep = true
      )
      outputs(
        ram,
        read_data = 1
      )
    }
    //testing if 64kb position of the memory can be accessed
    test(new RAM(
      GenConfig.selected.addressWidth,
      GenConfig.selected.dataWidth,
      GenConfig.selected.insMemSize
    )) { ram =>

      inputs(ram,
        write = true,
        read_addr = ignoreVal,
        write_addr = 65535,
        write_data = 65535,
        withStep = true
      )
      inputs(ram,
        write = false,
        read_addr = 65535,
        write_addr = ignoreVal,
        write_data = ignoreVal,
        withStep = true
      )
      outputs(
        ram,
        read_data = 65535
      )
    }
    //test that the memory should be written only after one posedge
    test(new RAM(
      GenConfig.selected.addressWidth,
      GenConfig.selected.dataWidth,
      GenConfig.selected.insMemSize
    )) { ram =>
      inputs(ram,
        write = true,
        read_addr = ignoreVal,
        write_addr = 0,
        write_data = 1,
        withStep = false //no posedge
      )
      inputs(ram,
        write = false,
        read_addr = 0,
        write_addr = ignoreVal,
        write_data = ignoreVal,
        withStep = true
      )
      outputs(
        ram,
        read_data = 0
      )
    }
  }
}
