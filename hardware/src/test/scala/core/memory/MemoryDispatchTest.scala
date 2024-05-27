package core.memory

import testutils.ValueUtils.{ignoreUInt, ignoreVal}
import chisel3._
import core.config._
import chiseltest._
import configs.GenConfig
import org.scalatest._
import org.scalatest.matchers.should.Matchers

class MemoryDispatchTest extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "MemoryDispatch"

  def input(dut: MemoryDispatch,
            cpu_state: CPUStateType.Value,
            ins_addr: UInt,
            unsigned: Boolean,
            read_data: Boolean,
            write_data: Boolean,
            data_width: DataWidth.Value,
            data_addr: UInt,
            data_write: Int,
            withStep: Boolean
           ): Unit = {
    dut.io.fault_occurs.poke(false.B)
    dut.io.cpu_state.poke(cpu_state.getUInt)
    dut.io.ins_addr.poke(ins_addr)
    dut.io.read_data.poke(read_data.B)
    dut.io.unsigned.poke(unsigned.B)
    dut.io.write_data.poke(write_data.B)
    dut.io.data_width.poke(data_width.getUInt)
    dut.io.data_addr.poke(data_addr)
    dut.io.data_write.poke(data_write.U)
    if (withStep) {
      dut.clock.step()
    }
  }

  def ins_outputs(
                   dut: MemoryDispatch,
                   ins_out: Int,
                 ): Unit = {
    dut.io.ins_out.expect(ins_out.U)
  }

  def data_outputs(
                    dut: MemoryDispatch,
                    data_out: UInt,
                  ): Unit = {
    dut.io.data_out.expect(data_out)
  }

  it should "correctly store instructions" in {
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = 0.U,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Word,
        data_addr = 0.U,
        data_write = 10,
        withStep = true
      )
      ins_outputs(
        memoryDispatch,
        ins_out = 10
      )
      println("_______________test finish_____________")
    }
    //store at 65535
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = 65535.U,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Word,
        data_addr = 65535.U,
        data_write = 10,
        withStep = true
      )
      ins_outputs(
        memoryDispatch,
        ins_out = 10
      )
      println("_______________test finish_____________")
    }
  }

  it should "correctly store value at data memory dispatch" in {
    //1. the base addr of data mem is GenConfig.selected.dataBegin, test to store at here
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataBegin,
        data_write = 10,
        withStep = true
      )
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = 10.U
      )
      println("_______________test finish_____________")
    }
    //test store at the end
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataEnd,
        data_write = 10,
        withStep = true
      )
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataEnd,
        data_write = ignoreVal,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = 10.U
      )
      println("_______________test finish_____________")
    }
  }

  it should "correctly handle data with in data memory" in {
    //test truncate correctness(sb) check by lw
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Byte,
        data_addr = GenConfig.s.dataBegin,
        data_write = 0x0000ffff,
        withStep = true
      )
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = 0xff.U
      )
      println("_______________test finish_____________")
    }
    //test truncate correctness(sh) check by lw
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.HalfWord,
        data_addr = GenConfig.s.dataBegin,
        data_write = 0x0fffffff,
        withStep = true
      )
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = 0xffff.U
      )
      println("_______________test finish_____________")
    }
    //test truncate correctness(lb,lbu,lh,lhu) by sw
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataBegin,
        data_write = 0x0fffffff,
        withStep = true
      )

      def verify(loadType: DataWidth.Value, unsigned: Boolean, correctVal: UInt): Unit = {
        input(
          memoryDispatch,
          cpu_state = CPUStateType.sWriteRegs,
          ins_addr = ignoreUInt,
          unsigned = unsigned,
          read_data = true,
          write_data = false,
          data_width = loadType,
          data_addr = GenConfig.s.dataBegin,
          data_write = ignoreVal,
          withStep = false
        )
        data_outputs(
          memoryDispatch,
          data_out = correctVal
        )
      }

      verify(DataWidth.Byte, true, 0xff.U)
      verify(DataWidth.Byte, false, "hffffffff".U)
      verify(DataWidth.HalfWord, true, 0xffff.U)
      verify(DataWidth.HalfWord, false, "hffffffff".U)
      println("_______________test finish_____________")
    }
    //multi-step testing
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Byte,
        data_addr = GenConfig.s.dataBegin,
        data_write = 0x0000ffff, //the ffff will be truncated to ff
        withStep = true
      )
      //lbu
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Byte,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = 0xff.U
      )
      //lb
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = false,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Byte,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = "hffffffff".U
      )
      //lhw
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.HalfWord,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = 0x00ff.U
      )
      //lw
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = 0xff.U //the ffff will be truncated to ff
      )
      println("_______________test finish_____________")
    }
  }

  it should "correctly mapping the mmio result" in {
    //read from btn
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      memoryDispatch.io.external.btn.button.poke(1.U)
      memoryDispatch.clock.step()
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = 0.U,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s._MMIO.btnAddr,
        data_write = 10,
        withStep = true
      )
      data_outputs(
        memoryDispatch,
        data_out = 1.U
      )
      println("_______________test finish_____________")
    }
    //read from switch
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      memoryDispatch.io.external.switches.switches.poke(1.U)
      memoryDispatch.clock.step()
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = 0.U,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s._MMIO.switchAddr,
        data_write = 10,
        withStep = true
      )
      data_outputs(
        memoryDispatch,
        data_out = 1.U
      )
      println("_______________test finish_____________")
    }
    //store into led
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = 0.U,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s._MMIO.ledAddr,
        data_write = 10,
        withStep = true
      )
      memoryDispatch.io.external.led.led.expect(10.U)
      println("_______________test finish_____________")
    }
    //store into seg7
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = 0.U,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s._MMIO.seg7Addr,
        data_write = 10,
        withStep = true
      )
      memoryDispatch.io.external.seg7.seg7.expect(10.U)
      println("_______________test finish_____________")
    }
  }

  it should "correctly avoid writing into non-writable reg(for cpu side)" in {
    //avoid write btn
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = 0.U,
        unsigned = true,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s._MMIO.btnAddr,
        data_write = 10,
        withStep = true
      )
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = 0.U,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s._MMIO.btnAddr,
        data_write = 10,
        withStep = false
      )
      data_outputs(
        memoryDispatch,
        data_out = 0.U
      )
      println("_______________test finish_____________")
    }
  }
  it should "correctly sb/sh with different aligned" in {
    //sb to addr: xxxx01
    test(new MemoryDispatch) { memoryDispatch =>
      println("_______________test begin_____________")
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = false,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Byte,
        data_addr = (GenConfig.s.dataBegin.litValue+1).U,
        data_write = 10,
        withStep = true
      )
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      // 00_00_0a_00
      data_outputs(
        memoryDispatch,
        data_out = 2560.U
      )
      //----------------
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = false,
        read_data = false,
        write_data = true,
        data_width = DataWidth.Byte,
        data_addr = GenConfig.s.dataBegin,
        data_write = 10,
        withStep = true
      )
      input(
        memoryDispatch,
        cpu_state = CPUStateType.sWriteRegs,
        ins_addr = ignoreUInt,
        unsigned = true,
        read_data = true,
        write_data = false,
        data_width = DataWidth.Word,
        data_addr = GenConfig.s.dataBegin,
        data_write = ignoreVal,
        withStep = false
      )
      // 00_00_0a_0a
      data_outputs(
        memoryDispatch,
        data_out = 2570.U
      )
      println("_______________test finish_____________")
    }
  }
}