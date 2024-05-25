package core.memory

import chisel3.util._
import chisel3._
import configs.GenConfig
import core.config._
import core.csr.{IFFault, MEMFault}
import device._
import utils.ExtendEnum

class MemoryDispatch extends Module {
  val io = IO(new Bundle {
    val uart_load = Input(Bool())
    val cpu_state = Input(CPUStateType.getWidth)

    //ins mem io
    val ins_addr = Input(UInt(32.W))
    val ins_out = Output(UInt(32.W))

    // data mem io
    val read_data = Input(Bool())
    val write_data = Input(Bool()) // the write signal will 屏蔽 the read signal
    val unsigned = Input(Bool())
    val data_width = Input(DataWidth.getWidth)

    val data_addr = Input(UInt(32.W))
    val data_write = Input(UInt(32.W))

    val data_out = Output(UInt(32.W))

    //连接board需要external连接outregs
    val external = Flipped(new MMIOOutBundle())

    //fault
    //generate mtval use pc is just ins_addr
    val IF_fault = Output(new IFFault())
    val MEM_fault = Output(new MEMFault())
  })
  //fault default
  io.IF_fault.IF_fault_type := IFFaultType.No.getUInt
  io.IF_fault.mtval:=DontCare
  io.MEM_fault.mem_fault_type := MEMFaultType.No.getUInt
  io.MEM_fault.mtval:=DontCare

  //32读取深度导致的
  val rw_mem_addr = io.data_addr >> 2
  val read_ins_addr = io.ins_addr >> 2

  //ram模块
  val insRAM = Module(new InsRAM()) //insRAM为存储指令的内存，只可data signal写，ins signal读
  val dataRAM = Module(new DataRAM()) //dataRAM为存储数据的内存，只可data signal读写
  val outRegisters = Module(new OutRegisters()) //outRegs靠 data signal读写

  //用于截取位宽
  val data_in = Wire(UInt(32.W))
  val data_out = Wire(UInt(32.W))

  //判断是否是写周期
  val is_write_clk = io.cpu_state === CPUStateType.cycle2_write.getUInt

  //Ins read
  insRAM.io2.read_addr := read_ins_addr
  io.ins_out := insRAM.io2.read_data

  //默认值连线
  insRAM.io.assign(
    write = false.B,
    read_addr = rw_mem_addr,
    write_addr = rw_mem_addr,
    write_data = data_in,
    read_data = DontCare
  )
  dataRAM.io.assign(
    write = false.B,
    read_addr = rw_mem_addr,
    write_addr = rw_mem_addr,
    write_data = data_in,
    read_data = DontCare
  )
  outRegisters.io.mem.assign(
    write = false.B,
    read_addr = rw_mem_addr,
    write_addr = rw_mem_addr,
    write_data = data_in,
    read_data = DontCare
  )
  outRegisters.io.external <> io.external

  //IF错误处理
  when(io.ins_addr(1, 0) =/= 0.U) {
    io.IF_fault.IF_fault_type := IFFaultType.InsMisaligned.getUInt
    io.IF_fault.mtval := io.ins_addr
  }.elsewhen(io.ins_addr > GenConfig.s.insEnd) {
    io.IF_fault.IF_fault_type := IFFaultType.InsFault.getUInt
    io.IF_fault.mtval := io.ins_addr
  }

  //如果探测到错误，一定要屏蔽写操作，其它无所谓
  //注意优先级：必须是MEM先IF后
  //MEM错误
  def setFault(faultType: MEMFaultType.Value, mtval: UInt): Unit = {
    //只有在非loadMode才能触发中断
    when(!io.uart_load) {
      io.MEM_fault.mem_fault_type := faultType.getUInt
      io.MEM_fault.mtval := mtval
    }
  }


  //第二套io:各个RAM的读写操作
  //第二个RAM读
  data_out := DontCare
  when(GenConfig.s.insBegin <= io.data_addr
    && io.data_addr <= GenConfig.s.insEnd) { //insMem
    data_out := insRAM.io.read_data
    when(io.write_data) {
      insRAM.io.write := is_write_clk && io.write_data
    }.elsewhen(io.read_data) {
    }.otherwise {
      //do nothing
    }
  }.elsewhen(GenConfig.s.dataBegin <= io.data_addr
    && io.data_addr <= GenConfig.s.dataEnd) { //dataMem
    //需要处理伪哈佛架构的地址偏移
    val havard_mem = (io.data_addr - GenConfig.s.dataBegin) >> 2
    dataRAM.io.read_addr := havard_mem
    dataRAM.io.write_addr := havard_mem
    data_out := dataRAM.io.read_data
    when(io.write_data) {
      dataRAM.io.write := is_write_clk && io.write_data
    }.elsewhen(io.read_data) {
    }.otherwise {
      //do nothing
    }
  }.elsewhen(GenConfig.s._MMIO.begin <= io.data_addr
    && io.data_addr <= GenConfig.s._MMIO.end) { //outRegs
    data_out := outRegisters.io.mem.read_data
    when(io.write_data) {
      outRegisters.io.mem.write := is_write_clk && io.write_data
    }.elsewhen(io.read_data) {
      data_out := outRegisters.io.mem.read_data
      //      printf("get from out regs %d \n",outRegisters.io.mem.read_data)
      //      printf("From addr: %d\n",outRegisters.io.mem.read_addr)
      //      printf("DATA OUT%d\n",data_out)
    }.otherwise {
      //do nothing
    }
  }.otherwise {
    when(io.read_data) {
      printf("Unexpected Mem address %x", io.data_addr)
      setFault(MEMFaultType.LoadFault, io.data_addr)
    }.elsewhen(io.write_data) {
      printf("Unexpected Mem address %x", io.data_addr)
      setFault(MEMFaultType.StoreFault, io.data_addr)
    }.otherwise {
      //do nothing
    }
  }
  //依据位宽生成data_in
  data_in := DontCare
  switch(io.data_width) {
    is(DataWidth.Byte.getUInt) {
      val value = io.data_write(7, 0)
      switch(io.data_addr(1, 0)) {
        is("b00".U) {
          data_in := Cat(data_out(31, 8), value)
        }
        is("b01".U) {
          data_in := Cat(data_out(31, 16), value, data_out(7, 0))
        }
        is("b10".U) {
          data_in := Cat(data_out(31, 24), value, data_out(15, 0))
        }
        is("b11".U) {
          data_in := Cat(value, data_out(23, 0))
        }
      }
    }
    is(DataWidth.HalfWord.getUInt) {
      val value = io.data_write(15, 0)
      when(io.data_addr(1) === "b0".U) {
        data_in := Cat(data_out(31, 16), value)
      }.otherwise {
        data_in := Cat(value, data_out(15, 0))
      }
    }
    is(DataWidth.Word.getUInt) {
      data_in := io.data_write
    }
  }
  //最后，依据data_width对读到的结果做处理
  //不支持非对齐内存
  //检查misAligned并且报错
  io.data_out := DontCare
  switch(io.data_width) {
    is(DataWidth.Byte.getUInt) {
      switch(io.data_addr(1, 0)) {
        is("b00".U) {
          val high_bit = Fill(24, Mux(io.unsigned, 0.U, data_out(7)))
          io.data_out := Cat(high_bit, data_out(7, 0))
        }
        is("b01".U) {
          val high_bit = Fill(24, Mux(io.unsigned, 0.U, data_out(15)))
          io.data_out := Cat(high_bit, data_out(15, 8))
        }
        is("b10".U) {
          val high_bit = Fill(24, Mux(io.unsigned, 0.U, data_out(23)))
          io.data_out := Cat(high_bit, data_out(23, 16))
        }
        is("b11".U) {
          val high_bit = Fill(24, Mux(io.unsigned, 0.U, data_out(31)))
          io.data_out := Cat(high_bit, data_out(31, 24))
        }
      }
    }
    is(DataWidth.HalfWord.getUInt) {
      //doesn't support unaligned memory access
      when(io.data_addr(0) === 0.U) {
        switch(io.data_addr(1, 0)) {
          is("b00".U) {
            val high_bit = Fill(16, Mux(io.unsigned, 0.U, data_out(15)))
            io.data_out := Cat(high_bit, data_out(15, 0))
          }
          is("b10".U) {
            val high_bit = Fill(16, Mux(io.unsigned, 0.U, data_out(31)))
            io.data_out := Cat(high_bit, data_out(31, 16))
          }
        }
      }.otherwise { //misAligned
        when(io.read_data) {
          setFault(MEMFaultType.LoadMisaligned, io.data_addr)
        }
        when(io.write_data) {
          setFault(MEMFaultType.StoreMisaligned, io.data_addr)
        }
      }
    }
    is(DataWidth.Word.getUInt) {
      when(io.data_addr(1, 0) === 0.U) {
        io.data_out := data_out
      }.otherwise {
        //misAligned
        when(io.read_data) {
          setFault(MEMFaultType.LoadMisaligned, io.data_addr)
        }
        when(io.write_data) {
          setFault(MEMFaultType.StoreMisaligned, io.data_addr)
        }
      }
    }
  }
}


object MemoryDispatch extends App {
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new MemoryDispatch(), //use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
