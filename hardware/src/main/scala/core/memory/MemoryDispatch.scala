package core.memory

import chisel3.util._
import chisel3._
import configs.GenConfig
import core.config._
import core.csr.MemFault
import device._

class MemoryDispatch extends Module {
  val io = IO(new Bundle {
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
    val fault = new MemFault()
  })
  //fault
  io.fault.mem_fault_type := MemFaultType.No.getUInt
  io.fault.mtval:=DontCare

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
  val is_write_clk = io.cpu_state === CPUStateType.sWriteRegs.getUInt


  //依据位宽生成data_in
  data_in := DontCare
  switch(io.data_width) {
    is(DataWidth.Byte.getUInt) {
      data_in := io.data_write(7, 0)
    }
    is(DataWidth.HalfWord.getUInt) {
      data_in := io.data_write(15, 0)
    }
    is(DataWidth.Word.getUInt) {
      data_in := io.data_write
    }
  }

  //默认值连线
  //第一套io:insRAM读
  insRAM.io.assign(
    write = false.B,
    read_addr = read_ins_addr,
    write_addr = rw_mem_addr,
    write_data = data_in,
    read_data = io.ins_out
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

  //第二套io:各个RAM的读写操作
  //第二个RAM读
  data_out := DontCare
  when(GenConfig.s.insBegin <= io.data_addr
    && io.data_addr <= GenConfig.s.insEnd) { //insMem
    when(io.write_data) {
      insRAM.io.write := is_write_clk && io.write_data
    }.elsewhen(io.read_data) {
      printf("Cant read insRAM")
    }.otherwise {
      //do nothing
    }
  }.elsewhen(GenConfig.s.dataBegin <= io.data_addr
    && io.data_addr <= GenConfig.s.dataEnd) { //dataMem
    //需要处理伪哈佛架构的地址偏移
    val havard_mem = (io.data_addr - GenConfig.s.dataBegin) >> 2
    when(io.write_data) {
      dataRAM.io.write_addr := havard_mem
      dataRAM.io.write := is_write_clk && io.write_data
    }.elsewhen(io.read_data) {
      dataRAM.io.read_addr := havard_mem
      data_out := dataRAM.io.read_data
    }.otherwise {
      //do nothing
    }
  }.elsewhen(GenConfig.s._MMIO.begin <= io.data_addr
    && io.data_addr <= GenConfig.s._MMIO.end) { //outRegs
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
    printf("Unexpected address!") //TODO throw err
  }
  //最后，依据data_width对读到的结果做处理
  //不支持非对齐内存
  io.data_out := DontCare
  switch(io.data_width) {
    is(DataWidth.Byte.getUInt) {
      val high_bit = Fill(24, Mux(io.unsigned, 0.U, data_out(7)))
      switch(read_ins_addr(1, 0)) {
        is("b00".U) {
          io.data_out := Cat(high_bit, data_out(7, 0))
        }
        is("b01".U) {
          io.data_out := Cat(high_bit, data_out(15, 8))
        }
        is("b10".U) {
          io.data_out := Cat(high_bit, data_out(23, 16))
        }
        is("b11".U) {
          io.data_out := Cat(high_bit, data_out(31, 24))
        }
      }
    }
    is(DataWidth.HalfWord.getUInt) {
      //doesn't support unaligned memory access
      val high_bit = Fill(16, Mux(io.unsigned, 0.U, data_out(15)))
      switch(read_ins_addr(1, 0)) {
        is("b00".U) {
          io.data_out := Cat(high_bit, data_out(15, 0))
        }
        is("b10".U) {
          io.data_out := Cat(high_bit, data_out(31, 16))
        }
      }
    }
    is(DataWidth.Word.getUInt) {
      io.data_out := data_out
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
