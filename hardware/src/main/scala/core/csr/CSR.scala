package core.csr

import chisel3._
import core.config._


class CSR extends Module {
  //TODO CSR Registers
  // 经过学习发现不需要考虑保持时间，因为有buf可以保证组合够长
  val io = IO(new Bundle {
    val cpu_state = Input(CPUStateType.getWidth)

    //csr_instructions
    val write = Input(Bool())

    val csr = Input(UInt(12.W))
    val write_data = Input(UInt(32.W))

    val csr_val = Output(UInt(32.W))

    //fault handling
    val mem_fault = Flipped(new MemFault)
    val ins_fault = Flipped(new InsFault)
    val io_interruption = Flipped(new IOFault)

    val pc = Input(UInt(32.W))

    val fault_state = Output(Bool())
    val fault_write_PC = Output(UInt(32.W))
  })


}
