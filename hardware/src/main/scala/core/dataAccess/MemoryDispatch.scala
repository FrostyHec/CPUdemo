package core.dataAccess

import chisel3._
import core.config._
class MemoryDispatch extends Module{
  val io=IO(new Bundle{
    val cpu_state=Input(CPUStateType.getWidth)

    val mem_read=Input(Bool())
    val mem_write=Input(Bool())
    val data_width=Input(DataWidth.getWidth)

    val addr=Input(UInt(32.W))
    val write_data=Input(UInt(32.W))

    val mem_out=Output(UInt(32.W))
  })
  //TODO data memory
}
