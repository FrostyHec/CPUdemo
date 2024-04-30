package core

import core.config._
import chisel3._

//TODO 检查时序：这个切换是否能满足时序要求？
class CPUState extends Module {
  val io = IO(new Bundle{
    val cpu_state = Output(CPUStateType.getWidth)
  })
  val state = RegInit(CPUStateType.sWritePC.getUInt)
  io.cpu_state := state
  when(state === CPUStateType.sWritePC.getUInt) {
    state := CPUStateType.sWriteRegs.getUInt
  }.otherwise {
    state := CPUStateType.sWritePC.getUInt
  }
}

