package core

import core.config._
import chisel3._

//TODO 检查时序：这个切换是否能满足时序要求？
class CPUState extends Module {
  val io = IO(new Bundle {
    val fault_state = Input(Bool())

    val cpu_state = Output(CPUStateType.getWidth)
  })
  val state = RegInit(CPUStateType.sWriteRegs.getUInt)
  io.cpu_state := state
  when(io.fault_state && state =/= CPUStateType.faultWrite.getUInt) {
    //TODO check correctness ,这个是保证faultWriteState只有一个时钟周期
    state := CPUStateType.faultWrite.getUInt
  }.otherwise {
    when(state === CPUStateType.faultWrite.getUInt) {
      state := CPUStateType.sWriteRegs.getUInt
    }.elsewhen(state === CPUStateType.sWriteRegs.getUInt) {
      state := CPUStateType.sWritePC.getUInt
    }.otherwise {
      state := CPUStateType.sWriteRegs.getUInt
    }
  }
}

