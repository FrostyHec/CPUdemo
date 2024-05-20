package core

import core.config._
import chisel3._

//TODO 检查时序：这个切换是否能满足时序要求？
class CPUState extends Module {
  val io = IO(new Bundle {
    val cpu_state = Output(CPUStateType.getWidth)

    val load_mode = Input(Bool())
  })
  val state = RegInit(CPUStateType.sWriteRegs.getUInt)

  when(!io.load_mode){
    io.cpu_state := state
    when(state===CPUStateType.sLoadMode.getUInt){
      state:=CPUStateType.sWriteRegs.getUInt
    }.elsewhen(state === CPUStateType.sWriteRegs.getUInt) {
      state := CPUStateType.sWritePC.getUInt
    }.otherwise {
      state := CPUStateType.sWriteRegs.getUInt
    }
  }.otherwise{
    io.cpu_state:=CPUStateType.sLoadMode.getUInt // immediate switch mode
    state:=CPUStateType.sLoadMode.getUInt
  }
}

