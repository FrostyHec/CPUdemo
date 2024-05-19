package core

import core.config._
import chisel3._
import configs.GenConfig

//TODO 检查时序：这个切换是否能满足时序要求？
class CPUState extends Module {
  val io = IO(new Bundle {
    val fault_state = Input(Bool())

    val cpu_state = Output(CPUStateType.getWidth)
  })
  val state = RegInit(CPUStateType.sWriteRegs.getUInt)
  io.cpu_state := state
  if(GenConfig.s.logDetails){
    printf("current state: %d\n",state)
  }
  when(io.fault_state) {//&& state =/= CPUStateType.faultWrite.getUInt
    //TODO check correctness ,这个是保证faultWriteState只有一个时钟周期
    //TODO check correctness ,直接放弃掉在状态机中存在faultWrite这个状态
    io.cpu_state:=CPUStateType.faultWrite.getUInt
//    state := CPUStateType.faultWrite.getUInt
  }.otherwise {
    when(state === CPUStateType.sWriteRegs.getUInt) {
      state := CPUStateType.sWritePC.getUInt
    }.otherwise {
      state := CPUStateType.sWriteRegs.getUInt
    }
  }
}

