package core

import chisel3.util._
import core.config._
import chisel3._
import configs.GenConfig

//TODO 检查时序：这个切换是否能满足时序要求？
class CPUState extends Module {
  val io = IO(new Bundle {
    val stall = Input(Bool())

    val cpu_state = Output(CPUStateType.getWidth)
  })
  val state = RegInit(CPUStateType.cycle1_read.getUInt)
  io.cpu_state:=state
  if (GenConfig.s.logDetails) {
    printf("current state: %d\n, is stall: %d", state,io.stall)
  }
  when(!io.stall) {
    switch(state){
      is(CPUStateType.cycle1_read.getUInt){
        state:=CPUStateType.cycle2_write.getUInt
      }
      is(CPUStateType.cycle2_write.getUInt){
        state:=CPUStateType.cycle3_layer.getUInt
      }
      is(CPUStateType.cycle3_layer.getUInt){
        state:=CPUStateType.cycle1_read.getUInt
      }
    }
  }
}

