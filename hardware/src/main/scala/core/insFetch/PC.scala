package core.insFetch

import chisel3._
import core.config._
/*
* PC register
*/
class PC extends Module{
  val io=IO(new Bundle {
    val cpu_state=Input(CPUStateType.getWidth)

    val nextAddr =Input(UInt(32.W))

    val addr=Output(UInt(32.W))
  })
  io.addr:=io.nextAddr
  //TODO  PC
}
