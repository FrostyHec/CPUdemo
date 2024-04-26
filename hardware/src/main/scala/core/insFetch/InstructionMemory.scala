package core.insFetch

import chisel3._
class InstructionMemory extends Module {
  val io = IO(new Bundle {
    val addr = Input(UInt(32.W))
    val instruction = Output(UInt(32.W))
  })
  //TODO ins mem
}
