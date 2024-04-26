package core.decode

import chisel3._
class InstructionDecoder extends Module {
  val io = IO(new Bundle {
    val instruction = Input(UInt(32.W))
    val opcode = Output(UInt(7.W))
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    val rd = Output(UInt(5.W))
    val func3=Output(UInt(3.W))
    val func7=Output(UInt(7.W))
    val raw_imm = Output(UInt(20.W))
  })
  //TODO ins decoder
}
