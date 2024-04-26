package core.execute

import chisel3._
import core.config._
class ALU extends Module {
  val io = IO(new Bundle {
    val operand1 = Input(UInt(32.W))
    val operand2 = Input(UInt(32.W))
    val alu_op = Input(ALUType.getWidth)
    val unsigned=Input(Bool())
    val result = Output(UInt(32.W))
  })
  //TODO alu
}
