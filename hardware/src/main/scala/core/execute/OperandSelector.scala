package core.execute

import chisel3._
import core.config._
class OperandSelector extends Module {
  val io=IO(new Bundle{
    val rs1_val=Input(UInt(32.W))
    val rs2_val=Input(UInt(32.W))
    val real_imm=Input(UInt(32.W))
    val operand2Type=Input(Operand2Type.getWidth)

    val operand1=Output(UInt(32.W))
    val operand2=Output(UInt(32.W))
  })
  //TODO op selector
}
