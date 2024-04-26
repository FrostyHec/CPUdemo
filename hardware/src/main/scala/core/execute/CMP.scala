package core.execute

import chisel3._
import core.config._
class CMP extends Module{
  val io = IO(new Bundle {
    val operand1 = Input(UInt(32.W))
    val operand2 = Input(UInt(32.W))
    val cmp_op = Input(CMPType.getWidth)
    val unsigned=Input(Bool())
    val result = Output(Bool())
  })
  //TODO cmp
}
