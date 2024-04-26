package core.decode

import chisel3._
import core.config._
class ControlUnit extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(7.W))
    val func3 = Input(UInt(3.W))
    val func7 = Input(UInt(7.W))
    val rs1=Input(UInt(5.W))
    val rs2=Input(UInt(5.W))
    val rd=Input(UInt(5.W))
    val raw_imm=Input(UInt(20.W))

    //output vals(direct in direct out)
    val rs1_out=Output(UInt(5.W))
    val rs2_out=Output(UInt(5.W))
    val rd_out=Output(UInt(5.W))
    val raw_imm_out=Output(UInt(20.W))

    // output signals
    val alu_type= Output(ALUType.getWidth)
    val cmp_type= Output(CMPType.getWidth)
    val unsigned=Output(Bool())
    val nextPC_type=Output(NextPCType.getWidth)
    val regs_write=Output(Bool())
    val imm_width_type=Output(ImmWidthType.getWidth)
    val operand2_type=Output(Operand2Type.getWidth)
    val au_type=Output(AUType.getWidth)
    val wd_type=Output(WriteBackType.getWidth)
    val memory_read=Output(Bool())
    val memory_write=Output(Bool())
    val data_width=Output(DataWidth.getWidth)
  })
  //TODO control unit
}
