package core.execute

import chisel3._
class Registers extends Module {
  val io=IO(new Bundle{
    val rs1=Input(UInt(5.W))
    val rs2=Input(UInt(5.W))
    val rd=Input(UInt(5.W))
    val write=Input(Bool())
    val write_data=Input(UInt(32.W))

    val rs1_val=Output(UInt(32.W))
    val rs2_val=Output(UInt(32.W))
  })
  //TODO register
}
