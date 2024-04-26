package core.writeBack
import chisel3._
import core.config._
class WriteDataSelector extends Module{
  val io=IO(new Bundle{
    val write_back_type=Input(WriteBackType.getWidth)

    val imm=Input(UInt(32.W))
    val mem_out=Input(UInt(32.W))
    val au_out=Input(UInt(32.W))
    val pc4=Input(UInt(32.W))
    val pcImm=Input(UInt(32.W))

    val write_data=Output(UInt(32.W))
  })
  //TODO write back module
}
