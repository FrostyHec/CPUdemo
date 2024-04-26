package core.execute
import chisel3._
import core.config._
class ImmGen extends Module{
  val io=IO(new Bundle{
    val raw_imm=Input(UInt(32.W))
    val unsigned=Input(Bool())
    val imm_width=Input(ImmWidthType.getWidth)

    val real_imm=Output(UInt(32.W))
  })
  //TODO imm gen
}
