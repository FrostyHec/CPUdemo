package core.execute
import chisel3._
import core.config._
class NextPCGen extends Module{
  val io=IO(new Bundle() {
    val nextPC_type=Input(NextPCType.getWidth)

    val cmp_result=Input(Bool())
    val alu_result=Input(UInt(32.W))
    val imm=Input(UInt(32.W))
    val pc=Input(UInt(32.W))

    val nextPC=Output(UInt(32.W))
  })
  //TODO pc Gen
}
