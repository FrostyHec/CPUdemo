package core.writeBack
import chisel3._
import core.config._
class AUSelector extends Module{
  val io=IO(new Bundle{
    val au_type=Input(AUType.getWidth)

    val alu_result=Input(UInt(32.W))
    val cmp_result=Input(Bool())

    val au_out=Output(UInt(32.W))
  })
  //TODO AU selector
}
