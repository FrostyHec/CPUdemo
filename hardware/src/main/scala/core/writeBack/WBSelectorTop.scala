package core.writeBack
import chisel3._
import chisel3.util._
import core.config._
class WBSelectorTop extends Module{
  val io =IO(new Bundle() {
    //au
    val au_type=Input(AUType.getWidth)

    val alu_result=Input(UInt(32.W))
    val cmp_result=Input(Bool())

    //wb
    val write_back_type=Input(WriteBackType.getWidth)

    val imm=Input(UInt(32.W))
    val mem_out=Input(UInt(32.W))
    val pc4=Input(UInt(32.W))
    val pcImm=Input(UInt(32.W))
    val csr_val = Input(UInt(32.W))

    val write_data=Output(UInt(32.W))
  })
  private val au_selector = Module(new AUSelector())
  private val wb_selector = Module(new WriteDataSelector())
  //au->wb
  au_selector.io.au_type := io.au_type
  au_selector.io.alu_result := io.alu_result
  au_selector.io.cmp_result := io.cmp_result
  wb_selector.io.au_out := au_selector.io.au_out

  //wb->out
  wb_selector.io.write_back_type := io.write_back_type
  wb_selector.io.imm := io.imm
  wb_selector.io.mem_out := io.mem_out
  wb_selector.io.pc4 := io.pc4
  wb_selector.io.pcImm := io.pcImm
  wb_selector.io.csr := io.csr_val
  io.write_data := wb_selector.io.write_data
}
