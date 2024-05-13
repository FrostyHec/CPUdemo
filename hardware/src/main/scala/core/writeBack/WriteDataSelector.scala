package core.writeBack
import chisel3._
import core.config._
import chisel3.util._
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

  io.write_data := DontCare

  switch(io.write_back_type) {
    is(WriteBackType.AU.getUInt) {
      io.write_data := io.au_out
    }
    is(WriteBackType.ImmGen.getUInt) {
      io.write_data := io.imm
    }
    is(WriteBackType.Mem.getUInt) {
      io.write_data := io.mem_out
    }
    is(WriteBackType.PC4.getUInt) {
      io.write_data := io.pc4
    }
  }
}

object WriteDataSelector extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new WriteDataSelector(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
