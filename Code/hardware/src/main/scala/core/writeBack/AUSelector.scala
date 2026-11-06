package core.writeBack
import chisel3._
import core.config._
import chisel3.util._
class AUSelector extends Module{
  val io=IO(new Bundle{
    val au_type=Input(AUType.getWidth)

    val alu_result=Input(UInt(32.W))
    val cmp_result=Input(Bool())

    val au_out=Output(UInt(32.W))
  })

  io.au_out := DontCare

  switch(io.au_type) {
    is(AUType.ALU.getUInt) {
      io.au_out := io.alu_result
    }
    is(AUType.CMP.getUInt) {
      io.au_out := io.cmp_result
    }
  }
}

object AUSelector extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new AUSelector(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
