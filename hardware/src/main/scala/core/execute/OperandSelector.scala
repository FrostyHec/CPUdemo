package core.execute

import chisel3._
import chisel3.util._
import core.config._

class OperandSelector extends Module {
  val io=IO(new Bundle{
    val rs1_val: UInt = Input(UInt(32.W))
    val rs2_val: UInt = Input(UInt(32.W))
    val real_imm: UInt = Input(UInt(32.W))
    val operand2Type: UInt = Input(Operand2Type.getWidth)

    val operand1: UInt = Output(UInt(32.W))
    val operand2: UInt = Output(UInt(32.W))
  })
  io.operand1 := io.rs1_val
  io.operand2 := DontCare
  switch(io.operand2Type){
    is(Operand2Type.Imm.getUInt){
      io.operand2 := io.real_imm
    }
    is(Operand2Type.Reg2.getUInt){
      io.operand2 := io.rs2_val
    }
  }
}

object OperandSelector extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new OperandSelector(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
