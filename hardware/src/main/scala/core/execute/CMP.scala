package core.execute

import chisel3._
import chisel3.util._
import core.config._

class CMP extends Module{
  val io = IO(new Bundle {
    val operand1: UInt = Input(UInt(32.W))
    val operand2: UInt = Input(UInt(32.W))
    val cmp_op: UInt = Input(CMPType.getWidth)
    val unsigned: Bool = Input(Bool())
    val result: Bool = Output(Bool())
  })
  io.result := DontCare
  switch(io.cmp_op){
    is(CMPType.LT.getUInt){
      when(io.unsigned){
        io.result := io.operand1.asUInt < io.operand2.asUInt
      }.otherwise{
        io.result := io.operand1.asSInt < io.operand2.asSInt
      }
    }
    is(CMPType.GE.getUInt){
      when(io.unsigned){
        io.result := io.operand1.asUInt >= io.operand2.asUInt
      }.otherwise{
        io.result := io.operand1.asSInt >= io.operand2.asSInt
      }
    }
    is(CMPType.EQ.getUInt){
      io.result := io.operand1 === io.operand2
    }
    is(CMPType.NE.getUInt){
      io.result := io.operand1 =/= io.operand2
    }
  }
}

object CMP extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new CMP(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
