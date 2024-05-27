package core.execute

import chisel3._
import chisel3.util._
import core.config._

class ALU extends Module {
  val io = IO(new Bundle {
    val operand1 = Input(UInt(32.W))
    val operand2 = Input(UInt(32.W))
    val alu_op: UInt = Input(ALUType.getWidth)
    val unsigned: Bool = Input(Bool())
    val result: UInt = Output(UInt(32.W))
  })
  io.result := DontCare
  switch(io.alu_op) {
    is(ALUType.ADD.getUInt) {
      io.result := io.operand1 + io.operand2
    }
    is(ALUType.SUB.getUInt) {
      io.result := io.operand1 - io.operand2
    }
    is(ALUType.XOR.getUInt) {
      io.result := io.operand1 ^ io.operand2
    }
    is(ALUType.OR.getUInt) {
      io.result := io.operand1 | io.operand2
    }
    is(ALUType.AND.getUInt) {
      io.result := io.operand1 & io.operand2
    }
    is(ALUType.SLL.getUInt) {
      io.result := io.operand1 << io.operand2(4, 0)
    }
    is(ALUType.SRA.getUInt) { // shift right arithmetic
      when(!io.unsigned) {
        io.result := (io.operand1.asSInt >> io.operand2(4, 0).asUInt).asUInt
      }.otherwise {
        io.result := io.operand1 >> io.operand2(4, 0)
      }
    }
    is(ALUType.SRL.getUInt) { // shift right logical
      io.result := (io.operand1 >> io.operand2(4, 0)).asUInt
    }
    is(ALUType.Not2And.getUInt) {
      io.result := (!io.operand2) & io.operand1
    }
  }
}


object ALU extends App { //name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new ALU(), //use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
