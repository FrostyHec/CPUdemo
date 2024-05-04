package core.decode

import chisel3._
import chisel3.util._
class InstructionDecoder extends Module {
  val io = IO(new Bundle {
    val instruction = Input(UInt(32.W))
    val opcode = Output(UInt(7.W))
    val rs1 = Output(UInt(5.W))
    val rs2 = Output(UInt(5.W))
    val rd = Output(UInt(5.W))
    val func3=Output(UInt(3.W))
    val func7=Output(UInt(7.W))
    val raw_imm = Output(UInt(20.W))
  })

  io.opcode := io.instruction(6, 0)
  io.rd := io.instruction(11, 7)
  io.func3 := io.instruction(14, 12)
  io.rs1 := io.instruction(19, 15)
  io.rs2 := io.instruction(24, 20)
  io.func7 := io.instruction(31, 25)

  io.raw_imm := 0.U

  switch(io.opcode(6, 0)) {
    is("b011_0011".U) { // R-type
      io.raw_imm := 0.U
    }
    is("b001_0011".U) { // I-type
      io.raw_imm := Cat(0.U(8.W), io.instruction(31, 20))
    }
    is("b000_0011".U) { // I-type
      io.raw_imm := Cat(0.U(8.W), io.instruction(31, 20))
    }
    is("b110_0111".U) { // I-type jalr
      io.raw_imm := Cat(0.U(8.W), io.instruction(31, 20))
    }
    is("b111_0011".U) { // I-type system call
      io.raw_imm := io.func7
    }
    is("b010_0011".U) { // S-type
      io.raw_imm := Cat(io.instruction(31, 25), io.instruction(11, 7))
    }
    is("b110_0011".U) { // B-type
      io.raw_imm := Cat(0.U(8.W), io.instruction(31, 31), io.instruction(7, 7),
        io.instruction(30, 25), io.instruction(11, 8))
    }
    is("b011_0111".U) { // U-type
      io.raw_imm := io.instruction(31, 12)
    }
    is("b001_0111".U) { // U-type
      io.raw_imm := io.instruction(31, 12)
    }
    is("b110_1111".U) { // J-type
      io.raw_imm := Cat(io.instruction(31, 31), io.instruction(19, 12),
        io.instruction(20, 20), io.instruction(30, 21))
    }
  }
}

object InstructionDecoder extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new InstructionDecoder(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
