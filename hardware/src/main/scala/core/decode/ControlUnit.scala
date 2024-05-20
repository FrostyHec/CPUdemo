package core.decode

import chisel3._
import core.config._
import chisel3.util._
class ControlUnit extends Module {
  val io = IO(new Bundle {
    val opcode = Input(UInt(7.W))
    val func3 = Input(UInt(3.W))
    val func7 = Input(UInt(7.W))
    val rs1 = Input(UInt(5.W))
    val rs2 = Input(UInt(5.W))
    val rd = Input(UInt(5.W))
    val raw_imm = Input(UInt(20.W))

    //output vals(direct in direct out)
    val rs1_out = Output(UInt(5.W))
    val rs2_out = Output(UInt(5.W))
    val rd_out = Output(UInt(5.W))
    val raw_imm_out = Output(UInt(20.W))

    // output signals
    val alu_type = Output(ALUType.getWidth)
    val cmp_type = Output(CMPType.getWidth)
    val unsigned = Output(Bool())
    val nextPC_type = Output(NextPCType.getWidth)
    val regs_write = Output(Bool())
    val imm_width_type = Output(ImmWidthType.getWidth)
    val operand2_type = Output(Operand2Type.getWidth)
    val au_type = Output(AUType.getWidth)
    val write_back_type = Output(WriteBackType.getWidth)
    val memory_read = Output(Bool())
    val memory_write = Output(Bool())
    val data_width = Output(DataWidth.getWidth)
  })

  io.rs1_out := io.rs1
  io.rs2_out := io.rs2
  io.rd_out := io.rd
  io.raw_imm_out := io.raw_imm

  io.alu_type := DontCare
  io.cmp_type := DontCare
  io.unsigned := DontCare
  io.nextPC_type := DontCare
  io.regs_write := DontCare
  io.imm_width_type := DontCare
  io.operand2_type := DontCare
  io.au_type := DontCare
  io.write_back_type := DontCare
  io.memory_read := DontCare
  io.memory_write := DontCare
  io.data_width := DontCare

  switch(io.opcode) {
    is("b011_0011".U) { // R-type
      io.nextPC_type := NextPCType.PC4.getUInt
      io.regs_write := "b1".U
      io.au_type := AUType.ALU.getUInt
      io.memory_read := "b0".U
      io.memory_write := "b0".U
      io.operand2_type := Operand2Type.Reg2.getUInt
      io.write_back_type := WriteBackType.AU.getUInt
      io.unsigned := "b0".U
      switch(Cat(io.func3, io.func7)) {
        is("b00_0000_0000".U) { // add
          io.alu_type := ALUType.ADD.getUInt
        }
        is("b00_0010_0000".U) { // sub
          io.alu_type := ALUType.SUB.getUInt
        }
        is("b10_0000_0000".U) { // xor
          io.alu_type := ALUType.XOR.getUInt
        }
        is("b11_0000_0000".U) { // or
          io.alu_type := ALUType.OR.getUInt
        }
        is("b11_1000_0000".U) { // and
          io.alu_type := ALUType.AND.getUInt
        }
        is("b00_1000_0000".U) { // sll
          io.alu_type := ALUType.SLL.getUInt
        }
        is("b10_1000_0000".U) { // srl
          io.alu_type := ALUType.SRL.getUInt
        }
        is("b10_1010_0000".U) { //sra
          io.alu_type := ALUType.SRA.getUInt
        }
        is("b01_0000_0000".U) { // slt
          io.au_type := AUType.CMP.getUInt
          io.cmp_type := CMPType.LT.getUInt
        }
        is("b01_1000_0000".U) { // sltu
          io.au_type := AUType.CMP.getUInt
          io.cmp_type := CMPType.LT.getUInt
          io.unsigned := "b1".U
        }
      }
    }

    is("b001_0011".U) { // I-type
      io.nextPC_type := NextPCType.PC4.getUInt
      io.regs_write := "b1".U
      io.au_type := AUType.ALU.getUInt
      io.memory_read := "b0".U
      io.memory_write := "b0".U
      io.operand2_type := Operand2Type.Imm.getUInt
      io.imm_width_type := ImmWidthType.Eleven.getUInt
      io.write_back_type := WriteBackType.AU.getUInt
      io.unsigned := "b0".U
      switch(io.func3) {
        is("b000".U) { // addi
          io.alu_type := ALUType.ADD.getUInt
        }
        is("b100".U) { // xori
          io.alu_type := ALUType.XOR.getUInt
        }
        is("b110".U) { // ori
          io.alu_type := ALUType.OR.getUInt
        }
        is("b111".U) { // andi
          io.alu_type := ALUType.AND.getUInt
        }
        is("b001".U) { // slli
          io.alu_type := ALUType.SLL.getUInt
        }
        is("b101".U) { // srli/srai
          switch(io.func7) { // srli
            is("b000_0000".U) {
              io.alu_type := ALUType.SRL.getUInt
            }
            is("b010_0000".U) { // srai
              io.alu_type := ALUType.SRA.getUInt
            }
          }
        }
        is("b010".U) { // slti
          io.au_type := AUType.CMP.getUInt
          io.cmp_type := CMPType.LT.getUInt
        }
        is("b011".U) { // sltiu
          io.au_type := AUType.CMP.getUInt
          io.cmp_type := CMPType.LT.getUInt
          io.unsigned := "b1".U
        }
      }
    }

    is("b000_0011".U) { // I-type load
      io.nextPC_type := NextPCType.PC4.getUInt
      io.regs_write := "b1".U
      io.au_type := AUType.ALU.getUInt
      io.memory_read := "b1".U
      io.memory_write := "b0".U
      io.operand2_type := Operand2Type.Imm.getUInt
      io.write_back_type := WriteBackType.Mem.getUInt
      io.imm_width_type := ImmWidthType.Eleven.getUInt
      io.unsigned := "b0".U
      io.alu_type := ALUType.ADD.getUInt
      switch(io.func3) {
        is("b000".U) { // lb
          io.data_width := DataWidth.Byte.getUInt
        }
        is("b001".U) { // lh
          io.data_width := DataWidth.HalfWord.getUInt
        }
        is("b010".U) { // lw
          io.data_width := DataWidth.Word.getUInt
        }
        is("b100".U) { // lbu
          io.data_width := DataWidth.Byte.getUInt
          io.unsigned := "b1".U
        }
        is("b101".U) { // lhu
          io.data_width := DataWidth.HalfWord.getUInt
          io.unsigned := "b1".U
        }
      }
    }

    is("b010_0011".U) { // S-type
      io.nextPC_type := NextPCType.PC4.getUInt
      io.regs_write := "b0".U
      io.au_type := AUType.ALU.getUInt
      io.memory_read := "b0".U
      io.memory_write := "b1".U
      io.operand2_type := Operand2Type.Imm.getUInt
      io.write_back_type := DontCare
      io.imm_width_type := ImmWidthType.Eleven.getUInt
      io.unsigned := "b0".U
      io.alu_type := ALUType.ADD.getUInt
      switch(io.func3) {
        is("b000".U) { // sb
          io.data_width := DataWidth.Byte.getUInt
        }
        is("b001".U) { // sh
          io.data_width := DataWidth.HalfWord.getUInt
        }
        is("b010".U) { // sw
          io.data_width := DataWidth.Word.getUInt
        }
      }
    }

    is("b110_0011".U) { // B-type
      io.nextPC_type := NextPCType.Branch.getUInt
      io.regs_write := "b0".U
      io.au_type := AUType.CMP.getUInt
      io.memory_read := "b0".U
      io.memory_write := "b0".U
      io.operand2_type := Operand2Type.Reg2.getUInt
      io.write_back_type := DontCare
      io.unsigned := "b0".U
      io.imm_width_type := ImmWidthType.Twelve.getUInt
      io.alu_type := DontCare
      switch(io.func3) {
        is("b000".U) { //beq
          io.cmp_type := CMPType.EQ.getUInt
        }
        is("b001".U) { //bne
          io.cmp_type := CMPType.NE.getUInt
        }
        is("b100".U) { //blt
          io.cmp_type := CMPType.LT.getUInt
        }
        is("b101".U) { //bge
          io.cmp_type := CMPType.GE.getUInt
        }
        is("b110".U) { //bltu
          io.unsigned := "b1".U
          io.cmp_type := CMPType.LT.getUInt
        }
        is("b111".U) { //bgeu
          io.unsigned := "b1".U
          io.cmp_type := CMPType.GE.getUInt
        }
      }
    }

    is("b110_1111".U) { // J-type jal
      io.nextPC_type := NextPCType.Branch.getUInt
      io.regs_write := "b1".U
      io.au_type := DontCare
      io.memory_read := "b0".U
      io.memory_write := "b0".U
      io.operand2_type := Operand2Type.Imm.getUInt
      io.write_back_type := WriteBackType.PC4.getUInt
      io.imm_width_type := ImmWidthType.Twelve.getUInt
      io.unsigned := "b0".U
      io.alu_type := DontCare
    }

    is("b110_0111".U) { // I-type jalr
      io.nextPC_type := NextPCType.Branch.getUInt
      io.regs_write := "b1".U
      io.memory_read := "b0".U
      io.memory_write := "b0".U
      io.operand2_type := Operand2Type.Imm.getUInt
      io.write_back_type := WriteBackType.PC4.getUInt
      io.imm_width_type := ImmWidthType.Eleven.getUInt
      io.unsigned := "b0".U
      io.au_type := AUType.ALU.getUInt
      io.alu_type := ALUType.ADD.getUInt
    }

    is("b011_0111".U) { // U-type lui
      io.nextPC_type := NextPCType.PC4.getUInt
      io.regs_write := "b1".U
      io.memory_read := "b0".U
      io.memory_write := "b0".U
      io.imm_width_type := ImmWidthType.ThirtyOne.getUInt
      io.write_back_type := WriteBackType.ImmGen.getUInt
      io.unsigned :="b0".U
      io.au_type := DontCare
      io.alu_type :=DontCare
    }

    is("b001_0111".U) { // U-type auipc
      io.nextPC_type := NextPCType.BranchFromALU.getUInt
      io.regs_write := "b1".U
      io.memory_read := "b0".U
      io.memory_write := "b0".U
      io.imm_width_type := ImmWidthType.ThirtyOne.getUInt
      io.write_back_type := WriteBackType.PCImm.getUInt
      io.unsigned := "b0".U
      io.au_type := DontCare
      io.alu_type := DontCare
    }
  }
}

object ControlUnit extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new ControlUnit(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
