package core.execute

import chisel3._
import chisel3.util._
import core.config._

class ImmGen extends Module{
  val io = IO(new Bundle{
    val raw_imm: UInt = Input(UInt(32.W))
    val unsigned: Bool = Input(Bool())
    val imm_width: UInt = Input(ImmWidthType.getWidth)

    val real_imm: UInt = Output(UInt(32.W))
  })
  io.real_imm := DontCare
  switch(io.imm_width){
    is(ImmWidthType.Eleven.getUInt){
      when(io.unsigned){
        io.real_imm := Cat(Fill(21, 0.U), io.raw_imm(11, 0))
      }.otherwise{
        io.real_imm := Cat(Fill(21, io.raw_imm(11)), io.raw_imm(11, 0))
      }
    }
    is(ImmWidthType.Twelve.getUInt){
      when(io.unsigned){
        io.real_imm := Cat(Fill(19, 0.U), io.raw_imm(11), io.raw_imm(0), io.raw_imm(10, 1), 0.U)
      }.otherwise{
        io.real_imm := Cat(Fill(20, io.raw_imm(11)), io.raw_imm(11, 0), 0.U)
        //        io.real_imm := Cat(Fill(20, io.raw_imm(11)), io.raw_imm(0), io.raw_imm(10, 1), 0.U)
      }
//      printf("real imm: %d\n", Cat(Fill(20, io.raw_imm(11)), io.raw_imm(11, 0), 0.U))
    }
    is(ImmWidthType.Twenty.getUInt){
      when(io.unsigned){
        io.real_imm := Cat(Fill(11, 0.U), io.raw_imm(19), io.raw_imm(7, 0), io.raw_imm(8) , io.raw_imm(18, 9), 0.U)
      }.otherwise{
        io.real_imm := Cat(Fill(12, io.raw_imm(19)), io.raw_imm(7, 0), io.raw_imm(8) , io.raw_imm(18, 9), 0.U)
      }
    }
    is(ImmWidthType.ThirtyOne.getUInt){
      io.real_imm := Cat(io.raw_imm(19, 0), Fill(12, 0.U))
    }
  }
}

object ImmGen extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new ImmGen(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}

