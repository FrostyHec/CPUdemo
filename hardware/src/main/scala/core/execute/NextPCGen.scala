package core.execute

import chisel3._
import chisel3.util._
import core.config._

class NextPCGen extends Module{
  val io=IO(new Bundle() {
    val nextPC_type: UInt = Input(NextPCType.getWidth)

    val cmp_result: Bool = Input(Bool())
    val alu_result: UInt = Input(UInt(32.W))
    val imm: UInt = Input(UInt(32.W))
    val pc: UInt = Input(UInt(32.W))

    val nextPC: UInt = Output(UInt(32.W))
    val pc4: UInt = Output(UInt(32.W))
    val pcImm: UInt = Output(UInt(32.W))
  })
  io.pc4 := io.pc + 4.U
  io.pcImm := 0.U//io.imm+io.pc//0.U
  io.nextPC := DontCare//io.pc+4.U
  switch(io.nextPC_type){
    is(NextPCType.PC4.getUInt){
      io.nextPC := io.pc + 4.U
    }
    // J type 的移位直接在immGen里面解决了，这里不再处理
    is(NextPCType.Branch.getUInt){
      io.nextPC := Mux(io.cmp_result, io.pc + io.imm, io.pc + 4.U)
      io.pcImm := io.pc + 4.U
    }
    is(NextPCType.BranchFromALU.getUInt){
//      io.nextPC := 4.U//io.pc + io.alu_result//4.U
      io.nextPC := io.alu_result//io.pc + io.alu_result//4.U
      io.pcImm := io.pc + io.alu_result
    }
  }
}

object NextPCGen extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new NextPCGen(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
