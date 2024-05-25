package core.insFetch

import chisel3._
import chisel3.util._
import core.config._

class NextPCGen extends Module{
  val io=IO(new Bundle() {
    val nextPC_control_signal: UInt = Input(NextPCControlSignal.getWidth)
    val instruction = Input(UInt(32.W))

    val pc: UInt = Input(UInt(32.W))
    val new_pc = Input(UInt(32.W))

    val nextPC: UInt = Output(UInt(32.W))
  })
  //Ins can be used for prediction
  //TODO JAL UNCONDITIONAL BRANCH
  io.nextPC:=io.pc
  switch(io.nextPC_control_signal){
    is(NextPCControlSignal.Normal.getUInt){
      io.nextPC:=io.pc+4.U//TODO  prediction when j
    }
    is(NextPCControlSignal.Stall.getUInt){
      io.nextPC:=io.pc
    }
    is(NextPCControlSignal.NewPC.getUInt){
      io.nextPC:=io.new_pc
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
