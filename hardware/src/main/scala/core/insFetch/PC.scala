package core.insFetch

import chisel3._
import core.config._
/*
* PC register
*/
class PC extends Module{
  val io=IO(new Bundle {
    val cpu_state=Input(CPUStateType.getWidth)

    val next_addr =Input(UInt(32.W))

    val addr=Output(UInt(32.W))
  })
  val pc=RegInit(0.U(32.W))
  io.addr:=pc
  when(io.cpu_state===CPUStateType.sWritePC.getUInt){
    pc:=io.next_addr
  }.otherwise{
    //do nothing
  }
}
object PC extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new PC(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}