package core.insFetch

import chisel3._
import configs.GenConfig
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
  when(io.fault_occurs){
    if(GenConfig.s.logDetails){
      printf("Write from Fault PC:%d\n",io.fault_write_PC)
    }
    pc:=io.fault_write_PC
  }.elsewhen(io.cpu_state===CPUStateType.sWritePC.getUInt){
    pc:=io.next_addr
  }.otherwise{
    //do nothing
  }
  //--------------------debugging code----------------------------
//  if(GenConfig.s.logDetails){
//    printf("----In PC, cpu_state : %d--- \n", io.cpu_state)
//    printf("input values next_addr: %d \n", io.next_addr)
//    printf("current pc states: %d \n", io.addr)
//  }
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