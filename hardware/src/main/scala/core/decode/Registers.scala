package core.decode

import chisel3._
import configs.GenConfig
import core.config.{CPUStateType, RegDebugIO}

/*
* risc32i-register
 */
class Registers extends Module {
  val io = IO(new Bundle {
    val cpu_state = Input(CPUStateType.getWidth)

    val write = Input(Bool())

    val rs1 = Input(UInt(5.W))
    val rs2 = Input(UInt(5.W))
    val rd = Input(UInt(5.W))
    val write_data = Input(UInt(32.W))

    val rs1_val = Output(UInt(32.W))
    val rs2_val = Output(UInt(32.W))
  })
  val regs = RegInit(VecInit(Seq.fill(32)(0.U(32.W))))
  io.rs1_val := regs(io.rs1)
  io.rs2_val := regs(io.rs2)
  when(io.cpu_state === CPUStateType.cycle1_read.getUInt//先写后读，在cycle1写在cycle2读
    && io.write
    && io.rd =/= 0.U
  ) {
    if(GenConfig.s.logDetails){
      printf("write to reg[%d] with data %d\n", io.rd, io.write_data)
    }
    regs(io.rd) := io.write_data
  }.otherwise {
    //不写入
  }

  //--------------------debugging code----------------------------
  val debug_io = if (GenConfig.s.debugMode) Some(IO(new RegDebugIO)) else None
  debug_io.foreach(dbg =>
    dbg.reg_vals := regs
  )
//  if(GenConfig.s.logDetails){
//    printf("----In regs, cpu_state : %d--- \n", io.cpu_state)
//    printf("input values write: %d, rs1: %d, rs2: %d, rd: %d, write_data: %d \n", io.write, io.rs1, io.rs2, io.rd, io.write_data)
//    //print all 32 regs, each line 4 regs
//    printf("prev reg states\n")
//    for(i <- 0 until 32){
//      printf("reg[%d ]: %d\t\t,", i.U, regs(i))
//      when(i.U % 4.U === 3.U){
//        printf("\n")
//      }
//    }
//  }
}

object Registers extends App { //name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new Registers(), //use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}