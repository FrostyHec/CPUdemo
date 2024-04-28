package core.execute

import chisel3.util._
import chisel3._
import core.config.CPUStateType

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

  when(io.cpu_state === CPUStateType.sWriteRegs.getUInt
    && io.write
    && io.rd =/= 0.U) {
    regs(io.rd) := io.write_data
  }.otherwise{
    //不写入
  }
}
object Registers extends App {//name had better to be same as class name, put under the class file
  // These lines generate the Verilog output
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new Registers(),//use your module class
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}