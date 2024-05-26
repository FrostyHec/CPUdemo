package core.insFetch

import chisel3._
import chisel3.util._
import core.config._

class PCPrediction extends Module {
  val io = IO(new Bundle() {
    val pc = Input(UInt(32.W))
    val instruction = Input(UInt(32.W))
    val predict_new_pc = Output(UInt(32.W))
  })
  io.predict_new_pc := io.pc + 4.U
  //jal special judge
  when(io.instruction(6, 0) === "b110_1111".U) {
    val raw_imm = Cat(Fill(11,io.instruction(31)),
      io.instruction(31, 31), io.instruction(19, 12),
      io.instruction(20, 20), io.instruction(30, 21))
    io.predict_new_pc := io.pc + (raw_imm << 1)
  }
  //TODO predict table
}
