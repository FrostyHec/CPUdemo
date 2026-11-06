package utils

import chisel3._

object Logger {
  def logPrediction(pc: UInt, next_pc: UInt, success: Bool): Unit = {
    when(success) {
      printf("branch prediction success on %x, predict not jump: %b\n", pc, next_pc === pc + 4.U)
    }.otherwise{
      printf("branch prediction failed on %x, predict not jump: %b\n", pc, next_pc === pc + 4.U)
    }
  }
}
