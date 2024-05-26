package utils

import chisel3._
import chisel3.util.random._

class PRNGWrapper(width: Int) extends Module {
  val io = IO(new Bundle {
    val random = Output(UInt(width.W))
  })
  if (width < 2) {
    io.random := 0.U //random failed
  } else {
    // 实例化 FibonacciLFSR
    val lfsr = Module(new MaxPeriodFibonacciLFSR(width))

    // 将 LFSR 的输出连接到 IO
    io.random := lfsr.io.out

    // 你可以根据需要设置种子和其他参数
    lfsr.io.seed := 1.U // 设置初始种子（如果需要）
    lfsr.io.increment := true.B // 使能 LFSR 递增
  }
}
