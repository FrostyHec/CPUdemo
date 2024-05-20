package Generate

import chisel3._
import configs.GenConfig

class ClockSeparator extends Module {
  val io = IO(new Bundle {
    val cpuClock = Output(Clock())
    val uartClock = Output(Clock())
  })//TODO时钟分频
  if (GenConfig.s.useIPClock) {
    printf("Doesn't support use ip")
  } else {
    io.cpuClock := clock
  }
}
