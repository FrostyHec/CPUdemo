package Generate

import chisel3._
import configs.GenConfig

class ClockSeparator extends Module {
  val io = IO(new Bundle {
    val cpuClock = Output(Clock())
  })
  if (GenConfig.s.useIPClock) {
    printf("Dont support use ip")
  } else {
    io.cpuClock := clock
  }
}
