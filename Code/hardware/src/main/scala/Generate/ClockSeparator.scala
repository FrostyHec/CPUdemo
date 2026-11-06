package Generate

import chisel3._
import configs.GenConfig

class ClockSeparator extends Module {
  val io = IO(new Bundle {
    val cpuClock = Output(Clock())
    val uartClock = Output(Clock())
  })
  //TODO 时钟分频
  io.uartClock := DontCare
  if (GenConfig.s.useIPClock) {
    val ip_clock = Module(new IPClock(GenConfig.s.ipClockName.get))
    ip_clock.io.clk_in1 := clock
    ip_clock.io.reset:=false.B
    io.cpuClock := ip_clock.io.clk_out2
  } else {
    io.cpuClock := clock
  }
}

class IPClock(name: String) extends BlackBox {
  val io = IO(new Bundle {
    val clk_in1 = Input(Clock()) //100mhz
    val reset = Input(Bool())
    val clk_out1 = Output(Clock()) //100mhz
    val clk_out2 = Output(Clock()) //20mhz
  })

  override def desiredName: String = name
}