package Generate


import chisel3._
import configs.GenConfig
import core.CoreTop
import core.config.CoreDebugIO
import device.{BoardBundle, DeviceTop}

class Top extends Module {
  val io = IO(new BoardBundle)
  val clockSeparator = Module(new ClockSeparator)
  val cpu = withClock(clockSeparator.io.cpuClock) {
    Module(new CoreTop)
  }
  val device = Module(new DeviceTop)

  //device-开发板
  device.io.board<>io

  //cpu-device
  cpu.io.external<>device.io.mmio


  //--------------------debugging code----------------------------
  // expose reg value to outside
  val debug_io = if (GenConfig.s.debugMode) Some(IO(new CoreDebugIO)) else None
  debug_io.foreach(total_dbg =>
    cpu.debug_io.foreach(cpu_dbg =>
      total_dbg <> cpu_dbg
    )
  )
}

object Top extends App {
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new Top,
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}