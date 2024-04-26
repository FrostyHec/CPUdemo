package day1

import chisel3._
import chisel3.util._

class HelloWorld(freq: Int, startOn: Boolean = false) extends Module {
  val io = IO(new Bundle {
    val led0 = Output(Bool())
  })
  // Blink LED every second using Chisel built-in util.Counter
  val led = RegInit(startOn.B)
  val (_, counterWrap) = Counter(true.B, freq / 2)
  when(counterWrap) {
    led := ~led
  }
  io.led0 := led
}
object Main extends App {
  // These lines generate the Verilog output
  println(
    new (chisel3.stage.ChiselStage).emitVerilog(
      new HelloWorld(100),
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}

