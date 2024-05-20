package Generate

import chisel3._
import chisel3.util._
import configs.GenConfig

class Debounce extends Module {
  val io = IO(new Bundle {
    val clock = Input(Clock())
    val in = Input(Bool())
    val out = Output(Bool())
  })

  val out1 = Module(new DFF())
  val out2 = Module(new DFF())

  out1.io.clk := io.clock
  out1.io.in := io.in

  out2.io.clk := io.clock
  out2.io.in := out1.io.out

  io.out := out1.io.out && !out2.io.out
}

class DFF extends Module {
  val io = IO(new Bundle {
    val clk = Input(Clock())
    val in = Input(Bool())
    val out = Output(Bool())
    val out_neg = Output(Bool())
  })

  val reg = withClock(io.clk) { RegNext(io.in, false.B) }

  io.out := reg
  io.out_neg := !reg
}
