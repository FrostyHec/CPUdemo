package hardware_verify

import chisel3.util._
import chisel3._
import device._

class Seg7VerifyTop extends Module {
  val io = IO(new Bundle {
    val seg7_in = Input(UInt(16.W))
    val seg7_out = new BoardSeg7Bundle
  })
  val seg7 = Module(new Seg7())
  seg7.io.mmio.seg7 := Cat(0.U(16.W), io.seg7_in)
  io.seg7_out <> seg7.io.board
}

object Seg7VerifyTop extends App {
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new Seg7VerifyTop,
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
