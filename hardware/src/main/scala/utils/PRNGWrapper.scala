package utils

import chisel3._
import chisel3.util._
import chisel3.util.random._

class PRNGWrapper(width: Int) extends Module {
  val io = IO(new Bundle {
    val random = Output(UInt(width.W))
  })
  if (width < 2) {
    io.random := 0.U //random failed
  } else {
    val lfsr = Module(new MaxPeriodFibonacciLFSR(width))

    io.random := lfsr.io.out.asUInt

    val seedWire = Wire(Valid(Vec(width,Bool())))
    seedWire.valid:=true.B
    seedWire.bits:=Cat( Fill(width-1,0.U),true.B).asBools
    lfsr.io.seed :=seedWire
    lfsr.io.increment := true.B
  }
}
