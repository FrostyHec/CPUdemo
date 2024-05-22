package hardware_verify

import chisel3._
import device._
import utils._

class UARTVerify extends Module {
  val io = IO(new Bundle {
    val board = new BoardUARTBundle
    val led = Output(UInt(8.W))
    val signal = Output(Bool())
  })
  val mmio=new MMIOUARTBundle
  val uart=Module(new UARTWrapper())
  io.board<>uart.io.board
  uart.io.mmio.txStart:=false.B
  uart.io.mmio.txData:=DontCare

  val out_led=RegInit(0.U(8.W))

  io.led:=out_led
  io.signal:=uart.io.mmio.rxValid
  when(uart.io.mmio.rxValid){
    out_led:=uart.io.mmio.rxData
  }
}
object UARTVerify extends App {
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new UARTVerify,
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}