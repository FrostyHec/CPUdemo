package hardware_verify;

import chisel3._
import chisel3.util.Cat
import core.config.CPUStateType
import core.memory.{MemoryDispatch, UARTLoader}
import device.UARTWrapper

class LoadVerify extends Module {
  val io= IO(new Bundle{
    val rx=Input(Bool())
    val tx=Output(Bool())
    val addr = Input(UInt(16.W))
    val led = Output(UInt(16.W))
  })
  val uart = Module(new UARTWrapper())
  val loader = Module(new UARTLoader())
  val mem_dispatch = Module(new MemoryDispatch())
  //io->uart
  uart.io.mmio.txData:=DontCare
  uart.io.mmio.txStart:=false.B
  uart.io.board.rx:=io.rx
  io.tx:=uart.io.board.tx
  //uart -> loader
  loader.io.cpu_state:=CPUStateType.sLoadMode.getUInt
  loader.io.rxData := uart.io.mmio.rxData
  loader.io.rxValid := uart.io.mmio.rxValid
  //loader->MemDispatch
  mem_dispatch.io.cpu_state:=CPUStateType.sLoadMode.getUInt

  mem_dispatch.io.unsigned:=true.B
  mem_dispatch.io.read_data:=loader.io.mem.mem_read
  mem_dispatch.io.write_data:=loader.io.mem.mem_write
  mem_dispatch.io.data_write:=loader.io.mem.data_to_write
  mem_dispatch.io.data_width:=loader.io.mem.data_width
  mem_dispatch.io.data_addr:=loader.io.mem.data_addr

  //useless
  mem_dispatch.io.external.uart.txReady:=false.B
  mem_dispatch.io.external.uart.rxValid:=false.B
  mem_dispatch.io.external.uart.rxData:=DontCare
  mem_dispatch.io.external.btn.button:=DontCare
  mem_dispatch.io.external.switches.switches:=DontCare

  //MemDispatch->Led
  mem_dispatch.io.ins_addr:=io.addr
  io.led:=Cat(uart.io.mmio.rxData,mem_dispatch.io.ins_out(7,0))
}
object LoadVerify extends App {
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new LoadVerify,
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}
