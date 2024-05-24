package core.memory

import chisel3.util._
import chisel3._
import configs.GenConfig
import device.MMIOOutBundle
import utils._

class OutRegisters extends Module {
  val io = IO(new Bundle {
    val mem = new RWMemoryPort(
      GenConfig.s.addressWidth,
      GenConfig.s.dataWidth
    )
    val external = Flipped(new MMIOOutBundle()) // from board
  })
  //TODO urat来个MMIO
  io.external.uart.txData := DontCare
  io.external.uart.txStart := false.B
  io.external.uart.rxReady := false.B

  //说实在我都在想是不是可以不用插这个reg
  //registers that write by board and read by cpu
  val btn = RegInit(0.U(GenConfig.s._MMIO.btnWidth.W))
  btn := io.external.btn.button

  val switches = RegInit(0.U(GenConfig.s._MMIO.switchWidth.W))
  switches := io.external.switches.switches

  //registers that write by cpu and read by board
  val led = RegInit(0.U(GenConfig.s._MMIO.ledWidth.W))
  io.external.led.led := led

  val seg7 = RegInit(0.U(GenConfig.s._MMIO.seg7Width.W))
  io.external.seg7.seg7 := seg7

  //cpu read and write by analyzing addr
  val addr: UInt = Mux(io.mem.write, io.mem.write_addr, io.mem.read_addr)
  io.mem.read_data := DontCare

  when(addr === (GenConfig.s._MMIO.btnAddr >> 2).asUInt) {
    when(io.mem.write) {
      printf("btn cannot write") //TODO Throw err
    }.otherwise {
      io.mem.read_data := btn
    }
  }.elsewhen(addr === (GenConfig.s._MMIO.switchAddr >> 2).asUInt) {
    when(io.mem.write) {
      printf("switches cannot write") //TODO Throw err
    }.otherwise {
      io.mem.read_data := switches
    }
  }.elsewhen(addr === (GenConfig.s._MMIO.ledAddr >> 2).asUInt) {
    when(io.mem.write) {
      led := io.mem.write_data
    }.otherwise {
      io.mem.read_data := led
    }
  }.elsewhen(addr === (GenConfig.s._MMIO.seg7Addr >> 2).asUInt) {
    when(io.mem.write) {
      seg7 := io.mem.write_data
    }.otherwise {
      io.mem.read_data := seg7
    }
  }.otherwise {
    //do nothing
  }
}

object OutRegisters extends App {
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new OutRegisters(),
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}