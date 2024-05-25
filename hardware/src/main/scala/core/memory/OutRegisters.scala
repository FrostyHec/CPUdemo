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
  //  io.external.uart.txData := DontCare
  //  io.external.uart.txStart := false.B
  //  io.external.uart.rxReady := false.B
  io.external.vga.value := DontCare

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

  //uart
  //rx
  val rxData = RegInit(0.U(GenConfig.s._MMIO.uartRxDataWidth.W))
  rxData := io.external.uart.rxData

  val rxVlaid = RegInit(0.U(GenConfig.s._MMIO.uartTxDataWidth.W))
  rxVlaid := io.external.uart.rxValid

  val rxReady = RegInit(0.U(GenConfig.s._MMIO.uartTxDataWidth.W))
  io.external.uart.rxReady := rxReady
  //tx
  val txData = RegInit(0.U(GenConfig.s._MMIO.uartTxDataWidth.W))
  io.external.uart.txData := txData

  val txStart = RegInit(0.U(GenConfig.s._MMIO.uartTxDataWidth.W))
  io.external.uart.txStart := txStart

  val txReady = RegInit(0.U(GenConfig.s._MMIO.uartTxDataWidth.W))
  txReady := io.external.uart.txReady

  //vga
  val vga = RegInit(0.U(GenConfig.s._MMIO.vgaWidth.W))

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
    }.elsewhen(addr === (GenConfig.s._MMIO.uartRxAddr >> 2).asUInt) {
      when(io.mem.write) {
        printf("UART cannot write") //TODO throw err when write
      }.otherwise {
        io.mem.read_data := rxData
      }
    }.elsewhen(addr === (GenConfig.s._MMIO.uartRxValidAddr >> 2).asUInt) {
      when(io.mem.write) {
        printf("UART cannot write") //TODO throw err when write
      }.otherwise {
        io.mem.read_data := rxVlaid
      }
    }.elsewhen(addr === (GenConfig.s._MMIO.uartRxReadyAddr >> 2).asUInt) {
      when(io.mem.write) {
        rxReady := io.mem.write_data
      }.otherwise {
        io.mem.read_data := rxReady
      }
    }.elsewhen(addr === (GenConfig.s._MMIO.uartTxAddr >> 2).asUInt) {
      when(io.mem.write) {
        txData := io.mem.write_data
      }.otherwise {
        io.mem.read_data := txData
      }
    }.elsewhen(addr === (GenConfig.s._MMIO.uartTxStart >> 2).asUInt) {
      when(io.mem.write) {
        txStart := io.mem.write_data
      }.otherwise {
        io.mem.read_data := txReady
      }
    }.elsewhen(addr === (GenConfig.s._MMIO.uartTxValid >> 2).asUInt) {
      when(io.mem.write) {
        printf("UART cannot write") //TODO throw err when write
      }.otherwise {
        io.mem.read_data := txReady
      }
    }
    .elsewhen(addr === (GenConfig.s._MMIO.vgaHexAddr >> 2).asUInt) {
      when(io.mem.write) {
        vga := io.mem.write_data
      }.otherwise {
        io.mem.read_data := vga
      }
    }.otherwise {
      //TODO throw err when read or write
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