package device

import chisel3._

class ExternalSignalBundle extends Bundle {
  val load_data_mode = Output(Bool())
}

class MMIOOutBundle extends Bundle {
  //cpu output devices
  val led = new MMIOLedBundle
  val seg7 = new MMIOSeg7Bundle

  //cpu input devices
  val btn = new MMIOButtonBundle
  val switches = new MMIOSwitchBundle

  val uart = new MMIOUARTBundle

  val vga = new MMIOVGABundle
}

class BoardBundle extends Bundle {
  //output devices
  val led = new BoardLedBundle
  val seg7 = new BoardSeg7Bundle

  //input devices
  val btn = new BoardButtonBundle
  val switch = new BoardSwitchBundle

  val uart = new BoardUARTBundle

  val vga = new MMIOVGABundle
}

class DeviceTop extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOOutBundle
    val board = new BoardBundle
    val external_signal = new ExternalSignalBundle
  })
  //TODO EXTERNAL SIGNAL
  io.external_signal.load_data_mode := false.B

  val led = Module(new Led)
  val seg7 = Module(new Seg7)
  val btn = Module(new Button)
  val switches = Module(new Switches)
  val uart = Module(new UARTWrapper) //TODO 例化的时候可能要传入特殊时钟
  val vga = Module(new VGA)
  //mmio
  led.io.mmio <> io.mmio.led
  seg7.io.mmio <> io.mmio.seg7
  btn.io.mmio <> io.mmio.btn
  switches.io.mmio <> io.mmio.switches
  uart.io.mmio <> io.mmio.uart
  vga.io.mmio <> io.mmio.vga

  //board
  led.io.board <> io.board.led
  seg7.io.board <> io.board.seg7
  btn.io.board <> io.board.btn
  switches.io.board <> io.board.switch
  uart.io.board <> io.board.uart
//  vga.io.board <> io.board.vga

  //external signal TODO set it
  io.external_signal.load_data_mode := io.board.switch.switches(23)
}
