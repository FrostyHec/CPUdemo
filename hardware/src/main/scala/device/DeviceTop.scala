package device

import chisel3._

class MMIOOutBundle extends Bundle {
  //cpu output devices
  val led = new MMIOLedBundle
  val seg7 = new MMIOSeg7Bundle

  //cpu input devices
  val btn = new MMIOButtonBundle
  val switches = new MMIOSwitchBundle
}

class BoardBundle extends Bundle {
  //output devices
  val led = new BoardLedBundle
  val seg7 = new BoardSeg7Bundle

  //input devices
  val btn = new BoardButtonBundle
  val switch = new BoardSwitchBundle
}

class DeviceTop extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOOutBundle
    val board = new BoardBundle
  })
  val led = Module(new Led)
  val seg7 = Module(new Seg7)
  val btn = Module(new Button)
  val switches = Module(new Switches)
  //mmio
  led.io.mmio <> io.mmio.led
  seg7.io.mmio <> io.mmio.seg7
  btn.io.mmio <> io.mmio.btn
  switches.io.mmio <> io.mmio.switches

  //board
  led.io.board <> io.board.led
  seg7.io.board <> io.board.seg7
  btn.io.board <> io.board.btn
  switches.io.board <> io.board.switch
}
