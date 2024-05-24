package device

import chisel3._
import configs.GenConfig
import Generate.Debounce

class MMIOSwitchBundle extends Bundle {
  val switches= Output(UInt(GenConfig.s._MMIO.switchWidth.W))
}

class BoardSwitchBundle extends Bundle {
  val switches= Input(UInt(GenConfig.s.board.switchWidth.W))
}
class Switches extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOSwitchBundle
    val board = new BoardSwitchBundle
  })
//  val switch_deb = Module(new Debounce())
//  // switch_deb.io.clock (100Hz)
//  switch_deb.io.in := io.board.switches
//  io.mmio.switches := switch_deb.io.out
  io.mmio.switches := io.board.switches
}
