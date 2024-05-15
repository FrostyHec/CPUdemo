package device

import chisel3._
import configs.GenConfig

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
  io.mmio.switches := io.board.switches
}
