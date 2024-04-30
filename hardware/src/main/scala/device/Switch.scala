package device

import chisel3._
import configs.GenConfig

class MMIOSwitchBundle extends Bundle {
  val switch= Output(UInt(GenConfig.s._MMIO.switchWidth.W))
}

class BoardSwitchBundle extends Bundle {
  val switch= Input(UInt(GenConfig.s.board.switchWidth.W))
}
class Switch extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOSwitchBundle
    val board = new BoardSwitchBundle
  }
  )
}
