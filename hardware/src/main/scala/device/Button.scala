package device

import chisel3._
import configs.GenConfig

class MMIOButtonBundle extends Bundle {
  val button= Output(UInt(GenConfig.s._MMIO.btnWidth.W))
}

class BoardButtonBundle extends Bundle {
  val button= Input(UInt(GenConfig.s.board.btnWidth.W))
}
class Button extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOButtonBundle
    val board = new BoardButtonBundle
  })

}
