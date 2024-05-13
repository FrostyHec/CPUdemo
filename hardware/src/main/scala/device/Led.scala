package device

import chisel3._
import configs.GenConfig

class MMIOLedBundle extends Bundle {
  val led = Input(UInt(GenConfig.s._MMIO.ledWidth.W))
}

class BoardLedBundle extends Bundle {
  val led = Output(UInt(GenConfig.s.board.ledWidth.W))
}

class Led extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOLedBundle
    val board = new BoardLedBundle
  }
  )
  //TODO led logic
}
