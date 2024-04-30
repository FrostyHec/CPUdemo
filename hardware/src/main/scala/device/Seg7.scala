package device

import chisel3._
import configs.GenConfig

class MMIOSeg7Bundle extends Bundle {
  val seg7 = Input(UInt(GenConfig.s._MMIO.seg7Width.W))
}

class BoardSeg7Bundle extends Bundle {
  //TODO seg7 out board bundle
}

class Seg7 extends Module {
  val io = IO(new Bundle {
    val mmio = new MMIOSeg7Bundle
    val board = new BoardSeg7Bundle
  }
  )
  //TODO seg7 logic
}
