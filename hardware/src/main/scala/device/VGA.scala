package device

import chisel3._

class MMIOVGABundle extends Bundle {
  val value = Input(UInt(32.W))
}

class BoardVGABundle extends Bundle {

}

class VGA extends Module {
  val io = IO(new Bundle() {
    val board = new BoardVGABundle
    val mmio = new MMIOVGABundle
  })

  //TODO black box
}
