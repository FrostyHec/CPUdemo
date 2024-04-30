package core.memory

import chisel3._
import utils._


class InsRAM extends Module {
  val io = IO(new RWMemoryPort(
    GenConfig.selected.addressWidth,
    GenConfig.selected.dataWidth
  ))
  private val insRAM = Module(new RAM(
    GenConfig.selected.addressWidth,
    GenConfig.selected.dataWidth,
    GenConfig.selected.insMemSize
  ))
  io <> insRAM.io
}
