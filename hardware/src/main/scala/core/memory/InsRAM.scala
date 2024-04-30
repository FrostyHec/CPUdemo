package core.memory

import chisel3._
import configs.GenConfig
import utils._


class InsRAM extends Module {
  val io = IO(new RWMemoryPort(
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth
  ))
  private val insRAM = Module(new RAM(
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth,
    GenConfig.s.insMemSize
  ))
  io <> insRAM.io
}
