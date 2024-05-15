package core.memory

import chisel3._
import configs.GenConfig
import utils._


class InsRAM extends Module {
  val io = IO(new RWMemoryPort(
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth
  ))
  val insRAM = Module(new RAM(
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth,
    GenConfig.s.insMemSize,
    GenConfig.s.insMemIPCoreName,
    GenConfig.s.initInsFile //debugging,support ins initialize
  ))
  io <> insRAM.io


}
