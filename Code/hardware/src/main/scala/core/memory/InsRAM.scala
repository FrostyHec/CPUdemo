package core.memory

import chisel3._
import configs.GenConfig
import utils._


class InsRAM extends Module {
  val io = IO(new RWMemoryPort(
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth
  ))
  val io2 = IO(new RMemoryPort(
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth
  ))
  val insRAM = Module(new DualPortRAM(
    GenConfig.s.ramAddressWidth,
    GenConfig.s.dataWidth,
    GenConfig.s.insMemSize,
    if(GenConfig.s.useIPMemory) GenConfig.s.insMemIPCoreName else None,
    GenConfig.s.initInsFile //debugging,support ins initialize
  ))
  io <> insRAM.io
  io2 <> insRAM.io2
}
