package core.memory

import chisel3._
import configs.GenConfig
import utils._

class DataRAM extends Module {
  val io = IO(new RWMemoryPort(
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth
  ))
  val dataRAM = Module(new RAM(
    GenConfig.s.ramAddressWidth,
    GenConfig.s.dataWidth,
    GenConfig.s.dataMemSize,
    if (GenConfig.s.useIPMemory) GenConfig.s.dataMemIPCoreName else None,
  ))
   io <> dataRAM.io
}
