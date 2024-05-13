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
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth,
    GenConfig.s.dataMemSize,
    GenConfig.s.dataMemIPCoreName,
  ))
   io <> dataRAM.io
}
