package core.memory

import chisel3._
import utils._

class DataRAM extends Module {
  val io = IO(new RWMemoryPort(
    GenConfig.selected.addressWidth,
    GenConfig.selected.dataWidth
  ))
  private val dataRAM = Module(new RAM(
    GenConfig.selected.addressWidth,
    GenConfig.selected.dataWidth,
    GenConfig.selected.dataMemSize
  ))
   io <> dataRAM.io
}
