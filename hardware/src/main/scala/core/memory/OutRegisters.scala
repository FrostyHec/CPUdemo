package core.memory
import chisel3._
import configs.GenConfig
import utils._
class OutRegisters extends Module{
  val io=IO(new RWMemoryPort(
    GenConfig.s.addressWidth,
    GenConfig.s.dataWidth
  ))
  io.read_data:=DontCare
  //TODO out register mapping the io from outside, thus an external io should be used
}
