package core.memory
import chisel3._
import utils._
class OutRegisters extends Module{
  val io=IO(new RWMemoryPort(
    GenConfig.selected.addressWidth,
    GenConfig.selected.dataWidth
  ))
  io.read_data:=DontCare
  //TODO out register mapping the io from outside, thus an external io should be used
}
