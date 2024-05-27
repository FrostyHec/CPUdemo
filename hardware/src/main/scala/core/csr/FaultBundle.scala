package core.csr

import chisel3._
import core.config._

class MemFault extends Bundle {
  val mem_fault_type = Output(MemFaultType.getWidth)
  val mtval = Output(UInt(32.W))
}
class InsFault extends Bundle {
  val ins_fault_type = Output(InsFaultType.getWidth)
  val mtval = Output(UInt(32.W))
}
class IOFault extends Bundle{
  val io_fault_occur = Output(Bool())
  val mtval = Output(UInt(32.W))
}
