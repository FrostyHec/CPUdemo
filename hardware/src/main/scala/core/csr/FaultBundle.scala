package core.csr

import chisel3._
import core.config._

class MEMFault extends Bundle {
  val mem_fault_type = MEMFaultType.getWidth
  val mtval = UInt(32.W)
}

class IFFault extends Bundle {
  val IF_fault_type = IFFaultType.No.getUInt(IFFaultType.getWidth)
  val mtval = UInt(32.W)
}

class IDFault extends Bundle {
  val ID_fault_type = IDFaultType.No.getUInt(IDFaultType.getWidth)
  val mtval = UInt(32.W)
}

class IOFault extends Bundle {
  val io_fault_occur = Output(Bool())
  val mtval = Output(UInt(32.W))
}
