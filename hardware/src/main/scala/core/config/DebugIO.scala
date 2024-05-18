package core.config

import chisel3._

class CoreDebugIO extends Bundle {
  //debugging io that expose the value inside
  val reg_vals = new RegDebugIO
  val csr_vals = new CSRDebugIO
}

class RegDebugIO extends Bundle {
  val reg_vals = Output(Vec(32, UInt(32.W)))
}

class CSRDebugIO extends Bundle{
  val mstatus = Output(UInt(32.W))
  val mie = Output(UInt(32.W))
  val mtvec = Output(UInt(32.W))
  val mscratch = Output(UInt(32.W))
  val mepc = Output(UInt(32.W))
  val mcause = Output(UInt(32.W))
  val mtval = Output(UInt(32.W))
  val mip = Output(UInt(32.W))
}