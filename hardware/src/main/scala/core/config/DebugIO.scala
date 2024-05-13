package core.config

import chisel3._

class CoreDebugIO extends Bundle {
  //debugging io that expose the value inside
  val reg_vals = new RegDebugIO
}

class RegDebugIO extends Bundle {
  val reg_vals = Output(Vec(32, UInt(32.W)))
}