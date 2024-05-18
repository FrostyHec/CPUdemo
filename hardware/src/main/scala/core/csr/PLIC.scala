package core.csr

import chisel3._

class PLIC extends Module {

  val io = IO(new Bundle{
    val interruption = new IOFault
  })
}
