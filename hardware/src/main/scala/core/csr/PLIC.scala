package core.csr

import chisel3._

class PLIC extends Module {

  val io = IO(new Bundle{
    val interruption = new IOFault
    //目前想的一个实现是:可以强制关掉这个程序
  })
  //default
  io.interruption.io_fault_occur := false.B
  io.interruption.mtval := DontCare
}
