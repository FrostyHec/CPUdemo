package core.pipeline
import chisel3._
import chisel3.util._
import core.config._
import core.csr.IFFault
import utils._
class IFIDReg extends Bundle {
  val pc = UInt(32.W)
  val ins = UInt(32.W) // init as nop ins
  val IF_fault = new IFFault
}
class IFID extends Module {
  val io = IO(new Bundle() {
    val signal = Input(LayerControlSignal.getWidth)
    val in = Input(new IFIDReg)
    val out = Output(new IFIDReg)
  })
  //TODO while write
}

