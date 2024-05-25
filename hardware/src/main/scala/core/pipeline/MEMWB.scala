package core.pipeline

import chisel3._
import chisel3.util._
import core.config._
import core.csr._

class MEMWBReg extends Bundle{
  //for debug from ex-mem
  val pc = UInt(32.W)
  val ins = UInt(32.W)
  //from EX-MEM
  val csr_val_to_reg = UInt(32.W)
  //signals
  val wb_signal = new WBControlSignal
  val alu_out = UInt(32.W)
  val cmp_out = Bool()

  //MEM stage out
  val data_out = UInt(32.W)
}
class MEMWB extends Module {
  val io = IO(new Bundle() {
    val signal = Input(LayerControlSignal.getWidth)
    val in = Input(new MEMWBReg)
    val out = Output(new MEMWBReg)
  })
  //TODO while write
}