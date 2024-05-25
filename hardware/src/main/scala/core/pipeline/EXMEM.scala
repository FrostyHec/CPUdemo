package core.pipeline
import chisel3._
import chisel3.util._
import core.config._
import core.csr.{IDFault, IFFault}
import utils._
class EXMEMReg extends Bundle{
  //for debug from id-ex
  val pc = UInt(32.W)
  val ins = UInt(32.W)
  //from ID-EX
  val resultStage  = ResultStageType.getWidth
  val IF_fault = new IFFault
  val ID_fault = new IDFault
  val csr_val_to_reg = UInt(32.W)
  val unsigned = Bool()
  //signals
  val mem_signal = new MEMControlSignal
  val wb_signal = new WBControlSignal

  //EX stage out
  val alu_out = UInt(32.W)
  val cmp_out = Bool()
}
class EXMEM extends Module {
  val io = IO(new Bundle() {
    val signal = Input(LayerControlSignal.getWidth)
    val in = Input(new EXMEMReg)
    val out = Output(new EXMEMReg)
  })
  //TODO while write
}
