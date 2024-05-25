package core.pipeline
import chisel3._
import chisel3.util._
import core.config._
import core.csr.{IDFault, IFFault}
import utils._
class MEMControlSignal extends Bundle{
  val read_data_en= Bool()
  val write_data_en = Bool()
  val data_width = DataWidth.getWidth
  val data_to_write = UInt(32.W)
}
class WBControlSignal extends Bundle{
  val au_type = AUType.getWidth
  val wb_type = WriteBackType.getWidth
  val imm = UInt(32.W)
  val rd = UInt(5.W)
  val write_reg = Bool()
  val csr_idx = Bool()
  val csr_write = Bool()
}
class IDEXReg extends Bundle{
  //from IF-ID
  val pc = UInt(32.W)
  val ins = UInt(32.W)//for debug
  val IF_fault = new IFFault
  //ID-EX(most from control signal)
  //operating value
  val op1 = UInt(32.W)
  val op2 = UInt(32.W)
  val csr_val_to_reg = UInt(32.W)
  // EX control signal
  val alu_op = ALUType.getWidth
  val cmp_op = CMPType.getWidth
  val unsigned = Bool()
  //MEM control signal
  val mem_signal = new MEMControlSignal

  //WB control signal
  val wb_signal = new WBControlSignal

  //pipeline control signal
  val result_stage = ResultStageType.getWidth
  val branch_type = BranchType.getWidth

  //ID Exception
  val ID_fault = new IDFault
}
class IDEX extends Module{
  val io = IO(new Bundle(){
    val signal = Input(LayerControlSignal.getWidth)
    val in = Input(new IDEXReg)
    val out = Output(new IDEXReg)
  })
  //TODO while write
}
