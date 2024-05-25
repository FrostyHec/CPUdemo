package core.pipeline

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import core.config._
import core.csr.{IDFault, IFFault}
import utils._

class EXMEMReg extends Bundle {
  //for debug from id-ex
  val pc = UInt(32.W)
  val ins = UInt(32.W)
  //from ID-EX
  val resultStage = ResultStageType.getWidth
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
    val cpu_state = Input(CPUStateType.getWidth)
    val signal = Input(LayerControlSignal.getWidth)
    val in = Input(new EXMEMReg)
    val out = Output(new EXMEMReg)
  })
  val init_val = new EXMEMReg().Lit(
    _.pc -> 0.U,
    _.ins -> 0.U,
    _.resultStage -> ResultStageType.EX.getUInt,
    _.IF_fault.IF_fault_type -> IFFaultType.No.getUInt,
    _.IF_fault.mtval -> 0.U,
    _.ID_fault.ID_fault_type -> IDFaultType.No.getUInt,
    _.ID_fault.mtval -> 0.U,
    _.csr_val_to_reg -> 0.U,
    _.unsigned -> false.B,

    _.mem_signal.read_data_en -> false.B,
    _.mem_signal.write_data_en -> false.B,
    _.mem_signal.data_width -> DataWidth.Byte.getUInt,
    _.mem_signal.data_to_write -> 0.U,

    _.wb_signal.au_type -> AUType.ALU.getUInt,
    _.wb_signal.wb_type -> WriteBackType.AU.getUInt,
    _.wb_signal.imm -> 0.U,
    _.wb_signal.rd -> 0.U,
    _.wb_signal.write_reg -> false.B,
    _.wb_signal.csr_idx -> 0.U,
    _.wb_signal.csr_write -> false.B,

    _.alu_out -> 0.U,
    _.cmp_out -> false.B,
  )
  val regs = RegInit(init_val)
  io.out := regs
  when(io.cpu_state === CPUStateType.cycle3_layer.getUInt) {
    switch(io.signal) {
      is(LayerControlSignal.Normal.getUInt) {
        regs := io.in
      }
      is(LayerControlSignal.Stall.getUInt) {
        //no change
      }
      is(LayerControlSignal.NOP.getUInt) {
        regs := init_val
      }
    }
  }
  //TODO while write
}
