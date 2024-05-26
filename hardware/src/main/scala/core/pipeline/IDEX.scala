package core.pipeline

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
import chisel3.util._
import core.config._
import core.csr.{IDFault, IFFault}
import utils._

class MEMControlSignal extends Bundle {
  val read_data_en = Bool()
  val write_data_en = Bool()
  val data_width = DataWidth.getWidth
  val data_to_write = UInt(32.W)
}

class WBControlSignal extends Bundle {
  val au_type = AUType.getWidth
  val wb_type = WriteBackType.getWidth
  val imm = UInt(32.W)
  val rd = UInt(5.W)
  val write_reg = Bool()
  val csr_idx = UInt(12.W)
  val csr_write = Bool()
}

class IDEXReg extends Bundle {
  //from IF-ID
  val pc = UInt(32.W)
  val ins = UInt(32.W) //for debug
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

class IDEX extends Module {
  val io = IO(new Bundle() {
    val cpu_state = Input(CPUStateType.getWidth)
    val signal = Input(LayerControlSignal.getWidth)
    val in = Input(new IDEXReg)
    val out = Output(new IDEXReg)
  })
  val init_val = new IDEXReg().Lit(
    _.pc -> 0.U,
    _.ins -> 0.U,
    _.IF_fault.IF_fault_type -> IFFaultType.No.getUInt,
    _.IF_fault.mtval -> 0.U,

    _.op1 -> 0.U,
    _.op2 -> 0.U,
    _.csr_val_to_reg -> 0.U,
    _.alu_op -> AUType.ALU.getUInt,
    _.cmp_op -> CMPType.EQ.getUInt,
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

    _.result_stage -> ResultStageType.EX.getUInt,
    _.branch_type -> BranchType.No.getUInt,

    _.ID_fault.ID_fault_type -> IDFaultType.No.getUInt,
  )
  val regs = RegInit(init_val)
  io.out := regs
  when(io.cpu_state === CPUStateType.cycle3_layer.getUInt) {
    switch(io.signal) {
      is(LayerControlSignal.Normal.getUInt) {
        when(io.in.IF_fault.IF_fault_type =/= IFFaultType.No.getUInt ||
          io.in.ID_fault.ID_fault_type =/= IDFaultType.No.getUInt) { // when fault occurs, set other to 0 ,only forward err
          regs := init_val
          regs.IF_fault := io.in.IF_fault
          regs.ID_fault := io.in.ID_fault
          regs.pc := io.in.pc
        }.otherwise {
          regs := io.in
        }
      }
      is(LayerControlSignal.Stall.getUInt) {
        //no change
      }
      is(LayerControlSignal.NOP.getUInt) {
        regs := init_val
      }
    }
  }
}

