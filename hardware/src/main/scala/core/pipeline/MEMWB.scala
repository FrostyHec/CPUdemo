package core.pipeline

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
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
    val cpu_state = Input(CPUStateType.getWidth)
    val signal = Input(LayerControlSignal.getWidth)
    val in = Input(new MEMWBReg)
    val out = Output(new MEMWBReg)
  })
  val init_val = new MEMWBReg().Lit(
    _.pc -> 0.U,
    _.ins -> 0.U,
    _.csr_val_to_reg -> 0.U,
    _.wb_signal.au_type -> AUType.ALU.getUInt,
    _.wb_signal.wb_type -> WriteBackType.AU.getUInt,
    _.wb_signal.imm -> 0.U,
    _.wb_signal.rd -> 0.U,
    _.wb_signal.write_reg -> false.B,
    _.wb_signal.csr_idx -> 0.U,
    _.wb_signal.csr_write -> false.B,
    _.alu_out -> 0.U,
    _.cmp_out -> false.B,
    _.data_out -> 0.U,
  )
  val regs= RegInit(init_val)
  io.out:=regs
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
}