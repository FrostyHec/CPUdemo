package core.pipeline

import chisel3._
import chisel3.experimental.BundleLiterals.AddBundleLiteralConstructor
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
    val cpu_state = Input(CPUStateType.getWidth)
    val signal = Input(LayerControlSignal.getWidth)
    val in = Input(new IFIDReg)
    val out = Output(new IFIDReg)
  })
  val init_val = new IFIDReg().Lit(
    _.pc -> 0.U,
    _.ins -> "h0000_0013".U, // nop
    _.IF_fault.IF_fault_type -> IFFaultType.No.getUInt
  )
  val regs = RegInit(init_val)
  io.out := regs
  when(io.cpu_state === CPUStateType.cycle3_layer.getUInt) {
    switch(io.signal) {
      is(LayerControlSignal.Normal.getUInt) {
        when(io.in.IF_fault.IF_fault_type =/= IFFaultType.No.getUInt) { // when fault occurs, set other to 0 ,only forward err
          regs := init_val
          regs.IF_fault := io.in.IF_fault
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

