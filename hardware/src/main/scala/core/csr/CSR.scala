package core.csr

import chisel3._
import chisel3.util._
import configs.GenConfig
import core.config._
import utils.ExtendEnum


class CSR extends Module {
  //TODO CSR Registers
  // 经过学习发现不需要考虑保持时间，因为有buf可以保证组合够长
  val io = IO(new Bundle {
    val cpu_state = Input(CPUStateType.getWidth)
    //cur_state
    val cur_privilege = Output(UInt(2.W))

    //csr_instructions
    val write = Input(Bool())

    val csr = Input(UInt(12.W))
    val write_data = Input(UInt(32.W))

    val csr_val = Output(UInt(32.W))

    //fault handling
    val mem_fault = Flipped(new MemFault)
    val ins_fault = Flipped(new InsFault)
    val io_interruption = Flipped(new IOFault)

    val pc = Input(UInt(32.W))

    val fault_state = Output(Bool())
    val fault_write_PC = Output(UInt(32.W))
  })
  val cur_privilege = RegInit(PrivilegeType.Machine)
  io.cur_privilege := cur_privilege

  val mstatus = RegInit(0.U(32.W)) // at 0x300
  val mie = RegInit(0.U(32.W)) // at 0x304
  val mtvec = RegInit(0.U(32.W)) // at 0x305

  val mscratch = RegInit(0.U(32.W)) // at 0x340
  val mepc = RegInit(0.U(32.W)) // at 0x341
  val mcause = RegInit(0.U(32.W)) // at 0x342
  val mtval = RegInit(0.U(32.W)) // at 0x343
  val mip = RegInit(0.U(32.W)) // at 0x344

  //default value
  io.csr_val := DontCare
  io.fault_state := false.B
  io.fault_write_PC := DontCare

  val global_interrupt_en = mstatus(3) // mstatus.MIE
  val io_interrupt_en = mie(11) // mie.MEIE
  val MPIE = mstatus(7) // mstatus.MPIE
  val MPP = mstatus(12, 11) // mstatus.MPP
  mip(11) := global_interrupt_en & io_interrupt_en & io.io_interruption.io_fault_occur

  val no_fault = (io.mem_fault.mem_fault_type === MemFaultType.No.getUInt) &
    (io.ins_fault.ins_fault_type === InsFaultType.No.getUInt) &
    !(global_interrupt_en & io_interrupt_en & io.io_interruption.io_fault_occur) // io fault

  def handleCSR(csr: UInt, reg: UInt): Unit = {
    when(io.csr === csr) {
      io.csr_val := reg
      when(io.cpu_state === CPUStateType.sWriteRegs.getUInt && io.write) {
        reg := io.write_data

        if (GenConfig.s.logDetails) {
          printf("write to csr[%x] with data %d\n", io.csr, io.write_data)
        }
      }
    }
  }

  def handleWrite(_mcause: UInt, _mtval: UInt): Unit = {
    io.fault_state := true.B
    mepc := io.pc
    io.fault_write_PC := mtvec
    mcause := _mcause
    mtval := _mtval
    global_interrupt_en:= false.B //MIE禁用中断写为0
    MPIE := global_interrupt_en //MPIE <-MIE
    MPP := cur_privilege
    cur_privilege := PrivilegeType.Machine // 切换至M模式
  }


  when(no_fault) {
    // normal csr reading and writing
    handleCSR(0x300.U, mstatus)
    handleCSR(0x304.U, mie)
    handleCSR(0x305.U, mtvec)
    handleCSR(0x340.U, mscratch)
    handleCSR(0x341.U, mepc)
    handleCSR(0x342.U, mcause)
    handleCSR(0x343.U, mtval)
    handleCSR(0x344.U, mip)
  }.otherwise {
    // fault handling
    //TODO  fault handling
    val fault_type = FaultHandlerType.writeFault.getUInt // 除了mret都是writeFault
    when(io.ins_fault.ins_fault_type === InsFaultType.Mret.getUInt) {
      fault_type := FaultHandlerType.leaveFault.getUInt
    }
    //带优先级地查询中断源并进行操作
    //MEM阶段的错误
    when(fault_type === FaultHandlerType.writeFault.getUInt) {
      when(io.mem_fault.mem_fault_type === MemFaultType.LoadMisaligned.getUInt) {
        handleWrite(4.U, io.mem_fault.mtval)
      }.elsewhen(io.mem_fault.mem_fault_type === MemFaultType.LoadFault.getUInt) {
          handleWrite(5.U, io.mem_fault.mtval)
        }.elsewhen(io.mem_fault.mem_fault_type === MemFaultType.StoreMisaligned.getUInt) {
          handleWrite(6.U, io.mem_fault.mtval)
        }.elsewhen(io.mem_fault.mem_fault_type === MemFaultType.StoreFault.getUInt) {
          handleWrite(7.U, io.mem_fault.mtval)
        }
        //ID阶段的错误
        .elsewhen(io.ins_fault.ins_fault_type === InsFaultType.IllegalIns.getUInt) {
          handleWrite(2.U, io.ins_fault.mtval)
        }.elsewhen(io.ins_fault.ins_fault_type === InsFaultType.BreakPoint.getUInt) {
          handleWrite(3.U, io.ins_fault.mtval)
        }.elsewhen(io.ins_fault.ins_fault_type === InsFaultType.EcallM.getUInt) {
          handleWrite(11.U, io.ins_fault.mtval)
        }
        //IF阶段的错误
        .elsewhen(io.mem_fault.mem_fault_type === MemFaultType.InsMisaligned.getUInt) {
          handleWrite(0.U, io.mem_fault.mtval)
        }
        .elsewhen(io.mem_fault.mem_fault_type === MemFaultType.InsFault.getUInt) {
          handleWrite(1.U, io.mem_fault.mtval)
        }
        //IO错误
        .elsewhen(io.io_interruption.io_fault_occur) {
          handleWrite(((1.U << 31.U).asUInt + 11.U), io.io_interruption.mtval) //最高位为1，后面最后为11的二进制数
        }
    }.otherwise { // 暂时只有mret
      io.fault_state := true.B
      io.fault_write_PC := mepc
      global_interrupt_en:=MPIE // MIE <- MPIE
      cur_privilege:=MPP
    }
  }


  //--------------------debugging code----------------------------
  val debug_io = if (GenConfig.s.debugMode) Some(IO(new CSRDebugIO)) else None
  debug_io.foreach(dbg => {
    dbg.mstatus := mstatus
    dbg.mie := mie
    dbg.mtvec := mtvec
    dbg.mscratch := mscratch
    dbg.mepc := mepc
    dbg.mcause := mcause
    dbg.mtval := mtval
    dbg.mip := mip
  })
}

object FaultHandlerType extends ExtendEnum {
  val writeFault, leaveFault = Value
}
