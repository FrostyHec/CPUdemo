package core.csr

import chisel3._
import chisel3.util._
import configs.GenConfig
import core.config._
import utils.ExtendEnum


class CSR extends Module {
  // 经过学习发现不需要考虑保持时间，因为有buf可以保证组合够长
  val io = IO(new Bundle {
    val cpu_state = Input(CPUStateType.getWidth)
    //cur_state
    val cur_privilege = Output(UInt(2.W))

    //csr_instructions
    val write = Input(Bool())
    val csr_idx_write = Input(UInt(12.W))
    val write_data = Input(UInt(32.W))

    val csr_idx_read = Input(UInt(12.W))
    val csr_val = Output(UInt(32.W))

    //fault handling
    val IF_fault = Input(new IFFault)
    val ID_fault = Input(new IDFault)
    val MEM_fault = Input(new MEMFault)
    val io_interruption = Flipped(new IOFault)

    val pc = Input(UInt(32.W))

    val fault_occurs = Output(Bool())
    val fault_new_PC = Output(UInt(32.W))
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

  val read_MIE = mstatus(3) // mstatus.MIE
  val read_MEIE = mie(11) // mie.MEIE
  val read_MPIE = mstatus(7) // mstatus.MPIE
  val read_MPP = mstatus(12, 11) // mstatus.MPP
  //  mip(11):= global_interrupt_en & io_interrupt_en & io.io_interruption.io_fault_occur
  mip := mip.bitSet(11.U, read_MIE & read_MEIE & io.io_interruption.io_fault_occur)

  val no_fault = (io.mem_fault.mem_fault_type === MEMFaultType.No.getUInt) &
    (io.ins_fault.ins_fault_type === IDFaultType.No.getUInt) &
    !(read_MIE & read_MEIE & io.io_interruption.io_fault_occur) // io fault

  def handleCSR(csr: UInt, reg: UInt): Unit = {
    when(io.csr === csr) { //输出值不需要no_fault
      io.csr_val := reg
      when(io.cpu_state === CPUStateType.sWriteRegs.getUInt && io.write&&no_fault) {
        reg := io.write_data
        if (GenConfig.s.logDetails) {
          printf("write to csr[%x] with data %d\n", io.csr, io.write_data)
        }
      }
    }
  }

  def handleWrite(_mcause: UInt, _mtval: UInt): Unit = {
    if(GenConfig.s.logDetails){
      printf("Writing for err handle\n")
    }

    io.fault_state := true.B
    mepc := io.pc
    io.fault_write_PC := mtvec
    mcause := _mcause
    mtval := _mtval
    mstatus := mstatus.bitSet(3.U, false.B) //MIE禁用中断写为0
    //    read_MPIE := read_MIE //MPIE <-MIE
    mstatus := mstatus.bitSet(7.U, read_MIE)
    //    read_MPP := cur_privilege
    mstatus := Cat(mstatus(31, 13), cur_privilege, mstatus(10, 0))
    cur_privilege := PrivilegeType.Machine // 切换至M模式
  }


  //default value
  //TODO ILLEGAL INSTRUCTION
  io.csr_val := DontCare
  io.fault_state := false.B
  io.fault_write_PC := DontCare
  // normal csr reading and writing
  //输出时不需要no fault检查，只要保证fault不会写入到reg即可，而这个是由外面这个大状态机保证的
  handleCSR(0x300.U, mstatus)
  handleCSR(0x304.U, mie)
  handleCSR(0x305.U, mtvec)
  handleCSR(0x340.U, mscratch)
  handleCSR(0x341.U, mepc)
  handleCSR(0x342.U, mcause)
  handleCSR(0x343.U, mtval)
  handleCSR(0x344.U, mip)
  when(!no_fault) {
    val fault_type = Wire(FaultHandlerType.getWidth)
    fault_type := FaultHandlerType.writeFault.getUInt // 除了mret都是writeFault
    when(io.ins_fault.ins_fault_type === IDFaultType.Mret.getUInt) {
      fault_type := FaultHandlerType.leaveFault.getUInt
    }
    //带优先级地查询中断源并进行操作
    //MEM阶段的错误
    when(fault_type === FaultHandlerType.writeFault.getUInt) {
      when(io.mem_fault.mem_fault_type === MEMFaultType.LoadMisaligned.getUInt) {
        handleWrite(4.U, io.mem_fault.mtval)
      }.elsewhen(io.mem_fault.mem_fault_type === MEMFaultType.LoadFault.getUInt) {
          handleWrite(5.U, io.mem_fault.mtval)
        }.elsewhen(io.mem_fault.mem_fault_type === MEMFaultType.StoreMisaligned.getUInt) {
          handleWrite(6.U, io.mem_fault.mtval)
        }.elsewhen(io.mem_fault.mem_fault_type === MEMFaultType.StoreFault.getUInt) {
          handleWrite(7.U, io.mem_fault.mtval)
        }
        //ID阶段的错误
        .elsewhen(io.ins_fault.ins_fault_type === IDFaultType.IllegalIns.getUInt) {
          handleWrite(2.U, io.ins_fault.mtval)
        }.elsewhen(io.ins_fault.ins_fault_type === IDFaultType.BreakPoint.getUInt) {
          handleWrite(3.U, io.ins_fault.mtval)
        }.elsewhen(io.ins_fault.ins_fault_type === IDFaultType.EcallM.getUInt) {
          handleWrite(11.U, io.ins_fault.mtval)
        }
        //IF阶段的错误
        .elsewhen(io.mem_fault.mem_fault_type === MEMFaultType.InsMisaligned.getUInt) {
          handleWrite(0.U, io.mem_fault.mtval)
        }
        .elsewhen(io.mem_fault.mem_fault_type === MEMFaultType.InsFault.getUInt) {
          handleWrite(1.U, io.mem_fault.mtval)
        }
        //IO错误
        .elsewhen(io.io_interruption.io_fault_occur) {
          handleWrite(((1.U << 31.U).asUInt + 11.U), io.io_interruption.mtval) //最高位为1，后面最后为11的二进制数
        }
    }.otherwise { // 暂时只有mret
      io.fault_state := true.B
      io.fault_write_PC := mepc
      //      read_MIE := read_MPIE // MIE <- MPIE
      mstatus := mstatus.bitSet(3.U, read_MPIE)
      cur_privilege := read_MPP
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
  if (GenConfig.s.logDetails) {
    when(!no_fault) {
      printf("Fault occurs! mem_fault:%d, ins_fault:%d, io_fault:%d, jump to:%d\n",
        io.mem_fault.mem_fault_type, io.ins_fault.ins_fault_type, io.io_interruption.io_fault_occur, io.fault_write_PC)
    }
  }
}

object FaultHandlerType extends ExtendEnum {
  val writeFault, leaveFault = Value
}
