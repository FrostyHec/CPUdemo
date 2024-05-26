package core

import chisel3._
import configs.GenConfig
import core.config._
import core.csr._
import core.memory._
import core.insFetch._
import core.writeBack._
import core.execute._
import core.decode._
import core.pipeline._
import device.{ExternalSignalBundle, MMIOOutBundle}


class CoreTop extends Module {
  val io = IO(new Bundle {
    val external = Flipped(new MMIOOutBundle())
    val external_signal = Flipped(new ExternalSignalBundle)
  })
  //global controller
  val conflictController = Module(new ConflictController())
  val state = Module(new CPUState())

  //CSR
  val CSR = Module(new CSR())
  val PLIC = Module(new PLIC())


  //ins fetch
  val pc = Module(new PC())
  val nextPCGen = Module(new NextPCGen())
  //ins decode
  val IF_ID = Module(new IFID())
  val insDecode = Module(new InstructionDecoder())
  val CU = Module(new ControlUnit())

  val regs = Module(new Registers())
  val immGen = Module(new ImmGen())
  val rs1_forward_selector = Module(new ForwardSelector())
  val rs2_forward_selector = Module(new ForwardSelector())
  val csr_forward_selector = Module(new ForwardSelector())
  val operandSelector = Module(new OperandSelector())
  val EXForwarding = Module(new WBSelectorTop())
  val MEMForwarding = Module(new WBSelectorTop())
  //execute
  val ID_EX = Module(new IDEX())
  val ALU = Module(new ALU())
  val CMP = Module(new CMP())

  //Memory
  val EX_MEM = Module(new EXMEM())
  val memory = Module(new MemoryDispatch())
  val uartLoader = Module(new UARTLoader())
  val memInSelector = Module(new MemWriteSelector())

  //write back
  val MEM_WB = Module(new MEMWB())
  val wb_selector = Module(new WBSelectorTop())

  //state
  state.io.stall := io.external_signal.load_data_mode

  //IF wire
  nextPCGen.io.nextPC_control_signal := conflictController.io.next_control_signal
  nextPCGen.io.pc := pc.io.addr // 当前的pc，下个上升沿写入pc+=4
  nextPCGen.io.new_pc := conflictController.io.new_pc
  nextPCGen.io.instruction := memory.io.ins_out

  pc.io.cpu_state := state.io.cpu_state
  pc.io.next_addr := nextPCGen.io.nextPC
  memory.io.ins_addr := pc.io.addr

  IF_ID.io.cpu_state:=state.io.cpu_state
  IF_ID.io.signal := conflictController.io.IFID_control_signal
  IF_ID.io.in.pc := pc.io.addr
  IF_ID.io.in.ins := memory.io.ins_out
  IF_ID.io.in.IF_fault := memory.io.IF_fault

  //ins decode wire
  insDecode.io.instruction := IF_ID.io.out.ins
  CU.io.instruction := IF_ID.io.out.ins
  CU.io.csr := insDecode.io.csr //TODO 0 if no use
  CU.io.opcode := insDecode.io.opcode
  CU.io.func3 := insDecode.io.func3
  CU.io.func7 := insDecode.io.func7
  CU.io.rs1 := insDecode.io.rs1 //0 if no use
  CU.io.rs2 := insDecode.io.rs2 //0 if no use
  CU.io.rd := insDecode.io.rd
  CU.io.raw_imm := insDecode.io.raw_imm
  CU.io.cur_privilege := CSR.io.cur_privilege
  //csr
  CSR.io.cpu_state := state.io.cpu_state
  CSR.io.csr_idx_read := CU.io.csr_out

  //register
  regs.io.cpu_state := state.io.cpu_state
  regs.io.rs1 := CU.io.rs1_out
  regs.io.rs2 := CU.io.rs2_out

  //immGen
  immGen.io.raw_imm := CU.io.raw_imm_out
  immGen.io.unsigned := CU.io.unsigned
  immGen.io.imm_width := CU.io.imm_width_type

  //forwarding selector
  rs1_forward_selector.io.forward_type := conflictController.io.rs1_forward_type
  rs1_forward_selector.io.origin_val := regs.io.rs1_val
  rs1_forward_selector.io.ex_forward := EXForwarding.io.write_data
  rs1_forward_selector.io.mem_forward := MEMForwarding.io.write_data
  rs2_forward_selector.io.forward_type := conflictController.io.rs2_forward_type
  rs2_forward_selector.io.origin_val := regs.io.rs2_val
  rs2_forward_selector.io.ex_forward := EXForwarding.io.write_data
  rs2_forward_selector.io.mem_forward := MEMForwarding.io.write_data

  csr_forward_selector.io.forward_type := conflictController.io.csr_forward_type
  csr_forward_selector.io.origin_val := CSR.io.csr_val
  csr_forward_selector.io.ex_forward := ALU.io.result // csr的值永远从ALU来
  csr_forward_selector.io.mem_forward := EX_MEM.io.out.alu_out

//  printf("csr_forward_type: %d, csr_val: %d, ex_forward: %d, mem_forward: %d\n",
//    conflictController.io.csr_forward_type, CSR.io.csr_val, EXForwarding.io.write_data, MEMForwarding.io.write_data)

  //operandSelector
  operandSelector.io.csr_val := csr_forward_selector.io.real_val
  operandSelector.io.rs1_val := rs1_forward_selector.io.real_val
  operandSelector.io.rs2_val := rs2_forward_selector.io.real_val
  operandSelector.io.real_imm := immGen.io.real_imm
  operandSelector.io.operand2Type := CU.io.operand2_type
  operandSelector.io.operand1Type := CU.io.operand1_type

  //layer
  ID_EX.io.cpu_state:=state.io.cpu_state
  ID_EX.io.signal := conflictController.io.IDEX_control_signal
  ID_EX.io.in.pc := IF_ID.io.out.pc
  ID_EX.io.in.ins := IF_ID.io.out.ins
  ID_EX.io.in.IF_fault := IF_ID.io.out.IF_fault

  ID_EX.io.in.op1 := operandSelector.io.operand1
  ID_EX.io.in.op2 := operandSelector.io.operand2
  ID_EX.io.in.csr_val_to_reg := csr_forward_selector.io.real_val

  ID_EX.io.in.alu_op := CU.io.alu_type
  ID_EX.io.in.cmp_op := CU.io.cmp_type
  ID_EX.io.in.unsigned := CU.io.unsigned

  ID_EX.io.in.mem_signal.read_data_en := CU.io.memory_read
  ID_EX.io.in.mem_signal.write_data_en := CU.io.memory_write
  ID_EX.io.in.mem_signal.data_width := CU.io.data_width
  ID_EX.io.in.mem_signal.data_to_write := rs2_forward_selector.io.real_val //TODO 写永远写rs2寄存器的值

  ID_EX.io.in.wb_signal.au_type := CU.io.au_type
  ID_EX.io.in.wb_signal.wb_type := CU.io.write_back_type
  ID_EX.io.in.wb_signal.imm := immGen.io.real_imm
  ID_EX.io.in.wb_signal.rd := CU.io.rd_out
  ID_EX.io.in.wb_signal.write_reg := CU.io.regs_write
  ID_EX.io.in.wb_signal.csr_idx := CU.io.csr_out
  ID_EX.io.in.wb_signal.csr_write := CU.io.csr_write

  ID_EX.io.in.result_stage := CU.io.result_stage
  ID_EX.io.in.branch_type := CU.io.branch_type
  ID_EX.io.in.ID_fault := CU.io.fault

  //execute
  //ALU
  ALU.io.operand1 := ID_EX.io.out.op1
  ALU.io.operand2 := ID_EX.io.out.op2
  ALU.io.alu_op := ID_EX.io.out.alu_op
  ALU.io.unsigned := ID_EX.io.out.cmp_op

  //CMP
  CMP.io.operand1 := ID_EX.io.out.op1
  CMP.io.operand2 := ID_EX.io.out.op2
  CMP.io.cmp_op := ID_EX.io.out.cmp_op
  CMP.io.unsigned := ID_EX.io.out.unsigned

  //forwarding
//  printf("EXForwarding, au_type: %d, alu_result: %d, cmp_result: %d, write_back_type: %d, imm: %d, mem_out: %d, pc4: %d, pcImm: %d, csr_val: %d\n",
//    ID_EX.io.out.wb_signal.au_type, ALU.io.result, CMP.io.result, ID_EX.io.out.wb_signal.wb_type, ID_EX.io.out.wb_signal.imm, ID_EX.io.out.mem_signal.data_to_write,
//    ID_EX.io.out.pc + 4.U, ID_EX.io.out.pc + ID_EX.io.out.wb_signal.imm, ID_EX.io.out.csr_val_to_reg)
  EXForwarding.io.au_type := ID_EX.io.out.wb_signal.au_type
  EXForwarding.io.alu_result := ALU.io.result

  EXForwarding.io.cmp_result := CMP.io.result
  EXForwarding.io.write_back_type := ID_EX.io.out.wb_signal.wb_type
  EXForwarding.io.imm := ID_EX.io.out.wb_signal.imm
  EXForwarding.io.mem_out := DontCare
  EXForwarding.io.pc4 := ID_EX.io.out.pc + 4.U
  EXForwarding.io.pcImm := ID_EX.io.out.pc + ID_EX.io.out.wb_signal.imm
  EXForwarding.io.csr_val := ID_EX.io.out.csr_val_to_reg

  //layer
  EX_MEM.io.cpu_state:=state.io.cpu_state
  EX_MEM.io.signal := conflictController.io.EXMEM_control_signal
  EX_MEM.io.in.pc := ID_EX.io.out.pc
  EX_MEM.io.in.ins := ID_EX.io.out.ins
  EX_MEM.io.in.resultStage := ID_EX.io.out.result_stage
  EX_MEM.io.in.IF_fault := ID_EX.io.out.IF_fault
  EX_MEM.io.in.ID_fault := ID_EX.io.out.ID_fault
  EX_MEM.io.in.csr_val_to_reg := ID_EX.io.out.csr_val_to_reg
  EX_MEM.io.in.unsigned := ID_EX.io.out.unsigned
  EX_MEM.io.in.mem_signal <> ID_EX.io.out.mem_signal
  EX_MEM.io.in.wb_signal <> ID_EX.io.out.wb_signal
  EX_MEM.io.in.alu_out := ALU.io.result
  EX_MEM.io.in.cmp_out := CMP.io.result

  //MEM stage
  //data access wire
  //UART load data mode
  uartLoader.io.uart_load := io.external_signal.load_data_mode
  uartLoader.io.rxValid := io.external.uart.rxValid
  uartLoader.io.rxData := io.external.uart.rxData
  io.external.uart.rxReady := uartLoader.io.rxReady

  //mem in selector
  memInSelector.io.uart_load := io.external_signal.load_data_mode
  memInSelector.io.uart_in <> uartLoader.io.mem
  memInSelector.io.cpu_read_data := EX_MEM.io.out.mem_signal.read_data_en
  memInSelector.io.cpu_write_data := EX_MEM.io.out.mem_signal.write_data_en
  memInSelector.io.cpu_data_width := EX_MEM.io.out.mem_signal.data_width
  memInSelector.io.cpu_data_addr := EX_MEM.io.out.alu_out
  memInSelector.io.cpu_data_write := EX_MEM.io.out.mem_signal.data_to_write

  //mem
  memory.io.uart_load := io.external_signal.load_data_mode
  memory.io.cpu_state := state.io.cpu_state
  memory.io.data_addr := memInSelector.io.data_addr //ALU.io.result
  memory.io.data_write := memInSelector.io.data_write //regs.io.rs2_val
  memory.io.data_width := memInSelector.io.data_width //CU.io.data_width
  memory.io.write_data := memInSelector.io.write_data //CU.io.memory_write
  memory.io.read_data := memInSelector.io.read_data //CU.io.memory_read
  memory.io.unsigned := EX_MEM.io.out.unsigned
  memory.io.external <> io.external //board

  //fault wire to csr========================================FAULT HANDLE
  CSR.io.io_interruption <> PLIC.io.interruption
  CSR.io.IF_fault <> EX_MEM.io.out.IF_fault
  CSR.io.ID_fault <> EX_MEM.io.out.ID_fault
  CSR.io.MEM_fault <> memory.io.MEM_fault
  CSR.io.pc := EX_MEM.io.out.pc

  //MEM forwarding
  MEMForwarding.io.au_type := EX_MEM.io.out.wb_signal.au_type
  MEMForwarding.io.alu_result := EX_MEM.io.out.alu_out
  MEMForwarding.io.cmp_result := EX_MEM.io.out.cmp_out
  MEMForwarding.io.write_back_type := EX_MEM.io.out.wb_signal.wb_type
  MEMForwarding.io.imm := EX_MEM.io.out.wb_signal.imm
  MEMForwarding.io.mem_out := memory.io.data_out
  MEMForwarding.io.pc4 := EX_MEM.io.out.pc + 4.U
  MEMForwarding.io.pcImm := EX_MEM.io.out.pc + EX_MEM.io.out.wb_signal.imm
  MEMForwarding.io.csr_val := EX_MEM.io.out.csr_val_to_reg


  //layer
  MEM_WB.io.cpu_state:=state.io.cpu_state
  MEM_WB.io.signal := conflictController.io.MEMWB_control_signal
  MEM_WB.io.in.pc := EX_MEM.io.out.pc
  MEM_WB.io.in.ins := EX_MEM.io.out.ins
  MEM_WB.io.in.csr_val_to_reg := EX_MEM.io.out.csr_val_to_reg
  MEM_WB.io.in.wb_signal <> EX_MEM.io.out.wb_signal
  MEM_WB.io.in.alu_out := EX_MEM.io.out.alu_out
  MEM_WB.io.in.cmp_out := EX_MEM.io.out.cmp_out
  MEM_WB.io.in.data_out := memory.io.data_out

  //write back wire
  //csr
  CSR.io.write := MEM_WB.io.out.wb_signal.csr_write
  CSR.io.csr_idx_write := MEM_WB.io.out.wb_signal.csr_idx
  CSR.io.write_data := MEM_WB.io.out.alu_out
  printf("CSR write en: %d, csr_idx: %d, write_data: %d\n",
    MEM_WB.io.out.wb_signal.csr_write, MEM_WB.io.out.wb_signal.csr_idx, MEM_WB.io.out.alu_out)

  //wb_top selector
  wb_selector.io.au_type := MEM_WB.io.out.wb_signal.au_type
  wb_selector.io.alu_result := MEM_WB.io.out.alu_out
  wb_selector.io.cmp_result := MEM_WB.io.out.cmp_out
  wb_selector.io.write_back_type := MEM_WB.io.out.wb_signal.wb_type
  wb_selector.io.imm := MEM_WB.io.out.wb_signal.imm
  wb_selector.io.mem_out := MEM_WB.io.out.data_out //memory value
  wb_selector.io.pc4 := MEM_WB.io.out.pc + 4.U
  wb_selector.io.pcImm := MEM_WB.io.out.pc + MEM_WB.io.out.wb_signal.imm
  wb_selector.io.csr_val := MEM_WB.io.out.csr_val_to_reg

  //regs
  regs.io.write := MEM_WB.io.out.wb_signal.write_reg
  regs.io.rd := MEM_WB.io.out.wb_signal.rd
  regs.io.write_data := wb_selector.io.write_data

  //conflictDetect
  conflictController.io.uart_loading := io.external_signal.load_data_mode
  //ID using regs/csr
  conflictController.io.rs1_use := CU.io.rs1_out
  conflictController.io.rs2_use := CU.io.rs2_out
  conflictController.io.csr_use := CU.io.csr_out
  //EX writing reg or csr
  conflictController.io.IDEX_reg_to_write := ID_EX.io.out.wb_signal.rd
  conflictController.io.IDEX_write_reg := ID_EX.io.out.wb_signal.write_reg
  conflictController.io.IDEX_csr_to_write := ID_EX.io.out.wb_signal.csr_idx
  conflictController.io.IDEX_write_csr := ID_EX.io.out.wb_signal.csr_write
  conflictController.io.IDEX_reg_result_stage := ID_EX.io.out.result_stage
  //MEM writing reg or csr
  conflictController.io.EXMEM_reg_to_write := EX_MEM.io.out.wb_signal.rd
  conflictController.io.EXMEM_write_reg := EX_MEM.io.out.wb_signal.write_reg
  conflictController.io.EXMEM_csr_to_write := EX_MEM.io.out.wb_signal.csr_idx
  conflictController.io.EXMEM_write_csr := EX_MEM.io.out.wb_signal.csr_write
  conflictController.io.EXMEM_reg_result_stage := EX_MEM.io.out.resultStage

  //ID-EX control hazard
  conflictController.io.branch_type := ID_EX.io.out.branch_type
  conflictController.io.pc := ID_EX.io.out.pc
  conflictController.io.imm := ID_EX.io.out.wb_signal.imm
  conflictController.io.alu_result := ALU.io.result
  conflictController.io.cmp_result := CMP.io.result
  conflictController.io.predict_next_pc := IF_ID.io.out.pc

  //exception hazard
  conflictController.io.exception_occurs := CSR.io.fault_occurs
  conflictController.io.exception_new_pc := CSR.io.fault_new_PC

  //  when(state.io.cpu_state === CPUStateType.sLoadMode.getUInt) { //TODO 检查load模式
  //    //因为output reg那里会赋值把前面的抵消掉，所以这里要再赋值一次
  //    //本来应该用一个mux的
  //    io.external.uart.rxReady := uartLoader.io.rxReady
  //  }

  //--------------------debugging code----------------------------
  // expose reg value to outside
  val debug_io = if (GenConfig.s.debugMode) Some(IO(new CoreDebugIO)) else None
  debug_io.foreach(coe_dbg => {
    regs.debug_io.foreach(reg_dbg =>
      coe_dbg.reg_vals <> reg_dbg
    )
    CSR.debug_io.foreach(csr_dbg =>
      coe_dbg.csr_vals <> csr_dbg
    )
  })
  when(!io.external_signal.load_data_mode || memInSelector.io.write_data) {
    if (GenConfig.s.logDetails) {
      //print all output signal for each module
      printf(s"-------------State %d---------------\n", state.io.cpu_state)
      printf("--IF stage--\n")
      printf("pc: %d\n", pc.io.addr)
      printf("instructions: %x\n", memory.io.ins_out)
      printf("nextPCGen with nextPC: %d\n", nextPCGen.io.nextPC)

      printf("--ID stage--\n")
      printf("IF-ID state: %d\n",IF_ID.io.signal)
      printf("PC: %d, Ins: %x\n",IF_ID.io.out.pc,IF_ID.io.out.ins)
      printf("reg operating rs_1:%d,rs_2:%d real_imm:%d\n", CU.io.rs1_out, CU.io.rs2_out, immGen.io.real_imm)
      //        printf("CU with rs1_out: %d, rs2_out: %d, rd_out: %d, raw_imm_out: %d\n" +
      //          "alu_type : %d  cmp_type: %d, unsigned: %d, nextPC_type: %d, regs_write: %d, imm_width_type: %d, operand2_type: %d,\n" +
      //          " au_type: %d, write_back_type: %d, memory_read: %d, memory_write: %d, data_width: %d\n",
      //          CU.io.rs1_out, CU.io.rs2_out, CU.io.rd_out, CU.io.raw_imm_out,
      //          CU.io.alu_type, CU.io.cmp_type, CU.io.unsigned, CU.io.nextPC_type, CU.io.regs_write, CU.io.imm_width_type, CU.io.operand2_type,
      //          CU.io.au_type, CU.io.write_back_type, CU.io.memory_read, CU.io.memory_write, CU.io.data_width)
      //        printf("ALU with input: op1: %d, op2 : %d\n",ALU.io.operand1,ALU.io.operand2)
      printf("OPselector out : op1_val: %d op2_val: %d imm: %d\n", operandSelector.io.operand1, operandSelector.io.operand2, operandSelector.io.real_imm)
      //              printf("Reg inout: rs1_val: %d, rs1_idx: %d\n",regs.io.rs1_val,regs.io.rs1)

      printf("--EX stage--\n")
      printf("ID-EX state: %d\n",ID_EX.io.signal)
      printf("PC: %d, Ins: %x\n",ID_EX.io.out.pc,ID_EX.io.out.ins)
      printf("ALU with result: %d,\n", ALU.io.result)
      printf("CMP with result: %d\n", CMP.io.result)

      printf("--MEM stage--\n")
      printf("EX-MEM state: %d\n",EX_MEM.io.signal)
      printf("PC: %d, Ins: %x\n",EX_MEM.io.out.pc,EX_MEM.io.out.ins)
      printf("memory with read_data: %d, write_data: %d, unsigned: %d, data_width: %d,\n data_addr: %d, data_write: %x,data_out: %x\n",
        memory.io.read_data, memory.io.write_data, memory.io.unsigned, memory.io.data_width, memory.io.data_addr, memory.io.data_write, memory.io.data_out)

      printf("--WB stage--\n")
      printf("MEM-WB state: %d\n",MEM_WB.io.signal)
      printf("PC: %d, Ins: %x\n",MEM_WB.io.out.pc,MEM_WB.io.out.ins)
      printf("writeDataSelector with write_data: %d, write-enable: %d,rd:%d\n",
        wb_selector.io.write_data, MEM_WB.io.out.wb_signal.write_reg,MEM_WB.io.out.wb_signal.rd)

      printf("uart out data: %x valid: %d\n", io.external.uart.rxData, io.external.uart.rxValid)
    }
  }
}

object CoreTop extends App {
  println(
    new(chisel3.stage.ChiselStage).emitVerilog(
      new CoreTop,
      Array(
        "--target-dir", "generated_dut/"
      )
    )
  )
}