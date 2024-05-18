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
import device.MMIOOutBundle


class CoreTop extends Module {
  val io = IO(new Bundle {
    // TODO: Add IO
    val external = Flipped(new MMIOOutBundle())
  })
  //CSR
  val CSR = Module(new CSR)

  //PLIC
  val PLIC = Module(new PLIC)

  //Memory
  val memory = Module(new MemoryDispatch())

  //clock FSM
  val state = Module(new CPUState())

  //ins fetch
  val pc = Module(new PC())

  //ins decode
  val insDecode = Module(new InstructionDecoder())
  val CU = Module(new ControlUnit())

  //execute
  val regs = Module(new Registers())
  val immGen = Module(new ImmGen())
  val operandSelector = Module(new OperandSelector())
  val ALU = Module(new ALU())
  val CMP = Module(new CMP())
  val nextPCGen = Module(new NextPCGen())

  //data access

  //write back
  val writeDataSelector = Module(new WriteDataSelector())
  val auSelector = Module(new AUSelector())

  //fault detect wire
  CSR.io.mem_fault <> memory.io.fault
  CSR.io.ins_fault <> CU.io.fault
  CSR.io.io_interruption <> PLIC.io.interruption
  CSR.io.pc:=pc.io.addr
  //fault write PC and global state machine
  state.io.fault_state := CSR.io.fault_state
  pc.io.fault_write_PC:=CSR.io.fault_write_PC

  //ins fetch wire
  pc.io.cpu_state := state.io.cpu_state
  pc.io.next_addr := nextPCGen.io.nextPC
  pc.io.fault_write_PC := CSR.io.fault_write_PC
  memory.io.ins_addr := pc.io.addr

  //ins decode wire
  insDecode.io.instruction := memory.io.ins_out
  CU.io.opcode := insDecode.io.opcode
  CU.io.func3 := insDecode.io.func3
  CU.io.func7 := insDecode.io.func7
  CU.io.rs1 := insDecode.io.rs1
  CU.io.rs2 := insDecode.io.rs2
  CU.io.rd := insDecode.io.rd
  CU.io.raw_imm := insDecode.io.raw_imm

  //execute wire
  //csr
  CSR.io.cpu_state := state.io.cpu_state
  CSR.io.csr := CU.io.csr

  //register
  regs.io.cpu_state := state.io.cpu_state
  regs.io.rs1 := CU.io.rs1_out
  regs.io.rs2 := CU.io.rs2_out
  regs.io.rd := CU.io.rd_out
  regs.io.write := CU.io.regs_write
  regs.io.write_data := writeDataSelector.io.write_data

  //immGen
  immGen.io.raw_imm := CU.io.raw_imm_out
  immGen.io.unsigned := CU.io.unsigned
  immGen.io.imm_width := CU.io.imm_width_type

  //operandSelector
  operandSelector.io.csr_val := CSR.io.csr_val
  operandSelector.io.rs1_val := regs.io.rs1_val
  operandSelector.io.rs2_val := regs.io.rs2_val
  operandSelector.io.real_imm := immGen.io.real_imm
  operandSelector.io.operand2Type := CU.io.operand2_type
  operandSelector.io.operand1Type := CU.io.operand1_type

  //ALU
  ALU.io.operand1 := operandSelector.io.operand1
  ALU.io.operand2 := operandSelector.io.operand2
  ALU.io.alu_op := CU.io.alu_type
  ALU.io.unsigned := CU.io.unsigned

  //CMP
  CMP.io.operand1 := operandSelector.io.operand1
  CMP.io.operand2 := operandSelector.io.operand2
  CMP.io.cmp_op := CU.io.cmp_type
  CMP.io.unsigned := CU.io.unsigned

  //nextPCGen
  nextPCGen.io.cmp_result := CMP.io.result
  nextPCGen.io.alu_result := ALU.io.result
  nextPCGen.io.imm := immGen.io.real_imm
  nextPCGen.io.pc := pc.io.addr
  nextPCGen.io.nextPC_type := CU.io.nextPC_type

  //data access wire
  memory.io.cpu_state := state.io.cpu_state
  memory.io.data_addr := ALU.io.result
  memory.io.data_write := regs.io.rs2_val
  memory.io.data_width := CU.io.data_width
  memory.io.write_data := CU.io.memory_write
  memory.io.read_data := CU.io.memory_read
  memory.io.unsigned := CU.io.unsigned
  memory.io.external <> io.external //board

  //write back wire
  //csr
  CSR.io.write := CU.io.csr_write
  CSR.io.write_data := ALU.io.result

  //au selector
  auSelector.io.alu_result := ALU.io.result
  auSelector.io.cmp_result := CMP.io.result
  auSelector.io.au_type := CU.io.au_type

  //WriteData
  writeDataSelector.io.imm := immGen.io.real_imm
  writeDataSelector.io.mem_out := memory.io.data_out
  writeDataSelector.io.au_out := auSelector.io.au_out
  writeDataSelector.io.pc4 := nextPCGen.io.pc4
  writeDataSelector.io.pcImm := nextPCGen.io.pcImm
  writeDataSelector.io.write_back_type := CU.io.write_back_type
  writeDataSelector.io.csr := CSR.io.csr_val

  //regs
  regs.io.write := CU.io.regs_write
  regs.io.write_data := writeDataSelector.io.write_data


  //--------------------debugging code----------------------------
  // expose reg value to outside
  val debug_io = if (GenConfig.s.debugMode) Some(IO(new CoreDebugIO)) else None
  debug_io.foreach(coe_dbg =>
    regs.debug_io.foreach(reg_dbg =>
      coe_dbg.reg_vals <> reg_dbg
    )
  )
  if (GenConfig.s.logDetails) {
    //print all output signal for each module
    printf(s"-------------State %d---------------\n", state.io.cpu_state)
    printf("pc: %d\n", pc.io.addr)
    printf("instructions: %d\n", memory.io.ins_out)
    printf("reg operating rs_1:%d,rs_2:%d real_imm:%d\n", CU.io.rs1_out, CU.io.rs2_out, immGen.io.real_imm)
    //    printf("CU with rs1_out: %d, rs2_out: %d, rd_out: %d, raw_imm_out: %d\n" +
    //      "alu_type : %d  cmp_type: %d, unsigned: %d, nextPC_type: %d, regs_write: %d, imm_width_type: %d, operand2_type: %d,\n" +
    //      " au_type: %d, write_back_type: %d, memory_read: %d, memory_write: %d, data_width: %d\n",
    //      CU.io.rs1_out, CU.io.rs2_out, CU.io.rd_out, CU.io.raw_imm_out,
    //      CU.io.alu_type, CU.io.cmp_type, CU.io.unsigned, CU.io.nextPC_type, CU.io.regs_write, CU.io.imm_width_type, CU.io.operand2_type,
    //      CU.io.au_type, CU.io.write_back_type, CU.io.memory_read, CU.io.memory_write, CU.io.data_width)
    printf("ALU with result: %d,", ALU.io.result)
    printf("CMP with result: %d\n", CMP.io.result)
    printf("nextPCGen with nextPC: %d\n", nextPCGen.io.nextPC)
    printf("memory with read_data: %d, write_data: %d, unsigned: %d, data_width: %d,\n data_addr: %d, data_write: %d,data_out: %d\n",
      memory.io.read_data, memory.io.write_data, memory.io.unsigned, memory.io.data_width, memory.io.data_addr, memory.io.data_write, memory.io.data_out)
    //    printf("writeDataSelector with write_data: %d, write-enable: %d,rd:%d\n", writeDataSelector.io.write_data, CU.io.regs_write, CU.io.rd_out)
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