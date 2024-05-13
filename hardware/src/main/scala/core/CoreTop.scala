package core

import chisel3._
import core.memory._
import core.insFetch._
import core.writeBack._
import core.execute._
import core.decode._
import device.MMIOOutBundle
class CoreTop extends Module {
  val io = IO(new Bundle {
    // TODO: Add IO
    val external=Flipped(new MMIOOutBundle())
  })
  //Memory
  val memory=Module(new MemoryDispatch())

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


  //ins fetch wire
  pc.io.cpu_state := state.io.cpu_state
  pc.io.next_addr := nextPCGen.io.nextPC
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
  operandSelector.io.rs1_val := regs.io.rs1_val
  operandSelector.io.rs2_val := regs.io.rs2_val
  operandSelector.io.real_imm := immGen.io.real_imm

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
  memory.io.unsigned:= CU.io.unsigned
  memory.io.external := io.external //board

  //write back wire
  //au selector
  auSelector.io.alu_result := ALU.io.result
  auSelector.io.cmp_result := CMP.io.result

  //WriteData
  writeDataSelector.io.imm := immGen.io.real_imm
  writeDataSelector.io.mem_out := memory.io.data_out
  writeDataSelector.io.au_out := auSelector.io.au_out
  writeDataSelector.io.pc4 := nextPCGen.io.pc4
  writeDataSelector.io.pcImm := nextPCGen.io.pcImm
  writeDataSelector.io.write_back_type := CU.io.write_back_type

  //regs
  regs.io.write := CU.io.regs_write
  regs.io.write_data := writeDataSelector.io.write_data
}