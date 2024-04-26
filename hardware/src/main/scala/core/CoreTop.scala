package core
import chisel3._
import core.dataAccess._
import core.insFetch._
import core.config._
import core.writeBack._
import core.execute._
import core.decode._

class CoreTop extends Module {
  val io = IO(new Bundle {
    // TODO: Add IO
  })
  //ins fetch
  val pc=Module(new PC())
  val insMem=Module(new InstructionMemory())

  //ins decode
  val insDecode=Module(new InstructionDecoder())
  val CU=Module(new ControlUnit())

  //execute
  val regs=Module(new Registers())
  val immGen=Module(new ImmGen())
  val operandSelector=Module(new OperandSelector())
  val ALU=Module(new ALU())
  val CMP=Module(new CMP())
  val nextPCGen=Module(new NextPCGen())

  //data access
  val dataMem=Module(new DataMemory())

  //write back
  val writeDataSelector=Module(new WriteDataSelector())
  val auSelector=Module(new AUSelector())


  //ins fetch wire
  pc.io.nextAddr:=nextPCGen.io.nextPC
  insMem.io.addr:=pc.io.addr

  //ins decode wire
  insDecode.io.instruction:=insMem.io.instruction
  CU.io.opcode:=insDecode.io.opcode
  CU.io.func3:=insDecode.io.func3
  CU.io.func7:=insDecode.io.func7
  CU.io.rs1:=insDecode.io.rs1
  CU.io.rs2:=insDecode.io.rs2
  CU.io.rd:=insDecode.io.rd
  CU.io.raw_imm:=insDecode.io.raw_imm

  //execute wire
  //regster
  regs.io.rs1:=CU.io.rs1_out
  regs.io.rs2:=CU.io.rs2_out
  regs.io.rd:=CU.io.rd_out
  regs.io.write:=CU.io.regs_write
  regs.io.write_data:=writeDataSelector.io.write_data

  //immGen
  immGen.io.raw_imm:=CU.io.raw_imm_out
  immGen.io.unsigned:=CU.io.unsigned
  immGen.io.imm_width:=CU.io.imm_width_type

  //operandSelector
  operandSelector.io.rs1_val:=regs.io.rs1_val
  operandSelector.io.rs2_val:=regs.io.rs2_val
  operandSelector.io.real_imm:=immGen.io.real_imm

  //ALU
  ALU.io.operand1:=operandSelector.io.operand1
  ALU.io.operand2:=operandSelector.io.operand2
  ALU.io.alu_op:=CU.io.alu_type
  ALU.io.unsigned:=CU.io.unsigned

  //CMP
  CMP.io.operand1:=operandSelector.io.operand1
  CMP.io.operand2:=operandSelector.io.operand2
  CMP.io.cmp_op:=CU.io.cmp_type
  CMP.io.unsigned:=CU.io.unsigned

  //nextPCGen
  nextPCGen.io.cmp_result:=CMP.io.result
  nextPCGen.io.alu_result:=ALU.io.result
  nextPCGen.io.imm:=immGen.io.real_imm
  nextPCGen.io.pc:=pc.io.addr
  nextPCGen.io.nextPC_type:=CU.io.nextPC_type

  //data access wire
  dataMem.io.addr:=ALU.io.result
  dataMem.io.write_data:=regs.io.rs2_val
  dataMem.io.data_width:=CU.io.data_width
  dataMem.io.mem_write:=CU.io.memory_write
  dataMem.io.mem_read:=CU.io.memory_read

  //write back wire
  //au selector

}
//TODO stage changed. AAA