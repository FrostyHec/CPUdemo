package core

import chisel3._
import configs.GenConfig
import core.config._
import core.memory._
import core.insFetch._
import core.writeBack._
import core.execute._
import core.decode._
import device.{ExternalSignalBundle, MMIOOutBundle}


class CoreTop extends Module {
  val io = IO(new Bundle {
    val external = Flipped(new MMIOOutBundle())
    val external_signal = Flipped(new ExternalSignalBundle)
  })
  //Memory
  val memory = Module(new MemoryDispatch())
  val uartLoader = Module(new UARTLoader())
  val memInSelector = Module(new MemWriteSelector())
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

  //UART Module
  uartLoader.io.cpu_state := state.io.cpu_state
  uartLoader.io.rxValid := io.external.uart.rxValid
  uartLoader.io.rxData := io.external.uart.rxData

  //data access
  //TODO WIRES ON DATA ACCESS,from 2 select 1
  memInSelector.io.cpu_state := state.io.cpu_state
  memInSelector.io.uart_in <> uartLoader.io.mem

  memInSelector.io.cpu_read_data := CU.io.memory_read
  memInSelector.io.cpu_write_data := CU.io.memory_write
  memInSelector.io.cpu_data_width := CU.io.data_width
  memInSelector.io.cpu_data_addr := ALU.io.result
  memInSelector.io.cpu_data_write := regs.io.rs2_val

  //write back
  val writeDataSelector = Module(new WriteDataSelector())
  val auSelector = Module(new AUSelector())

  //Load data mode
  state.io.load_mode := io.external_signal.load_data_mode

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
  operandSelector.io.operand2Type := CU.io.operand2_type

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
  memory.io.data_addr := memInSelector.io.data_addr //ALU.io.result
  memory.io.data_write := memInSelector.io.data_write //regs.io.rs2_val
  memory.io.data_width := memInSelector.io.data_width //CU.io.data_width
  memory.io.write_data := memInSelector.io.write_data //CU.io.memory_write
  memory.io.read_data := memInSelector.io.read_data //CU.io.memory_read
  memory.io.unsigned := CU.io.unsigned
  memory.io.external <> io.external //board

  //write back wire
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
    printf("instructions: %x\n", memory.io.ins_out)
    printf("reg operating rs_1:%d,rs_2:%d real_imm:%d\n", CU.io.rs1_out, CU.io.rs2_out, immGen.io.real_imm)
    //        printf("CU with rs1_out: %d, rs2_out: %d, rd_out: %d, raw_imm_out: %d\n" +
    //          "alu_type : %d  cmp_type: %d, unsigned: %d, nextPC_type: %d, regs_write: %d, imm_width_type: %d, operand2_type: %d,\n" +
    //          " au_type: %d, write_back_type: %d, memory_read: %d, memory_write: %d, data_width: %d\n",
    //          CU.io.rs1_out, CU.io.rs2_out, CU.io.rd_out, CU.io.raw_imm_out,
    //          CU.io.alu_type, CU.io.cmp_type, CU.io.unsigned, CU.io.nextPC_type, CU.io.regs_write, CU.io.imm_width_type, CU.io.operand2_type,
    //          CU.io.au_type, CU.io.write_back_type, CU.io.memory_read, CU.io.memory_write, CU.io.data_width)
    //        printf("ALU with input: op1: %d, op2 : %d\n",ALU.io.operand1,ALU.io.operand2)
    //        printf("OPselector inout : rs1_val: %d rs2_val: %d imm: %d\n",operandSelector.io.rs1_val,operandSelector.io.rs2_val,operandSelector.io.real_imm)
    //        printf("Reg inout: rs1_val: %d, rs1_idx: %d\n",regs.io.rs1_val,regs.io.rs1)
    printf("ALU with result: %d,", ALU.io.result)
    printf("CMP with result: %d\n", CMP.io.result)
    printf("nextPCGen with nextPC: %d\n", nextPCGen.io.nextPC)
    printf("memory with read_data: %d, write_data: %d, unsigned: %d, data_width: %d,\n data_addr: %d, data_write: %x,data_out: %x\n",
      memory.io.read_data, memory.io.write_data, memory.io.unsigned, memory.io.data_width, memory.io.data_addr, memory.io.data_write, memory.io.data_out)
    //    printf("writeDataSelector with write_data: %d, write-enable: %d,rd:%d\n", writeDataSelector.io.write_data, CU.io.regs_write, CU.io.rd_out)
    printf("uart out data: %x valid: %d\n",io.external.uart.rxData,io.external.uart.rxValid)
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