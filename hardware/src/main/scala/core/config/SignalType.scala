package core.config

import chisel3.experimental.EnumType
import utils.ExtendEnum

object ALUType extends ExtendEnum {
  val ADD, SUB, XOR, OR, AND, SLL, SRL, SRA,Not2And = Value
}

object CMPType extends ExtendEnum {
  val LT, GE, EQ, NE = Value
}

object NextPCType extends ExtendEnum {
  val PC4, Branch, BranchFromALU = Value
}

object ImmWidthType extends ExtendEnum {
  val Eleven, Twelve, Twenty, ThirtyOne = Value
}

object Operand2Type extends ExtendEnum {
  val Imm, Reg2 = Value
}

object AUType extends ExtendEnum {
  val ALU, CMP = Value
}

object WriteBackType extends ExtendEnum {
  val AU, Mem, PC4, ImmGen, CSR = Value
}

object DataWidth extends ExtendEnum {
  val Byte, HalfWord, Word = Value
}

object CPUStateType extends ExtendEnum {
  val sWritePC, sWriteRegs, faultWrite = Value
}

object MemFaultType extends ExtendEnum {
  val No,InsMisaligned, InsFault, LoadMisaligned, LoadFault, StoreMisaligned, StoreFault = Value
}

object InsFaultType extends ExtendEnum {
  val No,IllegalIns, BreakPoint, EcallM, Mret = Value
}
object Operand1Type extends ExtendEnum{
  val Reg1,CSR = Value
}