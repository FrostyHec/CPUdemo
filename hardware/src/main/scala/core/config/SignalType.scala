package core.config

import chisel3._
import utils.ExtendEnum

object ALUType extends ExtendEnum {
  val ADD, SUB, XOR, OR, AND, SLL, SRL, SRA, Not2And = Value
}

object CMPType extends ExtendEnum {
  val LT, GE, EQ, NE = Value
}

object NextPCType extends ExtendEnum {
  val PC4, Branch, BranchFromALU, BranchFromImm = Value
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
  val AU, Mem, PC4, ImmGen, PCImm, CSR = Value
}

object DataWidth extends ExtendEnum {
  val Byte, HalfWord, Word = Value
}

object CPUStateType extends ExtendEnum {
  val cycle1_read,cycle2_write,cycle3_layer = Value
}
object IFFaultType extends ExtendEnum {
  val No, InsMisaligned, InsFault = Value
}
object IDFaultType extends ExtendEnum {
  val No, IllegalIns, BreakPoint, EcallM, Mret = Value
}
object MEMFaultType extends ExtendEnum {
  val No, LoadMisaligned, LoadFault, StoreMisaligned, StoreFault = Value
}
object Operand1Type extends ExtendEnum {
  val Reg1, CSR = Value
}

object PrivilegeType {
  val width = 2.W
  val Machine = "b11".U
  val User = "b00".U
}

object LayerControlSignal extends ExtendEnum {
  val Normal, STALL, NOP = Value
}
object ResultStageType extends ExtendEnum {
  val EX, MEM = Value
}
object BranchType extends ExtendEnum {
  val No,BType,JALR = Value
}
object ForwardType extends ExtendEnum {
  val No, EXForward,MEMForward = Value
}
object NextPCControlSignal extends ExtendEnum {
  val Normal,STALL,NewPC = Value
}