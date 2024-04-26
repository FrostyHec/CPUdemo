package core.config

import utils.ExtendEnum

object ALUType extends ExtendEnum{
  val ADD,SUB,XOR,OR,AND,SLL,SRL,SRA,None = Value
}
object CMPType extends ExtendEnum{
  val LT,GE,EQ,NE,None = Value
}
object NextPCType extends ExtendEnum{
  val PC4,Branch,BranchFromALU = Value
}
object ImmWidthType extends ExtendEnum{
  val Five,Eleven,Twelve,Twenty,None = Value
}
object Operand2Type extends ExtendEnum{
  val Imm,Reg2 = Value
}
object AUType extends ExtendEnum{
  val ALU,CMP,None= Value
}
object WriteBackType extends ExtendEnum{
  val AU,Mem,PC4,ImmGen,None=Value
}
object DataWidth extends ExtendEnum{
  val Byte,HalfWord,Word,None=Value
}

