circuit OperandSelector :
  module OperandSelector :
    input clock : Clock
    input reset : UInt<1>
    input io_rs1_val : UInt<32>
    input io_rs2_val : UInt<32>
    input io_real_imm : UInt<32>
    input io_operand2Type : UInt<1>
    output io_operand1 : UInt<32>
    output io_operand2 : UInt<32>

    node _T = eq(UInt<1>("h0"), io_operand2Type) @[OperandSelector.scala 19:26]
    node _T_1 = eq(UInt<1>("h1"), io_operand2Type) @[OperandSelector.scala 19:26]
    node _GEN_0 = validif(_T_1, io_rs2_val) @[OperandSelector.scala 19:26 24:19]
    node _GEN_1 = mux(_T, io_real_imm, _GEN_0) @[OperandSelector.scala 19:26 21:19]
    io_operand1 <= io_rs1_val @[OperandSelector.scala 17:15]
    io_operand2 <= _GEN_1
