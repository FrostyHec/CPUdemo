circuit CMP :
  module CMP :
    input clock : Clock
    input reset : UInt<1>
    input io_operand1 : UInt<32>
    input io_operand2 : UInt<32>
    input io_cmp_op : UInt<2>
    input io_unsigned : UInt<1>
    output io_result : UInt<1>

    node _T = eq(UInt<1>("h0"), io_cmp_op) @[CMP.scala 16:20]
    node _io_result_T = lt(io_operand1, io_operand2) @[CMP.scala 19:41]
    node _io_result_T_1 = asSInt(io_operand1) @[CMP.scala 21:34]
    node _io_result_T_2 = asSInt(io_operand2) @[CMP.scala 21:55]
    node _io_result_T_3 = lt(_io_result_T_1, _io_result_T_2) @[CMP.scala 21:41]
    node _GEN_0 = mux(io_unsigned, _io_result_T, _io_result_T_3) @[CMP.scala 18:24 19:19 21:19]
    node _T_1 = eq(UInt<1>("h1"), io_cmp_op) @[CMP.scala 16:20]
    node _io_result_T_4 = geq(io_operand1, io_operand2) @[CMP.scala 26:41]
    node _io_result_T_5 = asSInt(io_operand1) @[CMP.scala 28:34]
    node _io_result_T_6 = asSInt(io_operand2) @[CMP.scala 28:56]
    node _io_result_T_7 = geq(_io_result_T_5, _io_result_T_6) @[CMP.scala 28:41]
    node _GEN_1 = mux(io_unsigned, _io_result_T_4, _io_result_T_7) @[CMP.scala 25:24 26:19 28:19]
    node _T_2 = eq(UInt<2>("h2"), io_cmp_op) @[CMP.scala 16:20]
    node _io_result_T_8 = eq(io_operand1, io_operand2) @[CMP.scala 32:32]
    node _T_3 = eq(UInt<2>("h3"), io_cmp_op) @[CMP.scala 16:20]
    node _io_result_T_9 = neq(io_operand1, io_operand2) @[CMP.scala 35:32]
    node _GEN_2 = validif(_T_3, _io_result_T_9) @[CMP.scala 16:20 35:17]
    node _GEN_3 = mux(_T_2, _io_result_T_8, _GEN_2) @[CMP.scala 16:20 32:17]
    node _GEN_4 = mux(_T_1, _GEN_1, _GEN_3) @[CMP.scala 16:20]
    node _GEN_5 = mux(_T, _GEN_0, _GEN_4) @[CMP.scala 16:20]
    io_result <= _GEN_5
