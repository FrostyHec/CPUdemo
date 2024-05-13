circuit ALU :
  module ALU :
    input clock : Clock
    input reset : UInt<1>
    input io_operand1 : UInt<32>
    input io_operand2 : UInt<32>
    input io_alu_op : UInt<3>
    input io_unsigned : UInt<1>
    output io_result : UInt<32>

    node _T = eq(UInt<1>("h0"), io_alu_op) @[ALU.scala 15:22]
    node _io_result_T = add(io_operand1, io_operand2) @[ALU.scala 17:32]
    node _io_result_T_1 = tail(_io_result_T, 1) @[ALU.scala 17:32]
    node _T_1 = eq(UInt<1>("h1"), io_alu_op) @[ALU.scala 15:22]
    node _io_result_T_2 = sub(io_operand1, io_operand2) @[ALU.scala 20:32]
    node _io_result_T_3 = tail(_io_result_T_2, 1) @[ALU.scala 20:32]
    node _T_2 = eq(UInt<2>("h2"), io_alu_op) @[ALU.scala 15:22]
    node _io_result_T_4 = xor(io_operand1, io_operand2) @[ALU.scala 23:32]
    node _T_3 = eq(UInt<2>("h3"), io_alu_op) @[ALU.scala 15:22]
    node _io_result_T_5 = or(io_operand1, io_operand2) @[ALU.scala 26:32]
    node _T_4 = eq(UInt<3>("h4"), io_alu_op) @[ALU.scala 15:22]
    node _io_result_T_6 = and(io_operand1, io_operand2) @[ALU.scala 29:32]
    node _T_5 = eq(UInt<3>("h5"), io_alu_op) @[ALU.scala 15:22]
    node _io_result_T_7 = bits(io_operand2, 4, 0) @[ALU.scala 32:46]
    node _io_result_T_8 = dshl(io_operand1, _io_result_T_7) @[ALU.scala 32:32]
    node _T_6 = eq(UInt<3>("h7"), io_alu_op) @[ALU.scala 15:22]
    node _T_7 = eq(io_unsigned, UInt<1>("h0")) @[ALU.scala 35:12]
    node _io_result_T_9 = asSInt(io_operand1) @[ALU.scala 36:35]
    node _io_result_T_10 = bits(io_operand2, 4, 0) @[ALU.scala 36:56]
    node _io_result_T_11 = dshr(_io_result_T_9, _io_result_T_10) @[ALU.scala 36:42]
    node _io_result_T_12 = asUInt(_io_result_T_11) @[ALU.scala 36:71]
    node _io_result_T_13 = bits(io_operand2, 4, 0) @[ALU.scala 38:48]
    node _io_result_T_14 = dshr(io_operand1, _io_result_T_13) @[ALU.scala 38:34]
    node _GEN_0 = mux(_T_7, _io_result_T_12, _io_result_T_14) @[ALU.scala 35:26 36:19 38:19]
    node _T_8 = eq(UInt<3>("h6"), io_alu_op) @[ALU.scala 15:22]
    node _io_result_T_15 = bits(io_operand2, 4, 0) @[ALU.scala 42:47]
    node _io_result_T_16 = dshr(io_operand1, _io_result_T_15) @[ALU.scala 42:33]
    node _GEN_1 = validif(_T_8, _io_result_T_16) @[ALU.scala 15:22 42:17]
    node _GEN_2 = mux(_T_6, _GEN_0, _GEN_1) @[ALU.scala 15:22]
    node _GEN_3 = mux(_T_5, _io_result_T_8, _GEN_2) @[ALU.scala 15:22 32:17]
    node _GEN_4 = mux(_T_4, _io_result_T_6, _GEN_3) @[ALU.scala 15:22 29:17]
    node _GEN_5 = mux(_T_3, _io_result_T_5, _GEN_4) @[ALU.scala 15:22 26:17]
    node _GEN_6 = mux(_T_2, _io_result_T_4, _GEN_5) @[ALU.scala 15:22 23:17]
    node _GEN_7 = mux(_T_1, _io_result_T_3, _GEN_6) @[ALU.scala 15:22 20:17]
    node _GEN_8 = mux(_T, _io_result_T_1, _GEN_7) @[ALU.scala 15:22 17:17]
    io_result <= bits(_GEN_8, 31, 0)
