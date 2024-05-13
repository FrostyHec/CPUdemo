circuit NextPCGen :
  module NextPCGen :
    input clock : Clock
    input reset : UInt<1>
    input io_nextPC_type : UInt<2>
    input io_cmp_result : UInt<1>
    input io_alu_result : UInt<32>
    input io_imm : UInt<32>
    input io_pc : UInt<32>
    output io_nextPC : UInt<32>
    output io_pc4 : UInt<32>
    output io_pcImm : UInt<32>

    node _io_pc4_T = add(io_pc, UInt<3>("h4")) @[NextPCGen.scala 21:19]
    node _io_pc4_T_1 = tail(_io_pc4_T, 1) @[NextPCGen.scala 21:19]
    node _T = eq(UInt<1>("h0"), io_nextPC_type) @[NextPCGen.scala 24:25]
    node _io_nextPC_T = add(io_pc, UInt<3>("h4")) @[NextPCGen.scala 26:26]
    node _io_nextPC_T_1 = tail(_io_nextPC_T, 1) @[NextPCGen.scala 26:26]
    node _T_1 = eq(UInt<1>("h1"), io_nextPC_type) @[NextPCGen.scala 24:25]
    node _io_nextPC_T_2 = add(io_pc, io_imm) @[NextPCGen.scala 30:45]
    node _io_nextPC_T_3 = tail(_io_nextPC_T_2, 1) @[NextPCGen.scala 30:45]
    node _io_nextPC_T_4 = add(io_pc, UInt<3>("h4")) @[NextPCGen.scala 30:61]
    node _io_nextPC_T_5 = tail(_io_nextPC_T_4, 1) @[NextPCGen.scala 30:61]
    node _io_nextPC_T_6 = mux(io_cmp_result, _io_nextPC_T_3, _io_nextPC_T_5) @[NextPCGen.scala 30:23]
    node _io_pcImm_T = add(io_pc, UInt<3>("h4")) @[NextPCGen.scala 31:25]
    node _io_pcImm_T_1 = tail(_io_pcImm_T, 1) @[NextPCGen.scala 31:25]
    node _T_2 = eq(UInt<2>("h2"), io_nextPC_type) @[NextPCGen.scala 24:25]
    node _io_nextPC_T_7 = add(io_pc, UInt<3>("h4")) @[NextPCGen.scala 34:26]
    node _io_nextPC_T_8 = tail(_io_nextPC_T_7, 1) @[NextPCGen.scala 34:26]
    node _io_pcImm_T_2 = add(io_pc, io_alu_result) @[NextPCGen.scala 35:25]
    node _io_pcImm_T_3 = tail(_io_pcImm_T_2, 1) @[NextPCGen.scala 35:25]
    node _GEN_0 = validif(_T_2, _io_nextPC_T_8) @[NextPCGen.scala 24:25 34:17]
    node _GEN_1 = mux(_T_2, _io_pcImm_T_3, UInt<1>("h0")) @[NextPCGen.scala 22:12 24:25 35:16]
    node _GEN_2 = mux(_T_1, _io_nextPC_T_6, _GEN_0) @[NextPCGen.scala 24:25 30:17]
    node _GEN_3 = mux(_T_1, _io_pcImm_T_1, _GEN_1) @[NextPCGen.scala 24:25 31:16]
    node _GEN_4 = mux(_T, _io_nextPC_T_1, _GEN_2) @[NextPCGen.scala 24:25 26:17]
    node _GEN_5 = mux(_T, UInt<1>("h0"), _GEN_3) @[NextPCGen.scala 22:12 24:25]
    io_nextPC <= _GEN_4
    io_pc4 <= _io_pc4_T_1 @[NextPCGen.scala 21:10]
    io_pcImm <= _GEN_5
