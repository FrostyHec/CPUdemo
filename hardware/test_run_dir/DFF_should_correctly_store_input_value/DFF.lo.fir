circuit DFF :
  module DFF :
    input clock : Clock
    input reset : UInt<1>
    input io_d : UInt<1>
    output io_q : UInt<1>

    reg qReg : UInt<1>, clock with :
      reset => (UInt<1>("h0"), qReg) @[DFF.scala 21:19]
    node _T = bits(reset, 0, 0) @[DFF.scala 24:9]
    node _T_1 = eq(_T, UInt<1>("h0")) @[DFF.scala 24:9]
    node _T_2 = bits(reset, 0, 0) @[DFF.scala 25:9]
    node _T_3 = eq(_T_2, UInt<1>("h0")) @[DFF.scala 25:9]
    io_q <= qReg @[DFF.scala 23:8]
    qReg <= mux(reset, UInt<1>("h0"), io_d) @[DFF.scala 21:{19,19} 22:8]
    printf(clock, and(and(UInt<1>("h1"), _T_1), UInt<1>("h1")), "d=%d q=%d\n", io_d, io_q) : printf @[DFF.scala 24:9]
    printf(clock, and(and(UInt<1>("h1"), _T_3), UInt<1>("h1")), "now reg: %d\n", qReg) : printf_1 @[DFF.scala 25:9]
