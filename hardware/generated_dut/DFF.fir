circuit DFF :
  module DFF :
    input clock : Clock
    input reset : UInt<1>
    output io : { flip d : UInt<1>, q : UInt<1>}

    clock is invalid
    reset is invalid
    io is invalid
    reg qReg : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[DFF.scala 21:19]
    qReg <= io.d @[DFF.scala 22:8]
    io.q <= qReg @[DFF.scala 23:8]
    node _T = bits(reset, 0, 0) @[DFF.scala 24:9]
    node _T_1 = eq(_T, UInt<1>("h0")) @[DFF.scala 24:9]
    when _T_1 : @[DFF.scala 24:9]
      printf(clock, UInt<1>("h1"), "d=%d q=%d\n", io.d, io.q) : printf @[DFF.scala 24:9]
    node _T_2 = bits(reset, 0, 0) @[DFF.scala 25:9]
    node _T_3 = eq(_T_2, UInt<1>("h0")) @[DFF.scala 25:9]
    when _T_3 : @[DFF.scala 25:9]
      printf(clock, UInt<1>("h1"), "now reg: %d\n", qReg) : printf_1 @[DFF.scala 25:9]

