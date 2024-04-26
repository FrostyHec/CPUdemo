circuit BasicTest_Anon :
  module BasicTest_Anon :
    input clock : Clock
    input reset : UInt<1>
    input io_in : UInt<8>
    output io_out : UInt<8>

    reg io_out_REG : UInt<8>, clock with :
      reset => (UInt<1>("h0"), io_out_REG) @[BasicTest.scala 15:24]
    io_out <= io_out_REG @[BasicTest.scala 15:14]
    io_out_REG <= mux(reset, UInt<1>("h0"), io_in) @[BasicTest.scala 15:{24,24,24}]
