circuit PC :
  module PC :
    input clock : Clock
    input reset : UInt<1>
    input io_cpu_state : UInt<1>
    input io_next_addr : UInt<32>
    output io_addr : UInt<32>

    reg pc : UInt<32>, clock with :
      reset => (UInt<1>("h0"), pc) @[PC.scala 16:17]
    node _T = eq(io_cpu_state, UInt<1>("h0")) @[PC.scala 18:20]
    node _GEN_0 = mux(_T, io_next_addr, pc) @[PC.scala 16:17 18:53 19:7]
    io_addr <= pc @[PC.scala 17:10]
    pc <= mux(reset, UInt<32>("h0"), _GEN_0) @[PC.scala 16:{17,17}]
