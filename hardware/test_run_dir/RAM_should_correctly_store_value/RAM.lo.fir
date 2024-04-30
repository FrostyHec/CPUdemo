circuit RAM :
  module RAM :
    input clock : Clock
    input reset : UInt<1>
    input io_write : UInt<1>
    input io_read_addr : UInt<32>
    input io_write_addr : UInt<32>
    input io_write_data : UInt<32>
    output io_read_data : UInt<32>

    mem mem : @[MemoryPacket.scala 30:18]
      data-type => UInt<32>
      depth => 65536
      read-latency => 0
      write-latency => 1
      reader => io_read_data_MPORT
      writer => MPORT
      read-under-write => undefined
    node _T = bits(io_write_addr, 15, 0)
    node _GEN_0 = validif(io_write, _T) @[MemoryPacket.scala 31:20]
    node _GEN_1 = validif(io_write, clock) @[MemoryPacket.scala 31:20]
    node _GEN_2 = mux(io_write, UInt<1>("h1"), UInt<1>("h0")) @[MemoryPacket.scala 30:18 31:20]
    node _GEN_3 = validif(io_write, UInt<1>("h1")) @[MemoryPacket.scala 31:20]
    node _GEN_4 = validif(io_write, io_write_data) @[MemoryPacket.scala 31:20]
    node _io_read_data_T = bits(io_read_addr, 15, 0) @[MemoryPacket.scala 34:29]
    io_read_data <= mem.io_read_data_MPORT.data @[MemoryPacket.scala 34:18]
    mem.io_read_data_MPORT.addr <= _io_read_data_T @[MemoryPacket.scala 34:29]
    mem.io_read_data_MPORT.en <= UInt<1>("h1") @[MemoryPacket.scala 34:29]
    mem.io_read_data_MPORT.clk <= clock @[MemoryPacket.scala 34:29]
    mem.MPORT.addr <= _GEN_0
    mem.MPORT.en <= _GEN_2
    mem.MPORT.clk <= _GEN_1
    mem.MPORT.data <= _GEN_4
    mem.MPORT.mask <= _GEN_3
