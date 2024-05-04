circuit MemoryDispatch :
  module RAM :
    input clock : Clock
    input reset : UInt<1>
    input io_write : UInt<1>
    input io_read_addr : UInt<32>
    input io_write_addr : UInt<32>
    input io_write_data : UInt<32>
    output io_read_data : UInt<32>

    mem mem : @[MemoryPacket.scala 32:18]
      data-type => UInt<32>
      depth => 65536
      read-latency => 0
      write-latency => 1
      reader => io_read_data_MPORT
      writer => MPORT
      read-under-write => undefined
    node _T = bits(io_write_addr, 15, 0)
    node _GEN_0 = validif(io_write, _T) @[MemoryPacket.scala 33:20]
    node _GEN_1 = validif(io_write, clock) @[MemoryPacket.scala 33:20]
    node _GEN_2 = mux(io_write, UInt<1>("h1"), UInt<1>("h0")) @[MemoryPacket.scala 32:18 33:20]
    node _GEN_3 = validif(io_write, UInt<1>("h1")) @[MemoryPacket.scala 33:20]
    node _GEN_4 = validif(io_write, io_write_data) @[MemoryPacket.scala 33:20]
    node _io_read_data_T = bits(io_read_addr, 15, 0) @[MemoryPacket.scala 36:29]
    io_read_data <= mem.io_read_data_MPORT.data @[MemoryPacket.scala 36:18]
    mem.io_read_data_MPORT.addr <= _io_read_data_T @[MemoryPacket.scala 36:29]
    mem.io_read_data_MPORT.en <= UInt<1>("h1") @[MemoryPacket.scala 36:29]
    mem.io_read_data_MPORT.clk <= clock @[MemoryPacket.scala 36:29]
    mem.MPORT.addr <= _GEN_0
    mem.MPORT.en <= _GEN_2
    mem.MPORT.clk <= _GEN_1
    mem.MPORT.data <= _GEN_4
    mem.MPORT.mask <= _GEN_3

  module InsRAM :
    input clock : Clock
    input reset : UInt<1>
    input io_write : UInt<1>
    input io_read_addr : UInt<32>
    input io_write_addr : UInt<32>
    input io_write_data : UInt<32>
    output io_read_data : UInt<32>

    inst insRAM of RAM @[InsRAM.scala 12:30]
    io_read_data <= insRAM.io_read_data @[InsRAM.scala 17:6]
    insRAM.clock <= clock
    insRAM.reset <= reset
    insRAM.io_write <= io_write @[InsRAM.scala 17:6]
    insRAM.io_read_addr <= io_read_addr @[InsRAM.scala 17:6]
    insRAM.io_write_addr <= io_write_addr @[InsRAM.scala 17:6]
    insRAM.io_write_data <= io_write_data @[InsRAM.scala 17:6]

  module OutRegisters :
    input clock : Clock
    input reset : UInt<1>
    input io_write : UInt<1>
    input io_read_addr : UInt<32>
    input io_write_addr : UInt<32>
    input io_write_data : UInt<32>
    output io_read_data : UInt<32>

    io_read_data is invalid

  module MemoryDispatch :
    input clock : Clock
    input reset : UInt<1>
    input io_cpu_state : UInt<1>
    input io_ins_addr : UInt<32>
    output io_ins_out : UInt<32>
    input io_read_data : UInt<1>
    input io_write_data : UInt<1>
    input io_unsigned : UInt<1>
    input io_data_width : UInt<2>
    input io_data_addr : UInt<32>
    input io_data_write : UInt<32>
    output io_data_out : UInt<32>

    inst insRAM of InsRAM @[MemoryDispatch.scala 34:22]
    inst dataRAM of InsRAM @[MemoryDispatch.scala 35:23]
    inst outRegisters of OutRegisters @[MemoryDispatch.scala 36:28]
    node rw_mem_addr = shr(io_data_addr, 2) @[MemoryDispatch.scala 30:34]
    node read_ins_addr = shr(io_ins_addr, 2) @[MemoryDispatch.scala 31:35]
    node is_write_clk = eq(io_cpu_state, UInt<1>("h1")) @[MemoryDispatch.scala 43:35]
    node _T = eq(UInt<1>("h0"), io_data_width) @[MemoryDispatch.scala 48:25]
    node _data_in_T = bits(io_data_write, 7, 0) @[MemoryDispatch.scala 50:31]
    node _T_1 = eq(UInt<1>("h1"), io_data_width) @[MemoryDispatch.scala 48:25]
    node _data_in_T_1 = bits(io_data_write, 15, 0) @[MemoryDispatch.scala 53:31]
    node _T_2 = eq(UInt<2>("h2"), io_data_width) @[MemoryDispatch.scala 48:25]
    node _GEN_0 = validif(_T_2, io_data_write) @[MemoryDispatch.scala 48:25 56:15]
    node _GEN_1 = mux(_T_1, _data_in_T_1, _GEN_0) @[MemoryDispatch.scala 48:25 53:15]
    node _GEN_2 = mux(_T, _data_in_T, _GEN_1) @[MemoryDispatch.scala 48:25 50:15]
    node _T_3 = leq(UInt<1>("h0"), io_data_addr) @[MemoryDispatch.scala 87:36]
    node _T_4 = leq(io_data_addr, UInt<16>("hffff")) @[MemoryDispatch.scala 88:21]
    node _T_5 = and(_T_3, _T_4) @[MemoryDispatch.scala 88:5]
    node _insRAM_io_write_T = and(is_write_clk, io_write_data) @[MemoryDispatch.scala 90:39]
    node _T_6 = bits(reset, 0, 0) @[MemoryDispatch.scala 92:13]
    node _T_7 = eq(_T_6, UInt<1>("h0")) @[MemoryDispatch.scala 92:13]
    node _GEN_3 = mux(io_write_data, _insRAM_io_write_T, UInt<1>("h0")) @[MemoryDispatch.scala 89:25 90:23 MemoryPacket.scala 16:16]
    node _T_8 = leq(UInt<17>("h10000"), io_data_addr) @[MemoryDispatch.scala 96:43]
    node _T_9 = leq(io_data_addr, UInt<17>("h1ffff")) @[MemoryDispatch.scala 97:21]
    node _T_10 = and(_T_8, _T_9) @[MemoryDispatch.scala 97:5]
    node _havard_mem_T = sub(io_data_addr, UInt<17>("h10000")) @[MemoryDispatch.scala 99:36]
    node _havard_mem_T_1 = tail(_havard_mem_T, 1) @[MemoryDispatch.scala 99:36]
    node havard_mem = shr(_havard_mem_T_1, 2) @[MemoryDispatch.scala 99:68]
    node _dataRAM_io_write_T = and(is_write_clk, io_write_data) @[MemoryDispatch.scala 102:40]
    node _GEN_4 = mux(io_read_data, havard_mem, rw_mem_addr) @[MemoryDispatch.scala 103:30 104:28 MemoryPacket.scala 17:20]
    node _GEN_5 = validif(io_read_data, dataRAM.io_read_data) @[MemoryDispatch.scala 103:30 105:16]
    node _GEN_6 = mux(io_write_data, havard_mem, rw_mem_addr) @[MemoryDispatch.scala 100:25 101:29 MemoryPacket.scala 18:21]
    node _GEN_7 = mux(io_write_data, _dataRAM_io_write_T, UInt<1>("h0")) @[MemoryDispatch.scala 100:25 102:24 MemoryPacket.scala 16:16]
    node _GEN_8 = mux(io_write_data, rw_mem_addr, _GEN_4) @[MemoryDispatch.scala 100:25 MemoryPacket.scala 17:20]
    node _GEN_9 = validif(eq(io_write_data, UInt<1>("h0")), _GEN_5) @[MemoryDispatch.scala 100:25]
    node _T_11 = leq(UInt<32>("hffffff00"), io_data_addr) @[MemoryDispatch.scala 109:51]
    node _T_12 = leq(io_data_addr, UInt<32>("hffffffff")) @[MemoryDispatch.scala 110:21]
    node _T_13 = and(_T_11, _T_12) @[MemoryDispatch.scala 110:5]
    node _outRegisters_io_write_T = and(is_write_clk, io_write_data) @[MemoryDispatch.scala 112:45]
    node _GEN_10 = validif(io_read_data, outRegisters.io_read_data) @[MemoryDispatch.scala 113:30 114:16]
    node _GEN_11 = mux(io_write_data, _outRegisters_io_write_T, UInt<1>("h0")) @[MemoryDispatch.scala 111:25 112:29 MemoryPacket.scala 16:16]
    node _GEN_12 = validif(eq(io_write_data, UInt<1>("h0")), _GEN_10) @[MemoryDispatch.scala 111:25]
    node _T_14 = bits(reset, 0, 0) @[MemoryDispatch.scala 119:11]
    node _T_15 = eq(_T_14, UInt<1>("h0")) @[MemoryDispatch.scala 119:11]
    node _GEN_13 = mux(_T_13, _GEN_11, UInt<1>("h0")) @[MemoryDispatch.scala 110:60 MemoryPacket.scala 16:16]
    node _GEN_14 = validif(_T_13, _GEN_12) @[MemoryDispatch.scala 110:60]
    node _GEN_15 = mux(_T_10, _GEN_6, rw_mem_addr) @[MemoryDispatch.scala 97:52 MemoryPacket.scala 18:21]
    node _GEN_16 = mux(_T_10, _GEN_7, UInt<1>("h0")) @[MemoryDispatch.scala 97:52 MemoryPacket.scala 16:16]
    node _GEN_17 = mux(_T_10, _GEN_8, rw_mem_addr) @[MemoryDispatch.scala 97:52 MemoryPacket.scala 17:20]
    node _GEN_18 = mux(_T_10, _GEN_9, _GEN_14) @[MemoryDispatch.scala 97:52]
    node _GEN_19 = mux(_T_10, UInt<1>("h0"), _GEN_13) @[MemoryDispatch.scala 97:52 MemoryPacket.scala 16:16]
    node _GEN_20 = mux(_T_5, _GEN_3, UInt<1>("h0")) @[MemoryDispatch.scala 88:51 MemoryPacket.scala 16:16]
    node _GEN_21 = mux(_T_5, rw_mem_addr, _GEN_15) @[MemoryDispatch.scala 88:51 MemoryPacket.scala 18:21]
    node _GEN_22 = mux(_T_5, UInt<1>("h0"), _GEN_16) @[MemoryDispatch.scala 88:51 MemoryPacket.scala 16:16]
    node _GEN_23 = mux(_T_5, rw_mem_addr, _GEN_17) @[MemoryDispatch.scala 88:51 MemoryPacket.scala 17:20]
    node _GEN_24 = validif(eq(_T_5, UInt<1>("h0")), _GEN_18) @[MemoryDispatch.scala 88:51]
    node _GEN_25 = mux(_T_5, UInt<1>("h0"), _GEN_19) @[MemoryDispatch.scala 88:51 MemoryPacket.scala 16:16]
    node _T_16 = eq(UInt<1>("h0"), io_data_width) @[MemoryDispatch.scala 124:25]
    node data_out = _GEN_24 @[MemoryDispatch.scala 40:22]
    node _high_bit_T = bits(data_out, 7, 7) @[MemoryDispatch.scala 126:57]
    node _high_bit_T_1 = mux(io_unsigned, UInt<1>("h0"), _high_bit_T) @[MemoryDispatch.scala 126:32]
    node _high_bit_T_2 = bits(_high_bit_T_1, 0, 0) @[Bitwise.scala 77:15]
    node high_bit = mux(_high_bit_T_2, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
    node _T_17 = bits(read_ins_addr, 1, 0) @[MemoryDispatch.scala 127:27]
    node _T_18 = eq(UInt<1>("h0"), _T_17) @[MemoryDispatch.scala 127:35]
    node _io_data_out_T = bits(data_out, 7, 0) @[MemoryDispatch.scala 129:48]
    node _io_data_out_T_1 = cat(high_bit, _io_data_out_T) @[Cat.scala 33:92]
    node _T_19 = eq(UInt<1>("h1"), _T_17) @[MemoryDispatch.scala 127:35]
    node _io_data_out_T_2 = bits(data_out, 15, 8) @[MemoryDispatch.scala 132:48]
    node _io_data_out_T_3 = cat(high_bit, _io_data_out_T_2) @[Cat.scala 33:92]
    node _T_20 = eq(UInt<2>("h2"), _T_17) @[MemoryDispatch.scala 127:35]
    node _io_data_out_T_4 = bits(data_out, 23, 16) @[MemoryDispatch.scala 135:48]
    node _io_data_out_T_5 = cat(high_bit, _io_data_out_T_4) @[Cat.scala 33:92]
    node _T_21 = eq(UInt<2>("h3"), _T_17) @[MemoryDispatch.scala 127:35]
    node _io_data_out_T_6 = bits(data_out, 31, 24) @[MemoryDispatch.scala 138:48]
    node _io_data_out_T_7 = cat(high_bit, _io_data_out_T_6) @[Cat.scala 33:92]
    node _GEN_26 = validif(_T_21, _io_data_out_T_7) @[MemoryDispatch.scala 127:35 138:23]
    node _GEN_27 = mux(_T_20, _io_data_out_T_5, _GEN_26) @[MemoryDispatch.scala 127:35 135:23]
    node _GEN_28 = mux(_T_19, _io_data_out_T_3, _GEN_27) @[MemoryDispatch.scala 127:35 132:23]
    node _GEN_29 = mux(_T_18, _io_data_out_T_1, _GEN_28) @[MemoryDispatch.scala 127:35 129:23]
    node _T_22 = eq(UInt<1>("h1"), io_data_width) @[MemoryDispatch.scala 124:25]
    node _high_bit_T_3 = bits(data_out, 15, 15) @[MemoryDispatch.scala 144:57]
    node _high_bit_T_4 = mux(io_unsigned, UInt<1>("h0"), _high_bit_T_3) @[MemoryDispatch.scala 144:32]
    node _high_bit_T_5 = bits(_high_bit_T_4, 0, 0) @[Bitwise.scala 77:15]
    node high_bit_1 = mux(_high_bit_T_5, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 77:12]
    node _T_23 = bits(read_ins_addr, 1, 0) @[MemoryDispatch.scala 145:27]
    node _T_24 = eq(UInt<1>("h0"), _T_23) @[MemoryDispatch.scala 145:35]
    node _io_data_out_T_8 = bits(data_out, 15, 0) @[MemoryDispatch.scala 147:48]
    node _io_data_out_T_9 = cat(high_bit_1, _io_data_out_T_8) @[Cat.scala 33:92]
    node _T_25 = eq(UInt<2>("h2"), _T_23) @[MemoryDispatch.scala 145:35]
    node _io_data_out_T_10 = bits(data_out, 31, 16) @[MemoryDispatch.scala 150:48]
    node _io_data_out_T_11 = cat(high_bit_1, _io_data_out_T_10) @[Cat.scala 33:92]
    node _GEN_30 = validif(_T_25, _io_data_out_T_11) @[MemoryDispatch.scala 145:35 150:23]
    node _GEN_31 = mux(_T_24, _io_data_out_T_9, _GEN_30) @[MemoryDispatch.scala 145:35 147:23]
    node _T_26 = eq(UInt<2>("h2"), io_data_width) @[MemoryDispatch.scala 124:25]
    node _GEN_32 = validif(_T_26, data_out) @[MemoryDispatch.scala 124:25 155:19]
    node _GEN_33 = mux(_T_22, _GEN_31, _GEN_32) @[MemoryDispatch.scala 124:25]
    node _GEN_34 = mux(_T_16, _GEN_29, _GEN_33) @[MemoryDispatch.scala 124:25]
    node data_in = _GEN_2 @[MemoryDispatch.scala 39:21]
    io_ins_out <= insRAM.io_read_data @[MemoryPacket.scala 21:16]
    io_data_out <= _GEN_34
    insRAM.clock <= clock
    insRAM.reset <= reset
    insRAM.io_write <= _GEN_20
    insRAM.io_read_addr <= pad(read_ins_addr, 32) @[MemoryPacket.scala 17:20]
    insRAM.io_write_addr <= pad(rw_mem_addr, 32) @[MemoryPacket.scala 18:21]
    insRAM.io_write_data <= data_in @[MemoryPacket.scala 19:21]
    dataRAM.clock <= clock
    dataRAM.reset <= reset
    dataRAM.io_write <= _GEN_22
    dataRAM.io_read_addr <= pad(_GEN_23, 32)
    dataRAM.io_write_addr <= pad(_GEN_21, 32)
    dataRAM.io_write_data <= data_in @[MemoryPacket.scala 19:21]
    outRegisters.clock <= clock
    outRegisters.reset <= reset
    outRegisters.io_write <= _GEN_25
    outRegisters.io_read_addr <= pad(rw_mem_addr, 32) @[MemoryPacket.scala 17:20]
    outRegisters.io_write_addr <= pad(rw_mem_addr, 32) @[MemoryPacket.scala 18:21]
    outRegisters.io_write_data <= data_in @[MemoryPacket.scala 19:21]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), _T_5), eq(io_write_data, UInt<1>("h0"))), io_read_data), _T_7), UInt<1>("h1")), "Cant read insRAM") : printf @[MemoryDispatch.scala 92:13]
    printf(clock, and(and(and(and(and(UInt<1>("h1"), eq(_T_5, UInt<1>("h0"))), eq(_T_10, UInt<1>("h0"))), eq(_T_13, UInt<1>("h0"))), _T_15), UInt<1>("h1")), "Unexpected address!") : printf_1 @[MemoryDispatch.scala 119:11]
