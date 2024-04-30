circuit MemoryDispatch :
  module RAM :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}

    cmem mem : UInt<32> [65536] @[MemoryPacket.scala 32:18]
    when io.write : @[MemoryPacket.scala 33:20]
      node _T = bits(io.write_addr, 15, 0)
      write mport MPORT = mem[_T], clock
      MPORT <= io.write_data
    node _io_read_data_T = bits(io.read_addr, 15, 0) @[MemoryPacket.scala 36:29]
    read mport io_read_data_MPORT = mem[_io_read_data_T], clock @[MemoryPacket.scala 36:29]
    io.read_data <= io_read_data_MPORT @[MemoryPacket.scala 36:18]

  module InsRAM :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}

    inst insRAM of RAM @[InsRAM.scala 12:30]
    insRAM.clock <= clock
    insRAM.reset <= reset
    io.read_data <= insRAM.io.read_data @[InsRAM.scala 17:6]
    insRAM.io.write_data <= io.write_data @[InsRAM.scala 17:6]
    insRAM.io.write_addr <= io.write_addr @[InsRAM.scala 17:6]
    insRAM.io.read_addr <= io.read_addr @[InsRAM.scala 17:6]
    insRAM.io.write <= io.write @[InsRAM.scala 17:6]

  module RAM_1 :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}

    cmem mem : UInt<32> [65536] @[MemoryPacket.scala 32:18]
    when io.write : @[MemoryPacket.scala 33:20]
      node _T = bits(io.write_addr, 15, 0)
      write mport MPORT = mem[_T], clock
      MPORT <= io.write_data
    node _io_read_data_T = bits(io.read_addr, 15, 0) @[MemoryPacket.scala 36:29]
    read mport io_read_data_MPORT = mem[_io_read_data_T], clock @[MemoryPacket.scala 36:29]
    io.read_data <= io_read_data_MPORT @[MemoryPacket.scala 36:18]

  module DataRAM :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}

    inst dataRAM of RAM_1 @[DataRAM.scala 11:31]
    dataRAM.clock <= clock
    dataRAM.reset <= reset
    io.read_data <= dataRAM.io.read_data @[DataRAM.scala 16:7]
    dataRAM.io.write_data <= io.write_data @[DataRAM.scala 16:7]
    dataRAM.io.write_addr <= io.write_addr @[DataRAM.scala 16:7]
    dataRAM.io.read_addr <= io.read_addr @[DataRAM.scala 16:7]
    dataRAM.io.write <= io.write @[DataRAM.scala 16:7]

  module OutRegisters :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}

    io.read_data is invalid @[OutRegisters.scala 9:15]

  module MemoryDispatch :
    input clock : Clock
    input reset : UInt<1>
    output io : { flip cpu_state : UInt<1>, flip ins_addr : UInt<32>, ins_out : UInt<32>, flip read_data : UInt<1>, flip write_data : UInt<1>, flip unsigned : UInt<1>, flip data_width : UInt<2>, flip data_addr : UInt<32>, flip data_write : UInt<32>, data_out : UInt<32>}

    node rw_mem_addr = shr(io.data_addr, 2) @[MemoryDispatch.scala 30:34]
    node read_ins_addr = shr(io.ins_addr, 2) @[MemoryDispatch.scala 31:35]
    inst insRAM of InsRAM @[MemoryDispatch.scala 34:22]
    insRAM.clock <= clock
    insRAM.reset <= reset
    inst dataRAM of DataRAM @[MemoryDispatch.scala 35:23]
    dataRAM.clock <= clock
    dataRAM.reset <= reset
    inst outRegisters of OutRegisters @[MemoryDispatch.scala 36:28]
    outRegisters.clock <= clock
    outRegisters.reset <= reset
    wire data_in : UInt<32> @[MemoryDispatch.scala 39:21]
    wire data_out : UInt<32> @[MemoryDispatch.scala 40:22]
    node is_write_clk = eq(io.cpu_state, UInt<1>("h1")) @[MemoryDispatch.scala 43:35]
    data_in is invalid @[MemoryDispatch.scala 47:10]
    node _T = eq(UInt<1>("h0"), io.data_width) @[MemoryDispatch.scala 48:25]
    when _T : @[MemoryDispatch.scala 48:25]
      node _data_in_T = bits(io.data_write, 7, 0) @[MemoryDispatch.scala 50:31]
      data_in <= _data_in_T @[MemoryDispatch.scala 50:15]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.data_width) @[MemoryDispatch.scala 48:25]
      when _T_1 : @[MemoryDispatch.scala 48:25]
        node _data_in_T_1 = bits(io.data_write, 15, 0) @[MemoryDispatch.scala 53:31]
        data_in <= _data_in_T_1 @[MemoryDispatch.scala 53:15]
      else :
        node _T_2 = eq(UInt<2>("h2"), io.data_width) @[MemoryDispatch.scala 48:25]
        when _T_2 : @[MemoryDispatch.scala 48:25]
          data_in <= io.data_write @[MemoryDispatch.scala 56:15]
    insRAM.io.write <= UInt<1>("h0") @[MemoryPacket.scala 16:16]
    insRAM.io.read_addr <= read_ins_addr @[MemoryPacket.scala 17:20]
    insRAM.io.write_addr <= rw_mem_addr @[MemoryPacket.scala 18:21]
    insRAM.io.write_data <= data_in @[MemoryPacket.scala 19:21]
    io.ins_out <= insRAM.io.read_data @[MemoryPacket.scala 21:16]
    dataRAM.io.write <= UInt<1>("h0") @[MemoryPacket.scala 16:16]
    dataRAM.io.read_addr <= rw_mem_addr @[MemoryPacket.scala 17:20]
    dataRAM.io.write_addr <= rw_mem_addr @[MemoryPacket.scala 18:21]
    dataRAM.io.write_data <= data_in @[MemoryPacket.scala 19:21]
    outRegisters.io.write <= UInt<1>("h0") @[MemoryPacket.scala 16:16]
    outRegisters.io.read_addr <= rw_mem_addr @[MemoryPacket.scala 17:20]
    outRegisters.io.write_addr <= rw_mem_addr @[MemoryPacket.scala 18:21]
    outRegisters.io.write_data <= data_in @[MemoryPacket.scala 19:21]
    data_out is invalid @[MemoryDispatch.scala 86:11]
    node _T_3 = leq(UInt<1>("h0"), io.data_addr) @[MemoryDispatch.scala 87:36]
    node _T_4 = leq(io.data_addr, UInt<16>("hffff")) @[MemoryDispatch.scala 88:21]
    node _T_5 = and(_T_3, _T_4) @[MemoryDispatch.scala 88:5]
    when _T_5 : @[MemoryDispatch.scala 88:51]
      when io.write_data : @[MemoryDispatch.scala 89:25]
        node _insRAM_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 90:39]
        insRAM.io.write <= _insRAM_io_write_T @[MemoryDispatch.scala 90:23]
      else :
        when io.read_data : @[MemoryDispatch.scala 91:30]
          node _T_6 = bits(reset, 0, 0) @[MemoryDispatch.scala 92:13]
          node _T_7 = eq(_T_6, UInt<1>("h0")) @[MemoryDispatch.scala 92:13]
          when _T_7 : @[MemoryDispatch.scala 92:13]
            printf(clock, UInt<1>("h1"), "Cant read insRAM") : printf @[MemoryDispatch.scala 92:13]
        else :
          skip
    else :
      node _T_8 = leq(UInt<17>("h10000"), io.data_addr) @[MemoryDispatch.scala 96:43]
      node _T_9 = leq(io.data_addr, UInt<17>("h1ffff")) @[MemoryDispatch.scala 97:21]
      node _T_10 = and(_T_8, _T_9) @[MemoryDispatch.scala 97:5]
      when _T_10 : @[MemoryDispatch.scala 97:52]
        node _havard_mem_T = sub(io.data_addr, UInt<17>("h10000")) @[MemoryDispatch.scala 99:36]
        node _havard_mem_T_1 = tail(_havard_mem_T, 1) @[MemoryDispatch.scala 99:36]
        node havard_mem = shr(_havard_mem_T_1, 2) @[MemoryDispatch.scala 99:68]
        when io.write_data : @[MemoryDispatch.scala 100:25]
          dataRAM.io.write_addr <= havard_mem @[MemoryDispatch.scala 101:29]
          node _dataRAM_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 102:40]
          dataRAM.io.write <= _dataRAM_io_write_T @[MemoryDispatch.scala 102:24]
        else :
          when io.read_data : @[MemoryDispatch.scala 103:30]
            dataRAM.io.read_addr <= havard_mem @[MemoryDispatch.scala 104:28]
            data_out <= dataRAM.io.read_data @[MemoryDispatch.scala 105:16]
          else :
            skip
      else :
        node _T_11 = leq(UInt<32>("hffffff00"), io.data_addr) @[MemoryDispatch.scala 109:51]
        node _T_12 = leq(io.data_addr, UInt<32>("hffffffff")) @[MemoryDispatch.scala 110:21]
        node _T_13 = and(_T_11, _T_12) @[MemoryDispatch.scala 110:5]
        when _T_13 : @[MemoryDispatch.scala 110:60]
          when io.write_data : @[MemoryDispatch.scala 111:25]
            node _outRegisters_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 112:45]
            outRegisters.io.write <= _outRegisters_io_write_T @[MemoryDispatch.scala 112:29]
          else :
            when io.read_data : @[MemoryDispatch.scala 113:30]
              data_out <= outRegisters.io.read_data @[MemoryDispatch.scala 114:16]
            else :
              skip
        else :
          node _T_14 = bits(reset, 0, 0) @[MemoryDispatch.scala 119:11]
          node _T_15 = eq(_T_14, UInt<1>("h0")) @[MemoryDispatch.scala 119:11]
          when _T_15 : @[MemoryDispatch.scala 119:11]
            printf(clock, UInt<1>("h1"), "Unexpected address!") : printf_1 @[MemoryDispatch.scala 119:11]
    io.data_out is invalid @[MemoryDispatch.scala 123:14]
    node _T_16 = eq(UInt<1>("h0"), io.data_width) @[MemoryDispatch.scala 124:25]
    when _T_16 : @[MemoryDispatch.scala 124:25]
      node _high_bit_T = bits(data_out, 7, 7) @[MemoryDispatch.scala 126:57]
      node _high_bit_T_1 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T) @[MemoryDispatch.scala 126:32]
      node _high_bit_T_2 = bits(_high_bit_T_1, 0, 0) @[Bitwise.scala 77:15]
      node high_bit = mux(_high_bit_T_2, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
      node _T_17 = bits(read_ins_addr, 1, 0) @[MemoryDispatch.scala 127:27]
      node _T_18 = eq(UInt<1>("h0"), _T_17) @[MemoryDispatch.scala 127:35]
      when _T_18 : @[MemoryDispatch.scala 127:35]
        node _io_data_out_T = bits(data_out, 7, 0) @[MemoryDispatch.scala 129:48]
        node _io_data_out_T_1 = cat(high_bit, _io_data_out_T) @[Cat.scala 33:92]
        io.data_out <= _io_data_out_T_1 @[MemoryDispatch.scala 129:23]
      else :
        node _T_19 = eq(UInt<1>("h1"), _T_17) @[MemoryDispatch.scala 127:35]
        when _T_19 : @[MemoryDispatch.scala 127:35]
          node _io_data_out_T_2 = bits(data_out, 15, 8) @[MemoryDispatch.scala 132:48]
          node _io_data_out_T_3 = cat(high_bit, _io_data_out_T_2) @[Cat.scala 33:92]
          io.data_out <= _io_data_out_T_3 @[MemoryDispatch.scala 132:23]
        else :
          node _T_20 = eq(UInt<2>("h2"), _T_17) @[MemoryDispatch.scala 127:35]
          when _T_20 : @[MemoryDispatch.scala 127:35]
            node _io_data_out_T_4 = bits(data_out, 23, 16) @[MemoryDispatch.scala 135:48]
            node _io_data_out_T_5 = cat(high_bit, _io_data_out_T_4) @[Cat.scala 33:92]
            io.data_out <= _io_data_out_T_5 @[MemoryDispatch.scala 135:23]
          else :
            node _T_21 = eq(UInt<2>("h3"), _T_17) @[MemoryDispatch.scala 127:35]
            when _T_21 : @[MemoryDispatch.scala 127:35]
              node _io_data_out_T_6 = bits(data_out, 31, 24) @[MemoryDispatch.scala 138:48]
              node _io_data_out_T_7 = cat(high_bit, _io_data_out_T_6) @[Cat.scala 33:92]
              io.data_out <= _io_data_out_T_7 @[MemoryDispatch.scala 138:23]
    else :
      node _T_22 = eq(UInt<1>("h1"), io.data_width) @[MemoryDispatch.scala 124:25]
      when _T_22 : @[MemoryDispatch.scala 124:25]
        node _high_bit_T_3 = bits(data_out, 15, 15) @[MemoryDispatch.scala 144:57]
        node _high_bit_T_4 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_3) @[MemoryDispatch.scala 144:32]
        node _high_bit_T_5 = bits(_high_bit_T_4, 0, 0) @[Bitwise.scala 77:15]
        node high_bit_1 = mux(_high_bit_T_5, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 77:12]
        node _T_23 = bits(read_ins_addr, 1, 0) @[MemoryDispatch.scala 145:27]
        node _T_24 = eq(UInt<1>("h0"), _T_23) @[MemoryDispatch.scala 145:35]
        when _T_24 : @[MemoryDispatch.scala 145:35]
          node _io_data_out_T_8 = bits(data_out, 15, 0) @[MemoryDispatch.scala 147:48]
          node _io_data_out_T_9 = cat(high_bit_1, _io_data_out_T_8) @[Cat.scala 33:92]
          io.data_out <= _io_data_out_T_9 @[MemoryDispatch.scala 147:23]
        else :
          node _T_25 = eq(UInt<2>("h2"), _T_23) @[MemoryDispatch.scala 145:35]
          when _T_25 : @[MemoryDispatch.scala 145:35]
            node _io_data_out_T_10 = bits(data_out, 31, 16) @[MemoryDispatch.scala 150:48]
            node _io_data_out_T_11 = cat(high_bit_1, _io_data_out_T_10) @[Cat.scala 33:92]
            io.data_out <= _io_data_out_T_11 @[MemoryDispatch.scala 150:23]
      else :
        node _T_26 = eq(UInt<2>("h2"), io.data_width) @[MemoryDispatch.scala 124:25]
        when _T_26 : @[MemoryDispatch.scala 124:25]
          io.data_out <= data_out @[MemoryDispatch.scala 155:19]

