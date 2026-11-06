circuit Top :
  extmodule clk_wiz_0 :
    input clk_in1 : Clock
    input reset : UInt<1>
    output clk_out1 : Clock
    output clk_out2 : Clock
    defname = clk_wiz_0

  module ClockSeparator :
    input clock : Clock
    input reset : Reset
    output io : { cpuClock : Clock, uartClock : Clock}

    io.uartClock is invalid @[ClockSeparator.scala 12:16]
    inst ip_clock of clk_wiz_0 @[ClockSeparator.scala 14:26]
    ip_clock.clk_out2 is invalid
    ip_clock.clk_out1 is invalid
    ip_clock.reset is invalid
    ip_clock.clk_in1 is invalid
    ip_clock.clk_in1 <= clock @[ClockSeparator.scala 15:25]
    ip_clock.reset <= reset @[ClockSeparator.scala 16:22]
    io.cpuClock <= ip_clock.clk_out2 @[ClockSeparator.scala 17:17]

  extmodule ins_mem :
    input clk : Clock
    input a : UInt<14>
    input d : UInt<32>
    input dpra : UInt<14>
    input we : UInt<1>
    output spo : UInt<32>
    output dpo : UInt<32>
    defname = ins_mem

  module DualPortRAM :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<14>, flip write_addr : UInt<14>, flip write_data : UInt<32>, read_data : UInt<32>}
    output io2 : { flip read_addr : UInt<14>, read_data : UInt<32>}

    inst ip_mem of ins_mem @[MemoryPacket.scala 63:24]
    ip_mem.dpo is invalid
    ip_mem.spo is invalid
    ip_mem.we is invalid
    ip_mem.dpra is invalid
    ip_mem.d is invalid
    ip_mem.a is invalid
    ip_mem.clk is invalid
    ip_mem.clk <= clock @[MemoryPacket.scala 64:19]
    node _ip_mem_io_a_T = mux(io.write, io.write_addr, io2.read_addr) @[MemoryPacket.scala 65:23]
    ip_mem.a <= _ip_mem_io_a_T @[MemoryPacket.scala 65:17]
    ip_mem.d <= io.write_data @[MemoryPacket.scala 66:17]
    ip_mem.dpra <= io2.read_addr @[MemoryPacket.scala 67:20]
    ip_mem.we <= io.write @[MemoryPacket.scala 68:18]
    io.read_data <= ip_mem.spo @[MemoryPacket.scala 69:18]
    io2.read_data <= ip_mem.dpo @[MemoryPacket.scala 70:19]

  module InsRAM :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}
    output io2 : { flip read_addr : UInt<32>, read_data : UInt<32>}

    inst insRAM of DualPortRAM @[InsRAM.scala 17:22]
    insRAM.clock <= clock
    insRAM.reset <= reset
    io.read_data <= insRAM.io.read_data @[InsRAM.scala 24:6]
    insRAM.io.write_data <= io.write_data @[InsRAM.scala 24:6]
    insRAM.io.write_addr <= io.write_addr @[InsRAM.scala 24:6]
    insRAM.io.read_addr <= io.read_addr @[InsRAM.scala 24:6]
    insRAM.io.write <= io.write @[InsRAM.scala 24:6]
    io2.read_data <= insRAM.io2.read_data @[InsRAM.scala 25:7]
    insRAM.io2.read_addr <= io2.read_addr @[InsRAM.scala 25:7]

  extmodule data_mem :
    input clk : Clock
    input a : UInt<32>
    input d : UInt<32>
    input we : UInt<1>
    output spo : UInt<32>
    defname = data_mem

  module RAM :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<14>, flip write_addr : UInt<14>, flip write_data : UInt<32>, read_data : UInt<32>}

    inst ip_ram of data_mem @[MemoryPacket.scala 31:24]
    ip_ram.spo is invalid
    ip_ram.we is invalid
    ip_ram.d is invalid
    ip_ram.a is invalid
    ip_ram.clk is invalid
    ip_ram.clk <= clock @[MemoryPacket.scala 32:19]
    node _ip_ram_io_a_T = mux(io.write, io.write_addr, io.read_addr) @[MemoryPacket.scala 33:23]
    ip_ram.a <= _ip_ram_io_a_T @[MemoryPacket.scala 33:17]
    ip_ram.d <= io.write_data @[MemoryPacket.scala 34:17]
    ip_ram.we <= io.write @[MemoryPacket.scala 35:18]
    io.read_data <= ip_ram.spo @[MemoryPacket.scala 36:18]

  module DataRAM :
    input clock : Clock
    input reset : Reset
    output io : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}

    inst dataRAM of RAM @[DataRAM.scala 12:23]
    dataRAM.clock <= clock
    dataRAM.reset <= reset
    io.read_data <= dataRAM.io.read_data @[DataRAM.scala 18:7]
    dataRAM.io.write_data <= io.write_data @[DataRAM.scala 18:7]
    dataRAM.io.write_addr <= io.write_addr @[DataRAM.scala 18:7]
    dataRAM.io.read_addr <= io.read_addr @[DataRAM.scala 18:7]
    dataRAM.io.write <= io.write @[DataRAM.scala 18:7]

  module OutRegisters :
    input clock : Clock
    input reset : Reset
    output io : { mem : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}, flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}}}

    io.external.uart.txData is invalid @[OutRegisters.scala 18:27]
    io.external.uart.txStart <= UInt<1>("h0") @[OutRegisters.scala 19:28]
    io.external.uart.rxReady <= UInt<1>("h0") @[OutRegisters.scala 20:28]
    reg btn : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 24:20]
    btn <= io.external.btn.button @[OutRegisters.scala 25:7]
    reg switches : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 27:25]
    switches <= io.external.switches.switches @[OutRegisters.scala 28:12]
    reg led : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 31:20]
    io.external.led.led <= led @[OutRegisters.scala 32:23]
    reg seg7 : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 34:21]
    io.external.seg7.seg7 <= seg7 @[OutRegisters.scala 35:25]
    node addr = mux(io.mem.write, io.mem.write_addr, io.mem.read_addr) @[OutRegisters.scala 38:23]
    io.mem.read_data is invalid @[OutRegisters.scala 39:20]
    node _T = shr(UInt<32>("hffffff04"), 2) @[OutRegisters.scala 41:44]
    node _T_1 = eq(addr, _T) @[OutRegisters.scala 41:13]
    when _T_1 : @[OutRegisters.scala 41:58]
      when io.mem.write : @[OutRegisters.scala 42:24]
        node _T_2 = asUInt(reset) @[OutRegisters.scala 43:13]
        node _T_3 = eq(_T_2, UInt<1>("h0")) @[OutRegisters.scala 43:13]
        when _T_3 : @[OutRegisters.scala 43:13]
          printf(clock, UInt<1>("h1"), "btn cannot write") : printf @[OutRegisters.scala 43:13]
      else :
        io.mem.read_data <= btn @[OutRegisters.scala 45:24]
    else :
      node _T_4 = shr(UInt<32>("hffffff08"), 2) @[OutRegisters.scala 47:53]
      node _T_5 = eq(addr, _T_4) @[OutRegisters.scala 47:19]
      when _T_5 : @[OutRegisters.scala 47:67]
        when io.mem.write : @[OutRegisters.scala 48:24]
          node _T_6 = asUInt(reset) @[OutRegisters.scala 49:13]
          node _T_7 = eq(_T_6, UInt<1>("h0")) @[OutRegisters.scala 49:13]
          when _T_7 : @[OutRegisters.scala 49:13]
            printf(clock, UInt<1>("h1"), "switches cannot write") : printf_1 @[OutRegisters.scala 49:13]
        else :
          io.mem.read_data <= switches @[OutRegisters.scala 51:24]
      else :
        node _T_8 = shr(UInt<32>("hffffff00"), 2) @[OutRegisters.scala 53:50]
        node _T_9 = eq(addr, _T_8) @[OutRegisters.scala 53:19]
        when _T_9 : @[OutRegisters.scala 53:64]
          when io.mem.write : @[OutRegisters.scala 54:24]
            led <= io.mem.write_data @[OutRegisters.scala 55:11]
          else :
            io.mem.read_data <= led @[OutRegisters.scala 57:24]
        else :
          node _T_10 = shr(UInt<32>("hffffff0c"), 2) @[OutRegisters.scala 59:51]
          node _T_11 = eq(addr, _T_10) @[OutRegisters.scala 59:19]
          when _T_11 : @[OutRegisters.scala 59:65]
            when io.mem.write : @[OutRegisters.scala 60:24]
              seg7 <= io.mem.write_data @[OutRegisters.scala 61:12]
            else :
              io.mem.read_data <= seg7 @[OutRegisters.scala 63:24]
          else :
            skip

  module MemoryDispatch :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip ins_addr : UInt<32>, ins_out : UInt<32>, flip read_data : UInt<1>, flip write_data : UInt<1>, flip unsigned : UInt<1>, flip data_width : UInt<2>, flip data_addr : UInt<32>, flip data_write : UInt<32>, data_out : UInt<32>, flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}}}

    node rw_mem_addr = shr(io.data_addr, 2) @[MemoryDispatch.scala 32:34]
    node read_ins_addr = shr(io.ins_addr, 2) @[MemoryDispatch.scala 33:35]
    inst insRAM of InsRAM @[MemoryDispatch.scala 36:22]
    insRAM.clock <= clock
    insRAM.reset <= reset
    inst dataRAM of DataRAM @[MemoryDispatch.scala 37:23]
    dataRAM.clock <= clock
    dataRAM.reset <= reset
    inst outRegisters of OutRegisters @[MemoryDispatch.scala 38:28]
    outRegisters.clock <= clock
    outRegisters.reset <= reset
    wire data_in : UInt<32> @[MemoryDispatch.scala 41:21]
    wire data_out : UInt<32> @[MemoryDispatch.scala 42:22]
    node _is_write_clk_T = eq(io.cpu_state, UInt<1>("h1")) @[MemoryDispatch.scala 45:35]
    node _is_write_clk_T_1 = eq(io.cpu_state, UInt<2>("h2")) @[MemoryDispatch.scala 45:87]
    node is_write_clk = or(_is_write_clk_T, _is_write_clk_T_1) @[MemoryDispatch.scala 45:71]
    insRAM.io2.read_addr <= read_ins_addr @[MemoryDispatch.scala 48:24]
    io.ins_out <= insRAM.io2.read_data @[MemoryDispatch.scala 49:14]
    insRAM.io.write <= UInt<1>("h0") @[MemoryPacket.scala 17:16]
    insRAM.io.read_addr <= rw_mem_addr @[MemoryPacket.scala 18:20]
    insRAM.io.write_addr <= rw_mem_addr @[MemoryPacket.scala 19:21]
    insRAM.io.write_data <= data_in @[MemoryPacket.scala 20:21]
    dataRAM.io.write <= UInt<1>("h0") @[MemoryPacket.scala 17:16]
    dataRAM.io.read_addr <= rw_mem_addr @[MemoryPacket.scala 18:20]
    dataRAM.io.write_addr <= rw_mem_addr @[MemoryPacket.scala 19:21]
    dataRAM.io.write_data <= data_in @[MemoryPacket.scala 20:21]
    outRegisters.io.mem.write <= UInt<1>("h0") @[MemoryPacket.scala 17:16]
    outRegisters.io.mem.read_addr <= rw_mem_addr @[MemoryPacket.scala 18:20]
    outRegisters.io.mem.write_addr <= rw_mem_addr @[MemoryPacket.scala 19:21]
    outRegisters.io.mem.write_data <= data_in @[MemoryPacket.scala 20:21]
    outRegisters.io.external <= io.external @[MemoryDispatch.scala 73:28]
    data_out is invalid @[MemoryDispatch.scala 77:12]
    node _T = leq(UInt<1>("h0"), io.data_addr) @[MemoryDispatch.scala 78:29]
    node _T_1 = leq(io.data_addr, UInt<16>("hffff")) @[MemoryDispatch.scala 79:21]
    node _T_2 = and(_T, _T_1) @[MemoryDispatch.scala 79:5]
    when _T_2 : @[MemoryDispatch.scala 79:44]
      data_out <= insRAM.io.read_data @[MemoryDispatch.scala 80:14]
      when io.write_data : @[MemoryDispatch.scala 81:25]
        node _insRAM_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 82:39]
        insRAM.io.write <= _insRAM_io_write_T @[MemoryDispatch.scala 82:23]
      else :
        when io.read_data : @[MemoryDispatch.scala 83:30]
          skip
        else :
          skip
    else :
      node _T_3 = leq(UInt<17>("h10000"), io.data_addr) @[MemoryDispatch.scala 88:36]
      node _T_4 = leq(io.data_addr, UInt<17>("h1ffff")) @[MemoryDispatch.scala 89:21]
      node _T_5 = and(_T_3, _T_4) @[MemoryDispatch.scala 89:5]
      when _T_5 : @[MemoryDispatch.scala 89:45]
        node _havard_mem_T = sub(io.data_addr, UInt<17>("h10000")) @[MemoryDispatch.scala 91:36]
        node _havard_mem_T_1 = tail(_havard_mem_T, 1) @[MemoryDispatch.scala 91:36]
        node havard_mem = shr(_havard_mem_T_1, 2) @[MemoryDispatch.scala 91:61]
        dataRAM.io.read_addr <= havard_mem @[MemoryDispatch.scala 92:26]
        dataRAM.io.write_addr <= havard_mem @[MemoryDispatch.scala 93:27]
        data_out <= dataRAM.io.read_data @[MemoryDispatch.scala 94:14]
        when io.write_data : @[MemoryDispatch.scala 95:25]
          node _dataRAM_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 96:40]
          dataRAM.io.write <= _dataRAM_io_write_T @[MemoryDispatch.scala 96:24]
        else :
          when io.read_data : @[MemoryDispatch.scala 97:30]
            skip
          else :
            skip
      else :
        node _T_6 = leq(UInt<32>("hffffff00"), io.data_addr) @[MemoryDispatch.scala 101:38]
        node _T_7 = leq(io.data_addr, UInt<32>("hffffffff")) @[MemoryDispatch.scala 102:21]
        node _T_8 = and(_T_6, _T_7) @[MemoryDispatch.scala 102:5]
        when _T_8 : @[MemoryDispatch.scala 102:47]
          data_out <= outRegisters.io.mem.read_data @[MemoryDispatch.scala 103:14]
          when io.write_data : @[MemoryDispatch.scala 104:25]
            node _outRegisters_io_mem_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 105:49]
            outRegisters.io.mem.write <= _outRegisters_io_mem_write_T @[MemoryDispatch.scala 105:33]
          else :
            when io.read_data : @[MemoryDispatch.scala 106:30]
              skip
            else :
              skip
        else :
          node _T_9 = asUInt(reset) @[MemoryDispatch.scala 114:11]
          node _T_10 = eq(_T_9, UInt<1>("h0")) @[MemoryDispatch.scala 114:11]
          when _T_10 : @[MemoryDispatch.scala 114:11]
            printf(clock, UInt<1>("h1"), "Unexpected address!") : printf @[MemoryDispatch.scala 114:11]
    data_in is invalid @[MemoryDispatch.scala 117:11]
    node _T_11 = eq(UInt<1>("h0"), io.data_width) @[MemoryDispatch.scala 118:25]
    when _T_11 : @[MemoryDispatch.scala 118:25]
      node value = bits(io.data_write, 7, 0) @[MemoryDispatch.scala 120:32]
      node _T_12 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 121:26]
      node _T_13 = eq(UInt<1>("h0"), _T_12) @[MemoryDispatch.scala 121:34]
      when _T_13 : @[MemoryDispatch.scala 121:34]
        node _data_in_T = bits(data_out, 31, 8) @[MemoryDispatch.scala 123:34]
        node _data_in_T_1 = cat(_data_in_T, value) @[Cat.scala 33:92]
        data_in <= _data_in_T_1 @[MemoryDispatch.scala 123:19]
      else :
        node _T_14 = eq(UInt<1>("h1"), _T_12) @[MemoryDispatch.scala 121:34]
        when _T_14 : @[MemoryDispatch.scala 121:34]
          node _data_in_T_2 = bits(data_out, 31, 16) @[MemoryDispatch.scala 126:34]
          node _data_in_T_3 = bits(data_out, 7, 0) @[MemoryDispatch.scala 126:59]
          node data_in_hi = cat(_data_in_T_2, value) @[Cat.scala 33:92]
          node _data_in_T_4 = cat(data_in_hi, _data_in_T_3) @[Cat.scala 33:92]
          data_in <= _data_in_T_4 @[MemoryDispatch.scala 126:19]
        else :
          node _T_15 = eq(UInt<2>("h2"), _T_12) @[MemoryDispatch.scala 121:34]
          when _T_15 : @[MemoryDispatch.scala 121:34]
            node _data_in_T_5 = bits(data_out, 31, 24) @[MemoryDispatch.scala 129:34]
            node _data_in_T_6 = bits(data_out, 15, 0) @[MemoryDispatch.scala 129:59]
            node data_in_hi_1 = cat(_data_in_T_5, value) @[Cat.scala 33:92]
            node _data_in_T_7 = cat(data_in_hi_1, _data_in_T_6) @[Cat.scala 33:92]
            data_in <= _data_in_T_7 @[MemoryDispatch.scala 129:19]
          else :
            node _T_16 = eq(UInt<2>("h3"), _T_12) @[MemoryDispatch.scala 121:34]
            when _T_16 : @[MemoryDispatch.scala 121:34]
              node _data_in_T_8 = bits(data_out, 23, 0) @[MemoryDispatch.scala 132:41]
              node _data_in_T_9 = cat(value, _data_in_T_8) @[Cat.scala 33:92]
              data_in <= _data_in_T_9 @[MemoryDispatch.scala 132:19]
    else :
      node _T_17 = eq(UInt<1>("h1"), io.data_width) @[MemoryDispatch.scala 118:25]
      when _T_17 : @[MemoryDispatch.scala 118:25]
        node value_1 = bits(io.data_write, 15, 0) @[MemoryDispatch.scala 137:32]
        node _T_18 = bits(io.data_addr, 1, 1) @[MemoryDispatch.scala 138:24]
        node _T_19 = eq(_T_18, UInt<1>("h0")) @[MemoryDispatch.scala 138:28]
        when _T_19 : @[MemoryDispatch.scala 138:40]
          node _data_in_T_10 = bits(data_out, 31, 16) @[MemoryDispatch.scala 139:32]
          node _data_in_T_11 = cat(_data_in_T_10, value_1) @[Cat.scala 33:92]
          data_in <= _data_in_T_11 @[MemoryDispatch.scala 139:17]
        else :
          node _data_in_T_12 = bits(data_out, 15, 0) @[MemoryDispatch.scala 141:39]
          node _data_in_T_13 = cat(value_1, _data_in_T_12) @[Cat.scala 33:92]
          data_in <= _data_in_T_13 @[MemoryDispatch.scala 141:17]
      else :
        node _T_20 = eq(UInt<2>("h2"), io.data_width) @[MemoryDispatch.scala 118:25]
        when _T_20 : @[MemoryDispatch.scala 118:25]
          data_in <= io.data_write @[MemoryDispatch.scala 145:15]
    io.data_out is invalid @[MemoryDispatch.scala 150:15]
    node _T_21 = eq(UInt<1>("h0"), io.data_width) @[MemoryDispatch.scala 151:25]
    when _T_21 : @[MemoryDispatch.scala 151:25]
      node _T_22 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 153:26]
      node _T_23 = eq(UInt<1>("h0"), _T_22) @[MemoryDispatch.scala 153:34]
      when _T_23 : @[MemoryDispatch.scala 153:34]
        node _high_bit_T = bits(data_out, 7, 7) @[MemoryDispatch.scala 155:65]
        node _high_bit_T_1 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T) @[MemoryDispatch.scala 155:38]
        node _high_bit_T_2 = bits(_high_bit_T_1, 0, 0) @[Bitwise.scala 77:15]
        node high_bit = mux(_high_bit_T_2, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
        node _io_data_out_T = bits(data_out, 7, 0) @[MemoryDispatch.scala 156:48]
        node _io_data_out_T_1 = cat(high_bit, _io_data_out_T) @[Cat.scala 33:92]
        io.data_out <= _io_data_out_T_1 @[MemoryDispatch.scala 156:23]
      else :
        node _T_24 = eq(UInt<1>("h1"), _T_22) @[MemoryDispatch.scala 153:34]
        when _T_24 : @[MemoryDispatch.scala 153:34]
          node _high_bit_T_3 = bits(data_out, 15, 15) @[MemoryDispatch.scala 159:65]
          node _high_bit_T_4 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_3) @[MemoryDispatch.scala 159:38]
          node _high_bit_T_5 = bits(_high_bit_T_4, 0, 0) @[Bitwise.scala 77:15]
          node high_bit_1 = mux(_high_bit_T_5, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
          node _io_data_out_T_2 = bits(data_out, 15, 8) @[MemoryDispatch.scala 160:48]
          node _io_data_out_T_3 = cat(high_bit_1, _io_data_out_T_2) @[Cat.scala 33:92]
          io.data_out <= _io_data_out_T_3 @[MemoryDispatch.scala 160:23]
        else :
          node _T_25 = eq(UInt<2>("h2"), _T_22) @[MemoryDispatch.scala 153:34]
          when _T_25 : @[MemoryDispatch.scala 153:34]
            node _high_bit_T_6 = bits(data_out, 23, 23) @[MemoryDispatch.scala 163:65]
            node _high_bit_T_7 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_6) @[MemoryDispatch.scala 163:38]
            node _high_bit_T_8 = bits(_high_bit_T_7, 0, 0) @[Bitwise.scala 77:15]
            node high_bit_2 = mux(_high_bit_T_8, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
            node _io_data_out_T_4 = bits(data_out, 23, 16) @[MemoryDispatch.scala 164:48]
            node _io_data_out_T_5 = cat(high_bit_2, _io_data_out_T_4) @[Cat.scala 33:92]
            io.data_out <= _io_data_out_T_5 @[MemoryDispatch.scala 164:23]
          else :
            node _T_26 = eq(UInt<2>("h3"), _T_22) @[MemoryDispatch.scala 153:34]
            when _T_26 : @[MemoryDispatch.scala 153:34]
              node _high_bit_T_9 = bits(data_out, 31, 31) @[MemoryDispatch.scala 167:65]
              node _high_bit_T_10 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_9) @[MemoryDispatch.scala 167:38]
              node _high_bit_T_11 = bits(_high_bit_T_10, 0, 0) @[Bitwise.scala 77:15]
              node high_bit_3 = mux(_high_bit_T_11, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
              node _io_data_out_T_6 = bits(data_out, 31, 24) @[MemoryDispatch.scala 168:48]
              node _io_data_out_T_7 = cat(high_bit_3, _io_data_out_T_6) @[Cat.scala 33:92]
              io.data_out <= _io_data_out_T_7 @[MemoryDispatch.scala 168:23]
    else :
      node _T_27 = eq(UInt<1>("h1"), io.data_width) @[MemoryDispatch.scala 151:25]
      when _T_27 : @[MemoryDispatch.scala 151:25]
        node _T_28 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 174:26]
        node _T_29 = eq(UInt<1>("h0"), _T_28) @[MemoryDispatch.scala 174:34]
        when _T_29 : @[MemoryDispatch.scala 174:34]
          node _high_bit_T_12 = bits(data_out, 15, 15) @[MemoryDispatch.scala 176:65]
          node _high_bit_T_13 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_12) @[MemoryDispatch.scala 176:38]
          node _high_bit_T_14 = bits(_high_bit_T_13, 0, 0) @[Bitwise.scala 77:15]
          node high_bit_4 = mux(_high_bit_T_14, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 77:12]
          node _io_data_out_T_8 = bits(data_out, 15, 0) @[MemoryDispatch.scala 177:48]
          node _io_data_out_T_9 = cat(high_bit_4, _io_data_out_T_8) @[Cat.scala 33:92]
          io.data_out <= _io_data_out_T_9 @[MemoryDispatch.scala 177:23]
        else :
          node _T_30 = eq(UInt<2>("h2"), _T_28) @[MemoryDispatch.scala 174:34]
          when _T_30 : @[MemoryDispatch.scala 174:34]
            node _high_bit_T_15 = bits(data_out, 31, 31) @[MemoryDispatch.scala 180:65]
            node _high_bit_T_16 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_15) @[MemoryDispatch.scala 180:38]
            node _high_bit_T_17 = bits(_high_bit_T_16, 0, 0) @[Bitwise.scala 77:15]
            node high_bit_5 = mux(_high_bit_T_17, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 77:12]
            node _io_data_out_T_10 = bits(data_out, 31, 16) @[MemoryDispatch.scala 181:48]
            node _io_data_out_T_11 = cat(high_bit_5, _io_data_out_T_10) @[Cat.scala 33:92]
            io.data_out <= _io_data_out_T_11 @[MemoryDispatch.scala 181:23]
      else :
        node _T_31 = eq(UInt<2>("h2"), io.data_width) @[MemoryDispatch.scala 151:25]
        when _T_31 : @[MemoryDispatch.scala 151:25]
          io.data_out <= data_out @[MemoryDispatch.scala 186:19]

  module UARTLoader :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip rxValid : UInt<1>, flip rxData : UInt<8>, rxReady : UInt<1>, mem : { mem_read : UInt<1>, mem_write : UInt<1>, data_to_write : UInt<32>, data_addr : UInt<32>, data_width : UInt<2>}}

    reg cur_state : UInt<2>, clock with :
      reset => (reset, UInt<2>("h0")) @[UARTLoader.scala 34:26]
    reg data_addr : UInt<32>, clock with :
      reset => (reset, UInt<32>("hffffffff")) @[UARTLoader.scala 35:26]
    io.mem.data_addr <= data_addr @[UARTLoader.scala 38:20]
    io.mem.data_width <= UInt<1>("h0") @[UARTLoader.scala 39:21]
    io.mem.data_to_write <= io.rxData @[UARTLoader.scala 40:24]
    io.mem.mem_read <= UInt<1>("h0") @[UARTLoader.scala 41:19]
    io.mem.mem_write <= UInt<1>("h0") @[UARTLoader.scala 42:20]
    reg rxReadyReg : UInt<1>, clock with :
      reset => (reset, UInt<1>("h0")) @[UARTLoader.scala 44:27]
    io.rxReady <= rxReadyReg @[UARTLoader.scala 45:13]
    node _T = eq(UInt<2>("h0"), cur_state) @[UARTLoader.scala 47:21]
    when _T : @[UARTLoader.scala 47:21]
      rxReadyReg <= UInt<1>("h0") @[UARTLoader.scala 49:17]
      node _T_1 = eq(io.cpu_state, UInt<2>("h2")) @[UARTLoader.scala 50:25]
      when _T_1 : @[UARTLoader.scala 50:61]
        data_addr <= UInt<32>("hffffffff") @[UARTLoader.scala 51:19]
        cur_state <= UInt<2>("h1") @[UARTLoader.scala 52:19]
    else :
      node _T_2 = eq(UInt<2>("h1"), cur_state) @[UARTLoader.scala 47:21]
      when _T_2 : @[UARTLoader.scala 47:21]
        rxReadyReg <= UInt<1>("h0") @[UARTLoader.scala 56:17]
        node _T_3 = neq(io.cpu_state, UInt<2>("h2")) @[UARTLoader.scala 57:25]
        when _T_3 : @[UARTLoader.scala 57:61]
          cur_state <= UInt<2>("h0") @[UARTLoader.scala 58:19]
        else :
          node _T_4 = eq(io.rxValid, UInt<1>("h0")) @[UARTLoader.scala 60:25]
          when _T_4 : @[UARTLoader.scala 60:38]
            cur_state <= UInt<2>("h3") @[UARTLoader.scala 61:21]
      else :
        node _T_5 = eq(UInt<2>("h3"), cur_state) @[UARTLoader.scala 47:21]
        when _T_5 : @[UARTLoader.scala 47:21]
          rxReadyReg <= UInt<1>("h0") @[UARTLoader.scala 66:17]
          node _T_6 = neq(io.cpu_state, UInt<2>("h2")) @[UARTLoader.scala 67:25]
          when _T_6 : @[UARTLoader.scala 67:61]
            cur_state <= UInt<2>("h0") @[UARTLoader.scala 68:19]
          else :
            node _T_7 = eq(io.rxValid, UInt<1>("h1")) @[UARTLoader.scala 70:25]
            when _T_7 : @[UARTLoader.scala 70:37]
              node _data_addr_T = add(data_addr, UInt<1>("h1")) @[UARTLoader.scala 71:34]
              node _data_addr_T_1 = tail(_data_addr_T, 1) @[UARTLoader.scala 71:34]
              data_addr <= _data_addr_T_1 @[UARTLoader.scala 71:21]
              cur_state <= UInt<2>("h2") @[UARTLoader.scala 72:21]
        else :
          node _T_8 = eq(UInt<2>("h2"), cur_state) @[UARTLoader.scala 47:21]
          when _T_8 : @[UARTLoader.scala 47:21]
            rxReadyReg <= UInt<1>("h0") @[UARTLoader.scala 77:17]
            node _T_9 = neq(io.cpu_state, UInt<2>("h2")) @[UARTLoader.scala 78:25]
            when _T_9 : @[UARTLoader.scala 78:61]
              cur_state <= UInt<2>("h0") @[UARTLoader.scala 79:19]
            else :
              node _T_10 = eq(io.rxValid, UInt<1>("h1")) @[UARTLoader.scala 81:25]
              when _T_10 : @[UARTLoader.scala 81:37]
                io.mem.mem_write <= UInt<1>("h1") @[UARTLoader.scala 82:28]
                rxReadyReg <= UInt<1>("h1") @[UARTLoader.scala 83:21]
              else :
                cur_state <= UInt<2>("h3") @[UARTLoader.scala 85:21]

  module MemWriteSelector :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip uart_in : { mem_read : UInt<1>, mem_write : UInt<1>, data_to_write : UInt<32>, data_addr : UInt<32>, data_width : UInt<2>}, flip cpu_read_data : UInt<1>, flip cpu_write_data : UInt<1>, flip cpu_data_width : UInt<2>, flip cpu_data_addr : UInt<32>, flip cpu_data_write : UInt<32>, read_data : UInt<1>, write_data : UInt<1>, data_width : UInt<2>, data_addr : UInt<32>, data_write : UInt<32>}

    node _T = eq(io.cpu_state, UInt<2>("h2")) @[MemWriteSelector.scala 29:20]
    when _T : @[MemWriteSelector.scala 29:54]
      io.read_data <= io.uart_in.mem_read @[MemWriteSelector.scala 30:17]
      io.write_data <= io.uart_in.mem_write @[MemWriteSelector.scala 31:18]
      io.data_width <= io.uart_in.data_width @[MemWriteSelector.scala 32:18]
      io.data_addr <= io.uart_in.data_addr @[MemWriteSelector.scala 33:17]
      io.data_write <= io.uart_in.data_to_write @[MemWriteSelector.scala 34:18]
    else :
      io.read_data <= io.cpu_read_data @[MemWriteSelector.scala 36:17]
      io.write_data <= io.cpu_write_data @[MemWriteSelector.scala 37:18]
      io.data_width <= io.cpu_data_width @[MemWriteSelector.scala 38:18]
      io.data_addr <= io.cpu_data_addr @[MemWriteSelector.scala 39:17]
      io.data_write <= io.cpu_data_write @[MemWriteSelector.scala 40:18]

  module CPUState :
    input clock : Clock
    input reset : Reset
    output io : { cpu_state : UInt<2>, flip load_mode : UInt<1>}

    reg state : UInt, clock with :
      reset => (reset, UInt<1>("h1")) @[CPUState.scala 13:22]
    node _T = eq(io.load_mode, UInt<1>("h0")) @[CPUState.scala 15:8]
    when _T : @[CPUState.scala 15:22]
      io.cpu_state <= state @[CPUState.scala 16:18]
      node _T_1 = eq(state, UInt<2>("h2")) @[CPUState.scala 17:15]
      when _T_1 : @[CPUState.scala 17:49]
        state <= UInt<1>("h1") @[CPUState.scala 18:12]
      else :
        node _T_2 = eq(state, UInt<1>("h1")) @[CPUState.scala 19:22]
        when _T_2 : @[CPUState.scala 19:59]
          state <= UInt<1>("h0") @[CPUState.scala 20:13]
        else :
          state <= UInt<1>("h1") @[CPUState.scala 22:13]
    else :
      io.cpu_state <= UInt<2>("h2") @[CPUState.scala 25:17]
      state <= UInt<2>("h2") @[CPUState.scala 26:10]

  module PC :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip next_addr : UInt<32>, addr : UInt<32>}

    reg pc : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[PC.scala 15:17]
    io.addr <= pc @[PC.scala 16:10]
    node _T = eq(io.cpu_state, UInt<1>("h0")) @[PC.scala 17:20]
    when _T : @[PC.scala 17:53]
      pc <= io.next_addr @[PC.scala 18:7]
    else :
      skip

  module InstructionDecoder :
    input clock : Clock
    input reset : Reset
    output io : { flip instruction : UInt<32>, opcode : UInt<7>, rs1 : UInt<5>, rs2 : UInt<5>, rd : UInt<5>, func3 : UInt<3>, func7 : UInt<7>, raw_imm : UInt<20>}

    node _io_opcode_T = bits(io.instruction, 6, 0) @[InstructionDecoder.scala 17:30]
    io.opcode <= _io_opcode_T @[InstructionDecoder.scala 17:13]
    node _io_rd_T = bits(io.instruction, 11, 7) @[InstructionDecoder.scala 18:26]
    io.rd <= _io_rd_T @[InstructionDecoder.scala 18:9]
    node _io_func3_T = bits(io.instruction, 14, 12) @[InstructionDecoder.scala 19:29]
    io.func3 <= _io_func3_T @[InstructionDecoder.scala 19:12]
    node _io_rs1_T = bits(io.instruction, 19, 15) @[InstructionDecoder.scala 20:27]
    io.rs1 <= _io_rs1_T @[InstructionDecoder.scala 20:10]
    node _io_rs2_T = bits(io.instruction, 24, 20) @[InstructionDecoder.scala 21:27]
    io.rs2 <= _io_rs2_T @[InstructionDecoder.scala 21:10]
    node _io_func7_T = bits(io.instruction, 31, 25) @[InstructionDecoder.scala 22:29]
    io.func7 <= _io_func7_T @[InstructionDecoder.scala 22:12]
    io.raw_imm <= UInt<1>("h0") @[InstructionDecoder.scala 24:14]
    node _T = bits(io.opcode, 6, 0) @[InstructionDecoder.scala 26:19]
    node _T_1 = eq(UInt<6>("h33"), _T) @[InstructionDecoder.scala 26:27]
    when _T_1 : @[InstructionDecoder.scala 26:27]
      io.raw_imm <= UInt<1>("h0") @[InstructionDecoder.scala 28:18]
    else :
      node _T_2 = eq(UInt<5>("h13"), _T) @[InstructionDecoder.scala 26:27]
      when _T_2 : @[InstructionDecoder.scala 26:27]
        node _io_raw_imm_T = bits(io.instruction, 31, 20) @[InstructionDecoder.scala 31:49]
        node _io_raw_imm_T_1 = cat(UInt<8>("h0"), _io_raw_imm_T) @[Cat.scala 33:92]
        io.raw_imm <= _io_raw_imm_T_1 @[InstructionDecoder.scala 31:18]
      else :
        node _T_3 = eq(UInt<2>("h3"), _T) @[InstructionDecoder.scala 26:27]
        when _T_3 : @[InstructionDecoder.scala 26:27]
          node _io_raw_imm_T_2 = bits(io.instruction, 31, 20) @[InstructionDecoder.scala 34:49]
          node _io_raw_imm_T_3 = cat(UInt<8>("h0"), _io_raw_imm_T_2) @[Cat.scala 33:92]
          io.raw_imm <= _io_raw_imm_T_3 @[InstructionDecoder.scala 34:18]
        else :
          node _T_4 = eq(UInt<7>("h67"), _T) @[InstructionDecoder.scala 26:27]
          when _T_4 : @[InstructionDecoder.scala 26:27]
            node _io_raw_imm_T_4 = bits(io.instruction, 31, 20) @[InstructionDecoder.scala 37:49]
            node _io_raw_imm_T_5 = cat(UInt<8>("h0"), _io_raw_imm_T_4) @[Cat.scala 33:92]
            io.raw_imm <= _io_raw_imm_T_5 @[InstructionDecoder.scala 37:18]
          else :
            node _T_5 = eq(UInt<7>("h73"), _T) @[InstructionDecoder.scala 26:27]
            when _T_5 : @[InstructionDecoder.scala 26:27]
              io.raw_imm <= io.func7 @[InstructionDecoder.scala 40:18]
            else :
              node _T_6 = eq(UInt<6>("h23"), _T) @[InstructionDecoder.scala 26:27]
              when _T_6 : @[InstructionDecoder.scala 26:27]
                node _io_raw_imm_T_6 = bits(io.instruction, 31, 25) @[InstructionDecoder.scala 43:39]
                node _io_raw_imm_T_7 = bits(io.instruction, 11, 7) @[InstructionDecoder.scala 43:63]
                node _io_raw_imm_T_8 = cat(_io_raw_imm_T_6, _io_raw_imm_T_7) @[Cat.scala 33:92]
                io.raw_imm <= _io_raw_imm_T_8 @[InstructionDecoder.scala 43:18]
              else :
                node _T_7 = eq(UInt<7>("h63"), _T) @[InstructionDecoder.scala 26:27]
                when _T_7 : @[InstructionDecoder.scala 26:27]
                  node _io_raw_imm_T_9 = bits(io.instruction, 31, 31) @[InstructionDecoder.scala 46:49]
                  node _io_raw_imm_T_10 = bits(io.instruction, 7, 7) @[InstructionDecoder.scala 46:73]
                  node _io_raw_imm_T_11 = bits(io.instruction, 30, 25) @[InstructionDecoder.scala 47:23]
                  node _io_raw_imm_T_12 = bits(io.instruction, 11, 8) @[InstructionDecoder.scala 47:47]
                  node io_raw_imm_lo = cat(_io_raw_imm_T_11, _io_raw_imm_T_12) @[Cat.scala 33:92]
                  node io_raw_imm_hi_hi = cat(UInt<8>("h0"), _io_raw_imm_T_9) @[Cat.scala 33:92]
                  node io_raw_imm_hi = cat(io_raw_imm_hi_hi, _io_raw_imm_T_10) @[Cat.scala 33:92]
                  node _io_raw_imm_T_13 = cat(io_raw_imm_hi, io_raw_imm_lo) @[Cat.scala 33:92]
                  io.raw_imm <= _io_raw_imm_T_13 @[InstructionDecoder.scala 46:18]
                else :
                  node _T_8 = eq(UInt<6>("h37"), _T) @[InstructionDecoder.scala 26:27]
                  when _T_8 : @[InstructionDecoder.scala 26:27]
                    node _io_raw_imm_T_14 = bits(io.instruction, 31, 12) @[InstructionDecoder.scala 50:35]
                    io.raw_imm <= _io_raw_imm_T_14 @[InstructionDecoder.scala 50:18]
                  else :
                    node _T_9 = eq(UInt<5>("h17"), _T) @[InstructionDecoder.scala 26:27]
                    when _T_9 : @[InstructionDecoder.scala 26:27]
                      node _io_raw_imm_T_15 = bits(io.instruction, 31, 12) @[InstructionDecoder.scala 53:35]
                      io.raw_imm <= _io_raw_imm_T_15 @[InstructionDecoder.scala 53:18]
                    else :
                      node _T_10 = eq(UInt<7>("h6f"), _T) @[InstructionDecoder.scala 26:27]
                      when _T_10 : @[InstructionDecoder.scala 26:27]
                        node _io_raw_imm_T_16 = bits(io.instruction, 31, 31) @[InstructionDecoder.scala 56:39]
                        node _io_raw_imm_T_17 = bits(io.instruction, 19, 12) @[InstructionDecoder.scala 56:63]
                        node _io_raw_imm_T_18 = bits(io.instruction, 20, 20) @[InstructionDecoder.scala 57:23]
                        node _io_raw_imm_T_19 = bits(io.instruction, 30, 21) @[InstructionDecoder.scala 57:47]
                        node io_raw_imm_lo_1 = cat(_io_raw_imm_T_18, _io_raw_imm_T_19) @[Cat.scala 33:92]
                        node io_raw_imm_hi_1 = cat(_io_raw_imm_T_16, _io_raw_imm_T_17) @[Cat.scala 33:92]
                        node _io_raw_imm_T_20 = cat(io_raw_imm_hi_1, io_raw_imm_lo_1) @[Cat.scala 33:92]
                        io.raw_imm <= _io_raw_imm_T_20 @[InstructionDecoder.scala 56:18]

  module ControlUnit :
    input clock : Clock
    input reset : Reset
    output io : { flip opcode : UInt<7>, flip func3 : UInt<3>, flip func7 : UInt<7>, flip rs1 : UInt<5>, flip rs2 : UInt<5>, flip rd : UInt<5>, flip raw_imm : UInt<20>, rs1_out : UInt<5>, rs2_out : UInt<5>, rd_out : UInt<5>, raw_imm_out : UInt<20>, alu_type : UInt<3>, cmp_type : UInt<2>, unsigned : UInt<1>, nextPC_type : UInt<2>, regs_write : UInt<1>, imm_width_type : UInt<2>, operand2_type : UInt<1>, au_type : UInt<1>, write_back_type : UInt<3>, memory_read : UInt<1>, memory_write : UInt<1>, data_width : UInt<2>}

    io.rs1_out <= io.rs1 @[ControlUnit.scala 37:14]
    io.rs2_out <= io.rs2 @[ControlUnit.scala 38:14]
    io.rd_out <= io.rd @[ControlUnit.scala 39:13]
    io.raw_imm_out <= io.raw_imm @[ControlUnit.scala 40:18]
    io.alu_type is invalid @[ControlUnit.scala 42:15]
    io.cmp_type is invalid @[ControlUnit.scala 43:15]
    io.unsigned is invalid @[ControlUnit.scala 44:15]
    io.nextPC_type is invalid @[ControlUnit.scala 45:18]
    io.regs_write is invalid @[ControlUnit.scala 46:17]
    io.imm_width_type is invalid @[ControlUnit.scala 47:21]
    io.operand2_type is invalid @[ControlUnit.scala 48:20]
    io.au_type is invalid @[ControlUnit.scala 49:14]
    io.write_back_type is invalid @[ControlUnit.scala 50:22]
    io.memory_read is invalid @[ControlUnit.scala 51:18]
    io.memory_write is invalid @[ControlUnit.scala 52:19]
    io.data_width is invalid @[ControlUnit.scala 53:17]
    node _T = eq(UInt<6>("h33"), io.opcode) @[ControlUnit.scala 55:21]
    when _T : @[ControlUnit.scala 55:21]
      io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 57:22]
      io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 58:21]
      io.au_type <= UInt<1>("h0") @[ControlUnit.scala 59:18]
      io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 60:22]
      io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 61:23]
      io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 62:24]
      io.write_back_type <= UInt<1>("h0") @[ControlUnit.scala 63:26]
      io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 64:19]
      node _T_1 = cat(io.func3, io.func7) @[Cat.scala 33:92]
      node _T_2 = eq(UInt<1>("h0"), _T_1) @[ControlUnit.scala 65:39]
      when _T_2 : @[ControlUnit.scala 65:39]
        io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 67:23]
      else :
        node _T_3 = eq(UInt<6>("h20"), _T_1) @[ControlUnit.scala 65:39]
        when _T_3 : @[ControlUnit.scala 65:39]
          io.alu_type <= UInt<1>("h1") @[ControlUnit.scala 70:23]
        else :
          node _T_4 = eq(UInt<10>("h200"), _T_1) @[ControlUnit.scala 65:39]
          when _T_4 : @[ControlUnit.scala 65:39]
            io.alu_type <= UInt<2>("h2") @[ControlUnit.scala 73:23]
          else :
            node _T_5 = eq(UInt<10>("h300"), _T_1) @[ControlUnit.scala 65:39]
            when _T_5 : @[ControlUnit.scala 65:39]
              io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 76:23]
            else :
              node _T_6 = eq(UInt<10>("h380"), _T_1) @[ControlUnit.scala 65:39]
              when _T_6 : @[ControlUnit.scala 65:39]
                io.alu_type <= UInt<3>("h4") @[ControlUnit.scala 79:23]
              else :
                node _T_7 = eq(UInt<8>("h80"), _T_1) @[ControlUnit.scala 65:39]
                when _T_7 : @[ControlUnit.scala 65:39]
                  io.alu_type <= UInt<3>("h5") @[ControlUnit.scala 82:23]
                else :
                  node _T_8 = eq(UInt<10>("h280"), _T_1) @[ControlUnit.scala 65:39]
                  when _T_8 : @[ControlUnit.scala 65:39]
                    io.alu_type <= UInt<3>("h6") @[ControlUnit.scala 85:23]
                  else :
                    node _T_9 = eq(UInt<10>("h2a0"), _T_1) @[ControlUnit.scala 65:39]
                    when _T_9 : @[ControlUnit.scala 65:39]
                      io.alu_type <= UInt<3>("h7") @[ControlUnit.scala 88:23]
                    else :
                      node _T_10 = eq(UInt<9>("h100"), _T_1) @[ControlUnit.scala 65:39]
                      when _T_10 : @[ControlUnit.scala 65:39]
                        io.au_type <= UInt<1>("h1") @[ControlUnit.scala 91:22]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 92:23]
                      else :
                        node _T_11 = eq(UInt<9>("h180"), _T_1) @[ControlUnit.scala 65:39]
                        when _T_11 : @[ControlUnit.scala 65:39]
                          io.au_type <= UInt<1>("h1") @[ControlUnit.scala 95:22]
                          io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 96:23]
                          io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 97:23]
    else :
      node _T_12 = eq(UInt<5>("h13"), io.opcode) @[ControlUnit.scala 55:21]
      when _T_12 : @[ControlUnit.scala 55:21]
        io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 103:22]
        io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 104:21]
        io.au_type <= UInt<1>("h0") @[ControlUnit.scala 105:18]
        io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 106:22]
        io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 107:23]
        io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 108:24]
        io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 109:25]
        io.write_back_type <= UInt<1>("h0") @[ControlUnit.scala 110:26]
        io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 111:19]
        node _T_13 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 112:24]
        when _T_13 : @[ControlUnit.scala 112:24]
          io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 114:23]
        else :
          node _T_14 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 112:24]
          when _T_14 : @[ControlUnit.scala 112:24]
            io.alu_type <= UInt<2>("h2") @[ControlUnit.scala 117:23]
          else :
            node _T_15 = eq(UInt<3>("h6"), io.func3) @[ControlUnit.scala 112:24]
            when _T_15 : @[ControlUnit.scala 112:24]
              io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 120:23]
            else :
              node _T_16 = eq(UInt<3>("h7"), io.func3) @[ControlUnit.scala 112:24]
              when _T_16 : @[ControlUnit.scala 112:24]
                io.alu_type <= UInt<3>("h4") @[ControlUnit.scala 123:23]
              else :
                node _T_17 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 112:24]
                when _T_17 : @[ControlUnit.scala 112:24]
                  io.alu_type <= UInt<3>("h5") @[ControlUnit.scala 126:23]
                else :
                  node _T_18 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 112:24]
                  when _T_18 : @[ControlUnit.scala 112:24]
                    node _T_19 = eq(UInt<1>("h0"), io.func7) @[ControlUnit.scala 129:28]
                    when _T_19 : @[ControlUnit.scala 129:28]
                      io.alu_type <= UInt<3>("h6") @[ControlUnit.scala 131:27]
                    else :
                      node _T_20 = eq(UInt<6>("h20"), io.func7) @[ControlUnit.scala 129:28]
                      when _T_20 : @[ControlUnit.scala 129:28]
                        io.alu_type <= UInt<3>("h7") @[ControlUnit.scala 134:27]
                  else :
                    node _T_21 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 112:24]
                    when _T_21 : @[ControlUnit.scala 112:24]
                      io.au_type <= UInt<1>("h1") @[ControlUnit.scala 139:22]
                      io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 140:23]
                    else :
                      node _T_22 = eq(UInt<2>("h3"), io.func3) @[ControlUnit.scala 112:24]
                      when _T_22 : @[ControlUnit.scala 112:24]
                        io.au_type <= UInt<1>("h1") @[ControlUnit.scala 143:22]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 144:23]
                        io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 145:23]
      else :
        node _T_23 = eq(UInt<2>("h3"), io.opcode) @[ControlUnit.scala 55:21]
        when _T_23 : @[ControlUnit.scala 55:21]
          io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 151:22]
          io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 152:21]
          io.au_type <= UInt<1>("h0") @[ControlUnit.scala 153:18]
          io.memory_read <= UInt<1>("h1") @[ControlUnit.scala 154:22]
          io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 155:23]
          io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 156:24]
          io.write_back_type <= UInt<1>("h1") @[ControlUnit.scala 157:26]
          io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 158:25]
          io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 159:19]
          io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 160:19]
          node _T_24 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 161:24]
          when _T_24 : @[ControlUnit.scala 161:24]
            io.data_width <= UInt<1>("h0") @[ControlUnit.scala 163:25]
          else :
            node _T_25 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 161:24]
            when _T_25 : @[ControlUnit.scala 161:24]
              io.data_width <= UInt<1>("h1") @[ControlUnit.scala 166:25]
            else :
              node _T_26 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 161:24]
              when _T_26 : @[ControlUnit.scala 161:24]
                io.data_width <= UInt<2>("h2") @[ControlUnit.scala 169:25]
              else :
                node _T_27 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 161:24]
                when _T_27 : @[ControlUnit.scala 161:24]
                  io.data_width <= UInt<1>("h0") @[ControlUnit.scala 172:25]
                  io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 173:23]
                else :
                  node _T_28 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 161:24]
                  when _T_28 : @[ControlUnit.scala 161:24]
                    io.data_width <= UInt<1>("h1") @[ControlUnit.scala 176:25]
                    io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 177:23]
        else :
          node _T_29 = eq(UInt<6>("h23"), io.opcode) @[ControlUnit.scala 55:21]
          when _T_29 : @[ControlUnit.scala 55:21]
            io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 183:22]
            io.regs_write <= UInt<1>("h0") @[ControlUnit.scala 184:21]
            io.au_type <= UInt<1>("h0") @[ControlUnit.scala 185:18]
            io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 186:22]
            io.memory_write <= UInt<1>("h1") @[ControlUnit.scala 187:23]
            io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 188:24]
            io.write_back_type is invalid @[ControlUnit.scala 189:26]
            io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 190:25]
            io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 191:19]
            io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 192:19]
            node _T_30 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 193:24]
            when _T_30 : @[ControlUnit.scala 193:24]
              io.data_width <= UInt<1>("h0") @[ControlUnit.scala 195:25]
            else :
              node _T_31 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 193:24]
              when _T_31 : @[ControlUnit.scala 193:24]
                io.data_width <= UInt<1>("h1") @[ControlUnit.scala 198:25]
              else :
                node _T_32 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 193:24]
                when _T_32 : @[ControlUnit.scala 193:24]
                  io.data_width <= UInt<2>("h2") @[ControlUnit.scala 201:25]
          else :
            node _T_33 = eq(UInt<7>("h63"), io.opcode) @[ControlUnit.scala 55:21]
            when _T_33 : @[ControlUnit.scala 55:21]
              io.nextPC_type <= UInt<1>("h1") @[ControlUnit.scala 207:22]
              io.regs_write <= UInt<1>("h0") @[ControlUnit.scala 208:21]
              io.au_type <= UInt<1>("h1") @[ControlUnit.scala 209:18]
              io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 210:22]
              io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 211:23]
              io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 212:24]
              io.write_back_type is invalid @[ControlUnit.scala 213:26]
              io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 214:19]
              io.imm_width_type <= UInt<1>("h1") @[ControlUnit.scala 215:25]
              io.alu_type is invalid @[ControlUnit.scala 216:19]
              node _T_34 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 217:24]
              when _T_34 : @[ControlUnit.scala 217:24]
                io.cmp_type <= UInt<2>("h2") @[ControlUnit.scala 219:23]
              else :
                node _T_35 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 217:24]
                when _T_35 : @[ControlUnit.scala 217:24]
                  io.cmp_type <= UInt<2>("h3") @[ControlUnit.scala 222:23]
                else :
                  node _T_36 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 217:24]
                  when _T_36 : @[ControlUnit.scala 217:24]
                    io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 225:23]
                  else :
                    node _T_37 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 217:24]
                    when _T_37 : @[ControlUnit.scala 217:24]
                      io.cmp_type <= UInt<1>("h1") @[ControlUnit.scala 228:23]
                    else :
                      node _T_38 = eq(UInt<3>("h6"), io.func3) @[ControlUnit.scala 217:24]
                      when _T_38 : @[ControlUnit.scala 217:24]
                        io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 231:23]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 232:23]
                      else :
                        node _T_39 = eq(UInt<3>("h7"), io.func3) @[ControlUnit.scala 217:24]
                        when _T_39 : @[ControlUnit.scala 217:24]
                          io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 235:23]
                          io.cmp_type <= UInt<1>("h1") @[ControlUnit.scala 236:23]
            else :
              node _T_40 = eq(UInt<7>("h6f"), io.opcode) @[ControlUnit.scala 55:21]
              when _T_40 : @[ControlUnit.scala 55:21]
                io.nextPC_type <= UInt<2>("h2") @[ControlUnit.scala 242:22]
                io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 243:21]
                io.au_type <= UInt<1>("h0") @[ControlUnit.scala 244:18]
                io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 245:22]
                io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 246:23]
                io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 247:24]
                io.write_back_type <= UInt<2>("h2") @[ControlUnit.scala 248:26]
                io.imm_width_type <= UInt<1>("h1") @[ControlUnit.scala 249:25]
                io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 250:19]
                io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 251:19]
              else :
                node _T_41 = eq(UInt<7>("h67"), io.opcode) @[ControlUnit.scala 55:21]
                when _T_41 : @[ControlUnit.scala 55:21]
                  io.nextPC_type <= UInt<2>("h2") @[ControlUnit.scala 255:22]
                  io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 256:21]
                  io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 257:22]
                  io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 258:23]
                  io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 259:24]
                  io.write_back_type <= UInt<2>("h2") @[ControlUnit.scala 260:26]
                  io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 261:25]
                  io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 262:19]
                  io.au_type <= UInt<1>("h0") @[ControlUnit.scala 263:18]
                  io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 264:19]
                else :
                  node _T_42 = eq(UInt<6>("h37"), io.opcode) @[ControlUnit.scala 55:21]
                  when _T_42 : @[ControlUnit.scala 55:21]
                    io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 268:22]
                    io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 269:21]
                    io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 270:22]
                    io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 271:23]
                    io.imm_width_type <= UInt<2>("h3") @[ControlUnit.scala 272:25]
                    io.write_back_type <= UInt<2>("h3") @[ControlUnit.scala 273:26]
                    io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 274:19]
                    io.au_type is invalid @[ControlUnit.scala 275:18]
                    io.alu_type is invalid @[ControlUnit.scala 276:19]
                  else :
                    node _T_43 = eq(UInt<5>("h17"), io.opcode) @[ControlUnit.scala 55:21]
                    when _T_43 : @[ControlUnit.scala 55:21]
                      io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 280:22]
                      io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 281:21]
                      io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 282:22]
                      io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 283:23]
                      io.imm_width_type <= UInt<2>("h3") @[ControlUnit.scala 284:25]
                      io.write_back_type <= UInt<3>("h4") @[ControlUnit.scala 285:26]
                      io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 286:19]
                      io.au_type is invalid @[ControlUnit.scala 287:18]
                      io.alu_type is invalid @[ControlUnit.scala 288:19]

  module Registers :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip write : UInt<1>, flip rs1 : UInt<5>, flip rs2 : UInt<5>, flip rd : UInt<5>, flip write_data : UInt<32>, rs1_val : UInt<32>, rs2_val : UInt<32>}

    wire _regs_WIRE : UInt<32>[32] @[Registers.scala 23:29]
    _regs_WIRE[0] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[1] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[2] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[3] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[4] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[5] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[6] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[7] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[8] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[9] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[10] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[11] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[12] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[13] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[14] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[15] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[16] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[17] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[18] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[19] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[20] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[21] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[22] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[23] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[24] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[25] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[26] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[27] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[28] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[29] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[30] <= UInt<32>("h0") @[Registers.scala 23:29]
    _regs_WIRE[31] <= UInt<32>("h0") @[Registers.scala 23:29]
    reg regs : UInt<32>[32], clock with :
      reset => (reset, _regs_WIRE) @[Registers.scala 23:21]
    io.rs1_val <= regs[io.rs1] @[Registers.scala 24:14]
    io.rs2_val <= regs[io.rs2] @[Registers.scala 25:14]
    node _T = eq(io.cpu_state, UInt<1>("h1")) @[Registers.scala 26:21]
    node _T_1 = and(_T, io.write) @[Registers.scala 27:5]
    node _T_2 = neq(io.rd, UInt<1>("h0")) @[Registers.scala 28:14]
    node _T_3 = and(_T_1, _T_2) @[Registers.scala 28:5]
    when _T_3 : @[Registers.scala 28:23]
      regs[io.rd] <= io.write_data @[Registers.scala 34:17]
    else :
      skip

  module ImmGen :
    input clock : Clock
    input reset : Reset
    output io : { flip raw_imm : UInt<20>, flip unsigned : UInt<1>, flip imm_width : UInt<2>, real_imm : UInt<32>}

    io.real_imm is invalid @[ImmGen.scala 15:15]
    node _T = eq(UInt<1>("h0"), io.imm_width) @[ImmGen.scala 16:23]
    when _T : @[ImmGen.scala 16:23]
      when io.unsigned : @[ImmGen.scala 18:24]
        node _io_real_imm_T = mux(UInt<1>("h0"), UInt<20>("hfffff"), UInt<20>("h0")) @[Bitwise.scala 77:12]
        node _io_real_imm_T_1 = bits(io.raw_imm, 11, 0) @[ImmGen.scala 19:53]
        node _io_real_imm_T_2 = cat(_io_real_imm_T, _io_real_imm_T_1) @[Cat.scala 33:92]
        io.real_imm <= _io_real_imm_T_2 @[ImmGen.scala 19:21]
      else :
        node _io_real_imm_T_3 = bits(io.raw_imm, 11, 11) @[ImmGen.scala 21:47]
        node _io_real_imm_T_4 = bits(_io_real_imm_T_3, 0, 0) @[Bitwise.scala 77:15]
        node _io_real_imm_T_5 = mux(_io_real_imm_T_4, UInt<20>("hfffff"), UInt<20>("h0")) @[Bitwise.scala 77:12]
        node _io_real_imm_T_6 = bits(io.raw_imm, 11, 0) @[ImmGen.scala 21:64]
        node _io_real_imm_T_7 = cat(_io_real_imm_T_5, _io_real_imm_T_6) @[Cat.scala 33:92]
        io.real_imm <= _io_real_imm_T_7 @[ImmGen.scala 21:21]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.imm_width) @[ImmGen.scala 16:23]
      when _T_1 : @[ImmGen.scala 16:23]
        when io.unsigned : @[ImmGen.scala 25:24]
          node _io_real_imm_T_8 = mux(UInt<1>("h0"), UInt<19>("h7ffff"), UInt<19>("h0")) @[Bitwise.scala 77:12]
          node _io_real_imm_T_9 = bits(io.raw_imm, 11, 0) @[ImmGen.scala 26:53]
          node io_real_imm_hi = cat(_io_real_imm_T_8, _io_real_imm_T_9) @[Cat.scala 33:92]
          node _io_real_imm_T_10 = cat(io_real_imm_hi, UInt<1>("h0")) @[Cat.scala 33:92]
          io.real_imm <= _io_real_imm_T_10 @[ImmGen.scala 26:21]
        else :
          node _io_real_imm_T_11 = bits(io.raw_imm, 11, 11) @[ImmGen.scala 28:47]
          node _io_real_imm_T_12 = bits(_io_real_imm_T_11, 0, 0) @[Bitwise.scala 77:15]
          node _io_real_imm_T_13 = mux(_io_real_imm_T_12, UInt<19>("h7ffff"), UInt<19>("h0")) @[Bitwise.scala 77:12]
          node _io_real_imm_T_14 = bits(io.raw_imm, 11, 0) @[ImmGen.scala 28:64]
          node io_real_imm_hi_1 = cat(_io_real_imm_T_13, _io_real_imm_T_14) @[Cat.scala 33:92]
          node _io_real_imm_T_15 = cat(io_real_imm_hi_1, UInt<1>("h0")) @[Cat.scala 33:92]
          io.real_imm <= _io_real_imm_T_15 @[ImmGen.scala 28:21]
      else :
        node _T_2 = eq(UInt<2>("h2"), io.imm_width) @[ImmGen.scala 16:23]
        when _T_2 : @[ImmGen.scala 16:23]
          when io.unsigned : @[ImmGen.scala 32:24]
            node _io_real_imm_T_16 = mux(UInt<1>("h0"), UInt<11>("h7ff"), UInt<11>("h0")) @[Bitwise.scala 77:12]
            node _io_real_imm_T_17 = bits(io.raw_imm, 19, 0) @[ImmGen.scala 33:53]
            node io_real_imm_hi_2 = cat(_io_real_imm_T_16, _io_real_imm_T_17) @[Cat.scala 33:92]
            node _io_real_imm_T_18 = cat(io_real_imm_hi_2, UInt<1>("h0")) @[Cat.scala 33:92]
            io.real_imm <= _io_real_imm_T_18 @[ImmGen.scala 33:21]
          else :
            node _io_real_imm_T_19 = bits(io.raw_imm, 19, 19) @[ImmGen.scala 35:47]
            node _io_real_imm_T_20 = bits(_io_real_imm_T_19, 0, 0) @[Bitwise.scala 77:15]
            node _io_real_imm_T_21 = mux(_io_real_imm_T_20, UInt<11>("h7ff"), UInt<11>("h0")) @[Bitwise.scala 77:12]
            node _io_real_imm_T_22 = bits(io.raw_imm, 19, 0) @[ImmGen.scala 35:64]
            node io_real_imm_hi_3 = cat(_io_real_imm_T_21, _io_real_imm_T_22) @[Cat.scala 33:92]
            node _io_real_imm_T_23 = cat(io_real_imm_hi_3, UInt<1>("h0")) @[Cat.scala 33:92]
            io.real_imm <= _io_real_imm_T_23 @[ImmGen.scala 35:21]
        else :
          node _T_3 = eq(UInt<2>("h3"), io.imm_width) @[ImmGen.scala 16:23]
          when _T_3 : @[ImmGen.scala 16:23]
            node _io_real_imm_T_24 = bits(io.raw_imm, 19, 0) @[ImmGen.scala 39:36]
            node _io_real_imm_T_25 = mux(UInt<1>("h0"), UInt<12>("hfff"), UInt<12>("h0")) @[Bitwise.scala 77:12]
            node _io_real_imm_T_26 = cat(_io_real_imm_T_24, _io_real_imm_T_25) @[Cat.scala 33:92]
            io.real_imm <= _io_real_imm_T_26 @[ImmGen.scala 39:19]

  module OperandSelector :
    input clock : Clock
    input reset : Reset
    output io : { flip rs1_val : UInt<32>, flip rs2_val : UInt<32>, flip real_imm : UInt<32>, flip operand2Type : UInt<1>, operand1 : UInt<32>, operand2 : UInt<32>}

    io.operand1 <= io.rs1_val @[OperandSelector.scala 17:15]
    io.operand2 is invalid @[OperandSelector.scala 18:15]
    node _T = eq(UInt<1>("h0"), io.operand2Type) @[OperandSelector.scala 19:26]
    when _T : @[OperandSelector.scala 19:26]
      io.operand2 <= io.real_imm @[OperandSelector.scala 21:19]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.operand2Type) @[OperandSelector.scala 19:26]
      when _T_1 : @[OperandSelector.scala 19:26]
        io.operand2 <= io.rs2_val @[OperandSelector.scala 24:19]

  module ALU :
    input clock : Clock
    input reset : Reset
    output io : { flip operand1 : UInt<32>, flip operand2 : UInt<32>, flip alu_op : UInt<3>, flip unsigned : UInt<1>, result : UInt<32>}

    io.result is invalid @[ALU.scala 14:13]
    node _T = eq(UInt<1>("h0"), io.alu_op) @[ALU.scala 15:22]
    when _T : @[ALU.scala 15:22]
      node _io_result_T = add(io.operand1, io.operand2) @[ALU.scala 17:32]
      node _io_result_T_1 = tail(_io_result_T, 1) @[ALU.scala 17:32]
      io.result <= _io_result_T_1 @[ALU.scala 17:17]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.alu_op) @[ALU.scala 15:22]
      when _T_1 : @[ALU.scala 15:22]
        node _io_result_T_2 = sub(io.operand1, io.operand2) @[ALU.scala 20:32]
        node _io_result_T_3 = tail(_io_result_T_2, 1) @[ALU.scala 20:32]
        io.result <= _io_result_T_3 @[ALU.scala 20:17]
      else :
        node _T_2 = eq(UInt<2>("h2"), io.alu_op) @[ALU.scala 15:22]
        when _T_2 : @[ALU.scala 15:22]
          node _io_result_T_4 = xor(io.operand1, io.operand2) @[ALU.scala 23:32]
          io.result <= _io_result_T_4 @[ALU.scala 23:17]
        else :
          node _T_3 = eq(UInt<2>("h3"), io.alu_op) @[ALU.scala 15:22]
          when _T_3 : @[ALU.scala 15:22]
            node _io_result_T_5 = or(io.operand1, io.operand2) @[ALU.scala 26:32]
            io.result <= _io_result_T_5 @[ALU.scala 26:17]
          else :
            node _T_4 = eq(UInt<3>("h4"), io.alu_op) @[ALU.scala 15:22]
            when _T_4 : @[ALU.scala 15:22]
              node _io_result_T_6 = and(io.operand1, io.operand2) @[ALU.scala 29:32]
              io.result <= _io_result_T_6 @[ALU.scala 29:17]
            else :
              node _T_5 = eq(UInt<3>("h5"), io.alu_op) @[ALU.scala 15:22]
              when _T_5 : @[ALU.scala 15:22]
                node _io_result_T_7 = bits(io.operand2, 4, 0) @[ALU.scala 32:46]
                node _io_result_T_8 = dshl(io.operand1, _io_result_T_7) @[ALU.scala 32:32]
                io.result <= _io_result_T_8 @[ALU.scala 32:17]
              else :
                node _T_6 = eq(UInt<3>("h7"), io.alu_op) @[ALU.scala 15:22]
                when _T_6 : @[ALU.scala 15:22]
                  node _T_7 = eq(io.unsigned, UInt<1>("h0")) @[ALU.scala 35:12]
                  when _T_7 : @[ALU.scala 35:26]
                    node _io_result_T_9 = asSInt(io.operand1) @[ALU.scala 36:35]
                    node _io_result_T_10 = bits(io.operand2, 4, 0) @[ALU.scala 36:56]
                    node _io_result_T_11 = dshr(_io_result_T_9, _io_result_T_10) @[ALU.scala 36:42]
                    node _io_result_T_12 = asUInt(_io_result_T_11) @[ALU.scala 36:71]
                    io.result <= _io_result_T_12 @[ALU.scala 36:19]
                  else :
                    node _io_result_T_13 = bits(io.operand2, 4, 0) @[ALU.scala 38:48]
                    node _io_result_T_14 = dshr(io.operand1, _io_result_T_13) @[ALU.scala 38:34]
                    io.result <= _io_result_T_14 @[ALU.scala 38:19]
                else :
                  node _T_8 = eq(UInt<3>("h6"), io.alu_op) @[ALU.scala 15:22]
                  when _T_8 : @[ALU.scala 15:22]
                    node _io_result_T_15 = bits(io.operand2, 4, 0) @[ALU.scala 42:47]
                    node _io_result_T_16 = dshr(io.operand1, _io_result_T_15) @[ALU.scala 42:33]
                    io.result <= _io_result_T_16 @[ALU.scala 42:17]

  module CMP :
    input clock : Clock
    input reset : Reset
    output io : { flip operand1 : UInt<32>, flip operand2 : UInt<32>, flip cmp_op : UInt<2>, flip unsigned : UInt<1>, result : UInt<1>}

    io.result is invalid @[CMP.scala 15:13]
    node _T = eq(UInt<1>("h0"), io.cmp_op) @[CMP.scala 16:20]
    when _T : @[CMP.scala 16:20]
      when io.unsigned : @[CMP.scala 18:24]
        node _io_result_T = lt(io.operand1, io.operand2) @[CMP.scala 19:41]
        io.result <= _io_result_T @[CMP.scala 19:19]
      else :
        node _io_result_T_1 = asSInt(io.operand1) @[CMP.scala 21:34]
        node _io_result_T_2 = asSInt(io.operand2) @[CMP.scala 21:55]
        node _io_result_T_3 = lt(_io_result_T_1, _io_result_T_2) @[CMP.scala 21:41]
        io.result <= _io_result_T_3 @[CMP.scala 21:19]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.cmp_op) @[CMP.scala 16:20]
      when _T_1 : @[CMP.scala 16:20]
        when io.unsigned : @[CMP.scala 25:24]
          node _io_result_T_4 = geq(io.operand1, io.operand2) @[CMP.scala 26:41]
          io.result <= _io_result_T_4 @[CMP.scala 26:19]
        else :
          node _io_result_T_5 = asSInt(io.operand1) @[CMP.scala 28:34]
          node _io_result_T_6 = asSInt(io.operand2) @[CMP.scala 28:56]
          node _io_result_T_7 = geq(_io_result_T_5, _io_result_T_6) @[CMP.scala 28:41]
          io.result <= _io_result_T_7 @[CMP.scala 28:19]
      else :
        node _T_2 = eq(UInt<2>("h2"), io.cmp_op) @[CMP.scala 16:20]
        when _T_2 : @[CMP.scala 16:20]
          node _io_result_T_8 = eq(io.operand1, io.operand2) @[CMP.scala 32:32]
          io.result <= _io_result_T_8 @[CMP.scala 32:17]
        else :
          node _T_3 = eq(UInt<2>("h3"), io.cmp_op) @[CMP.scala 16:20]
          when _T_3 : @[CMP.scala 16:20]
            node _io_result_T_9 = neq(io.operand1, io.operand2) @[CMP.scala 35:32]
            io.result <= _io_result_T_9 @[CMP.scala 35:17]

  module NextPCGen :
    input clock : Clock
    input reset : Reset
    output io : { flip nextPC_type : UInt<2>, flip cmp_result : UInt<1>, flip alu_result : UInt<32>, flip imm : UInt<32>, flip pc : UInt<32>, nextPC : UInt<32>, pc4 : UInt<32>, pcImm : UInt<32>}

    node _io_pc4_T = add(io.pc, UInt<3>("h4")) @[NextPCGen.scala 20:19]
    node _io_pc4_T_1 = tail(_io_pc4_T, 1) @[NextPCGen.scala 20:19]
    io.pc4 <= _io_pc4_T_1 @[NextPCGen.scala 20:10]
    io.pcImm <= UInt<1>("h0") @[NextPCGen.scala 21:12]
    io.nextPC is invalid @[NextPCGen.scala 22:13]
    node _T = eq(UInt<1>("h0"), io.nextPC_type) @[NextPCGen.scala 23:25]
    when _T : @[NextPCGen.scala 23:25]
      node _io_nextPC_T = add(io.pc, UInt<3>("h4")) @[NextPCGen.scala 25:26]
      node _io_nextPC_T_1 = tail(_io_nextPC_T, 1) @[NextPCGen.scala 25:26]
      io.nextPC <= _io_nextPC_T_1 @[NextPCGen.scala 25:17]
      node _io_pcImm_T = add(io.pc, io.alu_result) @[NextPCGen.scala 26:25]
      node _io_pcImm_T_1 = tail(_io_pcImm_T, 1) @[NextPCGen.scala 26:25]
      io.pcImm <= _io_pcImm_T_1 @[NextPCGen.scala 26:16]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.nextPC_type) @[NextPCGen.scala 23:25]
      when _T_1 : @[NextPCGen.scala 23:25]
        node _io_nextPC_T_2 = add(io.pc, io.imm) @[NextPCGen.scala 30:45]
        node _io_nextPC_T_3 = tail(_io_nextPC_T_2, 1) @[NextPCGen.scala 30:45]
        node _io_nextPC_T_4 = add(io.pc, UInt<3>("h4")) @[NextPCGen.scala 30:61]
        node _io_nextPC_T_5 = tail(_io_nextPC_T_4, 1) @[NextPCGen.scala 30:61]
        node _io_nextPC_T_6 = mux(io.cmp_result, _io_nextPC_T_3, _io_nextPC_T_5) @[NextPCGen.scala 30:23]
        io.nextPC <= _io_nextPC_T_6 @[NextPCGen.scala 30:17]
        node _io_pcImm_T_2 = add(io.pc, UInt<3>("h4")) @[NextPCGen.scala 31:25]
        node _io_pcImm_T_3 = tail(_io_pcImm_T_2, 1) @[NextPCGen.scala 31:25]
        io.pcImm <= _io_pcImm_T_3 @[NextPCGen.scala 31:16]
      else :
        node _T_2 = eq(UInt<2>("h2"), io.nextPC_type) @[NextPCGen.scala 23:25]
        when _T_2 : @[NextPCGen.scala 23:25]
          io.nextPC <= io.alu_result @[NextPCGen.scala 35:17]
          node _io_pcImm_T_4 = add(io.pc, io.alu_result) @[NextPCGen.scala 36:25]
          node _io_pcImm_T_5 = tail(_io_pcImm_T_4, 1) @[NextPCGen.scala 36:25]
          io.pcImm <= _io_pcImm_T_5 @[NextPCGen.scala 36:16]

  module WriteDataSelector :
    input clock : Clock
    input reset : Reset
    output io : { flip write_back_type : UInt<3>, flip imm : UInt<32>, flip mem_out : UInt<32>, flip au_out : UInt<32>, flip pc4 : UInt<32>, flip pcImm : UInt<32>, write_data : UInt<32>}

    io.write_data is invalid @[WriteDataSelector.scala 18:17]
    node _T = eq(UInt<1>("h0"), io.write_back_type) @[WriteDataSelector.scala 20:30]
    when _T : @[WriteDataSelector.scala 20:30]
      io.write_data <= io.au_out @[WriteDataSelector.scala 22:21]
    else :
      node _T_1 = eq(UInt<2>("h3"), io.write_back_type) @[WriteDataSelector.scala 20:30]
      when _T_1 : @[WriteDataSelector.scala 20:30]
        io.write_data <= io.imm @[WriteDataSelector.scala 25:21]
      else :
        node _T_2 = eq(UInt<1>("h1"), io.write_back_type) @[WriteDataSelector.scala 20:30]
        when _T_2 : @[WriteDataSelector.scala 20:30]
          io.write_data <= io.mem_out @[WriteDataSelector.scala 28:21]
        else :
          node _T_3 = eq(UInt<2>("h2"), io.write_back_type) @[WriteDataSelector.scala 20:30]
          when _T_3 : @[WriteDataSelector.scala 20:30]
            io.write_data <= io.pc4 @[WriteDataSelector.scala 31:21]
          else :
            node _T_4 = eq(UInt<3>("h4"), io.write_back_type) @[WriteDataSelector.scala 20:30]
            when _T_4 : @[WriteDataSelector.scala 20:30]
              io.write_data <= io.pcImm @[WriteDataSelector.scala 34:21]

  module AUSelector :
    input clock : Clock
    input reset : Reset
    output io : { flip au_type : UInt<1>, flip alu_result : UInt<32>, flip cmp_result : UInt<1>, au_out : UInt<32>}

    io.au_out is invalid @[AUSelector.scala 15:13]
    node _T = eq(UInt<1>("h0"), io.au_type) @[AUSelector.scala 17:22]
    when _T : @[AUSelector.scala 17:22]
      io.au_out <= io.alu_result @[AUSelector.scala 19:17]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.au_type) @[AUSelector.scala 17:22]
      when _T_1 : @[AUSelector.scala 17:22]
        io.au_out <= io.cmp_result @[AUSelector.scala 22:17]

  module CoreTop :
    input clock : Clock
    input reset : Reset
    output io : { flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}}, flip external_signal : { load_data_mode : UInt<1>}}

    inst memory of MemoryDispatch @[CoreTop.scala 20:22]
    memory.clock <= clock
    memory.reset <= reset
    inst uartLoader of UARTLoader @[CoreTop.scala 21:26]
    uartLoader.clock <= clock
    uartLoader.reset <= reset
    inst memInSelector of MemWriteSelector @[CoreTop.scala 22:29]
    memInSelector.clock <= clock
    memInSelector.reset <= reset
    inst state of CPUState @[CoreTop.scala 24:21]
    state.clock <= clock
    state.reset <= reset
    inst pc of PC @[CoreTop.scala 27:18]
    pc.clock <= clock
    pc.reset <= reset
    inst insDecode of InstructionDecoder @[CoreTop.scala 30:25]
    insDecode.clock <= clock
    insDecode.reset <= reset
    inst CU of ControlUnit @[CoreTop.scala 31:18]
    CU.clock <= clock
    CU.reset <= reset
    inst regs of Registers @[CoreTop.scala 34:20]
    regs.clock <= clock
    regs.reset <= reset
    inst immGen of ImmGen @[CoreTop.scala 35:22]
    immGen.clock <= clock
    immGen.reset <= reset
    inst operandSelector of OperandSelector @[CoreTop.scala 36:31]
    operandSelector.clock <= clock
    operandSelector.reset <= reset
    inst ALU of ALU @[CoreTop.scala 37:19]
    ALU.clock <= clock
    ALU.reset <= reset
    inst CMP of CMP @[CoreTop.scala 38:19]
    CMP.clock <= clock
    CMP.reset <= reset
    inst nextPCGen of NextPCGen @[CoreTop.scala 39:25]
    nextPCGen.clock <= clock
    nextPCGen.reset <= reset
    uartLoader.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 42:27]
    uartLoader.io.rxValid <= io.external.uart.rxValid @[CoreTop.scala 43:25]
    uartLoader.io.rxData <= io.external.uart.rxData @[CoreTop.scala 44:24]
    io.external.uart.rxReady <= uartLoader.io.rxReady @[CoreTop.scala 45:27]
    memInSelector.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 49:30]
    memInSelector.io.uart_in <= uartLoader.io.mem @[CoreTop.scala 50:28]
    memInSelector.io.cpu_read_data <= CU.io.memory_read @[CoreTop.scala 52:34]
    memInSelector.io.cpu_write_data <= CU.io.memory_write @[CoreTop.scala 53:35]
    memInSelector.io.cpu_data_width <= CU.io.data_width @[CoreTop.scala 54:35]
    memInSelector.io.cpu_data_addr <= ALU.io.result @[CoreTop.scala 55:34]
    memInSelector.io.cpu_data_write <= regs.io.rs2_val @[CoreTop.scala 56:35]
    inst writeDataSelector of WriteDataSelector @[CoreTop.scala 59:33]
    writeDataSelector.clock <= clock
    writeDataSelector.reset <= reset
    inst auSelector of AUSelector @[CoreTop.scala 60:26]
    auSelector.clock <= clock
    auSelector.reset <= reset
    state.io.load_mode <= io.external_signal.load_data_mode @[CoreTop.scala 63:22]
    pc.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 66:19]
    pc.io.next_addr <= nextPCGen.io.nextPC @[CoreTop.scala 67:19]
    memory.io.ins_addr <= pc.io.addr @[CoreTop.scala 68:22]
    insDecode.io.instruction <= memory.io.ins_out @[CoreTop.scala 71:28]
    CU.io.opcode <= insDecode.io.opcode @[CoreTop.scala 72:16]
    CU.io.func3 <= insDecode.io.func3 @[CoreTop.scala 73:15]
    CU.io.func7 <= insDecode.io.func7 @[CoreTop.scala 74:15]
    CU.io.rs1 <= insDecode.io.rs1 @[CoreTop.scala 75:13]
    CU.io.rs2 <= insDecode.io.rs2 @[CoreTop.scala 76:13]
    CU.io.rd <= insDecode.io.rd @[CoreTop.scala 77:12]
    CU.io.raw_imm <= insDecode.io.raw_imm @[CoreTop.scala 78:17]
    regs.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 82:21]
    regs.io.rs1 <= CU.io.rs1_out @[CoreTop.scala 83:15]
    regs.io.rs2 <= CU.io.rs2_out @[CoreTop.scala 84:15]
    regs.io.rd <= CU.io.rd_out @[CoreTop.scala 85:14]
    regs.io.write <= CU.io.regs_write @[CoreTop.scala 86:17]
    regs.io.write_data <= writeDataSelector.io.write_data @[CoreTop.scala 87:22]
    immGen.io.raw_imm <= CU.io.raw_imm_out @[CoreTop.scala 90:21]
    immGen.io.unsigned <= CU.io.unsigned @[CoreTop.scala 91:22]
    immGen.io.imm_width <= CU.io.imm_width_type @[CoreTop.scala 92:23]
    operandSelector.io.rs1_val <= regs.io.rs1_val @[CoreTop.scala 95:30]
    operandSelector.io.rs2_val <= regs.io.rs2_val @[CoreTop.scala 96:30]
    operandSelector.io.real_imm <= immGen.io.real_imm @[CoreTop.scala 97:31]
    operandSelector.io.operand2Type <= CU.io.operand2_type @[CoreTop.scala 98:35]
    ALU.io.operand1 <= operandSelector.io.operand1 @[CoreTop.scala 101:19]
    ALU.io.operand2 <= operandSelector.io.operand2 @[CoreTop.scala 102:19]
    ALU.io.alu_op <= CU.io.alu_type @[CoreTop.scala 103:17]
    ALU.io.unsigned <= CU.io.unsigned @[CoreTop.scala 104:19]
    CMP.io.operand1 <= operandSelector.io.operand1 @[CoreTop.scala 107:19]
    CMP.io.operand2 <= operandSelector.io.operand2 @[CoreTop.scala 108:19]
    CMP.io.cmp_op <= CU.io.cmp_type @[CoreTop.scala 109:17]
    CMP.io.unsigned <= CU.io.unsigned @[CoreTop.scala 110:19]
    nextPCGen.io.cmp_result <= CMP.io.result @[CoreTop.scala 113:27]
    nextPCGen.io.alu_result <= ALU.io.result @[CoreTop.scala 114:27]
    nextPCGen.io.imm <= immGen.io.real_imm @[CoreTop.scala 115:20]
    nextPCGen.io.pc <= pc.io.addr @[CoreTop.scala 116:19]
    nextPCGen.io.nextPC_type <= CU.io.nextPC_type @[CoreTop.scala 117:28]
    memory.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 120:23]
    memory.io.data_addr <= memInSelector.io.data_addr @[CoreTop.scala 121:23]
    memory.io.data_write <= memInSelector.io.data_write @[CoreTop.scala 122:24]
    memory.io.data_width <= memInSelector.io.data_width @[CoreTop.scala 123:24]
    memory.io.write_data <= memInSelector.io.write_data @[CoreTop.scala 124:24]
    memory.io.read_data <= memInSelector.io.read_data @[CoreTop.scala 125:23]
    memory.io.unsigned <= CU.io.unsigned @[CoreTop.scala 126:22]
    memory.io.external <= io.external @[CoreTop.scala 127:22]
    auSelector.io.alu_result <= ALU.io.result @[CoreTop.scala 131:28]
    auSelector.io.cmp_result <= CMP.io.result @[CoreTop.scala 132:28]
    auSelector.io.au_type <= CU.io.au_type @[CoreTop.scala 133:25]
    writeDataSelector.io.imm <= immGen.io.real_imm @[CoreTop.scala 136:28]
    writeDataSelector.io.mem_out <= memory.io.data_out @[CoreTop.scala 137:32]
    writeDataSelector.io.au_out <= auSelector.io.au_out @[CoreTop.scala 138:31]
    writeDataSelector.io.pc4 <= nextPCGen.io.pc4 @[CoreTop.scala 139:28]
    writeDataSelector.io.pcImm <= nextPCGen.io.pcImm @[CoreTop.scala 140:30]
    writeDataSelector.io.write_back_type <= CU.io.write_back_type @[CoreTop.scala 141:40]
    regs.io.write <= CU.io.regs_write @[CoreTop.scala 144:17]
    regs.io.write_data <= writeDataSelector.io.write_data @[CoreTop.scala 145:22]
    node _T = eq(state.io.cpu_state, UInt<2>("h2")) @[CoreTop.scala 147:26]
    when _T : @[CoreTop.scala 147:60]
      io.external.uart.rxReady <= uartLoader.io.rxReady @[CoreTop.scala 150:29]
    node _T_1 = neq(state.io.cpu_state, UInt<2>("h2")) @[CoreTop.scala 161:27]
    node _T_2 = or(_T_1, memInSelector.io.write_data) @[CoreTop.scala 161:62]
    when _T_2 : @[CoreTop.scala 161:94]
      skip

  module Led :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { flip led : UInt<32>}, board : { led : UInt<24>}}

    io.board.led <= io.mmio.led @[Led.scala 20:16]

  module HexToSeg7 :
    input clock : Clock
    input reset : Reset
    output io : { flip hexDigit : UInt<4>, seg7 : UInt<8>}

    wire io_seg7_seg7 : UInt<8> @[Seg7.scala 53:20]
    node _io_seg7_seg7_T = eq(UInt<1>("h1"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_1 = mux(_io_seg7_seg7_T, UInt<8>("h9f"), UInt<2>("h3")) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_2 = eq(UInt<2>("h2"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_3 = mux(_io_seg7_seg7_T_2, UInt<6>("h25"), _io_seg7_seg7_T_1) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_4 = eq(UInt<2>("h3"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_5 = mux(_io_seg7_seg7_T_4, UInt<4>("hd"), _io_seg7_seg7_T_3) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_6 = eq(UInt<3>("h4"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_7 = mux(_io_seg7_seg7_T_6, UInt<8>("h9b"), _io_seg7_seg7_T_5) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_8 = eq(UInt<3>("h5"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_9 = mux(_io_seg7_seg7_T_8, UInt<7>("h49"), _io_seg7_seg7_T_7) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_10 = eq(UInt<3>("h6"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_11 = mux(_io_seg7_seg7_T_10, UInt<7>("h41"), _io_seg7_seg7_T_9) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_12 = eq(UInt<3>("h7"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_13 = mux(_io_seg7_seg7_T_12, UInt<5>("h1f"), _io_seg7_seg7_T_11) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_14 = eq(UInt<4>("h8"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_15 = mux(_io_seg7_seg7_T_14, UInt<1>("h1"), _io_seg7_seg7_T_13) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_16 = eq(UInt<4>("h9"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_17 = mux(_io_seg7_seg7_T_16, UInt<4>("h9"), _io_seg7_seg7_T_15) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_18 = eq(UInt<4>("ha"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_19 = mux(_io_seg7_seg7_T_18, UInt<5>("h11"), _io_seg7_seg7_T_17) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_20 = eq(UInt<4>("hb"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_21 = mux(_io_seg7_seg7_T_20, UInt<8>("hc1"), _io_seg7_seg7_T_19) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_22 = eq(UInt<4>("hc"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_23 = mux(_io_seg7_seg7_T_22, UInt<7>("h61"), _io_seg7_seg7_T_21) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_24 = eq(UInt<4>("hd"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_25 = mux(_io_seg7_seg7_T_24, UInt<8>("h85"), _io_seg7_seg7_T_23) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_26 = eq(UInt<4>("he"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_27 = mux(_io_seg7_seg7_T_26, UInt<7>("h61"), _io_seg7_seg7_T_25) @[Mux.scala 81:58]
    node _io_seg7_seg7_T_28 = eq(UInt<4>("hf"), io.hexDigit) @[Mux.scala 81:61]
    node _io_seg7_seg7_T_29 = mux(_io_seg7_seg7_T_28, UInt<7>("h71"), _io_seg7_seg7_T_27) @[Mux.scala 81:58]
    io_seg7_seg7 <= _io_seg7_seg7_T_29 @[Seg7.scala 54:10]
    io.seg7 <= io_seg7_seg7 @[Seg7.scala 82:11]

  module Seg7 :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { flip seg7 : UInt<32>}, board : { seg7 : UInt<8>, an : UInt<8>}}

    node _digits_T = bits(io.mmio.seg7, 3, 0) @[Seg7.scala 25:57]
    node _digits_T_1 = bits(io.mmio.seg7, 7, 4) @[Seg7.scala 25:57]
    node _digits_T_2 = bits(io.mmio.seg7, 11, 8) @[Seg7.scala 25:57]
    node _digits_T_3 = bits(io.mmio.seg7, 15, 12) @[Seg7.scala 25:57]
    node _digits_T_4 = bits(io.mmio.seg7, 19, 16) @[Seg7.scala 25:57]
    node _digits_T_5 = bits(io.mmio.seg7, 23, 20) @[Seg7.scala 25:57]
    node _digits_T_6 = bits(io.mmio.seg7, 27, 24) @[Seg7.scala 25:57]
    node _digits_T_7 = bits(io.mmio.seg7, 31, 28) @[Seg7.scala 25:57]
    wire digits : UInt<4>[8] @[Seg7.scala 25:23]
    digits[0] <= _digits_T @[Seg7.scala 25:23]
    digits[1] <= _digits_T_1 @[Seg7.scala 25:23]
    digits[2] <= _digits_T_2 @[Seg7.scala 25:23]
    digits[3] <= _digits_T_3 @[Seg7.scala 25:23]
    digits[4] <= _digits_T_4 @[Seg7.scala 25:23]
    digits[5] <= _digits_T_5 @[Seg7.scala 25:23]
    digits[6] <= _digits_T_6 @[Seg7.scala 25:23]
    digits[7] <= _digits_T_7 @[Seg7.scala 25:23]
    reg counter : UInt<3>, clock with :
      reset => (reset, UInt<3>("h0")) @[Seg7.scala 27:24]
    inst seg7Module of HexToSeg7 @[Seg7.scala 33:26]
    seg7Module.clock <= clock
    seg7Module.reset <= reset
    seg7Module.io.hexDigit <= digits[counter] @[Seg7.scala 34:26]
    io.board.seg7 <= seg7Module.io.seg7 @[Seg7.scala 35:17]
    node _io_board_an_T = eq(UInt<1>("h1"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_1 = mux(_io_board_an_T, UInt<8>("hfd"), UInt<8>("hfe")) @[Mux.scala 81:58]
    node _io_board_an_T_2 = eq(UInt<2>("h2"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_3 = mux(_io_board_an_T_2, UInt<8>("hfb"), _io_board_an_T_1) @[Mux.scala 81:58]
    node _io_board_an_T_4 = eq(UInt<2>("h3"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_5 = mux(_io_board_an_T_4, UInt<8>("hf7"), _io_board_an_T_3) @[Mux.scala 81:58]
    node _io_board_an_T_6 = eq(UInt<3>("h4"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_7 = mux(_io_board_an_T_6, UInt<8>("hef"), _io_board_an_T_5) @[Mux.scala 81:58]
    node _io_board_an_T_8 = eq(UInt<3>("h5"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_9 = mux(_io_board_an_T_8, UInt<8>("hdf"), _io_board_an_T_7) @[Mux.scala 81:58]
    node _io_board_an_T_10 = eq(UInt<3>("h6"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_11 = mux(_io_board_an_T_10, UInt<8>("hbf"), _io_board_an_T_9) @[Mux.scala 81:58]
    node _io_board_an_T_12 = eq(UInt<3>("h7"), counter) @[Mux.scala 81:61]
    node _io_board_an_T_13 = mux(_io_board_an_T_12, UInt<7>("h7f"), _io_board_an_T_11) @[Mux.scala 81:58]
    io.board.an <= _io_board_an_T_13 @[Seg7.scala 37:15]
    node _counter_T = add(counter, UInt<1>("h1")) @[Seg7.scala 48:22]
    node _counter_T_1 = tail(_counter_T, 1) @[Seg7.scala 48:22]
    counter <= _counter_T_1 @[Seg7.scala 48:11]

  module Button :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { button : UInt<32>}, board : { flip button : UInt<5>}}

    io.mmio.button <= io.board.button @[Button.scala 24:18]

  module Switches :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { switches : UInt<32>}, board : { flip switches : UInt<24>}}

    io.mmio.switches <= io.board.switches @[Switches.scala 23:20]

  extmodule uart :
    input clk : UInt<1>
    input rst : UInt<1>
    input s_axis_tdata : UInt<8>
    input s_axis_tvalid : UInt<1>
    output s_axis_tready : UInt<1>
    output m_axis_tdata : UInt<8>
    output m_axis_tvalid : UInt<1>
    input m_axis_tready : UInt<1>
    input rxd : UInt<1>
    output txd : UInt<1>
    output tx_busy : UInt<1>
    output rx_busy : UInt<1>
    output rx_overrun_error : UInt<1>
    output rx_frame_error : UInt<1>
    input prescale : UInt<16>
    defname = uart

  module UARTWrapper :
    input clock : Clock
    input reset : Reset
    output io : { board : { flip rx : UInt<1>, tx : UInt<1>}, mmio : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}}

    inst uart of uart @[UARTWrapper.scala 32:22]
    uart.prescale is invalid
    uart.rx_frame_error is invalid
    uart.rx_overrun_error is invalid
    uart.rx_busy is invalid
    uart.tx_busy is invalid
    uart.txd is invalid
    uart.rxd is invalid
    uart.m_axis_tready is invalid
    uart.m_axis_tvalid is invalid
    uart.m_axis_tdata is invalid
    uart.s_axis_tready is invalid
    uart.s_axis_tvalid is invalid
    uart.s_axis_tdata is invalid
    uart.rst is invalid
    uart.clk is invalid
    node _uart_io_clk_T = asUInt(clock) @[UARTWrapper.scala 33:26]
    node _uart_io_clk_T_1 = bits(_uart_io_clk_T, 0, 0) @[UARTWrapper.scala 33:26]
    uart.clk <= _uart_io_clk_T_1 @[UARTWrapper.scala 33:17]
    node _uart_io_rst_T = asUInt(reset) @[UARTWrapper.scala 34:26]
    uart.rst <= _uart_io_rst_T @[UARTWrapper.scala 34:17]
    uart.s_axis_tdata <= io.mmio.txData @[UARTWrapper.scala 36:26]
    uart.s_axis_tvalid <= io.mmio.txStart @[UARTWrapper.scala 37:27]
    io.mmio.txReady <= uart.s_axis_tready @[UARTWrapper.scala 38:21]
    uart.rxd <= io.board.rx @[UARTWrapper.scala 40:17]
    io.board.tx <= uart.txd @[UARTWrapper.scala 41:17]
    io.mmio.rxData <= uart.m_axis_tdata @[UARTWrapper.scala 43:20]
    io.mmio.rxValid <= uart.m_axis_tvalid @[UARTWrapper.scala 44:21]
    uart.m_axis_tready <= io.mmio.rxReady @[UARTWrapper.scala 45:27]
    uart.prescale <= UInt<11>("h516") @[UARTWrapper.scala 47:22]

  module DeviceTop :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}}, board : { led : { led : UInt<24>}, seg7 : { seg7 : UInt<8>, an : UInt<8>}, btn : { flip button : UInt<5>}, switch : { flip switches : UInt<24>}, uart : { flip rx : UInt<1>, tx : UInt<1>}}, external_signal : { load_data_mode : UInt<1>}}

    io.external_signal.load_data_mode <= UInt<1>("h0") @[DeviceTop.scala 40:36]
    inst led of Led @[DeviceTop.scala 42:19]
    led.clock <= clock
    led.reset <= reset
    inst seg7 of Seg7 @[DeviceTop.scala 43:20]
    seg7.clock <= clock
    seg7.reset <= reset
    inst btn of Button @[DeviceTop.scala 44:19]
    btn.clock <= clock
    btn.reset <= reset
    inst switches of Switches @[DeviceTop.scala 45:24]
    switches.clock <= clock
    switches.reset <= reset
    inst uart of UARTWrapper @[DeviceTop.scala 46:20]
    uart.clock <= clock
    uart.reset <= reset
    led.io.mmio.led <= io.mmio.led.led @[DeviceTop.scala 48:15]
    seg7.io.mmio.seg7 <= io.mmio.seg7.seg7 @[DeviceTop.scala 49:16]
    io.mmio.btn <= btn.io.mmio @[DeviceTop.scala 50:15]
    io.mmio.switches <= switches.io.mmio @[DeviceTop.scala 51:20]
    io.mmio.uart.txReady <= uart.io.mmio.txReady @[DeviceTop.scala 52:16]
    uart.io.mmio.txStart <= io.mmio.uart.txStart @[DeviceTop.scala 52:16]
    uart.io.mmio.txData <= io.mmio.uart.txData @[DeviceTop.scala 52:16]
    uart.io.mmio.rxReady <= io.mmio.uart.rxReady @[DeviceTop.scala 52:16]
    io.mmio.uart.rxValid <= uart.io.mmio.rxValid @[DeviceTop.scala 52:16]
    io.mmio.uart.rxData <= uart.io.mmio.rxData @[DeviceTop.scala 52:16]
    io.board.led <= led.io.board @[DeviceTop.scala 55:16]
    io.board.seg7 <= seg7.io.board @[DeviceTop.scala 56:17]
    btn.io.board.button <= io.board.btn.button @[DeviceTop.scala 57:16]
    switches.io.board.switches <= io.board.switch.switches @[DeviceTop.scala 58:21]
    io.board.uart.tx <= uart.io.board.tx @[DeviceTop.scala 59:16]
    uart.io.board.rx <= io.board.uart.rx @[DeviceTop.scala 59:16]
    node _io_external_signal_load_data_mode_T = bits(io.board.switch.switches, 23, 23) @[DeviceTop.scala 62:62]
    io.external_signal.load_data_mode <= _io_external_signal_load_data_mode_T @[DeviceTop.scala 62:36]

  module Top :
    input clock : Clock
    input reset : UInt<1>
    output io : { led : { led : UInt<24>}, seg7 : { seg7 : UInt<8>, an : UInt<8>}, btn : { flip button : UInt<5>}, switch : { flip switches : UInt<24>}, uart : { flip rx : UInt<1>, tx : UInt<1>}}

    inst clockSeparator of ClockSeparator @[Top.scala 15:30]
    clockSeparator.clock <= clock
    clockSeparator.reset <= reset
    inst cpu of CoreTop @[Top.scala 17:11]
    cpu.clock <= clockSeparator.io.cpuClock
    cpu.reset <= reset
    inst device of DeviceTop @[Top.scala 19:22]
    device.clock <= clock
    device.reset <= reset
    io.uart.tx <= device.io.board.uart.tx @[Top.scala 22:18]
    device.io.board.uart.rx <= io.uart.rx @[Top.scala 22:18]
    device.io.board.switch.switches <= io.switch.switches @[Top.scala 22:18]
    device.io.board.btn.button <= io.btn.button @[Top.scala 22:18]
    io.seg7 <= device.io.board.seg7 @[Top.scala 22:18]
    io.led <= device.io.board.led @[Top.scala 22:18]
    cpu.io.external <= device.io.mmio @[Top.scala 25:18]
    cpu.io.external_signal <= device.io.external_signal @[Top.scala 26:25]

