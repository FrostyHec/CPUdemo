circuit LoadVerify :
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
    output io : { board : { flip rx : UInt<1>, tx : UInt<1>}, mmio : { rxData : UInt<8>, rxValid : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}}

    inst uart of uart @[UARTWrapper.scala 30:22]
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
    node _uart_io_clk_T = asUInt(clock) @[UARTWrapper.scala 31:26]
    node _uart_io_clk_T_1 = bits(_uart_io_clk_T, 0, 0) @[UARTWrapper.scala 31:26]
    uart.clk <= _uart_io_clk_T_1 @[UARTWrapper.scala 31:17]
    node _uart_io_rst_T = asUInt(reset) @[UARTWrapper.scala 32:26]
    uart.rst <= _uart_io_rst_T @[UARTWrapper.scala 32:17]
    uart.s_axis_tdata <= io.mmio.txData @[UARTWrapper.scala 34:26]
    uart.s_axis_tvalid <= io.mmio.txStart @[UARTWrapper.scala 35:27]
    io.mmio.txReady <= uart.s_axis_tready @[UARTWrapper.scala 36:21]
    uart.rxd <= io.board.rx @[UARTWrapper.scala 38:17]
    io.board.tx <= uart.txd @[UARTWrapper.scala 39:17]
    io.mmio.rxData <= uart.m_axis_tdata @[UARTWrapper.scala 41:20]
    io.mmio.rxValid <= uart.m_axis_tvalid @[UARTWrapper.scala 42:21]
    uart.m_axis_tready <= UInt<1>("h0") @[UARTWrapper.scala 43:27]
    uart.prescale <= UInt<11>("h516") @[UARTWrapper.scala 45:22]

  module UARTLoader :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip rxValid : UInt<1>, flip rxData : UInt<8>, mem : { mem_read : UInt<1>, mem_write : UInt<1>, data_to_write : UInt<32>, data_addr : UInt<32>, data_width : UInt<2>}}

    reg cur_state : UInt<2>, clock with :
      reset => (reset, UInt<2>("h0")) @[UARTLoader.scala 33:26]
    reg data_addr : UInt<32>, clock with :
      reset => (reset, UInt<32>("hffffffff")) @[UARTLoader.scala 34:26]
    io.mem.data_addr <= data_addr @[UARTLoader.scala 37:20]
    io.mem.data_width <= UInt<1>("h0") @[UARTLoader.scala 38:21]
    io.mem.data_to_write <= io.rxData @[UARTLoader.scala 39:24]
    io.mem.mem_read <= UInt<1>("h0") @[UARTLoader.scala 40:19]
    io.mem.mem_write <= UInt<1>("h0") @[UARTLoader.scala 41:20]
    node _T = eq(UInt<2>("h0"), cur_state) @[UARTLoader.scala 43:21]
    when _T : @[UARTLoader.scala 43:21]
      node _T_1 = eq(io.cpu_state, UInt<2>("h2")) @[UARTLoader.scala 45:25]
      when _T_1 : @[UARTLoader.scala 45:61]
        data_addr <= UInt<32>("hffffffff") @[UARTLoader.scala 46:19]
        cur_state <= UInt<2>("h1") @[UARTLoader.scala 47:19]
    else :
      node _T_2 = eq(UInt<2>("h1"), cur_state) @[UARTLoader.scala 43:21]
      when _T_2 : @[UARTLoader.scala 43:21]
        node _T_3 = neq(io.cpu_state, UInt<2>("h2")) @[UARTLoader.scala 51:25]
        when _T_3 : @[UARTLoader.scala 51:61]
          cur_state <= UInt<2>("h0") @[UARTLoader.scala 52:19]
        else :
          node _T_4 = eq(io.rxValid, UInt<1>("h0")) @[UARTLoader.scala 54:25]
          when _T_4 : @[UARTLoader.scala 54:38]
            cur_state <= UInt<2>("h3") @[UARTLoader.scala 55:21]
      else :
        node _T_5 = eq(UInt<2>("h3"), cur_state) @[UARTLoader.scala 43:21]
        when _T_5 : @[UARTLoader.scala 43:21]
          node _T_6 = neq(io.cpu_state, UInt<2>("h2")) @[UARTLoader.scala 60:25]
          when _T_6 : @[UARTLoader.scala 60:61]
            cur_state <= UInt<2>("h0") @[UARTLoader.scala 61:19]
          else :
            node _T_7 = eq(io.rxValid, UInt<1>("h1")) @[UARTLoader.scala 63:25]
            when _T_7 : @[UARTLoader.scala 63:37]
              node _data_addr_T = add(data_addr, UInt<1>("h1")) @[UARTLoader.scala 64:34]
              node _data_addr_T_1 = tail(_data_addr_T, 1) @[UARTLoader.scala 64:34]
              data_addr <= _data_addr_T_1 @[UARTLoader.scala 64:21]
              cur_state <= UInt<2>("h2") @[UARTLoader.scala 65:21]
        else :
          node _T_8 = eq(UInt<2>("h2"), cur_state) @[UARTLoader.scala 43:21]
          when _T_8 : @[UARTLoader.scala 43:21]
            node _T_9 = neq(io.cpu_state, UInt<2>("h2")) @[UARTLoader.scala 70:25]
            when _T_9 : @[UARTLoader.scala 70:61]
              cur_state <= UInt<2>("h0") @[UARTLoader.scala 71:19]
            else :
              node _T_10 = eq(io.rxValid, UInt<1>("h1")) @[UARTLoader.scala 73:25]
              when _T_10 : @[UARTLoader.scala 73:37]
                io.mem.mem_write <= UInt<1>("h1") @[UARTLoader.scala 74:28]
              else :
                cur_state <= UInt<2>("h3") @[UARTLoader.scala 76:21]

  extmodule ins_mem :
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

    inst ip_mem of ins_mem @[MemoryPacket.scala 62:24]
    ip_mem.dpo is invalid
    ip_mem.spo is invalid
    ip_mem.we is invalid
    ip_mem.dpra is invalid
    ip_mem.d is invalid
    ip_mem.a is invalid
    node _ip_mem_io_a_T = mux(io.write, io.write_addr, io2.read_addr) @[MemoryPacket.scala 63:21]
    ip_mem.a <= _ip_mem_io_a_T @[MemoryPacket.scala 63:16]
    ip_mem.d <= io.write_data @[MemoryPacket.scala 64:16]
    ip_mem.dpra <= io2.read_addr @[MemoryPacket.scala 65:19]
    ip_mem.we <= io.write @[MemoryPacket.scala 66:17]
    io.read_data <= ip_mem.spo @[MemoryPacket.scala 67:17]
    io2.read_data <= ip_mem.dpo @[MemoryPacket.scala 68:18]

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
    node _ip_ram_io_a_T = mux(io.write, io.write_addr, io.read_addr) @[MemoryPacket.scala 32:21]
    ip_ram.a <= _ip_ram_io_a_T @[MemoryPacket.scala 32:16]
    ip_ram.d <= io.write_data @[MemoryPacket.scala 33:16]
    ip_ram.we <= io.write @[MemoryPacket.scala 34:17]
    io.read_data <= ip_ram.spo @[MemoryPacket.scala 35:17]

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
    output io : { mem : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}, flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}}}

    io.external.uart.txData is invalid @[OutRegisters.scala 18:27]
    io.external.uart.txStart <= UInt<1>("h0") @[OutRegisters.scala 19:28]
    reg btn : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 23:20]
    btn <= io.external.btn.button @[OutRegisters.scala 24:7]
    reg switches : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 26:25]
    switches <= io.external.switches.switches @[OutRegisters.scala 27:12]
    reg led : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 30:20]
    io.external.led.led <= led @[OutRegisters.scala 31:23]
    reg seg7 : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 33:21]
    io.external.seg7.seg7 <= seg7 @[OutRegisters.scala 34:25]
    node addr = mux(io.mem.write, io.mem.write_addr, io.mem.read_addr) @[OutRegisters.scala 37:23]
    io.mem.read_data is invalid @[OutRegisters.scala 38:20]
    node _T = shr(UInt<32>("hffffff04"), 2) @[OutRegisters.scala 40:44]
    node _T_1 = eq(addr, _T) @[OutRegisters.scala 40:13]
    when _T_1 : @[OutRegisters.scala 40:58]
      when io.mem.write : @[OutRegisters.scala 41:24]
        node _T_2 = asUInt(reset) @[OutRegisters.scala 42:13]
        node _T_3 = eq(_T_2, UInt<1>("h0")) @[OutRegisters.scala 42:13]
        when _T_3 : @[OutRegisters.scala 42:13]
          printf(clock, UInt<1>("h1"), "btn cannot write") : printf @[OutRegisters.scala 42:13]
      else :
        io.mem.read_data <= btn @[OutRegisters.scala 44:24]
    else :
      node _T_4 = shr(UInt<32>("hffffff08"), 2) @[OutRegisters.scala 46:53]
      node _T_5 = eq(addr, _T_4) @[OutRegisters.scala 46:19]
      when _T_5 : @[OutRegisters.scala 46:67]
        when io.mem.write : @[OutRegisters.scala 47:24]
          node _T_6 = asUInt(reset) @[OutRegisters.scala 48:13]
          node _T_7 = eq(_T_6, UInt<1>("h0")) @[OutRegisters.scala 48:13]
          when _T_7 : @[OutRegisters.scala 48:13]
            printf(clock, UInt<1>("h1"), "switches cannot write") : printf_1 @[OutRegisters.scala 48:13]
        else :
          io.mem.read_data <= switches @[OutRegisters.scala 50:24]
      else :
        node _T_8 = shr(UInt<32>("hffffff00"), 2) @[OutRegisters.scala 52:50]
        node _T_9 = eq(addr, _T_8) @[OutRegisters.scala 52:19]
        when _T_9 : @[OutRegisters.scala 52:64]
          when io.mem.write : @[OutRegisters.scala 53:24]
            led <= io.mem.write_data @[OutRegisters.scala 54:11]
          else :
            io.mem.read_data <= led @[OutRegisters.scala 56:24]
        else :
          node _T_10 = shr(UInt<32>("hffffff0c"), 2) @[OutRegisters.scala 58:51]
          node _T_11 = eq(addr, _T_10) @[OutRegisters.scala 58:19]
          when _T_11 : @[OutRegisters.scala 58:65]
            when io.mem.write : @[OutRegisters.scala 59:24]
              seg7 <= io.mem.write_data @[OutRegisters.scala 60:12]
            else :
              io.mem.read_data <= seg7 @[OutRegisters.scala 62:24]
          else :
            skip

  module MemoryDispatch :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip ins_addr : UInt<32>, ins_out : UInt<32>, flip read_data : UInt<1>, flip write_data : UInt<1>, flip unsigned : UInt<1>, flip data_width : UInt<2>, flip data_addr : UInt<32>, flip data_write : UInt<32>, data_out : UInt<32>, flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}}}

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

  module LoadVerify :
    input clock : Clock
    input reset : UInt<1>
    output io : { flip rx : UInt<1>, tx : UInt<1>, flip addr : UInt<16>, led : UInt<16>}

    inst uart of UARTWrapper @[LoadVerify.scala 15:20]
    uart.clock <= clock
    uart.reset <= reset
    inst loader of UARTLoader @[LoadVerify.scala 16:22]
    loader.clock <= clock
    loader.reset <= reset
    inst mem_dispatch of MemoryDispatch @[LoadVerify.scala 17:28]
    mem_dispatch.clock <= clock
    mem_dispatch.reset <= reset
    uart.io.mmio.txData is invalid @[LoadVerify.scala 19:22]
    uart.io.mmio.txStart <= UInt<1>("h0") @[LoadVerify.scala 20:23]
    uart.io.board.rx <= io.rx @[LoadVerify.scala 21:19]
    io.tx <= uart.io.board.tx @[LoadVerify.scala 22:8]
    loader.io.cpu_state <= UInt<2>("h2") @[LoadVerify.scala 24:22]
    loader.io.rxData <= uart.io.mmio.rxData @[LoadVerify.scala 25:20]
    loader.io.rxValid <= uart.io.mmio.rxValid @[LoadVerify.scala 26:21]
    mem_dispatch.io.cpu_state <= UInt<2>("h2") @[LoadVerify.scala 28:28]
    mem_dispatch.io.unsigned <= UInt<1>("h1") @[LoadVerify.scala 30:27]
    mem_dispatch.io.read_data <= loader.io.mem.mem_read @[LoadVerify.scala 31:28]
    mem_dispatch.io.write_data <= loader.io.mem.mem_write @[LoadVerify.scala 32:29]
    mem_dispatch.io.data_write <= loader.io.mem.data_to_write @[LoadVerify.scala 33:29]
    mem_dispatch.io.data_width <= loader.io.mem.data_width @[LoadVerify.scala 34:29]
    mem_dispatch.io.data_addr <= loader.io.mem.data_addr @[LoadVerify.scala 35:28]
    mem_dispatch.io.external.uart.txReady <= UInt<1>("h0") @[LoadVerify.scala 38:40]
    mem_dispatch.io.external.uart.rxValid <= UInt<1>("h0") @[LoadVerify.scala 39:40]
    mem_dispatch.io.external.uart.rxData is invalid @[LoadVerify.scala 40:39]
    mem_dispatch.io.external.btn.button is invalid @[LoadVerify.scala 41:38]
    mem_dispatch.io.external.switches.switches is invalid @[LoadVerify.scala 42:45]
    mem_dispatch.io.ins_addr <= io.addr @[LoadVerify.scala 45:27]
    node _io_led_T = bits(mem_dispatch.io.ins_out, 15, 0) @[LoadVerify.scala 46:34]
    io.led <= _io_led_T @[LoadVerify.scala 46:9]

