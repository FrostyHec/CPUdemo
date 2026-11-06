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

  module UARTLoader :
    input clock : Clock
    input reset : Reset
    output io : { flip uart_load : UInt<1>, flip rxValid : UInt<1>, flip rxData : UInt<8>, rxReady : UInt<1>, mem : { mem_read : UInt<1>, mem_write : UInt<1>, data_to_write : UInt<32>, data_addr : UInt<32>, data_width : UInt<2>}}

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
      when io.uart_load : @[UARTLoader.scala 50:26]
        data_addr <= UInt<32>("hffffffff") @[UARTLoader.scala 51:19]
        cur_state <= UInt<2>("h1") @[UARTLoader.scala 52:19]
    else :
      node _T_1 = eq(UInt<2>("h1"), cur_state) @[UARTLoader.scala 47:21]
      when _T_1 : @[UARTLoader.scala 47:21]
        rxReadyReg <= UInt<1>("h0") @[UARTLoader.scala 56:17]
        node _T_2 = eq(io.uart_load, UInt<1>("h0")) @[UARTLoader.scala 57:12]
        when _T_2 : @[UARTLoader.scala 57:27]
          cur_state <= UInt<2>("h0") @[UARTLoader.scala 58:19]
        else :
          node _T_3 = eq(io.rxValid, UInt<1>("h0")) @[UARTLoader.scala 60:25]
          when _T_3 : @[UARTLoader.scala 60:38]
            cur_state <= UInt<2>("h3") @[UARTLoader.scala 61:21]
      else :
        node _T_4 = eq(UInt<2>("h3"), cur_state) @[UARTLoader.scala 47:21]
        when _T_4 : @[UARTLoader.scala 47:21]
          rxReadyReg <= UInt<1>("h0") @[UARTLoader.scala 66:17]
          node _T_5 = eq(io.uart_load, UInt<1>("h0")) @[UARTLoader.scala 67:12]
          when _T_5 : @[UARTLoader.scala 67:27]
            cur_state <= UInt<2>("h0") @[UARTLoader.scala 68:19]
          else :
            node _T_6 = eq(io.rxValid, UInt<1>("h1")) @[UARTLoader.scala 70:25]
            when _T_6 : @[UARTLoader.scala 70:37]
              node _data_addr_T = add(data_addr, UInt<1>("h1")) @[UARTLoader.scala 71:34]
              node _data_addr_T_1 = tail(_data_addr_T, 1) @[UARTLoader.scala 71:34]
              data_addr <= _data_addr_T_1 @[UARTLoader.scala 71:21]
              cur_state <= UInt<2>("h2") @[UARTLoader.scala 72:21]
        else :
          node _T_7 = eq(UInt<2>("h2"), cur_state) @[UARTLoader.scala 47:21]
          when _T_7 : @[UARTLoader.scala 47:21]
            rxReadyReg <= UInt<1>("h0") @[UARTLoader.scala 77:17]
            node _T_8 = eq(io.uart_load, UInt<1>("h0")) @[UARTLoader.scala 78:12]
            when _T_8 : @[UARTLoader.scala 78:27]
              cur_state <= UInt<2>("h0") @[UARTLoader.scala 79:19]
            else :
              node _T_9 = eq(io.rxValid, UInt<1>("h1")) @[UARTLoader.scala 81:25]
              when _T_9 : @[UARTLoader.scala 81:37]
                io.mem.mem_write <= UInt<1>("h1") @[UARTLoader.scala 82:28]
                rxReadyReg <= UInt<1>("h1") @[UARTLoader.scala 83:21]
              else :
                cur_state <= UInt<2>("h3") @[UARTLoader.scala 85:21]

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
    output io : { mem : { flip write : UInt<1>, flip read_addr : UInt<32>, flip write_addr : UInt<32>, flip write_data : UInt<32>, read_data : UInt<32>}, flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}, vga : { flip value : UInt<32>}}}

    io.external.vga.value is invalid @[OutRegisters.scala 21:25]
    reg btn : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 25:20]
    btn <= io.external.btn.button @[OutRegisters.scala 26:7]
    reg switches : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 28:25]
    switches <= io.external.switches.switches @[OutRegisters.scala 29:12]
    reg led : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 32:20]
    io.external.led.led <= led @[OutRegisters.scala 33:23]
    reg seg7 : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 35:21]
    io.external.seg7.seg7 <= seg7 @[OutRegisters.scala 36:25]
    reg rxData : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 40:23]
    rxData <= io.external.uart.rxData @[OutRegisters.scala 41:10]
    reg rxVlaid : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 43:24]
    rxVlaid <= io.external.uart.rxValid @[OutRegisters.scala 44:11]
    reg rxReady : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 46:24]
    io.external.uart.rxReady <= rxReady @[OutRegisters.scala 47:28]
    reg txData : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 49:23]
    io.external.uart.txData <= txData @[OutRegisters.scala 50:27]
    reg txStart : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 52:24]
    io.external.uart.txStart <= txStart @[OutRegisters.scala 53:28]
    reg txReady : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 55:24]
    txReady <= io.external.uart.txReady @[OutRegisters.scala 56:11]
    reg vga : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[OutRegisters.scala 59:20]
    node addr = mux(io.mem.write, io.mem.write_addr, io.mem.read_addr) @[OutRegisters.scala 62:23]
    io.mem.read_data is invalid @[OutRegisters.scala 63:20]
    node _T = shr(UInt<32>("hffffff04"), 2) @[OutRegisters.scala 65:44]
    node _T_1 = eq(addr, _T) @[OutRegisters.scala 65:13]
    when _T_1 : @[OutRegisters.scala 65:58]
      when io.mem.write : @[OutRegisters.scala 66:24]
        node _T_2 = asUInt(reset) @[OutRegisters.scala 67:13]
        node _T_3 = eq(_T_2, UInt<1>("h0")) @[OutRegisters.scala 67:13]
        when _T_3 : @[OutRegisters.scala 67:13]
          printf(clock, UInt<1>("h1"), "btn cannot write") : printf @[OutRegisters.scala 67:13]
      else :
        io.mem.read_data <= btn @[OutRegisters.scala 69:24]
    else :
      node _T_4 = shr(UInt<32>("hffffff08"), 2) @[OutRegisters.scala 71:53]
      node _T_5 = eq(addr, _T_4) @[OutRegisters.scala 71:19]
      when _T_5 : @[OutRegisters.scala 71:67]
        when io.mem.write : @[OutRegisters.scala 72:26]
          node _T_6 = asUInt(reset) @[OutRegisters.scala 73:15]
          node _T_7 = eq(_T_6, UInt<1>("h0")) @[OutRegisters.scala 73:15]
          when _T_7 : @[OutRegisters.scala 73:15]
            printf(clock, UInt<1>("h1"), "switches cannot write") : printf_1 @[OutRegisters.scala 73:15]
        else :
          io.mem.read_data <= switches @[OutRegisters.scala 75:26]
      else :
        node _T_8 = shr(UInt<32>("hffffff00"), 2) @[OutRegisters.scala 77:52]
        node _T_9 = eq(addr, _T_8) @[OutRegisters.scala 77:21]
        when _T_9 : @[OutRegisters.scala 77:66]
          when io.mem.write : @[OutRegisters.scala 78:26]
            led <= io.mem.write_data @[OutRegisters.scala 79:13]
          else :
            io.mem.read_data <= led @[OutRegisters.scala 81:26]
        else :
          node _T_10 = shr(UInt<32>("hffffff0c"), 2) @[OutRegisters.scala 83:53]
          node _T_11 = eq(addr, _T_10) @[OutRegisters.scala 83:21]
          when _T_11 : @[OutRegisters.scala 83:67]
            when io.mem.write : @[OutRegisters.scala 84:26]
              seg7 <= io.mem.write_data @[OutRegisters.scala 85:14]
            else :
              io.mem.read_data <= seg7 @[OutRegisters.scala 87:26]
          else :
            node _T_12 = shr(UInt<32>("hffffff14"), 2) @[OutRegisters.scala 89:55]
            node _T_13 = eq(addr, _T_12) @[OutRegisters.scala 89:21]
            when _T_13 : @[OutRegisters.scala 89:69]
              when io.mem.write : @[OutRegisters.scala 90:26]
                node _T_14 = asUInt(reset) @[OutRegisters.scala 91:15]
                node _T_15 = eq(_T_14, UInt<1>("h0")) @[OutRegisters.scala 91:15]
                when _T_15 : @[OutRegisters.scala 91:15]
                  printf(clock, UInt<1>("h1"), "UART cannot write") : printf_2 @[OutRegisters.scala 91:15]
              else :
                io.mem.read_data <= rxData @[OutRegisters.scala 93:26]
            else :
              node _T_16 = shr(UInt<32>("hffffff18"), 2) @[OutRegisters.scala 95:60]
              node _T_17 = eq(addr, _T_16) @[OutRegisters.scala 95:21]
              when _T_17 : @[OutRegisters.scala 95:74]
                when io.mem.write : @[OutRegisters.scala 96:26]
                  node _T_18 = asUInt(reset) @[OutRegisters.scala 97:15]
                  node _T_19 = eq(_T_18, UInt<1>("h0")) @[OutRegisters.scala 97:15]
                  when _T_19 : @[OutRegisters.scala 97:15]
                    printf(clock, UInt<1>("h1"), "UART cannot write") : printf_3 @[OutRegisters.scala 97:15]
                else :
                  io.mem.read_data <= rxVlaid @[OutRegisters.scala 99:26]
              else :
                node _T_20 = shr(UInt<32>("hffffff1c"), 2) @[OutRegisters.scala 101:60]
                node _T_21 = eq(addr, _T_20) @[OutRegisters.scala 101:21]
                when _T_21 : @[OutRegisters.scala 101:74]
                  when io.mem.write : @[OutRegisters.scala 102:26]
                    rxReady <= io.mem.write_data @[OutRegisters.scala 103:17]
                  else :
                    io.mem.read_data <= rxReady @[OutRegisters.scala 105:26]
                else :
                  node _T_22 = shr(UInt<32>("hffffff20"), 2) @[OutRegisters.scala 107:55]
                  node _T_23 = eq(addr, _T_22) @[OutRegisters.scala 107:21]
                  when _T_23 : @[OutRegisters.scala 107:69]
                    when io.mem.write : @[OutRegisters.scala 108:26]
                      txData <= io.mem.write_data @[OutRegisters.scala 109:16]
                    else :
                      io.mem.read_data <= txData @[OutRegisters.scala 111:26]
                  else :
                    node _T_24 = shr(UInt<32>("hffffff28"), 2) @[OutRegisters.scala 113:56]
                    node _T_25 = eq(addr, _T_24) @[OutRegisters.scala 113:21]
                    when _T_25 : @[OutRegisters.scala 113:70]
                      when io.mem.write : @[OutRegisters.scala 114:26]
                        txStart <= io.mem.write_data @[OutRegisters.scala 115:17]
                      else :
                        io.mem.read_data <= txReady @[OutRegisters.scala 117:26]
                    else :
                      node _T_26 = shr(UInt<32>("hffffff24"), 2) @[OutRegisters.scala 119:56]
                      node _T_27 = eq(addr, _T_26) @[OutRegisters.scala 119:21]
                      when _T_27 : @[OutRegisters.scala 119:70]
                        when io.mem.write : @[OutRegisters.scala 120:26]
                          node _T_28 = asUInt(reset) @[OutRegisters.scala 121:15]
                          node _T_29 = eq(_T_28, UInt<1>("h0")) @[OutRegisters.scala 121:15]
                          when _T_29 : @[OutRegisters.scala 121:15]
                            printf(clock, UInt<1>("h1"), "UART cannot write") : printf_4 @[OutRegisters.scala 121:15]
                        else :
                          io.mem.read_data <= txReady @[OutRegisters.scala 123:26]
                      else :
                        node _T_30 = shr(UInt<32>("hffffff10"), 2) @[OutRegisters.scala 126:54]
                        node _T_31 = eq(addr, _T_30) @[OutRegisters.scala 126:20]
                        when _T_31 : @[OutRegisters.scala 126:68]
                          when io.mem.write : @[OutRegisters.scala 127:26]
                            vga <= io.mem.write_data @[OutRegisters.scala 128:13]
                          else :
                            io.mem.read_data <= vga @[OutRegisters.scala 130:26]
                        else :
                          skip

  module MemoryDispatch :
    input clock : Clock
    input reset : Reset
    output io : { flip uart_load : UInt<1>, flip cpu_state : UInt<2>, flip ins_addr : UInt<32>, ins_out : UInt<32>, flip read_data : UInt<1>, flip write_data : UInt<1>, flip unsigned : UInt<1>, flip data_width : UInt<2>, flip data_addr : UInt<32>, flip data_write : UInt<32>, data_out : UInt<32>, flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}, vga : { flip value : UInt<32>}}, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, MEM_fault : { mem_fault_type : UInt<3>, mtval : UInt<32>}}

    io.IF_fault.IF_fault_type <= UInt<1>("h0") @[MemoryDispatch.scala 40:29]
    io.IF_fault.mtval is invalid @[MemoryDispatch.scala 41:20]
    io.MEM_fault.mem_fault_type <= UInt<1>("h0") @[MemoryDispatch.scala 42:31]
    io.MEM_fault.mtval is invalid @[MemoryDispatch.scala 43:21]
    node rw_mem_addr = shr(io.data_addr, 2) @[MemoryDispatch.scala 46:34]
    node read_ins_addr = shr(io.ins_addr, 2) @[MemoryDispatch.scala 47:35]
    inst insRAM of InsRAM @[MemoryDispatch.scala 50:22]
    insRAM.clock <= clock
    insRAM.reset <= reset
    inst dataRAM of DataRAM @[MemoryDispatch.scala 51:23]
    dataRAM.clock <= clock
    dataRAM.reset <= reset
    inst outRegisters of OutRegisters @[MemoryDispatch.scala 52:28]
    outRegisters.clock <= clock
    outRegisters.reset <= reset
    wire data_in : UInt<32> @[MemoryDispatch.scala 55:21]
    wire data_out : UInt<32> @[MemoryDispatch.scala 56:22]
    node _is_write_clk_T = eq(io.cpu_state, UInt<1>("h1")) @[MemoryDispatch.scala 59:36]
    node is_write_clk = or(_is_write_clk_T, io.uart_load) @[MemoryDispatch.scala 59:74]
    insRAM.io2.read_addr <= read_ins_addr @[MemoryDispatch.scala 62:24]
    io.ins_out <= insRAM.io2.read_data @[MemoryDispatch.scala 63:14]
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
    outRegisters.io.external <= io.external @[MemoryDispatch.scala 87:28]
    node _T = bits(io.ins_addr, 1, 0) @[MemoryDispatch.scala 90:19]
    node _T_1 = neq(_T, UInt<1>("h0")) @[MemoryDispatch.scala 90:26]
    when _T_1 : @[MemoryDispatch.scala 90:35]
      io.IF_fault.IF_fault_type <= UInt<1>("h1") @[MemoryDispatch.scala 91:31]
      io.IF_fault.mtval <= io.ins_addr @[MemoryDispatch.scala 92:23]
    else :
      node _T_2 = gt(io.ins_addr, UInt<16>("hffff")) @[MemoryDispatch.scala 93:26]
      when _T_2 : @[MemoryDispatch.scala 93:48]
        io.IF_fault.IF_fault_type <= UInt<2>("h2") @[MemoryDispatch.scala 94:31]
        io.IF_fault.mtval <= io.ins_addr @[MemoryDispatch.scala 95:23]
    data_out is invalid @[MemoryDispatch.scala 112:12]
    node _T_3 = leq(UInt<1>("h0"), io.data_addr) @[MemoryDispatch.scala 113:29]
    node _T_4 = leq(io.data_addr, UInt<16>("hffff")) @[MemoryDispatch.scala 114:21]
    node _T_5 = and(_T_3, _T_4) @[MemoryDispatch.scala 114:5]
    when _T_5 : @[MemoryDispatch.scala 114:44]
      data_out <= insRAM.io.read_data @[MemoryDispatch.scala 115:14]
      when io.write_data : @[MemoryDispatch.scala 116:25]
        node _insRAM_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 117:39]
        insRAM.io.write <= _insRAM_io_write_T @[MemoryDispatch.scala 117:23]
      else :
        when io.read_data : @[MemoryDispatch.scala 118:30]
          skip
        else :
          skip
    else :
      node _T_6 = leq(UInt<17>("h10000"), io.data_addr) @[MemoryDispatch.scala 122:36]
      node _T_7 = leq(io.data_addr, UInt<17>("h1ffff")) @[MemoryDispatch.scala 123:21]
      node _T_8 = and(_T_6, _T_7) @[MemoryDispatch.scala 123:5]
      when _T_8 : @[MemoryDispatch.scala 123:45]
        node _havard_mem_T = sub(io.data_addr, UInt<17>("h10000")) @[MemoryDispatch.scala 125:36]
        node _havard_mem_T_1 = tail(_havard_mem_T, 1) @[MemoryDispatch.scala 125:36]
        node havard_mem = shr(_havard_mem_T_1, 2) @[MemoryDispatch.scala 125:61]
        dataRAM.io.read_addr <= havard_mem @[MemoryDispatch.scala 126:26]
        dataRAM.io.write_addr <= havard_mem @[MemoryDispatch.scala 127:27]
        data_out <= dataRAM.io.read_data @[MemoryDispatch.scala 128:14]
        when io.write_data : @[MemoryDispatch.scala 129:25]
          node _dataRAM_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 130:40]
          dataRAM.io.write <= _dataRAM_io_write_T @[MemoryDispatch.scala 130:24]
        else :
          when io.read_data : @[MemoryDispatch.scala 131:30]
            skip
          else :
            skip
      else :
        node _T_9 = leq(UInt<32>("hffffff00"), io.data_addr) @[MemoryDispatch.scala 135:38]
        node _T_10 = leq(io.data_addr, UInt<32>("hffffffff")) @[MemoryDispatch.scala 136:21]
        node _T_11 = and(_T_9, _T_10) @[MemoryDispatch.scala 136:5]
        when _T_11 : @[MemoryDispatch.scala 136:47]
          data_out <= outRegisters.io.mem.read_data @[MemoryDispatch.scala 137:14]
          when io.write_data : @[MemoryDispatch.scala 138:25]
            node _outRegisters_io_mem_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 139:49]
            outRegisters.io.mem.write <= _outRegisters_io_mem_write_T @[MemoryDispatch.scala 139:33]
          else :
            when io.read_data : @[MemoryDispatch.scala 140:30]
              data_out <= outRegisters.io.mem.read_data @[MemoryDispatch.scala 141:16]
            else :
              skip
        else :
          when io.read_data : @[MemoryDispatch.scala 149:24]
            node _T_12 = asUInt(reset) @[MemoryDispatch.scala 150:13]
            node _T_13 = eq(_T_12, UInt<1>("h0")) @[MemoryDispatch.scala 150:13]
            when _T_13 : @[MemoryDispatch.scala 150:13]
              printf(clock, UInt<1>("h1"), "Unexpected Mem address %x", io.data_addr) : printf @[MemoryDispatch.scala 150:13]
            node _T_14 = eq(io.uart_load, UInt<1>("h0")) @[MemoryDispatch.scala 103:10]
            when _T_14 : @[MemoryDispatch.scala 103:25]
              io.MEM_fault.mem_fault_type <= UInt<2>("h2") @[MemoryDispatch.scala 104:35]
              io.MEM_fault.mtval <= io.data_addr @[MemoryDispatch.scala 105:26]
          else :
            when io.write_data : @[MemoryDispatch.scala 152:31]
              node _T_15 = asUInt(reset) @[MemoryDispatch.scala 153:13]
              node _T_16 = eq(_T_15, UInt<1>("h0")) @[MemoryDispatch.scala 153:13]
              when _T_16 : @[MemoryDispatch.scala 153:13]
                printf(clock, UInt<1>("h1"), "Unexpected Mem address %x", io.data_addr) : printf_1 @[MemoryDispatch.scala 153:13]
              node _T_17 = eq(io.uart_load, UInt<1>("h0")) @[MemoryDispatch.scala 103:10]
              when _T_17 : @[MemoryDispatch.scala 103:25]
                io.MEM_fault.mem_fault_type <= UInt<3>("h4") @[MemoryDispatch.scala 104:35]
                io.MEM_fault.mtval <= io.data_addr @[MemoryDispatch.scala 105:26]
            else :
              skip
    data_in is invalid @[MemoryDispatch.scala 160:11]
    node _T_18 = eq(UInt<1>("h0"), io.data_width) @[MemoryDispatch.scala 161:25]
    when _T_18 : @[MemoryDispatch.scala 161:25]
      node value = bits(io.data_write, 7, 0) @[MemoryDispatch.scala 163:32]
      node _T_19 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 164:26]
      node _T_20 = eq(UInt<1>("h0"), _T_19) @[MemoryDispatch.scala 164:34]
      when _T_20 : @[MemoryDispatch.scala 164:34]
        node _data_in_T = bits(data_out, 31, 8) @[MemoryDispatch.scala 166:34]
        node _data_in_T_1 = cat(_data_in_T, value) @[Cat.scala 33:92]
        data_in <= _data_in_T_1 @[MemoryDispatch.scala 166:19]
      else :
        node _T_21 = eq(UInt<1>("h1"), _T_19) @[MemoryDispatch.scala 164:34]
        when _T_21 : @[MemoryDispatch.scala 164:34]
          node _data_in_T_2 = bits(data_out, 31, 16) @[MemoryDispatch.scala 169:34]
          node _data_in_T_3 = bits(data_out, 7, 0) @[MemoryDispatch.scala 169:59]
          node data_in_hi = cat(_data_in_T_2, value) @[Cat.scala 33:92]
          node _data_in_T_4 = cat(data_in_hi, _data_in_T_3) @[Cat.scala 33:92]
          data_in <= _data_in_T_4 @[MemoryDispatch.scala 169:19]
        else :
          node _T_22 = eq(UInt<2>("h2"), _T_19) @[MemoryDispatch.scala 164:34]
          when _T_22 : @[MemoryDispatch.scala 164:34]
            node _data_in_T_5 = bits(data_out, 31, 24) @[MemoryDispatch.scala 172:34]
            node _data_in_T_6 = bits(data_out, 15, 0) @[MemoryDispatch.scala 172:59]
            node data_in_hi_1 = cat(_data_in_T_5, value) @[Cat.scala 33:92]
            node _data_in_T_7 = cat(data_in_hi_1, _data_in_T_6) @[Cat.scala 33:92]
            data_in <= _data_in_T_7 @[MemoryDispatch.scala 172:19]
          else :
            node _T_23 = eq(UInt<2>("h3"), _T_19) @[MemoryDispatch.scala 164:34]
            when _T_23 : @[MemoryDispatch.scala 164:34]
              node _data_in_T_8 = bits(data_out, 23, 0) @[MemoryDispatch.scala 175:41]
              node _data_in_T_9 = cat(value, _data_in_T_8) @[Cat.scala 33:92]
              data_in <= _data_in_T_9 @[MemoryDispatch.scala 175:19]
    else :
      node _T_24 = eq(UInt<1>("h1"), io.data_width) @[MemoryDispatch.scala 161:25]
      when _T_24 : @[MemoryDispatch.scala 161:25]
        node value_1 = bits(io.data_write, 15, 0) @[MemoryDispatch.scala 180:32]
        node _T_25 = bits(io.data_addr, 1, 1) @[MemoryDispatch.scala 181:24]
        node _T_26 = eq(_T_25, UInt<1>("h0")) @[MemoryDispatch.scala 181:28]
        when _T_26 : @[MemoryDispatch.scala 181:40]
          node _data_in_T_10 = bits(data_out, 31, 16) @[MemoryDispatch.scala 182:32]
          node _data_in_T_11 = cat(_data_in_T_10, value_1) @[Cat.scala 33:92]
          data_in <= _data_in_T_11 @[MemoryDispatch.scala 182:17]
        else :
          node _data_in_T_12 = bits(data_out, 15, 0) @[MemoryDispatch.scala 184:39]
          node _data_in_T_13 = cat(value_1, _data_in_T_12) @[Cat.scala 33:92]
          data_in <= _data_in_T_13 @[MemoryDispatch.scala 184:17]
      else :
        node _T_27 = eq(UInt<2>("h2"), io.data_width) @[MemoryDispatch.scala 161:25]
        when _T_27 : @[MemoryDispatch.scala 161:25]
          data_in <= io.data_write @[MemoryDispatch.scala 188:15]
    io.data_out is invalid @[MemoryDispatch.scala 194:15]
    node _T_28 = eq(UInt<1>("h0"), io.data_width) @[MemoryDispatch.scala 195:25]
    when _T_28 : @[MemoryDispatch.scala 195:25]
      node _T_29 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 197:26]
      node _T_30 = eq(UInt<1>("h0"), _T_29) @[MemoryDispatch.scala 197:34]
      when _T_30 : @[MemoryDispatch.scala 197:34]
        node _high_bit_T = bits(data_out, 7, 7) @[MemoryDispatch.scala 199:65]
        node _high_bit_T_1 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T) @[MemoryDispatch.scala 199:38]
        node _high_bit_T_2 = bits(_high_bit_T_1, 0, 0) @[Bitwise.scala 77:15]
        node high_bit = mux(_high_bit_T_2, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
        node _io_data_out_T = bits(data_out, 7, 0) @[MemoryDispatch.scala 200:48]
        node _io_data_out_T_1 = cat(high_bit, _io_data_out_T) @[Cat.scala 33:92]
        io.data_out <= _io_data_out_T_1 @[MemoryDispatch.scala 200:23]
      else :
        node _T_31 = eq(UInt<1>("h1"), _T_29) @[MemoryDispatch.scala 197:34]
        when _T_31 : @[MemoryDispatch.scala 197:34]
          node _high_bit_T_3 = bits(data_out, 15, 15) @[MemoryDispatch.scala 203:65]
          node _high_bit_T_4 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_3) @[MemoryDispatch.scala 203:38]
          node _high_bit_T_5 = bits(_high_bit_T_4, 0, 0) @[Bitwise.scala 77:15]
          node high_bit_1 = mux(_high_bit_T_5, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
          node _io_data_out_T_2 = bits(data_out, 15, 8) @[MemoryDispatch.scala 204:48]
          node _io_data_out_T_3 = cat(high_bit_1, _io_data_out_T_2) @[Cat.scala 33:92]
          io.data_out <= _io_data_out_T_3 @[MemoryDispatch.scala 204:23]
        else :
          node _T_32 = eq(UInt<2>("h2"), _T_29) @[MemoryDispatch.scala 197:34]
          when _T_32 : @[MemoryDispatch.scala 197:34]
            node _high_bit_T_6 = bits(data_out, 23, 23) @[MemoryDispatch.scala 207:65]
            node _high_bit_T_7 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_6) @[MemoryDispatch.scala 207:38]
            node _high_bit_T_8 = bits(_high_bit_T_7, 0, 0) @[Bitwise.scala 77:15]
            node high_bit_2 = mux(_high_bit_T_8, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
            node _io_data_out_T_4 = bits(data_out, 23, 16) @[MemoryDispatch.scala 208:48]
            node _io_data_out_T_5 = cat(high_bit_2, _io_data_out_T_4) @[Cat.scala 33:92]
            io.data_out <= _io_data_out_T_5 @[MemoryDispatch.scala 208:23]
          else :
            node _T_33 = eq(UInt<2>("h3"), _T_29) @[MemoryDispatch.scala 197:34]
            when _T_33 : @[MemoryDispatch.scala 197:34]
              node _high_bit_T_9 = bits(data_out, 31, 31) @[MemoryDispatch.scala 211:65]
              node _high_bit_T_10 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_9) @[MemoryDispatch.scala 211:38]
              node _high_bit_T_11 = bits(_high_bit_T_10, 0, 0) @[Bitwise.scala 77:15]
              node high_bit_3 = mux(_high_bit_T_11, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
              node _io_data_out_T_6 = bits(data_out, 31, 24) @[MemoryDispatch.scala 212:48]
              node _io_data_out_T_7 = cat(high_bit_3, _io_data_out_T_6) @[Cat.scala 33:92]
              io.data_out <= _io_data_out_T_7 @[MemoryDispatch.scala 212:23]
    else :
      node _T_34 = eq(UInt<1>("h1"), io.data_width) @[MemoryDispatch.scala 195:25]
      when _T_34 : @[MemoryDispatch.scala 195:25]
        node _T_35 = bits(io.data_addr, 0, 0) @[MemoryDispatch.scala 218:24]
        node _T_36 = eq(_T_35, UInt<1>("h0")) @[MemoryDispatch.scala 218:28]
        when _T_36 : @[MemoryDispatch.scala 218:37]
          node _T_37 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 219:28]
          node _T_38 = eq(UInt<1>("h0"), _T_37) @[MemoryDispatch.scala 219:36]
          when _T_38 : @[MemoryDispatch.scala 219:36]
            node _high_bit_T_12 = bits(data_out, 15, 15) @[MemoryDispatch.scala 221:67]
            node _high_bit_T_13 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_12) @[MemoryDispatch.scala 221:40]
            node _high_bit_T_14 = bits(_high_bit_T_13, 0, 0) @[Bitwise.scala 77:15]
            node high_bit_4 = mux(_high_bit_T_14, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 77:12]
            node _io_data_out_T_8 = bits(data_out, 15, 0) @[MemoryDispatch.scala 222:50]
            node _io_data_out_T_9 = cat(high_bit_4, _io_data_out_T_8) @[Cat.scala 33:92]
            io.data_out <= _io_data_out_T_9 @[MemoryDispatch.scala 222:25]
          else :
            node _T_39 = eq(UInt<2>("h2"), _T_37) @[MemoryDispatch.scala 219:36]
            when _T_39 : @[MemoryDispatch.scala 219:36]
              node _high_bit_T_15 = bits(data_out, 31, 31) @[MemoryDispatch.scala 225:67]
              node _high_bit_T_16 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_15) @[MemoryDispatch.scala 225:40]
              node _high_bit_T_17 = bits(_high_bit_T_16, 0, 0) @[Bitwise.scala 77:15]
              node high_bit_5 = mux(_high_bit_T_17, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 77:12]
              node _io_data_out_T_10 = bits(data_out, 31, 16) @[MemoryDispatch.scala 226:50]
              node _io_data_out_T_11 = cat(high_bit_5, _io_data_out_T_10) @[Cat.scala 33:92]
              io.data_out <= _io_data_out_T_11 @[MemoryDispatch.scala 226:25]
        else :
          when io.read_data : @[MemoryDispatch.scala 230:28]
            node _T_40 = eq(io.uart_load, UInt<1>("h0")) @[MemoryDispatch.scala 103:10]
            when _T_40 : @[MemoryDispatch.scala 103:25]
              io.MEM_fault.mem_fault_type <= UInt<1>("h1") @[MemoryDispatch.scala 104:35]
              io.MEM_fault.mtval <= io.data_addr @[MemoryDispatch.scala 105:26]
          when io.write_data : @[MemoryDispatch.scala 233:29]
            node _T_41 = eq(io.uart_load, UInt<1>("h0")) @[MemoryDispatch.scala 103:10]
            when _T_41 : @[MemoryDispatch.scala 103:25]
              io.MEM_fault.mem_fault_type <= UInt<2>("h3") @[MemoryDispatch.scala 104:35]
              io.MEM_fault.mtval <= io.data_addr @[MemoryDispatch.scala 105:26]
      else :
        node _T_42 = eq(UInt<2>("h2"), io.data_width) @[MemoryDispatch.scala 195:25]
        when _T_42 : @[MemoryDispatch.scala 195:25]
          node _T_43 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 239:24]
          node _T_44 = eq(_T_43, UInt<1>("h0")) @[MemoryDispatch.scala 239:31]
          when _T_44 : @[MemoryDispatch.scala 239:40]
            io.data_out <= data_out @[MemoryDispatch.scala 240:21]
          else :
            when io.read_data : @[MemoryDispatch.scala 243:28]
              node _T_45 = eq(io.uart_load, UInt<1>("h0")) @[MemoryDispatch.scala 103:10]
              when _T_45 : @[MemoryDispatch.scala 103:25]
                io.MEM_fault.mem_fault_type <= UInt<1>("h1") @[MemoryDispatch.scala 104:35]
                io.MEM_fault.mtval <= io.data_addr @[MemoryDispatch.scala 105:26]
            when io.write_data : @[MemoryDispatch.scala 246:29]
              node _T_46 = eq(io.uart_load, UInt<1>("h0")) @[MemoryDispatch.scala 103:10]
              when _T_46 : @[MemoryDispatch.scala 103:25]
                io.MEM_fault.mem_fault_type <= UInt<2>("h3") @[MemoryDispatch.scala 104:35]
                io.MEM_fault.mtval <= io.data_addr @[MemoryDispatch.scala 105:26]

  module LoadVerify :
    input clock : Clock
    input reset : UInt<1>
    output io : { flip rx : UInt<1>, tx : UInt<1>, flip addr : UInt<16>, led : UInt<16>}

    inst uart of UARTWrapper @[LoadVerify.scala 16:20]
    uart.clock <= clock
    uart.reset <= reset
    inst loader of UARTLoader @[LoadVerify.scala 17:22]
    loader.clock <= clock
    loader.reset <= reset
    inst mem_dispatch of MemoryDispatch @[LoadVerify.scala 18:28]
    mem_dispatch.clock <= clock
    mem_dispatch.reset <= reset
    uart.io.mmio.txData is invalid @[LoadVerify.scala 20:22]
    uart.io.mmio.txStart <= UInt<1>("h0") @[LoadVerify.scala 21:23]
    uart.io.board.rx <= io.rx @[LoadVerify.scala 22:19]
    io.tx <= uart.io.board.tx @[LoadVerify.scala 23:8]
    uart.io.mmio.rxReady <= loader.io.rxReady @[LoadVerify.scala 25:23]
    loader.io.uart_load <= UInt<1>("h1") @[LoadVerify.scala 26:22]
    loader.io.rxData <= uart.io.mmio.rxData @[LoadVerify.scala 27:20]
    loader.io.rxValid <= uart.io.mmio.rxValid @[LoadVerify.scala 28:21]
    mem_dispatch.io.cpu_state <= UInt<1>("h0") @[LoadVerify.scala 30:28]
    mem_dispatch.io.uart_load <= UInt<1>("h1") @[LoadVerify.scala 31:28]
    mem_dispatch.io.unsigned <= UInt<1>("h1") @[LoadVerify.scala 33:27]
    mem_dispatch.io.read_data <= loader.io.mem.mem_read @[LoadVerify.scala 34:28]
    mem_dispatch.io.write_data <= loader.io.mem.mem_write @[LoadVerify.scala 35:29]
    mem_dispatch.io.data_write <= loader.io.mem.data_to_write @[LoadVerify.scala 36:29]
    mem_dispatch.io.data_width <= loader.io.mem.data_width @[LoadVerify.scala 37:29]
    mem_dispatch.io.data_addr <= loader.io.mem.data_addr @[LoadVerify.scala 38:28]
    mem_dispatch.io.external.uart.txReady <= UInt<1>("h0") @[LoadVerify.scala 41:40]
    mem_dispatch.io.external.uart.rxValid <= UInt<1>("h0") @[LoadVerify.scala 42:40]
    mem_dispatch.io.external.uart.rxData is invalid @[LoadVerify.scala 43:39]
    mem_dispatch.io.external.btn.button is invalid @[LoadVerify.scala 44:38]
    mem_dispatch.io.external.switches.switches is invalid @[LoadVerify.scala 45:45]
    mem_dispatch.io.ins_addr <= io.addr @[LoadVerify.scala 48:27]
    node _io_led_T = bits(mem_dispatch.io.ins_out, 7, 0) @[LoadVerify.scala 49:58]
    node _io_led_T_1 = cat(uart.io.mmio.rxData, _io_led_T) @[Cat.scala 33:92]
    io.led <= _io_led_T_1 @[LoadVerify.scala 49:9]

