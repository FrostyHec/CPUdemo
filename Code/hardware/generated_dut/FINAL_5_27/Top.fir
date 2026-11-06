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
    ip_clock.reset <= UInt<1>("h0") @[ClockSeparator.scala 16:22]
    io.cpuClock <= ip_clock.clk_out2 @[ClockSeparator.scala 17:17]

  module CSR :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, cur_privilege : UInt<2>, flip write : UInt<1>, flip csr : UInt<12>, flip write_data : UInt<32>, csr_val : UInt<32>, flip mem_fault : { mem_fault_type : UInt<3>, mtval : UInt<32>}, flip ins_fault : { ins_fault_type : UInt<3>, mtval : UInt<32>}, flip io_interruption : { io_fault_occur : UInt<1>, mtval : UInt<32>}, flip pc : UInt<32>, fault_state : UInt<1>, fault_write_PC : UInt<32>}

    reg cur_privilege : UInt<2>, clock with :
      reset => (reset, UInt<2>("h3")) @[CSR.scala 35:30]
    io.cur_privilege <= cur_privilege @[CSR.scala 36:20]
    reg mstatus : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 38:24]
    reg mie : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 39:20]
    reg mtvec : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 40:22]
    reg mscratch : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 42:25]
    reg mepc : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 43:21]
    reg mcause : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 44:23]
    reg mtval : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 45:22]
    reg mip : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 46:20]
    node read_MIE = bits(mstatus, 3, 3) @[CSR.scala 48:25]
    node read_MEIE = bits(mie, 11, 11) @[CSR.scala 49:22]
    node read_MPIE = bits(mstatus, 7, 7) @[CSR.scala 50:26]
    node read_MPP = bits(mstatus, 12, 11) @[CSR.scala 51:25]
    node _mip_T = and(read_MIE, read_MEIE) @[CSR.scala 53:36]
    node _mip_T_1 = and(_mip_T, io.io_interruption.io_fault_occur) @[CSR.scala 53:48]
    node _mip_T_2 = dshl(UInt<1>("h1"), UInt<4>("hb")) @[CSR.scala 53:20]
    node _mip_T_3 = or(mip, _mip_T_2) @[CSR.scala 53:20]
    node _mip_T_4 = not(mip) @[CSR.scala 53:20]
    node _mip_T_5 = or(_mip_T_4, _mip_T_2) @[CSR.scala 53:20]
    node _mip_T_6 = not(_mip_T_5) @[CSR.scala 53:20]
    node _mip_T_7 = mux(_mip_T_1, _mip_T_3, _mip_T_6) @[CSR.scala 53:20]
    mip <= _mip_T_7 @[CSR.scala 53:7]
    node _no_fault_T = eq(io.mem_fault.mem_fault_type, UInt<1>("h0")) @[CSR.scala 55:47]
    node _no_fault_T_1 = eq(io.ins_fault.ins_fault_type, UInt<1>("h0")) @[CSR.scala 56:34]
    node _no_fault_T_2 = and(_no_fault_T, _no_fault_T_1) @[CSR.scala 55:76]
    node _no_fault_T_3 = and(read_MIE, read_MEIE) @[CSR.scala 57:16]
    node _no_fault_T_4 = and(_no_fault_T_3, io.io_interruption.io_fault_occur) @[CSR.scala 57:28]
    node _no_fault_T_5 = eq(_no_fault_T_4, UInt<1>("h0")) @[CSR.scala 57:5]
    node no_fault = and(_no_fault_T_2, _no_fault_T_5) @[CSR.scala 56:63]
    io.csr_val is invalid @[CSR.scala 92:14]
    io.fault_state <= UInt<1>("h0") @[CSR.scala 93:18]
    io.fault_write_PC is invalid @[CSR.scala 94:21]
    node _T = eq(io.csr, UInt<10>("h300")) @[CSR.scala 60:17]
    when _T : @[CSR.scala 60:26]
      io.csr_val <= mstatus @[CSR.scala 61:18]
      node _T_1 = eq(io.cpu_state, UInt<1>("h1")) @[CSR.scala 62:25]
      node _T_2 = and(_T_1, io.write) @[CSR.scala 62:61]
      node _T_3 = and(_T_2, no_fault) @[CSR.scala 62:72]
      when _T_3 : @[CSR.scala 62:84]
        mstatus <= io.write_data @[CSR.scala 63:13]
    node _T_4 = eq(io.csr, UInt<10>("h304")) @[CSR.scala 60:17]
    when _T_4 : @[CSR.scala 60:26]
      io.csr_val <= mie @[CSR.scala 61:18]
      node _T_5 = eq(io.cpu_state, UInt<1>("h1")) @[CSR.scala 62:25]
      node _T_6 = and(_T_5, io.write) @[CSR.scala 62:61]
      node _T_7 = and(_T_6, no_fault) @[CSR.scala 62:72]
      when _T_7 : @[CSR.scala 62:84]
        mie <= io.write_data @[CSR.scala 63:13]
    node _T_8 = eq(io.csr, UInt<10>("h305")) @[CSR.scala 60:17]
    when _T_8 : @[CSR.scala 60:26]
      io.csr_val <= mtvec @[CSR.scala 61:18]
      node _T_9 = eq(io.cpu_state, UInt<1>("h1")) @[CSR.scala 62:25]
      node _T_10 = and(_T_9, io.write) @[CSR.scala 62:61]
      node _T_11 = and(_T_10, no_fault) @[CSR.scala 62:72]
      when _T_11 : @[CSR.scala 62:84]
        mtvec <= io.write_data @[CSR.scala 63:13]
    node _T_12 = eq(io.csr, UInt<10>("h340")) @[CSR.scala 60:17]
    when _T_12 : @[CSR.scala 60:26]
      io.csr_val <= mscratch @[CSR.scala 61:18]
      node _T_13 = eq(io.cpu_state, UInt<1>("h1")) @[CSR.scala 62:25]
      node _T_14 = and(_T_13, io.write) @[CSR.scala 62:61]
      node _T_15 = and(_T_14, no_fault) @[CSR.scala 62:72]
      when _T_15 : @[CSR.scala 62:84]
        mscratch <= io.write_data @[CSR.scala 63:13]
    node _T_16 = eq(io.csr, UInt<10>("h341")) @[CSR.scala 60:17]
    when _T_16 : @[CSR.scala 60:26]
      io.csr_val <= mepc @[CSR.scala 61:18]
      node _T_17 = eq(io.cpu_state, UInt<1>("h1")) @[CSR.scala 62:25]
      node _T_18 = and(_T_17, io.write) @[CSR.scala 62:61]
      node _T_19 = and(_T_18, no_fault) @[CSR.scala 62:72]
      when _T_19 : @[CSR.scala 62:84]
        mepc <= io.write_data @[CSR.scala 63:13]
    node _T_20 = eq(io.csr, UInt<10>("h342")) @[CSR.scala 60:17]
    when _T_20 : @[CSR.scala 60:26]
      io.csr_val <= mcause @[CSR.scala 61:18]
      node _T_21 = eq(io.cpu_state, UInt<1>("h1")) @[CSR.scala 62:25]
      node _T_22 = and(_T_21, io.write) @[CSR.scala 62:61]
      node _T_23 = and(_T_22, no_fault) @[CSR.scala 62:72]
      when _T_23 : @[CSR.scala 62:84]
        mcause <= io.write_data @[CSR.scala 63:13]
    node _T_24 = eq(io.csr, UInt<10>("h343")) @[CSR.scala 60:17]
    when _T_24 : @[CSR.scala 60:26]
      io.csr_val <= mtval @[CSR.scala 61:18]
      node _T_25 = eq(io.cpu_state, UInt<1>("h1")) @[CSR.scala 62:25]
      node _T_26 = and(_T_25, io.write) @[CSR.scala 62:61]
      node _T_27 = and(_T_26, no_fault) @[CSR.scala 62:72]
      when _T_27 : @[CSR.scala 62:84]
        mtval <= io.write_data @[CSR.scala 63:13]
    node _T_28 = eq(io.csr, UInt<10>("h344")) @[CSR.scala 60:17]
    when _T_28 : @[CSR.scala 60:26]
      io.csr_val <= mip @[CSR.scala 61:18]
      node _T_29 = eq(io.cpu_state, UInt<1>("h1")) @[CSR.scala 62:25]
      node _T_30 = and(_T_29, io.write) @[CSR.scala 62:61]
      node _T_31 = and(_T_30, no_fault) @[CSR.scala 62:72]
      when _T_31 : @[CSR.scala 62:84]
        mip <= io.write_data @[CSR.scala 63:13]
    node _T_32 = eq(no_fault, UInt<1>("h0")) @[CSR.scala 105:8]
    when _T_32 : @[CSR.scala 105:19]
      wire fault_type : UInt<1> @[CSR.scala 106:26]
      fault_type <= UInt<1>("h0") @[CSR.scala 107:16]
      node _T_33 = eq(io.ins_fault.ins_fault_type, UInt<3>("h4")) @[CSR.scala 108:38]
      when _T_33 : @[CSR.scala 108:69]
        fault_type <= UInt<1>("h1") @[CSR.scala 109:18]
      node _T_34 = eq(fault_type, UInt<1>("h0")) @[CSR.scala 113:21]
      when _T_34 : @[CSR.scala 113:62]
        node _T_35 = eq(io.mem_fault.mem_fault_type, UInt<2>("h3")) @[CSR.scala 114:40]
        when _T_35 : @[CSR.scala 114:81]
          io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
          mepc <= io.pc @[CSR.scala 77:10]
          io.fault_write_PC <= mtvec @[CSR.scala 78:23]
          mcause <= UInt<3>("h4") @[CSR.scala 79:12]
          mtval <= io.mem_fault.mtval @[CSR.scala 80:11]
          node _mstatus_T = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
          node _mstatus_T_1 = or(mstatus, _mstatus_T) @[CSR.scala 81:30]
          node _mstatus_T_2 = not(mstatus) @[CSR.scala 81:30]
          node _mstatus_T_3 = or(_mstatus_T_2, _mstatus_T) @[CSR.scala 81:30]
          node _mstatus_T_4 = not(_mstatus_T_3) @[CSR.scala 81:30]
          node _mstatus_T_5 = mux(UInt<1>("h0"), _mstatus_T_1, _mstatus_T_4) @[CSR.scala 81:30]
          mstatus <= _mstatus_T_5 @[CSR.scala 81:13]
          node _mstatus_T_6 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
          node _mstatus_T_7 = or(mstatus, _mstatus_T_6) @[CSR.scala 83:30]
          node _mstatus_T_8 = not(mstatus) @[CSR.scala 83:30]
          node _mstatus_T_9 = or(_mstatus_T_8, _mstatus_T_6) @[CSR.scala 83:30]
          node _mstatus_T_10 = not(_mstatus_T_9) @[CSR.scala 83:30]
          node _mstatus_T_11 = mux(read_MIE, _mstatus_T_7, _mstatus_T_10) @[CSR.scala 83:30]
          mstatus <= _mstatus_T_11 @[CSR.scala 83:13]
          node _mstatus_T_12 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
          node _mstatus_T_13 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
          node mstatus_hi = cat(_mstatus_T_12, cur_privilege) @[Cat.scala 33:92]
          node _mstatus_T_14 = cat(mstatus_hi, _mstatus_T_13) @[Cat.scala 33:92]
          mstatus <= _mstatus_T_14 @[CSR.scala 85:13]
          cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
        else :
          node _T_36 = eq(io.mem_fault.mem_fault_type, UInt<3>("h4")) @[CSR.scala 116:46]
          when _T_36 : @[CSR.scala 116:82]
            io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
            mepc <= io.pc @[CSR.scala 77:10]
            io.fault_write_PC <= mtvec @[CSR.scala 78:23]
            mcause <= UInt<3>("h5") @[CSR.scala 79:12]
            mtval <= io.mem_fault.mtval @[CSR.scala 80:11]
            node _mstatus_T_15 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
            node _mstatus_T_16 = or(mstatus, _mstatus_T_15) @[CSR.scala 81:30]
            node _mstatus_T_17 = not(mstatus) @[CSR.scala 81:30]
            node _mstatus_T_18 = or(_mstatus_T_17, _mstatus_T_15) @[CSR.scala 81:30]
            node _mstatus_T_19 = not(_mstatus_T_18) @[CSR.scala 81:30]
            node _mstatus_T_20 = mux(UInt<1>("h0"), _mstatus_T_16, _mstatus_T_19) @[CSR.scala 81:30]
            mstatus <= _mstatus_T_20 @[CSR.scala 81:13]
            node _mstatus_T_21 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
            node _mstatus_T_22 = or(mstatus, _mstatus_T_21) @[CSR.scala 83:30]
            node _mstatus_T_23 = not(mstatus) @[CSR.scala 83:30]
            node _mstatus_T_24 = or(_mstatus_T_23, _mstatus_T_21) @[CSR.scala 83:30]
            node _mstatus_T_25 = not(_mstatus_T_24) @[CSR.scala 83:30]
            node _mstatus_T_26 = mux(read_MIE, _mstatus_T_22, _mstatus_T_25) @[CSR.scala 83:30]
            mstatus <= _mstatus_T_26 @[CSR.scala 83:13]
            node _mstatus_T_27 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
            node _mstatus_T_28 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
            node mstatus_hi_1 = cat(_mstatus_T_27, cur_privilege) @[Cat.scala 33:92]
            node _mstatus_T_29 = cat(mstatus_hi_1, _mstatus_T_28) @[Cat.scala 33:92]
            mstatus <= _mstatus_T_29 @[CSR.scala 85:13]
            cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
          else :
            node _T_37 = eq(io.mem_fault.mem_fault_type, UInt<3>("h5")) @[CSR.scala 118:48]
            when _T_37 : @[CSR.scala 118:90]
              io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
              mepc <= io.pc @[CSR.scala 77:10]
              io.fault_write_PC <= mtvec @[CSR.scala 78:23]
              mcause <= UInt<3>("h6") @[CSR.scala 79:12]
              mtval <= io.mem_fault.mtval @[CSR.scala 80:11]
              node _mstatus_T_30 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
              node _mstatus_T_31 = or(mstatus, _mstatus_T_30) @[CSR.scala 81:30]
              node _mstatus_T_32 = not(mstatus) @[CSR.scala 81:30]
              node _mstatus_T_33 = or(_mstatus_T_32, _mstatus_T_30) @[CSR.scala 81:30]
              node _mstatus_T_34 = not(_mstatus_T_33) @[CSR.scala 81:30]
              node _mstatus_T_35 = mux(UInt<1>("h0"), _mstatus_T_31, _mstatus_T_34) @[CSR.scala 81:30]
              mstatus <= _mstatus_T_35 @[CSR.scala 81:13]
              node _mstatus_T_36 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
              node _mstatus_T_37 = or(mstatus, _mstatus_T_36) @[CSR.scala 83:30]
              node _mstatus_T_38 = not(mstatus) @[CSR.scala 83:30]
              node _mstatus_T_39 = or(_mstatus_T_38, _mstatus_T_36) @[CSR.scala 83:30]
              node _mstatus_T_40 = not(_mstatus_T_39) @[CSR.scala 83:30]
              node _mstatus_T_41 = mux(read_MIE, _mstatus_T_37, _mstatus_T_40) @[CSR.scala 83:30]
              mstatus <= _mstatus_T_41 @[CSR.scala 83:13]
              node _mstatus_T_42 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
              node _mstatus_T_43 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
              node mstatus_hi_2 = cat(_mstatus_T_42, cur_privilege) @[Cat.scala 33:92]
              node _mstatus_T_44 = cat(mstatus_hi_2, _mstatus_T_43) @[Cat.scala 33:92]
              mstatus <= _mstatus_T_44 @[CSR.scala 85:13]
              cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
            else :
              node _T_38 = eq(io.mem_fault.mem_fault_type, UInt<3>("h6")) @[CSR.scala 120:48]
              when _T_38 : @[CSR.scala 120:85]
                io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
                mepc <= io.pc @[CSR.scala 77:10]
                io.fault_write_PC <= mtvec @[CSR.scala 78:23]
                mcause <= UInt<3>("h7") @[CSR.scala 79:12]
                mtval <= io.mem_fault.mtval @[CSR.scala 80:11]
                node _mstatus_T_45 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
                node _mstatus_T_46 = or(mstatus, _mstatus_T_45) @[CSR.scala 81:30]
                node _mstatus_T_47 = not(mstatus) @[CSR.scala 81:30]
                node _mstatus_T_48 = or(_mstatus_T_47, _mstatus_T_45) @[CSR.scala 81:30]
                node _mstatus_T_49 = not(_mstatus_T_48) @[CSR.scala 81:30]
                node _mstatus_T_50 = mux(UInt<1>("h0"), _mstatus_T_46, _mstatus_T_49) @[CSR.scala 81:30]
                mstatus <= _mstatus_T_50 @[CSR.scala 81:13]
                node _mstatus_T_51 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
                node _mstatus_T_52 = or(mstatus, _mstatus_T_51) @[CSR.scala 83:30]
                node _mstatus_T_53 = not(mstatus) @[CSR.scala 83:30]
                node _mstatus_T_54 = or(_mstatus_T_53, _mstatus_T_51) @[CSR.scala 83:30]
                node _mstatus_T_55 = not(_mstatus_T_54) @[CSR.scala 83:30]
                node _mstatus_T_56 = mux(read_MIE, _mstatus_T_52, _mstatus_T_55) @[CSR.scala 83:30]
                mstatus <= _mstatus_T_56 @[CSR.scala 83:13]
                node _mstatus_T_57 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
                node _mstatus_T_58 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
                node mstatus_hi_3 = cat(_mstatus_T_57, cur_privilege) @[Cat.scala 33:92]
                node _mstatus_T_59 = cat(mstatus_hi_3, _mstatus_T_58) @[Cat.scala 33:92]
                mstatus <= _mstatus_T_59 @[CSR.scala 85:13]
                cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
              else :
                node _T_39 = eq(io.ins_fault.ins_fault_type, UInt<1>("h1")) @[CSR.scala 124:47]
                when _T_39 : @[CSR.scala 124:84]
                  io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
                  mepc <= io.pc @[CSR.scala 77:10]
                  io.fault_write_PC <= mtvec @[CSR.scala 78:23]
                  mcause <= UInt<2>("h2") @[CSR.scala 79:12]
                  mtval <= io.ins_fault.mtval @[CSR.scala 80:11]
                  node _mstatus_T_60 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
                  node _mstatus_T_61 = or(mstatus, _mstatus_T_60) @[CSR.scala 81:30]
                  node _mstatus_T_62 = not(mstatus) @[CSR.scala 81:30]
                  node _mstatus_T_63 = or(_mstatus_T_62, _mstatus_T_60) @[CSR.scala 81:30]
                  node _mstatus_T_64 = not(_mstatus_T_63) @[CSR.scala 81:30]
                  node _mstatus_T_65 = mux(UInt<1>("h0"), _mstatus_T_61, _mstatus_T_64) @[CSR.scala 81:30]
                  mstatus <= _mstatus_T_65 @[CSR.scala 81:13]
                  node _mstatus_T_66 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
                  node _mstatus_T_67 = or(mstatus, _mstatus_T_66) @[CSR.scala 83:30]
                  node _mstatus_T_68 = not(mstatus) @[CSR.scala 83:30]
                  node _mstatus_T_69 = or(_mstatus_T_68, _mstatus_T_66) @[CSR.scala 83:30]
                  node _mstatus_T_70 = not(_mstatus_T_69) @[CSR.scala 83:30]
                  node _mstatus_T_71 = mux(read_MIE, _mstatus_T_67, _mstatus_T_70) @[CSR.scala 83:30]
                  mstatus <= _mstatus_T_71 @[CSR.scala 83:13]
                  node _mstatus_T_72 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
                  node _mstatus_T_73 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
                  node mstatus_hi_4 = cat(_mstatus_T_72, cur_privilege) @[Cat.scala 33:92]
                  node _mstatus_T_74 = cat(mstatus_hi_4, _mstatus_T_73) @[Cat.scala 33:92]
                  mstatus <= _mstatus_T_74 @[CSR.scala 85:13]
                  cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
                else :
                  node _T_40 = eq(io.ins_fault.ins_fault_type, UInt<2>("h2")) @[CSR.scala 126:48]
                  when _T_40 : @[CSR.scala 126:85]
                    io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
                    mepc <= io.pc @[CSR.scala 77:10]
                    io.fault_write_PC <= mtvec @[CSR.scala 78:23]
                    mcause <= UInt<2>("h3") @[CSR.scala 79:12]
                    mtval <= io.ins_fault.mtval @[CSR.scala 80:11]
                    node _mstatus_T_75 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
                    node _mstatus_T_76 = or(mstatus, _mstatus_T_75) @[CSR.scala 81:30]
                    node _mstatus_T_77 = not(mstatus) @[CSR.scala 81:30]
                    node _mstatus_T_78 = or(_mstatus_T_77, _mstatus_T_75) @[CSR.scala 81:30]
                    node _mstatus_T_79 = not(_mstatus_T_78) @[CSR.scala 81:30]
                    node _mstatus_T_80 = mux(UInt<1>("h0"), _mstatus_T_76, _mstatus_T_79) @[CSR.scala 81:30]
                    mstatus <= _mstatus_T_80 @[CSR.scala 81:13]
                    node _mstatus_T_81 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
                    node _mstatus_T_82 = or(mstatus, _mstatus_T_81) @[CSR.scala 83:30]
                    node _mstatus_T_83 = not(mstatus) @[CSR.scala 83:30]
                    node _mstatus_T_84 = or(_mstatus_T_83, _mstatus_T_81) @[CSR.scala 83:30]
                    node _mstatus_T_85 = not(_mstatus_T_84) @[CSR.scala 83:30]
                    node _mstatus_T_86 = mux(read_MIE, _mstatus_T_82, _mstatus_T_85) @[CSR.scala 83:30]
                    mstatus <= _mstatus_T_86 @[CSR.scala 83:13]
                    node _mstatus_T_87 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
                    node _mstatus_T_88 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
                    node mstatus_hi_5 = cat(_mstatus_T_87, cur_privilege) @[Cat.scala 33:92]
                    node _mstatus_T_89 = cat(mstatus_hi_5, _mstatus_T_88) @[Cat.scala 33:92]
                    mstatus <= _mstatus_T_89 @[CSR.scala 85:13]
                    cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
                  else :
                    node _T_41 = eq(io.ins_fault.ins_fault_type, UInt<2>("h3")) @[CSR.scala 128:48]
                    when _T_41 : @[CSR.scala 128:81]
                      io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
                      mepc <= io.pc @[CSR.scala 77:10]
                      io.fault_write_PC <= mtvec @[CSR.scala 78:23]
                      mcause <= UInt<4>("hb") @[CSR.scala 79:12]
                      mtval <= io.ins_fault.mtval @[CSR.scala 80:11]
                      node _mstatus_T_90 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
                      node _mstatus_T_91 = or(mstatus, _mstatus_T_90) @[CSR.scala 81:30]
                      node _mstatus_T_92 = not(mstatus) @[CSR.scala 81:30]
                      node _mstatus_T_93 = or(_mstatus_T_92, _mstatus_T_90) @[CSR.scala 81:30]
                      node _mstatus_T_94 = not(_mstatus_T_93) @[CSR.scala 81:30]
                      node _mstatus_T_95 = mux(UInt<1>("h0"), _mstatus_T_91, _mstatus_T_94) @[CSR.scala 81:30]
                      mstatus <= _mstatus_T_95 @[CSR.scala 81:13]
                      node _mstatus_T_96 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
                      node _mstatus_T_97 = or(mstatus, _mstatus_T_96) @[CSR.scala 83:30]
                      node _mstatus_T_98 = not(mstatus) @[CSR.scala 83:30]
                      node _mstatus_T_99 = or(_mstatus_T_98, _mstatus_T_96) @[CSR.scala 83:30]
                      node _mstatus_T_100 = not(_mstatus_T_99) @[CSR.scala 83:30]
                      node _mstatus_T_101 = mux(read_MIE, _mstatus_T_97, _mstatus_T_100) @[CSR.scala 83:30]
                      mstatus <= _mstatus_T_101 @[CSR.scala 83:13]
                      node _mstatus_T_102 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
                      node _mstatus_T_103 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
                      node mstatus_hi_6 = cat(_mstatus_T_102, cur_privilege) @[Cat.scala 33:92]
                      node _mstatus_T_104 = cat(mstatus_hi_6, _mstatus_T_103) @[Cat.scala 33:92]
                      mstatus <= _mstatus_T_104 @[CSR.scala 85:13]
                      cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
                    else :
                      node _T_42 = eq(io.mem_fault.mem_fault_type, UInt<1>("h1")) @[CSR.scala 132:47]
                      when _T_42 : @[CSR.scala 132:87]
                        io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
                        mepc <= io.pc @[CSR.scala 77:10]
                        io.fault_write_PC <= mtvec @[CSR.scala 78:23]
                        mcause <= UInt<1>("h0") @[CSR.scala 79:12]
                        mtval <= io.mem_fault.mtval @[CSR.scala 80:11]
                        node _mstatus_T_105 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
                        node _mstatus_T_106 = or(mstatus, _mstatus_T_105) @[CSR.scala 81:30]
                        node _mstatus_T_107 = not(mstatus) @[CSR.scala 81:30]
                        node _mstatus_T_108 = or(_mstatus_T_107, _mstatus_T_105) @[CSR.scala 81:30]
                        node _mstatus_T_109 = not(_mstatus_T_108) @[CSR.scala 81:30]
                        node _mstatus_T_110 = mux(UInt<1>("h0"), _mstatus_T_106, _mstatus_T_109) @[CSR.scala 81:30]
                        mstatus <= _mstatus_T_110 @[CSR.scala 81:13]
                        node _mstatus_T_111 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
                        node _mstatus_T_112 = or(mstatus, _mstatus_T_111) @[CSR.scala 83:30]
                        node _mstatus_T_113 = not(mstatus) @[CSR.scala 83:30]
                        node _mstatus_T_114 = or(_mstatus_T_113, _mstatus_T_111) @[CSR.scala 83:30]
                        node _mstatus_T_115 = not(_mstatus_T_114) @[CSR.scala 83:30]
                        node _mstatus_T_116 = mux(read_MIE, _mstatus_T_112, _mstatus_T_115) @[CSR.scala 83:30]
                        mstatus <= _mstatus_T_116 @[CSR.scala 83:13]
                        node _mstatus_T_117 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
                        node _mstatus_T_118 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
                        node mstatus_hi_7 = cat(_mstatus_T_117, cur_privilege) @[Cat.scala 33:92]
                        node _mstatus_T_119 = cat(mstatus_hi_7, _mstatus_T_118) @[Cat.scala 33:92]
                        mstatus <= _mstatus_T_119 @[CSR.scala 85:13]
                        cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
                      else :
                        node _T_43 = eq(io.mem_fault.mem_fault_type, UInt<2>("h2")) @[CSR.scala 135:47]
                        when _T_43 : @[CSR.scala 135:82]
                          io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
                          mepc <= io.pc @[CSR.scala 77:10]
                          io.fault_write_PC <= mtvec @[CSR.scala 78:23]
                          mcause <= UInt<1>("h1") @[CSR.scala 79:12]
                          mtval <= io.mem_fault.mtval @[CSR.scala 80:11]
                          node _mstatus_T_120 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
                          node _mstatus_T_121 = or(mstatus, _mstatus_T_120) @[CSR.scala 81:30]
                          node _mstatus_T_122 = not(mstatus) @[CSR.scala 81:30]
                          node _mstatus_T_123 = or(_mstatus_T_122, _mstatus_T_120) @[CSR.scala 81:30]
                          node _mstatus_T_124 = not(_mstatus_T_123) @[CSR.scala 81:30]
                          node _mstatus_T_125 = mux(UInt<1>("h0"), _mstatus_T_121, _mstatus_T_124) @[CSR.scala 81:30]
                          mstatus <= _mstatus_T_125 @[CSR.scala 81:13]
                          node _mstatus_T_126 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
                          node _mstatus_T_127 = or(mstatus, _mstatus_T_126) @[CSR.scala 83:30]
                          node _mstatus_T_128 = not(mstatus) @[CSR.scala 83:30]
                          node _mstatus_T_129 = or(_mstatus_T_128, _mstatus_T_126) @[CSR.scala 83:30]
                          node _mstatus_T_130 = not(_mstatus_T_129) @[CSR.scala 83:30]
                          node _mstatus_T_131 = mux(read_MIE, _mstatus_T_127, _mstatus_T_130) @[CSR.scala 83:30]
                          mstatus <= _mstatus_T_131 @[CSR.scala 83:13]
                          node _mstatus_T_132 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
                          node _mstatus_T_133 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
                          node mstatus_hi_8 = cat(_mstatus_T_132, cur_privilege) @[Cat.scala 33:92]
                          node _mstatus_T_134 = cat(mstatus_hi_8, _mstatus_T_133) @[Cat.scala 33:92]
                          mstatus <= _mstatus_T_134 @[CSR.scala 85:13]
                          cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
                        else :
                          when io.io_interruption.io_fault_occur : @[CSR.scala 139:54]
                            node _T_44 = dshl(UInt<1>("h1"), UInt<5>("h1f")) @[CSR.scala 140:29]
                            node _T_45 = add(_T_44, UInt<4>("hb")) @[CSR.scala 140:45]
                            node _T_46 = tail(_T_45, 1) @[CSR.scala 140:45]
                            io.fault_state <= UInt<1>("h1") @[CSR.scala 76:20]
                            mepc <= io.pc @[CSR.scala 77:10]
                            io.fault_write_PC <= mtvec @[CSR.scala 78:23]
                            mcause <= _T_46 @[CSR.scala 79:12]
                            mtval <= io.io_interruption.mtval @[CSR.scala 80:11]
                            node _mstatus_T_135 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 81:30]
                            node _mstatus_T_136 = or(mstatus, _mstatus_T_135) @[CSR.scala 81:30]
                            node _mstatus_T_137 = not(mstatus) @[CSR.scala 81:30]
                            node _mstatus_T_138 = or(_mstatus_T_137, _mstatus_T_135) @[CSR.scala 81:30]
                            node _mstatus_T_139 = not(_mstatus_T_138) @[CSR.scala 81:30]
                            node _mstatus_T_140 = mux(UInt<1>("h0"), _mstatus_T_136, _mstatus_T_139) @[CSR.scala 81:30]
                            mstatus <= _mstatus_T_140 @[CSR.scala 81:13]
                            node _mstatus_T_141 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 83:30]
                            node _mstatus_T_142 = or(mstatus, _mstatus_T_141) @[CSR.scala 83:30]
                            node _mstatus_T_143 = not(mstatus) @[CSR.scala 83:30]
                            node _mstatus_T_144 = or(_mstatus_T_143, _mstatus_T_141) @[CSR.scala 83:30]
                            node _mstatus_T_145 = not(_mstatus_T_144) @[CSR.scala 83:30]
                            node _mstatus_T_146 = mux(read_MIE, _mstatus_T_142, _mstatus_T_145) @[CSR.scala 83:30]
                            mstatus <= _mstatus_T_146 @[CSR.scala 83:13]
                            node _mstatus_T_147 = bits(mstatus, 31, 13) @[CSR.scala 85:27]
                            node _mstatus_T_148 = bits(mstatus, 10, 0) @[CSR.scala 85:59]
                            node mstatus_hi_9 = cat(_mstatus_T_147, cur_privilege) @[Cat.scala 33:92]
                            node _mstatus_T_149 = cat(mstatus_hi_9, _mstatus_T_148) @[Cat.scala 33:92]
                            mstatus <= _mstatus_T_149 @[CSR.scala 85:13]
                            cur_privilege <= UInt<2>("h3") @[CSR.scala 86:19]
      else :
        io.fault_state <= UInt<1>("h1") @[CSR.scala 143:22]
        io.fault_write_PC <= mepc @[CSR.scala 144:25]
        node _mstatus_T_150 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 146:32]
        node _mstatus_T_151 = or(mstatus, _mstatus_T_150) @[CSR.scala 146:32]
        node _mstatus_T_152 = not(mstatus) @[CSR.scala 146:32]
        node _mstatus_T_153 = or(_mstatus_T_152, _mstatus_T_150) @[CSR.scala 146:32]
        node _mstatus_T_154 = not(_mstatus_T_153) @[CSR.scala 146:32]
        node _mstatus_T_155 = mux(read_MPIE, _mstatus_T_151, _mstatus_T_154) @[CSR.scala 146:32]
        mstatus <= _mstatus_T_155 @[CSR.scala 146:15]
        cur_privilege <= read_MPP @[CSR.scala 147:21]

  module PLIC :
    input clock : Clock
    input reset : Reset
    output io : { interruption : { io_fault_occur : UInt<1>, mtval : UInt<32>}}

    io.interruption.io_fault_occur <= UInt<1>("h0") @[PLIC.scala 12:34]
    io.interruption.mtval is invalid @[PLIC.scala 13:25]

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
    output io : { flip cpu_state : UInt<2>, flip fault_occurs : UInt<1>, flip ins_addr : UInt<32>, ins_out : UInt<32>, flip read_data : UInt<1>, flip write_data : UInt<1>, flip unsigned : UInt<1>, flip data_width : UInt<2>, flip data_addr : UInt<32>, flip data_write : UInt<32>, data_out : UInt<32>, flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}, vga : { flip value : UInt<32>}}, fault : { mem_fault_type : UInt<3>, mtval : UInt<32>}, flip pc : UInt<32>}

    io.fault.mem_fault_type <= UInt<1>("h0") @[MemoryDispatch.scala 38:27]
    io.fault.mtval is invalid @[MemoryDispatch.scala 39:18]
    node rw_mem_addr = shr(io.data_addr, 2) @[MemoryDispatch.scala 42:34]
    node read_ins_addr = shr(io.ins_addr, 2) @[MemoryDispatch.scala 43:35]
    inst insRAM of InsRAM @[MemoryDispatch.scala 46:22]
    insRAM.clock <= clock
    insRAM.reset <= reset
    inst dataRAM of DataRAM @[MemoryDispatch.scala 47:23]
    dataRAM.clock <= clock
    dataRAM.reset <= reset
    inst outRegisters of OutRegisters @[MemoryDispatch.scala 48:28]
    outRegisters.clock <= clock
    outRegisters.reset <= reset
    wire data_in : UInt<32> @[MemoryDispatch.scala 51:21]
    wire data_out : UInt<32> @[MemoryDispatch.scala 52:22]
    node _is_write_clk_T = eq(io.cpu_state, UInt<1>("h1")) @[MemoryDispatch.scala 55:36]
    node _is_write_clk_T_1 = eq(io.cpu_state, UInt<2>("h3")) @[MemoryDispatch.scala 55:88]
    node _is_write_clk_T_2 = or(_is_write_clk_T, _is_write_clk_T_1) @[MemoryDispatch.scala 55:72]
    node _is_write_clk_T_3 = eq(io.fault_occurs, UInt<1>("h0")) @[MemoryDispatch.scala 55:126]
    node is_write_clk = and(_is_write_clk_T_2, _is_write_clk_T_3) @[MemoryDispatch.scala 55:123]
    insRAM.io2.read_addr <= read_ins_addr @[MemoryDispatch.scala 58:24]
    io.ins_out <= insRAM.io2.read_data @[MemoryDispatch.scala 59:14]
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
    outRegisters.io.external <= io.external @[MemoryDispatch.scala 83:28]
    data_out is invalid @[MemoryDispatch.scala 99:12]
    node _T = leq(UInt<1>("h0"), io.data_addr) @[MemoryDispatch.scala 100:29]
    node _T_1 = leq(io.data_addr, UInt<16>("hffff")) @[MemoryDispatch.scala 101:21]
    node _T_2 = and(_T, _T_1) @[MemoryDispatch.scala 101:5]
    when _T_2 : @[MemoryDispatch.scala 101:44]
      data_out <= insRAM.io.read_data @[MemoryDispatch.scala 102:14]
      when io.write_data : @[MemoryDispatch.scala 103:25]
        node _insRAM_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 104:39]
        insRAM.io.write <= _insRAM_io_write_T @[MemoryDispatch.scala 104:23]
      else :
        when io.read_data : @[MemoryDispatch.scala 105:30]
          skip
        else :
          skip
    else :
      node _T_3 = leq(UInt<17>("h10000"), io.data_addr) @[MemoryDispatch.scala 109:36]
      node _T_4 = leq(io.data_addr, UInt<17>("h1ffff")) @[MemoryDispatch.scala 110:21]
      node _T_5 = and(_T_3, _T_4) @[MemoryDispatch.scala 110:5]
      when _T_5 : @[MemoryDispatch.scala 110:45]
        node _havard_mem_T = sub(io.data_addr, UInt<17>("h10000")) @[MemoryDispatch.scala 112:36]
        node _havard_mem_T_1 = tail(_havard_mem_T, 1) @[MemoryDispatch.scala 112:36]
        node havard_mem = shr(_havard_mem_T_1, 2) @[MemoryDispatch.scala 112:61]
        dataRAM.io.read_addr <= havard_mem @[MemoryDispatch.scala 113:26]
        dataRAM.io.write_addr <= havard_mem @[MemoryDispatch.scala 114:27]
        data_out <= dataRAM.io.read_data @[MemoryDispatch.scala 115:14]
        when io.write_data : @[MemoryDispatch.scala 116:25]
          node _dataRAM_io_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 117:40]
          dataRAM.io.write <= _dataRAM_io_write_T @[MemoryDispatch.scala 117:24]
        else :
          when io.read_data : @[MemoryDispatch.scala 118:30]
            skip
          else :
            skip
      else :
        node _T_6 = leq(UInt<32>("hffffff00"), io.data_addr) @[MemoryDispatch.scala 122:38]
        node _T_7 = leq(io.data_addr, UInt<32>("hffffffff")) @[MemoryDispatch.scala 123:21]
        node _T_8 = and(_T_6, _T_7) @[MemoryDispatch.scala 123:5]
        when _T_8 : @[MemoryDispatch.scala 123:47]
          data_out <= outRegisters.io.mem.read_data @[MemoryDispatch.scala 124:14]
          when io.write_data : @[MemoryDispatch.scala 125:25]
            node _outRegisters_io_mem_write_T = and(is_write_clk, io.write_data) @[MemoryDispatch.scala 126:49]
            outRegisters.io.mem.write <= _outRegisters_io_mem_write_T @[MemoryDispatch.scala 126:33]
          else :
            when io.read_data : @[MemoryDispatch.scala 127:30]
              data_out <= outRegisters.io.mem.read_data @[MemoryDispatch.scala 128:16]
            else :
              skip
        else :
          when io.read_data : @[MemoryDispatch.scala 136:24]
            node _T_9 = asUInt(reset) @[MemoryDispatch.scala 137:13]
            node _T_10 = eq(_T_9, UInt<1>("h0")) @[MemoryDispatch.scala 137:13]
            when _T_10 : @[MemoryDispatch.scala 137:13]
              printf(clock, UInt<1>("h1"), "Unexpected Mem address %x", io.data_addr) : printf @[MemoryDispatch.scala 137:13]
            node _T_11 = neq(io.cpu_state, UInt<2>("h3")) @[MemoryDispatch.scala 90:22]
            when _T_11 : @[MemoryDispatch.scala 90:57]
              io.fault.mem_fault_type <= UInt<3>("h4") @[MemoryDispatch.scala 91:31]
              io.fault.mtval <= io.data_addr @[MemoryDispatch.scala 92:22]
          else :
            when io.write_data : @[MemoryDispatch.scala 139:31]
              node _T_12 = asUInt(reset) @[MemoryDispatch.scala 140:13]
              node _T_13 = eq(_T_12, UInt<1>("h0")) @[MemoryDispatch.scala 140:13]
              when _T_13 : @[MemoryDispatch.scala 140:13]
                printf(clock, UInt<1>("h1"), "Unexpected Mem address %x", io.data_addr) : printf_1 @[MemoryDispatch.scala 140:13]
              node _T_14 = neq(io.cpu_state, UInt<2>("h3")) @[MemoryDispatch.scala 90:22]
              when _T_14 : @[MemoryDispatch.scala 90:57]
                io.fault.mem_fault_type <= UInt<3>("h6") @[MemoryDispatch.scala 91:31]
                io.fault.mtval <= io.data_addr @[MemoryDispatch.scala 92:22]
            else :
              skip
    data_in is invalid @[MemoryDispatch.scala 147:11]
    node _T_15 = eq(UInt<1>("h0"), io.data_width) @[MemoryDispatch.scala 148:25]
    when _T_15 : @[MemoryDispatch.scala 148:25]
      node value = bits(io.data_write, 7, 0) @[MemoryDispatch.scala 150:32]
      node _T_16 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 151:26]
      node _T_17 = eq(UInt<1>("h0"), _T_16) @[MemoryDispatch.scala 151:34]
      when _T_17 : @[MemoryDispatch.scala 151:34]
        node _data_in_T = bits(data_out, 31, 8) @[MemoryDispatch.scala 153:34]
        node _data_in_T_1 = cat(_data_in_T, value) @[Cat.scala 33:92]
        data_in <= _data_in_T_1 @[MemoryDispatch.scala 153:19]
      else :
        node _T_18 = eq(UInt<1>("h1"), _T_16) @[MemoryDispatch.scala 151:34]
        when _T_18 : @[MemoryDispatch.scala 151:34]
          node _data_in_T_2 = bits(data_out, 31, 16) @[MemoryDispatch.scala 156:34]
          node _data_in_T_3 = bits(data_out, 7, 0) @[MemoryDispatch.scala 156:59]
          node data_in_hi = cat(_data_in_T_2, value) @[Cat.scala 33:92]
          node _data_in_T_4 = cat(data_in_hi, _data_in_T_3) @[Cat.scala 33:92]
          data_in <= _data_in_T_4 @[MemoryDispatch.scala 156:19]
        else :
          node _T_19 = eq(UInt<2>("h2"), _T_16) @[MemoryDispatch.scala 151:34]
          when _T_19 : @[MemoryDispatch.scala 151:34]
            node _data_in_T_5 = bits(data_out, 31, 24) @[MemoryDispatch.scala 159:34]
            node _data_in_T_6 = bits(data_out, 15, 0) @[MemoryDispatch.scala 159:59]
            node data_in_hi_1 = cat(_data_in_T_5, value) @[Cat.scala 33:92]
            node _data_in_T_7 = cat(data_in_hi_1, _data_in_T_6) @[Cat.scala 33:92]
            data_in <= _data_in_T_7 @[MemoryDispatch.scala 159:19]
          else :
            node _T_20 = eq(UInt<2>("h3"), _T_16) @[MemoryDispatch.scala 151:34]
            when _T_20 : @[MemoryDispatch.scala 151:34]
              node _data_in_T_8 = bits(data_out, 23, 0) @[MemoryDispatch.scala 162:41]
              node _data_in_T_9 = cat(value, _data_in_T_8) @[Cat.scala 33:92]
              data_in <= _data_in_T_9 @[MemoryDispatch.scala 162:19]
    else :
      node _T_21 = eq(UInt<1>("h1"), io.data_width) @[MemoryDispatch.scala 148:25]
      when _T_21 : @[MemoryDispatch.scala 148:25]
        node value_1 = bits(io.data_write, 15, 0) @[MemoryDispatch.scala 167:32]
        node _T_22 = bits(io.data_addr, 1, 1) @[MemoryDispatch.scala 168:24]
        node _T_23 = eq(_T_22, UInt<1>("h0")) @[MemoryDispatch.scala 168:28]
        when _T_23 : @[MemoryDispatch.scala 168:40]
          node _data_in_T_10 = bits(data_out, 31, 16) @[MemoryDispatch.scala 169:32]
          node _data_in_T_11 = cat(_data_in_T_10, value_1) @[Cat.scala 33:92]
          data_in <= _data_in_T_11 @[MemoryDispatch.scala 169:17]
        else :
          node _data_in_T_12 = bits(data_out, 15, 0) @[MemoryDispatch.scala 171:39]
          node _data_in_T_13 = cat(value_1, _data_in_T_12) @[Cat.scala 33:92]
          data_in <= _data_in_T_13 @[MemoryDispatch.scala 171:17]
      else :
        node _T_24 = eq(UInt<2>("h2"), io.data_width) @[MemoryDispatch.scala 148:25]
        when _T_24 : @[MemoryDispatch.scala 148:25]
          data_in <= io.data_write @[MemoryDispatch.scala 175:15]
    io.data_out is invalid @[MemoryDispatch.scala 181:15]
    node _T_25 = eq(UInt<1>("h0"), io.data_width) @[MemoryDispatch.scala 182:25]
    when _T_25 : @[MemoryDispatch.scala 182:25]
      node _T_26 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 184:26]
      node _T_27 = eq(UInt<1>("h0"), _T_26) @[MemoryDispatch.scala 184:34]
      when _T_27 : @[MemoryDispatch.scala 184:34]
        node _high_bit_T = bits(data_out, 7, 7) @[MemoryDispatch.scala 186:65]
        node _high_bit_T_1 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T) @[MemoryDispatch.scala 186:38]
        node _high_bit_T_2 = bits(_high_bit_T_1, 0, 0) @[Bitwise.scala 77:15]
        node high_bit = mux(_high_bit_T_2, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
        node _io_data_out_T = bits(data_out, 7, 0) @[MemoryDispatch.scala 187:48]
        node _io_data_out_T_1 = cat(high_bit, _io_data_out_T) @[Cat.scala 33:92]
        io.data_out <= _io_data_out_T_1 @[MemoryDispatch.scala 187:23]
      else :
        node _T_28 = eq(UInt<1>("h1"), _T_26) @[MemoryDispatch.scala 184:34]
        when _T_28 : @[MemoryDispatch.scala 184:34]
          node _high_bit_T_3 = bits(data_out, 15, 15) @[MemoryDispatch.scala 190:65]
          node _high_bit_T_4 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_3) @[MemoryDispatch.scala 190:38]
          node _high_bit_T_5 = bits(_high_bit_T_4, 0, 0) @[Bitwise.scala 77:15]
          node high_bit_1 = mux(_high_bit_T_5, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
          node _io_data_out_T_2 = bits(data_out, 15, 8) @[MemoryDispatch.scala 191:48]
          node _io_data_out_T_3 = cat(high_bit_1, _io_data_out_T_2) @[Cat.scala 33:92]
          io.data_out <= _io_data_out_T_3 @[MemoryDispatch.scala 191:23]
        else :
          node _T_29 = eq(UInt<2>("h2"), _T_26) @[MemoryDispatch.scala 184:34]
          when _T_29 : @[MemoryDispatch.scala 184:34]
            node _high_bit_T_6 = bits(data_out, 23, 23) @[MemoryDispatch.scala 194:65]
            node _high_bit_T_7 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_6) @[MemoryDispatch.scala 194:38]
            node _high_bit_T_8 = bits(_high_bit_T_7, 0, 0) @[Bitwise.scala 77:15]
            node high_bit_2 = mux(_high_bit_T_8, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
            node _io_data_out_T_4 = bits(data_out, 23, 16) @[MemoryDispatch.scala 195:48]
            node _io_data_out_T_5 = cat(high_bit_2, _io_data_out_T_4) @[Cat.scala 33:92]
            io.data_out <= _io_data_out_T_5 @[MemoryDispatch.scala 195:23]
          else :
            node _T_30 = eq(UInt<2>("h3"), _T_26) @[MemoryDispatch.scala 184:34]
            when _T_30 : @[MemoryDispatch.scala 184:34]
              node _high_bit_T_9 = bits(data_out, 31, 31) @[MemoryDispatch.scala 198:65]
              node _high_bit_T_10 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_9) @[MemoryDispatch.scala 198:38]
              node _high_bit_T_11 = bits(_high_bit_T_10, 0, 0) @[Bitwise.scala 77:15]
              node high_bit_3 = mux(_high_bit_T_11, UInt<24>("hffffff"), UInt<24>("h0")) @[Bitwise.scala 77:12]
              node _io_data_out_T_6 = bits(data_out, 31, 24) @[MemoryDispatch.scala 199:48]
              node _io_data_out_T_7 = cat(high_bit_3, _io_data_out_T_6) @[Cat.scala 33:92]
              io.data_out <= _io_data_out_T_7 @[MemoryDispatch.scala 199:23]
    else :
      node _T_31 = eq(UInt<1>("h1"), io.data_width) @[MemoryDispatch.scala 182:25]
      when _T_31 : @[MemoryDispatch.scala 182:25]
        node _T_32 = bits(io.data_addr, 0, 0) @[MemoryDispatch.scala 205:24]
        node _T_33 = eq(_T_32, UInt<1>("h0")) @[MemoryDispatch.scala 205:28]
        when _T_33 : @[MemoryDispatch.scala 205:37]
          node _T_34 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 206:28]
          node _T_35 = eq(UInt<1>("h0"), _T_34) @[MemoryDispatch.scala 206:36]
          when _T_35 : @[MemoryDispatch.scala 206:36]
            node _high_bit_T_12 = bits(data_out, 15, 15) @[MemoryDispatch.scala 208:67]
            node _high_bit_T_13 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_12) @[MemoryDispatch.scala 208:40]
            node _high_bit_T_14 = bits(_high_bit_T_13, 0, 0) @[Bitwise.scala 77:15]
            node high_bit_4 = mux(_high_bit_T_14, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 77:12]
            node _io_data_out_T_8 = bits(data_out, 15, 0) @[MemoryDispatch.scala 209:50]
            node _io_data_out_T_9 = cat(high_bit_4, _io_data_out_T_8) @[Cat.scala 33:92]
            io.data_out <= _io_data_out_T_9 @[MemoryDispatch.scala 209:25]
          else :
            node _T_36 = eq(UInt<2>("h2"), _T_34) @[MemoryDispatch.scala 206:36]
            when _T_36 : @[MemoryDispatch.scala 206:36]
              node _high_bit_T_15 = bits(data_out, 31, 31) @[MemoryDispatch.scala 212:67]
              node _high_bit_T_16 = mux(io.unsigned, UInt<1>("h0"), _high_bit_T_15) @[MemoryDispatch.scala 212:40]
              node _high_bit_T_17 = bits(_high_bit_T_16, 0, 0) @[Bitwise.scala 77:15]
              node high_bit_5 = mux(_high_bit_T_17, UInt<16>("hffff"), UInt<16>("h0")) @[Bitwise.scala 77:12]
              node _io_data_out_T_10 = bits(data_out, 31, 16) @[MemoryDispatch.scala 213:50]
              node _io_data_out_T_11 = cat(high_bit_5, _io_data_out_T_10) @[Cat.scala 33:92]
              io.data_out <= _io_data_out_T_11 @[MemoryDispatch.scala 213:25]
        else :
          when io.read_data : @[MemoryDispatch.scala 217:28]
            node _T_37 = neq(io.cpu_state, UInt<2>("h3")) @[MemoryDispatch.scala 90:22]
            when _T_37 : @[MemoryDispatch.scala 90:57]
              io.fault.mem_fault_type <= UInt<2>("h3") @[MemoryDispatch.scala 91:31]
              io.fault.mtval <= io.data_addr @[MemoryDispatch.scala 92:22]
          when io.write_data : @[MemoryDispatch.scala 220:29]
            node _T_38 = neq(io.cpu_state, UInt<2>("h3")) @[MemoryDispatch.scala 90:22]
            when _T_38 : @[MemoryDispatch.scala 90:57]
              io.fault.mem_fault_type <= UInt<3>("h5") @[MemoryDispatch.scala 91:31]
              io.fault.mtval <= io.data_addr @[MemoryDispatch.scala 92:22]
      else :
        node _T_39 = eq(UInt<2>("h2"), io.data_width) @[MemoryDispatch.scala 182:25]
        when _T_39 : @[MemoryDispatch.scala 182:25]
          node _T_40 = bits(io.data_addr, 1, 0) @[MemoryDispatch.scala 226:24]
          node _T_41 = eq(_T_40, UInt<1>("h0")) @[MemoryDispatch.scala 226:31]
          when _T_41 : @[MemoryDispatch.scala 226:40]
            io.data_out <= data_out @[MemoryDispatch.scala 227:21]
          else :
            when io.read_data : @[MemoryDispatch.scala 230:28]
              node _T_42 = neq(io.cpu_state, UInt<2>("h3")) @[MemoryDispatch.scala 90:22]
              when _T_42 : @[MemoryDispatch.scala 90:57]
                io.fault.mem_fault_type <= UInt<2>("h3") @[MemoryDispatch.scala 91:31]
                io.fault.mtval <= io.data_addr @[MemoryDispatch.scala 92:22]
            when io.write_data : @[MemoryDispatch.scala 233:29]
              node _T_43 = neq(io.cpu_state, UInt<2>("h3")) @[MemoryDispatch.scala 90:22]
              when _T_43 : @[MemoryDispatch.scala 90:57]
                io.fault.mem_fault_type <= UInt<3>("h5") @[MemoryDispatch.scala 91:31]
                io.fault.mtval <= io.data_addr @[MemoryDispatch.scala 92:22]
    node _T_44 = bits(io.ins_addr, 1, 0) @[MemoryDispatch.scala 242:19]
    node _T_45 = neq(_T_44, UInt<1>("h0")) @[MemoryDispatch.scala 242:26]
    when _T_45 : @[MemoryDispatch.scala 242:35]
      io.fault.mem_fault_type <= UInt<1>("h1") @[MemoryDispatch.scala 243:29]
      io.fault.mtval <= io.pc @[MemoryDispatch.scala 244:20]
    else :
      node _T_46 = gt(io.ins_addr, UInt<16>("hffff")) @[MemoryDispatch.scala 245:26]
      when _T_46 : @[MemoryDispatch.scala 245:48]
        io.fault.mem_fault_type <= UInt<2>("h2") @[MemoryDispatch.scala 246:29]
        io.fault.mtval <= io.pc @[MemoryDispatch.scala 247:20]

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
      node _T_1 = eq(io.cpu_state, UInt<2>("h3")) @[UARTLoader.scala 50:25]
      when _T_1 : @[UARTLoader.scala 50:61]
        data_addr <= UInt<32>("hffffffff") @[UARTLoader.scala 51:19]
        cur_state <= UInt<2>("h1") @[UARTLoader.scala 52:19]
    else :
      node _T_2 = eq(UInt<2>("h1"), cur_state) @[UARTLoader.scala 47:21]
      when _T_2 : @[UARTLoader.scala 47:21]
        rxReadyReg <= UInt<1>("h0") @[UARTLoader.scala 56:17]
        node _T_3 = neq(io.cpu_state, UInt<2>("h3")) @[UARTLoader.scala 57:25]
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
          node _T_6 = neq(io.cpu_state, UInt<2>("h3")) @[UARTLoader.scala 67:25]
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
            node _T_9 = neq(io.cpu_state, UInt<2>("h3")) @[UARTLoader.scala 78:25]
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

    node _T = eq(io.cpu_state, UInt<2>("h3")) @[MemWriteSelector.scala 30:20]
    when _T : @[MemWriteSelector.scala 30:54]
      io.read_data <= io.uart_in.mem_read @[MemWriteSelector.scala 31:17]
      io.write_data <= io.uart_in.mem_write @[MemWriteSelector.scala 32:18]
      io.data_width <= io.uart_in.data_width @[MemWriteSelector.scala 33:18]
      io.data_addr <= io.uart_in.data_addr @[MemWriteSelector.scala 34:17]
      io.data_write <= io.uart_in.data_to_write @[MemWriteSelector.scala 35:18]
    else :
      io.read_data <= io.cpu_read_data @[MemWriteSelector.scala 37:17]
      io.write_data <= io.cpu_write_data @[MemWriteSelector.scala 38:18]
      io.data_width <= io.cpu_data_width @[MemWriteSelector.scala 39:18]
      io.data_addr <= io.cpu_data_addr @[MemWriteSelector.scala 40:17]
      io.data_write <= io.cpu_data_write @[MemWriteSelector.scala 41:18]

  module CPUState :
    input clock : Clock
    input reset : Reset
    output io : { flip fault_state : UInt<1>, cpu_state : UInt<2>, flip load_mode : UInt<1>}

    reg state : UInt, clock with :
      reset => (reset, UInt<1>("h1")) @[CPUState.scala 16:22]
    node _T = eq(io.load_mode, UInt<1>("h0")) @[CPUState.scala 20:8]
    when _T : @[CPUState.scala 20:23]
      io.cpu_state <= state @[CPUState.scala 21:18]
      node _T_1 = eq(state, UInt<2>("h3")) @[CPUState.scala 22:16]
      when _T_1 : @[CPUState.scala 22:52]
        state <= UInt<1>("h1") @[CPUState.scala 23:13]
      else :
        when io.fault_state : @[CPUState.scala 24:32]
          skip
        else :
          node _T_2 = eq(state, UInt<1>("h1")) @[CPUState.scala 29:22]
          when _T_2 : @[CPUState.scala 29:59]
            state <= UInt<1>("h0") @[CPUState.scala 30:13]
          else :
            state <= UInt<1>("h1") @[CPUState.scala 32:13]
    else :
      io.cpu_state <= UInt<2>("h3") @[CPUState.scala 36:18]
      state <= UInt<2>("h3") @[CPUState.scala 37:11]

  module PC :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip fault_occurs : UInt<1>, flip next_addr : UInt<32>, flip fault_write_PC : UInt<32>, addr : UInt<32>}

    reg pc : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[PC.scala 20:17]
    io.addr <= pc @[PC.scala 21:10]
    when io.fault_occurs : @[PC.scala 22:24]
      pc <= io.fault_write_PC @[PC.scala 26:7]
    else :
      node _T = eq(io.cpu_state, UInt<1>("h0")) @[PC.scala 27:26]
      when _T : @[PC.scala 27:59]
        pc <= io.next_addr @[PC.scala 28:7]
      else :
        skip

  module InstructionDecoder :
    input clock : Clock
    input reset : Reset
    output io : { flip instruction : UInt<32>, opcode : UInt<7>, rs1 : UInt<5>, rs2 : UInt<5>, rd : UInt<5>, func3 : UInt<3>, func7 : UInt<7>, raw_imm : UInt<20>, csr : UInt<12>}

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
    node _io_csr_T = bits(io.instruction, 31, 20) @[InstructionDecoder.scala 23:27]
    io.csr <= _io_csr_T @[InstructionDecoder.scala 23:10]
    io.raw_imm <= UInt<1>("h0") @[InstructionDecoder.scala 25:14]
    node _T = bits(io.opcode, 6, 0) @[InstructionDecoder.scala 27:19]
    node _T_1 = eq(UInt<6>("h33"), _T) @[InstructionDecoder.scala 27:27]
    when _T_1 : @[InstructionDecoder.scala 27:27]
      io.raw_imm <= UInt<1>("h0") @[InstructionDecoder.scala 29:18]
    else :
      node _T_2 = eq(UInt<5>("h13"), _T) @[InstructionDecoder.scala 27:27]
      when _T_2 : @[InstructionDecoder.scala 27:27]
        node _io_raw_imm_T = bits(io.instruction, 31, 20) @[InstructionDecoder.scala 32:49]
        node _io_raw_imm_T_1 = cat(UInt<8>("h0"), _io_raw_imm_T) @[Cat.scala 33:92]
        io.raw_imm <= _io_raw_imm_T_1 @[InstructionDecoder.scala 32:18]
      else :
        node _T_3 = eq(UInt<2>("h3"), _T) @[InstructionDecoder.scala 27:27]
        when _T_3 : @[InstructionDecoder.scala 27:27]
          node _io_raw_imm_T_2 = bits(io.instruction, 31, 20) @[InstructionDecoder.scala 35:49]
          node _io_raw_imm_T_3 = cat(UInt<8>("h0"), _io_raw_imm_T_2) @[Cat.scala 33:92]
          io.raw_imm <= _io_raw_imm_T_3 @[InstructionDecoder.scala 35:18]
        else :
          node _T_4 = eq(UInt<7>("h67"), _T) @[InstructionDecoder.scala 27:27]
          when _T_4 : @[InstructionDecoder.scala 27:27]
            node _io_raw_imm_T_4 = bits(io.instruction, 31, 20) @[InstructionDecoder.scala 38:49]
            node _io_raw_imm_T_5 = cat(UInt<8>("h0"), _io_raw_imm_T_4) @[Cat.scala 33:92]
            io.raw_imm <= _io_raw_imm_T_5 @[InstructionDecoder.scala 38:18]
          else :
            node _T_5 = eq(UInt<7>("h73"), _T) @[InstructionDecoder.scala 27:27]
            when _T_5 : @[InstructionDecoder.scala 27:27]
              node _io_raw_imm_T_6 = bits(io.instruction, 31, 20) @[InstructionDecoder.scala 41:49]
              node _io_raw_imm_T_7 = cat(UInt<8>("h0"), _io_raw_imm_T_6) @[Cat.scala 33:92]
              io.raw_imm <= _io_raw_imm_T_7 @[InstructionDecoder.scala 41:18]
            else :
              node _T_6 = eq(UInt<6>("h23"), _T) @[InstructionDecoder.scala 27:27]
              when _T_6 : @[InstructionDecoder.scala 27:27]
                node _io_raw_imm_T_8 = bits(io.instruction, 31, 25) @[InstructionDecoder.scala 44:39]
                node _io_raw_imm_T_9 = bits(io.instruction, 11, 7) @[InstructionDecoder.scala 44:63]
                node _io_raw_imm_T_10 = cat(_io_raw_imm_T_8, _io_raw_imm_T_9) @[Cat.scala 33:92]
                io.raw_imm <= _io_raw_imm_T_10 @[InstructionDecoder.scala 44:18]
              else :
                node _T_7 = eq(UInt<7>("h63"), _T) @[InstructionDecoder.scala 27:27]
                when _T_7 : @[InstructionDecoder.scala 27:27]
                  node _io_raw_imm_T_11 = bits(io.instruction, 31, 31) @[InstructionDecoder.scala 47:49]
                  node _io_raw_imm_T_12 = bits(io.instruction, 7, 7) @[InstructionDecoder.scala 47:73]
                  node _io_raw_imm_T_13 = bits(io.instruction, 30, 25) @[InstructionDecoder.scala 48:23]
                  node _io_raw_imm_T_14 = bits(io.instruction, 11, 8) @[InstructionDecoder.scala 48:47]
                  node io_raw_imm_lo = cat(_io_raw_imm_T_13, _io_raw_imm_T_14) @[Cat.scala 33:92]
                  node io_raw_imm_hi_hi = cat(UInt<8>("h0"), _io_raw_imm_T_11) @[Cat.scala 33:92]
                  node io_raw_imm_hi = cat(io_raw_imm_hi_hi, _io_raw_imm_T_12) @[Cat.scala 33:92]
                  node _io_raw_imm_T_15 = cat(io_raw_imm_hi, io_raw_imm_lo) @[Cat.scala 33:92]
                  io.raw_imm <= _io_raw_imm_T_15 @[InstructionDecoder.scala 47:18]
                else :
                  node _T_8 = eq(UInt<6>("h37"), _T) @[InstructionDecoder.scala 27:27]
                  when _T_8 : @[InstructionDecoder.scala 27:27]
                    node _io_raw_imm_T_16 = bits(io.instruction, 31, 12) @[InstructionDecoder.scala 51:35]
                    io.raw_imm <= _io_raw_imm_T_16 @[InstructionDecoder.scala 51:18]
                  else :
                    node _T_9 = eq(UInt<5>("h17"), _T) @[InstructionDecoder.scala 27:27]
                    when _T_9 : @[InstructionDecoder.scala 27:27]
                      node _io_raw_imm_T_17 = bits(io.instruction, 31, 12) @[InstructionDecoder.scala 54:35]
                      io.raw_imm <= _io_raw_imm_T_17 @[InstructionDecoder.scala 54:18]
                    else :
                      node _T_10 = eq(UInt<7>("h6f"), _T) @[InstructionDecoder.scala 27:27]
                      when _T_10 : @[InstructionDecoder.scala 27:27]
                        node _io_raw_imm_T_18 = bits(io.instruction, 31, 31) @[InstructionDecoder.scala 57:39]
                        node _io_raw_imm_T_19 = bits(io.instruction, 19, 12) @[InstructionDecoder.scala 57:63]
                        node _io_raw_imm_T_20 = bits(io.instruction, 20, 20) @[InstructionDecoder.scala 58:23]
                        node _io_raw_imm_T_21 = bits(io.instruction, 30, 21) @[InstructionDecoder.scala 58:47]
                        node io_raw_imm_lo_1 = cat(_io_raw_imm_T_20, _io_raw_imm_T_21) @[Cat.scala 33:92]
                        node io_raw_imm_hi_1 = cat(_io_raw_imm_T_18, _io_raw_imm_T_19) @[Cat.scala 33:92]
                        node _io_raw_imm_T_22 = cat(io_raw_imm_hi_1, io_raw_imm_lo_1) @[Cat.scala 33:92]
                        io.raw_imm <= _io_raw_imm_T_22 @[InstructionDecoder.scala 57:18]

  module ControlUnit :
    input clock : Clock
    input reset : Reset
    output io : { flip instruction : UInt<32>, flip opcode : UInt<7>, flip func3 : UInt<3>, flip func7 : UInt<7>, flip rs1 : UInt<5>, flip rs2 : UInt<5>, flip rd : UInt<5>, flip raw_imm : UInt<20>, flip csr : UInt<12>, flip cur_privilege : UInt<2>, rs1_out : UInt<5>, rs2_out : UInt<5>, rd_out : UInt<5>, raw_imm_out : UInt<20>, csr_out : UInt<12>, alu_type : UInt<4>, cmp_type : UInt<2>, unsigned : UInt<1>, nextPC_type : UInt<2>, regs_write : UInt<1>, imm_width_type : UInt<2>, operand2_type : UInt<1>, au_type : UInt<1>, write_back_type : UInt<3>, memory_read : UInt<1>, memory_write : UInt<1>, data_width : UInt<2>, csr_write : UInt<1>, operand1_type : UInt<1>, fault : { ins_fault_type : UInt<3>, mtval : UInt<32>}}

    io.fault.ins_fault_type <= UInt<1>("h0") @[ControlUnit.scala 52:27]
    io.fault.mtval <= UInt<1>("h0") @[ControlUnit.scala 53:18]
    io.csr_out <= io.csr @[ControlUnit.scala 55:14]
    io.rs1_out <= io.rs1 @[ControlUnit.scala 56:14]
    io.rs2_out <= io.rs2 @[ControlUnit.scala 57:14]
    io.rd_out <= io.rd @[ControlUnit.scala 58:13]
    io.raw_imm_out <= io.raw_imm @[ControlUnit.scala 59:18]
    io.alu_type is invalid @[ControlUnit.scala 61:15]
    io.cmp_type is invalid @[ControlUnit.scala 62:15]
    io.unsigned is invalid @[ControlUnit.scala 63:15]
    io.nextPC_type is invalid @[ControlUnit.scala 64:18]
    io.regs_write is invalid @[ControlUnit.scala 65:17]
    io.imm_width_type is invalid @[ControlUnit.scala 66:21]
    io.operand2_type is invalid @[ControlUnit.scala 67:20]
    io.au_type is invalid @[ControlUnit.scala 68:14]
    io.write_back_type is invalid @[ControlUnit.scala 69:22]
    io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 70:18]
    io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 71:19]
    io.data_width is invalid @[ControlUnit.scala 72:17]
    io.csr_write <= UInt<1>("h0") @[ControlUnit.scala 75:16]
    io.operand1_type <= UInt<1>("h0") @[ControlUnit.scala 76:20]
    node _T = eq(UInt<6>("h33"), io.opcode) @[ControlUnit.scala 78:21]
    when _T : @[ControlUnit.scala 78:21]
      io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 80:22]
      io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 81:21]
      io.au_type <= UInt<1>("h0") @[ControlUnit.scala 82:18]
      io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 83:22]
      io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 84:23]
      io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 85:24]
      io.write_back_type <= UInt<1>("h0") @[ControlUnit.scala 86:26]
      io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 87:19]
      node _T_1 = cat(io.func3, io.func7) @[Cat.scala 33:92]
      node _T_2 = eq(UInt<1>("h0"), _T_1) @[ControlUnit.scala 88:39]
      when _T_2 : @[ControlUnit.scala 88:39]
        io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 90:23]
      else :
        node _T_3 = eq(UInt<6>("h20"), _T_1) @[ControlUnit.scala 88:39]
        when _T_3 : @[ControlUnit.scala 88:39]
          io.alu_type <= UInt<1>("h1") @[ControlUnit.scala 93:23]
        else :
          node _T_4 = eq(UInt<10>("h200"), _T_1) @[ControlUnit.scala 88:39]
          when _T_4 : @[ControlUnit.scala 88:39]
            io.alu_type <= UInt<2>("h2") @[ControlUnit.scala 96:23]
          else :
            node _T_5 = eq(UInt<10>("h300"), _T_1) @[ControlUnit.scala 88:39]
            when _T_5 : @[ControlUnit.scala 88:39]
              io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 99:23]
            else :
              node _T_6 = eq(UInt<10>("h380"), _T_1) @[ControlUnit.scala 88:39]
              when _T_6 : @[ControlUnit.scala 88:39]
                io.alu_type <= UInt<3>("h4") @[ControlUnit.scala 102:23]
              else :
                node _T_7 = eq(UInt<8>("h80"), _T_1) @[ControlUnit.scala 88:39]
                when _T_7 : @[ControlUnit.scala 88:39]
                  io.alu_type <= UInt<3>("h5") @[ControlUnit.scala 105:23]
                else :
                  node _T_8 = eq(UInt<10>("h280"), _T_1) @[ControlUnit.scala 88:39]
                  when _T_8 : @[ControlUnit.scala 88:39]
                    io.alu_type <= UInt<3>("h6") @[ControlUnit.scala 108:23]
                  else :
                    node _T_9 = eq(UInt<10>("h2a0"), _T_1) @[ControlUnit.scala 88:39]
                    when _T_9 : @[ControlUnit.scala 88:39]
                      io.alu_type <= UInt<3>("h7") @[ControlUnit.scala 111:23]
                    else :
                      node _T_10 = eq(UInt<9>("h100"), _T_1) @[ControlUnit.scala 88:39]
                      when _T_10 : @[ControlUnit.scala 88:39]
                        io.au_type <= UInt<1>("h1") @[ControlUnit.scala 114:22]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 115:23]
                      else :
                        node _T_11 = eq(UInt<9>("h180"), _T_1) @[ControlUnit.scala 88:39]
                        when _T_11 : @[ControlUnit.scala 88:39]
                          io.au_type <= UInt<1>("h1") @[ControlUnit.scala 118:22]
                          io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 119:23]
                          io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 120:23]
    else :
      node _T_12 = eq(UInt<5>("h13"), io.opcode) @[ControlUnit.scala 78:21]
      when _T_12 : @[ControlUnit.scala 78:21]
        io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 125:22]
        io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 126:21]
        io.au_type <= UInt<1>("h0") @[ControlUnit.scala 127:18]
        io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 128:22]
        io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 129:23]
        io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 130:24]
        io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 131:25]
        io.write_back_type <= UInt<1>("h0") @[ControlUnit.scala 132:26]
        io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 133:19]
        node _T_13 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 134:24]
        when _T_13 : @[ControlUnit.scala 134:24]
          io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 136:23]
        else :
          node _T_14 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 134:24]
          when _T_14 : @[ControlUnit.scala 134:24]
            io.alu_type <= UInt<2>("h2") @[ControlUnit.scala 139:23]
          else :
            node _T_15 = eq(UInt<3>("h6"), io.func3) @[ControlUnit.scala 134:24]
            when _T_15 : @[ControlUnit.scala 134:24]
              io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 142:23]
            else :
              node _T_16 = eq(UInt<3>("h7"), io.func3) @[ControlUnit.scala 134:24]
              when _T_16 : @[ControlUnit.scala 134:24]
                io.alu_type <= UInt<3>("h4") @[ControlUnit.scala 145:23]
              else :
                node _T_17 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 134:24]
                when _T_17 : @[ControlUnit.scala 134:24]
                  io.alu_type <= UInt<3>("h5") @[ControlUnit.scala 148:23]
                else :
                  node _T_18 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 134:24]
                  when _T_18 : @[ControlUnit.scala 134:24]
                    node _T_19 = eq(UInt<1>("h0"), io.func7) @[ControlUnit.scala 151:28]
                    when _T_19 : @[ControlUnit.scala 151:28]
                      io.alu_type <= UInt<3>("h6") @[ControlUnit.scala 153:27]
                    else :
                      node _T_20 = eq(UInt<6>("h20"), io.func7) @[ControlUnit.scala 151:28]
                      when _T_20 : @[ControlUnit.scala 151:28]
                        io.alu_type <= UInt<3>("h7") @[ControlUnit.scala 156:27]
                  else :
                    node _T_21 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 134:24]
                    when _T_21 : @[ControlUnit.scala 134:24]
                      io.au_type <= UInt<1>("h1") @[ControlUnit.scala 161:22]
                      io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 162:23]
                    else :
                      node _T_22 = eq(UInt<2>("h3"), io.func3) @[ControlUnit.scala 134:24]
                      when _T_22 : @[ControlUnit.scala 134:24]
                        io.au_type <= UInt<1>("h1") @[ControlUnit.scala 165:22]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 166:23]
                        io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 167:23]
      else :
        node _T_23 = eq(UInt<2>("h3"), io.opcode) @[ControlUnit.scala 78:21]
        when _T_23 : @[ControlUnit.scala 78:21]
          io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 173:22]
          io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 174:21]
          io.au_type <= UInt<1>("h0") @[ControlUnit.scala 175:18]
          io.memory_read <= UInt<1>("h1") @[ControlUnit.scala 176:22]
          io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 177:23]
          io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 178:24]
          io.write_back_type <= UInt<1>("h1") @[ControlUnit.scala 179:26]
          io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 180:25]
          io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 181:19]
          io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 182:19]
          node _T_24 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 183:24]
          when _T_24 : @[ControlUnit.scala 183:24]
            io.data_width <= UInt<1>("h0") @[ControlUnit.scala 185:25]
          else :
            node _T_25 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 183:24]
            when _T_25 : @[ControlUnit.scala 183:24]
              io.data_width <= UInt<1>("h1") @[ControlUnit.scala 188:25]
            else :
              node _T_26 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 183:24]
              when _T_26 : @[ControlUnit.scala 183:24]
                io.data_width <= UInt<2>("h2") @[ControlUnit.scala 191:25]
              else :
                node _T_27 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 183:24]
                when _T_27 : @[ControlUnit.scala 183:24]
                  io.data_width <= UInt<1>("h0") @[ControlUnit.scala 194:25]
                  io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 195:23]
                else :
                  node _T_28 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 183:24]
                  when _T_28 : @[ControlUnit.scala 183:24]
                    io.data_width <= UInt<1>("h1") @[ControlUnit.scala 198:25]
                    io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 199:23]
        else :
          node _T_29 = eq(UInt<6>("h23"), io.opcode) @[ControlUnit.scala 78:21]
          when _T_29 : @[ControlUnit.scala 78:21]
            io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 205:22]
            io.regs_write <= UInt<1>("h0") @[ControlUnit.scala 206:21]
            io.au_type <= UInt<1>("h0") @[ControlUnit.scala 207:18]
            io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 208:22]
            io.memory_write <= UInt<1>("h1") @[ControlUnit.scala 209:23]
            io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 210:24]
            io.write_back_type is invalid @[ControlUnit.scala 211:26]
            io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 212:25]
            io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 213:19]
            io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 214:19]
            node _T_30 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 215:24]
            when _T_30 : @[ControlUnit.scala 215:24]
              io.data_width <= UInt<1>("h0") @[ControlUnit.scala 217:25]
            else :
              node _T_31 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 215:24]
              when _T_31 : @[ControlUnit.scala 215:24]
                io.data_width <= UInt<1>("h1") @[ControlUnit.scala 220:25]
              else :
                node _T_32 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 215:24]
                when _T_32 : @[ControlUnit.scala 215:24]
                  io.data_width <= UInt<2>("h2") @[ControlUnit.scala 223:25]
          else :
            node _T_33 = eq(UInt<7>("h63"), io.opcode) @[ControlUnit.scala 78:21]
            when _T_33 : @[ControlUnit.scala 78:21]
              io.nextPC_type <= UInt<1>("h1") @[ControlUnit.scala 229:22]
              io.regs_write <= UInt<1>("h0") @[ControlUnit.scala 230:21]
              io.au_type <= UInt<1>("h1") @[ControlUnit.scala 231:18]
              io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 232:22]
              io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 233:23]
              io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 234:24]
              io.write_back_type is invalid @[ControlUnit.scala 235:26]
              io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 236:19]
              io.imm_width_type <= UInt<1>("h1") @[ControlUnit.scala 237:25]
              io.alu_type is invalid @[ControlUnit.scala 238:19]
              node _T_34 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 239:24]
              when _T_34 : @[ControlUnit.scala 239:24]
                io.cmp_type <= UInt<2>("h2") @[ControlUnit.scala 241:23]
              else :
                node _T_35 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 239:24]
                when _T_35 : @[ControlUnit.scala 239:24]
                  io.cmp_type <= UInt<2>("h3") @[ControlUnit.scala 244:23]
                else :
                  node _T_36 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 239:24]
                  when _T_36 : @[ControlUnit.scala 239:24]
                    io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 247:23]
                  else :
                    node _T_37 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 239:24]
                    when _T_37 : @[ControlUnit.scala 239:24]
                      io.cmp_type <= UInt<1>("h1") @[ControlUnit.scala 250:23]
                    else :
                      node _T_38 = eq(UInt<3>("h6"), io.func3) @[ControlUnit.scala 239:24]
                      when _T_38 : @[ControlUnit.scala 239:24]
                        io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 253:23]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 254:23]
                      else :
                        node _T_39 = eq(UInt<3>("h7"), io.func3) @[ControlUnit.scala 239:24]
                        when _T_39 : @[ControlUnit.scala 239:24]
                          io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 257:23]
                          io.cmp_type <= UInt<1>("h1") @[ControlUnit.scala 258:23]
            else :
              node _T_40 = eq(UInt<7>("h6f"), io.opcode) @[ControlUnit.scala 78:21]
              when _T_40 : @[ControlUnit.scala 78:21]
                io.nextPC_type <= UInt<2>("h3") @[ControlUnit.scala 264:22]
                io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 265:21]
                io.au_type <= UInt<1>("h0") @[ControlUnit.scala 266:18]
                io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 267:22]
                io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 268:23]
                io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 269:24]
                io.write_back_type <= UInt<2>("h2") @[ControlUnit.scala 270:26]
                io.imm_width_type <= UInt<1>("h1") @[ControlUnit.scala 271:25]
                io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 272:19]
                io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 273:19]
              else :
                node _T_41 = eq(UInt<7>("h67"), io.opcode) @[ControlUnit.scala 78:21]
                when _T_41 : @[ControlUnit.scala 78:21]
                  io.nextPC_type <= UInt<2>("h2") @[ControlUnit.scala 277:22]
                  io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 278:21]
                  io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 279:22]
                  io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 280:23]
                  io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 281:24]
                  io.write_back_type <= UInt<2>("h2") @[ControlUnit.scala 282:26]
                  io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 283:25]
                  io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 284:19]
                  io.au_type <= UInt<1>("h0") @[ControlUnit.scala 285:18]
                  io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 286:19]
                else :
                  node _T_42 = eq(UInt<6>("h37"), io.opcode) @[ControlUnit.scala 78:21]
                  when _T_42 : @[ControlUnit.scala 78:21]
                    io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 290:22]
                    io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 291:21]
                    io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 292:22]
                    io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 293:23]
                    io.imm_width_type <= UInt<2>("h3") @[ControlUnit.scala 294:25]
                    io.write_back_type <= UInt<2>("h3") @[ControlUnit.scala 295:26]
                    io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 296:19]
                    io.au_type is invalid @[ControlUnit.scala 297:18]
                    io.alu_type is invalid @[ControlUnit.scala 298:19]
                  else :
                    node _T_43 = eq(UInt<5>("h17"), io.opcode) @[ControlUnit.scala 78:21]
                    when _T_43 : @[ControlUnit.scala 78:21]
                      io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 302:22]
                      io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 303:21]
                      io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 304:22]
                      io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 305:23]
                      io.imm_width_type <= UInt<2>("h3") @[ControlUnit.scala 306:25]
                      io.write_back_type <= UInt<3>("h4") @[ControlUnit.scala 307:26]
                      io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 308:19]
                      io.au_type is invalid @[ControlUnit.scala 309:18]
                      io.alu_type is invalid @[ControlUnit.scala 310:19]
                    else :
                      node _T_44 = eq(UInt<7>("h73"), io.opcode) @[ControlUnit.scala 78:21]
                      when _T_44 : @[ControlUnit.scala 78:21]
                        io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 313:19]
                        io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 314:22]
                        io.au_type <= UInt<1>("h0") @[ControlUnit.scala 315:18]
                        io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 316:25]
                        io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 317:21]
                        io.write_back_type <= UInt<3>("h5") @[ControlUnit.scala 318:26]
                        io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 319:23]
                        io.csr_write <= UInt<1>("h1") @[ControlUnit.scala 320:20]
                        io.operand1_type <= UInt<1>("h1") @[ControlUnit.scala 321:24]
                        io.rs2_out <= io.rs1 @[ControlUnit.scala 322:18]
                        node _T_45 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 324:24]
                        when _T_45 : @[ControlUnit.scala 324:24]
                          node _T_46 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 326:33]
                          when _T_46 : @[ControlUnit.scala 326:46]
                            io.fault.ins_fault_type <= UInt<1>("h1") @[ControlUnit.scala 327:37]
                            io.fault.mtval <= io.instruction @[ControlUnit.scala 328:28]
                          else :
                            io.operand1_type <= UInt<1>("h0") @[ControlUnit.scala 330:30]
                            io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 331:24]
                            io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 332:25]
                            io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 333:30]
                        else :
                          node _T_47 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 324:24]
                          when _T_47 : @[ControlUnit.scala 324:24]
                            node _T_48 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 337:33]
                            when _T_48 : @[ControlUnit.scala 337:46]
                              io.fault.ins_fault_type <= UInt<1>("h1") @[ControlUnit.scala 338:37]
                              io.fault.mtval <= io.instruction @[ControlUnit.scala 339:28]
                            else :
                              io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 341:25]
                              io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 342:30]
                          else :
                            node _T_49 = eq(UInt<2>("h3"), io.func3) @[ControlUnit.scala 324:24]
                            when _T_49 : @[ControlUnit.scala 324:24]
                              node _T_50 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 346:33]
                              when _T_50 : @[ControlUnit.scala 346:46]
                                io.fault.ins_fault_type <= UInt<1>("h1") @[ControlUnit.scala 347:37]
                                io.fault.mtval <= io.instruction @[ControlUnit.scala 348:28]
                              else :
                                io.alu_type <= UInt<4>("h8") @[ControlUnit.scala 350:25]
                                io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 351:30]
                            else :
                              node _T_51 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 324:24]
                              when _T_51 : @[ControlUnit.scala 324:24]
                                node _T_52 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 355:33]
                                when _T_52 : @[ControlUnit.scala 355:46]
                                  io.fault.ins_fault_type <= UInt<1>("h1") @[ControlUnit.scala 356:37]
                                  io.fault.mtval <= io.instruction @[ControlUnit.scala 357:28]
                                else :
                                  io.operand1_type <= UInt<1>("h0") @[ControlUnit.scala 359:30]
                                  io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 360:24]
                                  node _io_raw_imm_out_T = cat(UInt<27>("h0"), io.rs1) @[Cat.scala 33:92]
                                  io.raw_imm_out <= _io_raw_imm_out_T @[ControlUnit.scala 361:27]
                                  io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 362:25]
                                  io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 363:30]
                              else :
                                node _T_53 = eq(UInt<3>("h6"), io.func3) @[ControlUnit.scala 324:24]
                                when _T_53 : @[ControlUnit.scala 324:24]
                                  node _T_54 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 367:33]
                                  when _T_54 : @[ControlUnit.scala 367:46]
                                    io.fault.ins_fault_type <= UInt<1>("h1") @[ControlUnit.scala 368:37]
                                    io.fault.mtval <= io.instruction @[ControlUnit.scala 369:28]
                                  else :
                                    node _io_raw_imm_out_T_1 = cat(UInt<27>("h0"), io.rs1) @[Cat.scala 33:92]
                                    io.raw_imm_out <= _io_raw_imm_out_T_1 @[ControlUnit.scala 371:27]
                                    io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 372:25]
                                    io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 373:30]
                                else :
                                  node _T_55 = eq(UInt<3>("h7"), io.func3) @[ControlUnit.scala 324:24]
                                  when _T_55 : @[ControlUnit.scala 324:24]
                                    node _T_56 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 377:33]
                                    when _T_56 : @[ControlUnit.scala 377:46]
                                      io.fault.ins_fault_type <= UInt<1>("h1") @[ControlUnit.scala 378:37]
                                      io.fault.mtval <= io.instruction @[ControlUnit.scala 379:28]
                                    else :
                                      node _io_raw_imm_out_T_2 = cat(UInt<27>("h0"), io.rs1) @[Cat.scala 33:92]
                                      io.raw_imm_out <= _io_raw_imm_out_T_2 @[ControlUnit.scala 381:27]
                                      io.alu_type <= UInt<4>("h8") @[ControlUnit.scala 382:25]
                                      io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 383:30]
                                  else :
                                    node _T_57 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 324:24]
                                    when _T_57 : @[ControlUnit.scala 324:24]
                                      node _T_58 = eq(UInt<1>("h0"), io.raw_imm) @[ControlUnit.scala 387:30]
                                      when _T_58 : @[ControlUnit.scala 387:30]
                                        io.fault.ins_fault_type <= UInt<2>("h3") @[ControlUnit.scala 389:39]
                                      else :
                                        node _T_59 = eq(UInt<1>("h1"), io.raw_imm) @[ControlUnit.scala 387:30]
                                        when _T_59 : @[ControlUnit.scala 387:30]
                                          io.fault.ins_fault_type <= UInt<2>("h2") @[ControlUnit.scala 392:39]
                                        else :
                                          node _T_60 = eq(UInt<10>("h302"), io.raw_imm) @[ControlUnit.scala 387:30]
                                          when _T_60 : @[ControlUnit.scala 387:30]
                                            node _T_61 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 395:37]
                                            when _T_61 : @[ControlUnit.scala 395:50]
                                              io.fault.ins_fault_type <= UInt<1>("h1") @[ControlUnit.scala 396:41]
                                              io.fault.mtval <= io.instruction @[ControlUnit.scala 397:32]
                                            else :
                                              io.fault.ins_fault_type <= UInt<3>("h4") @[ControlUnit.scala 399:41]

  module Registers :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip fault_occurs : UInt<1>, flip write : UInt<1>, flip rs1 : UInt<5>, flip rs2 : UInt<5>, flip rd : UInt<5>, flip write_data : UInt<32>, rs1_val : UInt<32>, rs2_val : UInt<32>}

    wire _regs_WIRE : UInt<32>[32] @[Registers.scala 24:29]
    _regs_WIRE[0] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[1] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[2] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[3] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[4] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[5] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[6] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[7] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[8] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[9] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[10] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[11] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[12] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[13] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[14] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[15] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[16] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[17] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[18] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[19] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[20] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[21] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[22] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[23] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[24] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[25] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[26] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[27] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[28] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[29] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[30] <= UInt<32>("h0") @[Registers.scala 24:29]
    _regs_WIRE[31] <= UInt<32>("h0") @[Registers.scala 24:29]
    reg regs : UInt<32>[32], clock with :
      reset => (reset, _regs_WIRE) @[Registers.scala 24:21]
    io.rs1_val <= regs[io.rs1] @[Registers.scala 25:14]
    io.rs2_val <= regs[io.rs2] @[Registers.scala 26:14]
    node _T = eq(io.cpu_state, UInt<1>("h1")) @[Registers.scala 27:21]
    node _T_1 = and(_T, io.write) @[Registers.scala 28:5]
    node _T_2 = neq(io.rd, UInt<1>("h0")) @[Registers.scala 29:14]
    node _T_3 = and(_T_1, _T_2) @[Registers.scala 29:5]
    node _T_4 = eq(io.fault_occurs, UInt<1>("h0")) @[Registers.scala 30:9]
    node _T_5 = and(_T_3, _T_4) @[Registers.scala 30:5]
    when _T_5 : @[Registers.scala 31:5]
      regs[io.rd] <= io.write_data @[Registers.scala 37:17]
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
    output io : { flip operand2Type : UInt<1>, flip operand1Type : UInt<1>, flip rs1_val : UInt<32>, flip rs2_val : UInt<32>, flip real_imm : UInt<32>, flip csr_val : UInt<32>, operand1 : UInt<32>, operand2 : UInt<32>}

    io.operand1 is invalid @[OperandSelector.scala 20:15]
    node _T = eq(UInt<1>("h0"), io.operand1Type) @[OperandSelector.scala 21:26]
    when _T : @[OperandSelector.scala 21:26]
      io.operand1 <= io.rs1_val @[OperandSelector.scala 23:18]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.operand1Type) @[OperandSelector.scala 21:26]
      when _T_1 : @[OperandSelector.scala 21:26]
        io.operand1 <= io.csr_val @[OperandSelector.scala 27:18]
    io.operand2 is invalid @[OperandSelector.scala 31:15]
    node _T_2 = eq(UInt<1>("h0"), io.operand2Type) @[OperandSelector.scala 32:27]
    when _T_2 : @[OperandSelector.scala 32:27]
      io.operand2 <= io.real_imm @[OperandSelector.scala 34:19]
    else :
      node _T_3 = eq(UInt<1>("h1"), io.operand2Type) @[OperandSelector.scala 32:27]
      when _T_3 : @[OperandSelector.scala 32:27]
        io.operand2 <= io.rs2_val @[OperandSelector.scala 37:19]

  module ALU :
    input clock : Clock
    input reset : Reset
    output io : { flip operand1 : UInt<32>, flip operand2 : UInt<32>, flip alu_op : UInt<4>, flip unsigned : UInt<1>, result : UInt<32>}

    io.result is invalid @[ALU.scala 15:13]
    node _T = eq(UInt<1>("h0"), io.alu_op) @[ALU.scala 16:21]
    when _T : @[ALU.scala 16:21]
      node _io_result_T = add(io.operand1, io.operand2) @[ALU.scala 18:32]
      node _io_result_T_1 = tail(_io_result_T, 1) @[ALU.scala 18:32]
      io.result <= _io_result_T_1 @[ALU.scala 18:17]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.alu_op) @[ALU.scala 16:21]
      when _T_1 : @[ALU.scala 16:21]
        node _io_result_T_2 = sub(io.operand1, io.operand2) @[ALU.scala 21:32]
        node _io_result_T_3 = tail(_io_result_T_2, 1) @[ALU.scala 21:32]
        io.result <= _io_result_T_3 @[ALU.scala 21:17]
      else :
        node _T_2 = eq(UInt<2>("h2"), io.alu_op) @[ALU.scala 16:21]
        when _T_2 : @[ALU.scala 16:21]
          node _io_result_T_4 = xor(io.operand1, io.operand2) @[ALU.scala 24:32]
          io.result <= _io_result_T_4 @[ALU.scala 24:17]
        else :
          node _T_3 = eq(UInt<2>("h3"), io.alu_op) @[ALU.scala 16:21]
          when _T_3 : @[ALU.scala 16:21]
            node _io_result_T_5 = or(io.operand1, io.operand2) @[ALU.scala 27:32]
            io.result <= _io_result_T_5 @[ALU.scala 27:17]
          else :
            node _T_4 = eq(UInt<3>("h4"), io.alu_op) @[ALU.scala 16:21]
            when _T_4 : @[ALU.scala 16:21]
              node _io_result_T_6 = and(io.operand1, io.operand2) @[ALU.scala 30:32]
              io.result <= _io_result_T_6 @[ALU.scala 30:17]
            else :
              node _T_5 = eq(UInt<3>("h5"), io.alu_op) @[ALU.scala 16:21]
              when _T_5 : @[ALU.scala 16:21]
                node _io_result_T_7 = bits(io.operand2, 4, 0) @[ALU.scala 33:46]
                node _io_result_T_8 = dshl(io.operand1, _io_result_T_7) @[ALU.scala 33:32]
                io.result <= _io_result_T_8 @[ALU.scala 33:17]
              else :
                node _T_6 = eq(UInt<3>("h7"), io.alu_op) @[ALU.scala 16:21]
                when _T_6 : @[ALU.scala 16:21]
                  node _T_7 = eq(io.unsigned, UInt<1>("h0")) @[ALU.scala 36:12]
                  when _T_7 : @[ALU.scala 36:26]
                    node _io_result_T_9 = asSInt(io.operand1) @[ALU.scala 37:35]
                    node _io_result_T_10 = bits(io.operand2, 4, 0) @[ALU.scala 37:56]
                    node _io_result_T_11 = dshr(_io_result_T_9, _io_result_T_10) @[ALU.scala 37:42]
                    node _io_result_T_12 = asUInt(_io_result_T_11) @[ALU.scala 37:71]
                    io.result <= _io_result_T_12 @[ALU.scala 37:19]
                  else :
                    node _io_result_T_13 = bits(io.operand2, 4, 0) @[ALU.scala 39:48]
                    node _io_result_T_14 = dshr(io.operand1, _io_result_T_13) @[ALU.scala 39:34]
                    io.result <= _io_result_T_14 @[ALU.scala 39:19]
                else :
                  node _T_8 = eq(UInt<3>("h6"), io.alu_op) @[ALU.scala 16:21]
                  when _T_8 : @[ALU.scala 16:21]
                    node _io_result_T_15 = bits(io.operand2, 4, 0) @[ALU.scala 43:47]
                    node _io_result_T_16 = dshr(io.operand1, _io_result_T_15) @[ALU.scala 43:33]
                    io.result <= _io_result_T_16 @[ALU.scala 43:17]
                  else :
                    node _T_9 = eq(UInt<4>("h8"), io.alu_op) @[ALU.scala 16:21]
                    when _T_9 : @[ALU.scala 16:21]
                      node _io_result_T_17 = eq(io.operand2, UInt<1>("h0")) @[ALU.scala 46:21]
                      node _io_result_T_18 = and(_io_result_T_17, io.operand1) @[ALU.scala 46:35]
                      io.result <= _io_result_T_18 @[ALU.scala 46:17]

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
        else :
          node _T_3 = eq(UInt<2>("h3"), io.nextPC_type) @[NextPCGen.scala 23:25]
          when _T_3 : @[NextPCGen.scala 23:25]
            node _io_nextPC_T_7 = add(io.pc, io.imm) @[NextPCGen.scala 39:26]
            node _io_nextPC_T_8 = tail(_io_nextPC_T_7, 1) @[NextPCGen.scala 39:26]
            io.nextPC <= _io_nextPC_T_8 @[NextPCGen.scala 39:17]
            node _io_pcImm_T_6 = add(io.pc, io.imm) @[NextPCGen.scala 40:25]
            node _io_pcImm_T_7 = tail(_io_pcImm_T_6, 1) @[NextPCGen.scala 40:25]
            io.pcImm <= _io_pcImm_T_7 @[NextPCGen.scala 40:16]

  module WriteDataSelector :
    input clock : Clock
    input reset : Reset
    output io : { flip write_back_type : UInt<3>, flip imm : UInt<32>, flip mem_out : UInt<32>, flip au_out : UInt<32>, flip pc4 : UInt<32>, flip pcImm : UInt<32>, flip csr : UInt<32>, write_data : UInt<32>}

    io.write_data is invalid @[WriteDataSelector.scala 19:17]
    node _T = eq(UInt<1>("h0"), io.write_back_type) @[WriteDataSelector.scala 21:30]
    when _T : @[WriteDataSelector.scala 21:30]
      io.write_data <= io.au_out @[WriteDataSelector.scala 23:21]
    else :
      node _T_1 = eq(UInt<2>("h3"), io.write_back_type) @[WriteDataSelector.scala 21:30]
      when _T_1 : @[WriteDataSelector.scala 21:30]
        io.write_data <= io.imm @[WriteDataSelector.scala 26:21]
      else :
        node _T_2 = eq(UInt<1>("h1"), io.write_back_type) @[WriteDataSelector.scala 21:30]
        when _T_2 : @[WriteDataSelector.scala 21:30]
          io.write_data <= io.mem_out @[WriteDataSelector.scala 29:21]
        else :
          node _T_3 = eq(UInt<2>("h2"), io.write_back_type) @[WriteDataSelector.scala 21:30]
          when _T_3 : @[WriteDataSelector.scala 21:30]
            io.write_data <= io.pc4 @[WriteDataSelector.scala 32:21]
          else :
            node _T_4 = eq(UInt<3>("h4"), io.write_back_type) @[WriteDataSelector.scala 21:30]
            when _T_4 : @[WriteDataSelector.scala 21:30]
              io.write_data <= io.pcImm @[WriteDataSelector.scala 35:21]
            else :
              node _T_5 = eq(UInt<3>("h5"), io.write_back_type) @[WriteDataSelector.scala 21:30]
              when _T_5 : @[WriteDataSelector.scala 21:30]
                io.write_data <= io.csr @[WriteDataSelector.scala 38:21]

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
    output io : { flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}, vga : { flip value : UInt<32>}}, flip external_signal : { load_data_mode : UInt<1>}}

    inst CSR of CSR @[CoreTop.scala 21:19]
    CSR.clock <= clock
    CSR.reset <= reset
    inst PLIC of PLIC @[CoreTop.scala 24:20]
    PLIC.clock <= clock
    PLIC.reset <= reset
    inst memory of MemoryDispatch @[CoreTop.scala 27:22]
    memory.clock <= clock
    memory.reset <= reset
    inst uartLoader of UARTLoader @[CoreTop.scala 28:26]
    uartLoader.clock <= clock
    uartLoader.reset <= reset
    inst memInSelector of MemWriteSelector @[CoreTop.scala 29:29]
    memInSelector.clock <= clock
    memInSelector.reset <= reset
    inst state of CPUState @[CoreTop.scala 34:21]
    state.clock <= clock
    state.reset <= reset
    inst pc of PC @[CoreTop.scala 38:18]
    pc.clock <= clock
    pc.reset <= reset
    inst insDecode of InstructionDecoder @[CoreTop.scala 41:25]
    insDecode.clock <= clock
    insDecode.reset <= reset
    inst CU of ControlUnit @[CoreTop.scala 42:18]
    CU.clock <= clock
    CU.reset <= reset
    inst regs of Registers @[CoreTop.scala 45:20]
    regs.clock <= clock
    regs.reset <= reset
    inst immGen of ImmGen @[CoreTop.scala 46:22]
    immGen.clock <= clock
    immGen.reset <= reset
    inst operandSelector of OperandSelector @[CoreTop.scala 47:31]
    operandSelector.clock <= clock
    operandSelector.reset <= reset
    inst ALU of ALU @[CoreTop.scala 48:19]
    ALU.clock <= clock
    ALU.reset <= reset
    inst CMP of CMP @[CoreTop.scala 49:19]
    CMP.clock <= clock
    CMP.reset <= reset
    inst nextPCGen of NextPCGen @[CoreTop.scala 50:25]
    nextPCGen.clock <= clock
    nextPCGen.reset <= reset
    uartLoader.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 53:27]
    uartLoader.io.rxValid <= io.external.uart.rxValid @[CoreTop.scala 54:25]
    uartLoader.io.rxData <= io.external.uart.rxData @[CoreTop.scala 55:24]
    io.external.uart.rxReady <= uartLoader.io.rxReady @[CoreTop.scala 56:28]
    memInSelector.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 61:30]
    memInSelector.io.uart_in <= uartLoader.io.mem @[CoreTop.scala 62:28]
    memInSelector.io.cpu_read_data <= CU.io.memory_read @[CoreTop.scala 64:34]
    memInSelector.io.cpu_write_data <= CU.io.memory_write @[CoreTop.scala 65:35]
    memInSelector.io.cpu_data_width <= CU.io.data_width @[CoreTop.scala 66:35]
    memInSelector.io.cpu_data_addr <= ALU.io.result @[CoreTop.scala 67:34]
    memInSelector.io.cpu_data_write <= regs.io.rs2_val @[CoreTop.scala 68:35]
    inst writeDataSelector of WriteDataSelector @[CoreTop.scala 71:33]
    writeDataSelector.clock <= clock
    writeDataSelector.reset <= reset
    inst auSelector of AUSelector @[CoreTop.scala 72:26]
    auSelector.clock <= clock
    auSelector.reset <= reset
    state.io.load_mode <= io.external_signal.load_data_mode @[CoreTop.scala 75:22]
    CSR.io.mem_fault <= memory.io.fault @[CoreTop.scala 78:20]
    CSR.io.ins_fault <= CU.io.fault @[CoreTop.scala 79:20]
    CSR.io.io_interruption <= PLIC.io.interruption @[CoreTop.scala 80:26]
    CSR.io.pc <= pc.io.addr @[CoreTop.scala 81:13]
    state.io.fault_state <= CSR.io.fault_state @[CoreTop.scala 83:24]
    pc.io.fault_write_PC <= CSR.io.fault_write_PC @[CoreTop.scala 84:24]
    CU.io.instruction <= memory.io.ins_out @[CoreTop.scala 86:21]
    memory.io.pc <= pc.io.addr @[CoreTop.scala 87:16]
    pc.io.fault_occurs <= CSR.io.fault_state @[CoreTop.scala 88:22]
    memory.io.fault_occurs <= CSR.io.fault_state @[CoreTop.scala 89:26]
    regs.io.fault_occurs <= CSR.io.fault_state @[CoreTop.scala 90:24]
    pc.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 94:19]
    pc.io.next_addr <= nextPCGen.io.nextPC @[CoreTop.scala 95:19]
    pc.io.fault_write_PC <= CSR.io.fault_write_PC @[CoreTop.scala 96:24]
    memory.io.ins_addr <= pc.io.addr @[CoreTop.scala 97:22]
    insDecode.io.instruction <= memory.io.ins_out @[CoreTop.scala 100:28]
    CU.io.csr <= insDecode.io.csr @[CoreTop.scala 101:13]
    CU.io.opcode <= insDecode.io.opcode @[CoreTop.scala 102:16]
    CU.io.func3 <= insDecode.io.func3 @[CoreTop.scala 103:15]
    CU.io.func7 <= insDecode.io.func7 @[CoreTop.scala 104:15]
    CU.io.rs1 <= insDecode.io.rs1 @[CoreTop.scala 105:13]
    CU.io.rs2 <= insDecode.io.rs2 @[CoreTop.scala 106:13]
    CU.io.rd <= insDecode.io.rd @[CoreTop.scala 107:12]
    CU.io.raw_imm <= insDecode.io.raw_imm @[CoreTop.scala 108:17]
    CU.io.cur_privilege <= CSR.io.cur_privilege @[CoreTop.scala 109:23]
    CSR.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 112:20]
    CSR.io.csr <= CU.io.csr_out @[CoreTop.scala 113:14]
    regs.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 116:21]
    regs.io.rs1 <= CU.io.rs1_out @[CoreTop.scala 117:15]
    regs.io.rs2 <= CU.io.rs2_out @[CoreTop.scala 118:15]
    regs.io.rd <= CU.io.rd_out @[CoreTop.scala 119:14]
    regs.io.write <= CU.io.regs_write @[CoreTop.scala 120:17]
    regs.io.write_data <= writeDataSelector.io.write_data @[CoreTop.scala 121:22]
    immGen.io.raw_imm <= CU.io.raw_imm_out @[CoreTop.scala 124:21]
    immGen.io.unsigned <= CU.io.unsigned @[CoreTop.scala 125:22]
    immGen.io.imm_width <= CU.io.imm_width_type @[CoreTop.scala 126:23]
    operandSelector.io.csr_val <= CSR.io.csr_val @[CoreTop.scala 129:30]
    operandSelector.io.rs1_val <= regs.io.rs1_val @[CoreTop.scala 130:30]
    operandSelector.io.rs2_val <= regs.io.rs2_val @[CoreTop.scala 131:30]
    operandSelector.io.real_imm <= immGen.io.real_imm @[CoreTop.scala 132:31]
    operandSelector.io.operand2Type <= CU.io.operand2_type @[CoreTop.scala 133:35]
    operandSelector.io.operand1Type <= CU.io.operand1_type @[CoreTop.scala 134:35]
    ALU.io.operand1 <= operandSelector.io.operand1 @[CoreTop.scala 137:19]
    ALU.io.operand2 <= operandSelector.io.operand2 @[CoreTop.scala 138:19]
    ALU.io.alu_op <= CU.io.alu_type @[CoreTop.scala 139:17]
    ALU.io.unsigned <= CU.io.unsigned @[CoreTop.scala 140:19]
    CMP.io.operand1 <= operandSelector.io.operand1 @[CoreTop.scala 143:19]
    CMP.io.operand2 <= operandSelector.io.operand2 @[CoreTop.scala 144:19]
    CMP.io.cmp_op <= CU.io.cmp_type @[CoreTop.scala 145:17]
    CMP.io.unsigned <= CU.io.unsigned @[CoreTop.scala 146:19]
    nextPCGen.io.cmp_result <= CMP.io.result @[CoreTop.scala 149:27]
    nextPCGen.io.alu_result <= ALU.io.result @[CoreTop.scala 150:27]
    nextPCGen.io.imm <= immGen.io.real_imm @[CoreTop.scala 151:20]
    nextPCGen.io.pc <= pc.io.addr @[CoreTop.scala 152:19]
    nextPCGen.io.nextPC_type <= CU.io.nextPC_type @[CoreTop.scala 153:28]
    memory.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 156:23]
    memory.io.data_addr <= memInSelector.io.data_addr @[CoreTop.scala 157:23]
    memory.io.data_write <= memInSelector.io.data_write @[CoreTop.scala 158:24]
    memory.io.data_width <= memInSelector.io.data_width @[CoreTop.scala 159:24]
    memory.io.write_data <= memInSelector.io.write_data @[CoreTop.scala 160:24]
    memory.io.read_data <= memInSelector.io.read_data @[CoreTop.scala 161:23]
    memory.io.unsigned <= CU.io.unsigned @[CoreTop.scala 162:22]
    memory.io.external <= io.external @[CoreTop.scala 163:22]
    CSR.io.write <= CU.io.csr_write @[CoreTop.scala 167:16]
    CSR.io.write_data <= ALU.io.result @[CoreTop.scala 168:21]
    auSelector.io.alu_result <= ALU.io.result @[CoreTop.scala 171:28]
    auSelector.io.cmp_result <= CMP.io.result @[CoreTop.scala 172:28]
    auSelector.io.au_type <= CU.io.au_type @[CoreTop.scala 173:25]
    writeDataSelector.io.imm <= immGen.io.real_imm @[CoreTop.scala 176:28]
    writeDataSelector.io.mem_out <= memory.io.data_out @[CoreTop.scala 177:32]
    writeDataSelector.io.au_out <= auSelector.io.au_out @[CoreTop.scala 178:31]
    writeDataSelector.io.pc4 <= nextPCGen.io.pc4 @[CoreTop.scala 179:28]
    writeDataSelector.io.pcImm <= nextPCGen.io.pcImm @[CoreTop.scala 180:30]
    writeDataSelector.io.write_back_type <= CU.io.write_back_type @[CoreTop.scala 181:40]
    writeDataSelector.io.csr <= CSR.io.csr_val @[CoreTop.scala 182:28]
    regs.io.write <= CU.io.regs_write @[CoreTop.scala 185:17]
    regs.io.write_data <= writeDataSelector.io.write_data @[CoreTop.scala 186:22]
    node _T = eq(state.io.cpu_state, UInt<2>("h3")) @[CoreTop.scala 188:27]
    when _T : @[CoreTop.scala 188:63]
      io.external.uart.rxReady <= uartLoader.io.rxReady @[CoreTop.scala 191:30]
    node _T_1 = neq(state.io.cpu_state, UInt<2>("h3")) @[CoreTop.scala 205:27]
    node _T_2 = or(_T_1, memInSelector.io.write_data) @[CoreTop.scala 205:62]
    when _T_2 : @[CoreTop.scala 205:94]
      skip

  module Led :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { flip led : UInt<32>}, board : { led : UInt<24>}}

    io.board.led <= io.mmio.led @[Led.scala 20:16]

  module HexToSeg7 :
    input clock : Clock
    input reset : Reset
    output io : { flip hexDigit : UInt<4>, flip special : UInt<2>, flip counter : UInt<3>, seg7 : UInt<8>}

    node _T = eq(io.special, UInt<1>("h0")) @[Seg7.scala 140:20]
    when _T : @[Seg7.scala 140:29]
      wire io_seg7_seg7 : UInt<8> @[Seg7.scala 76:20]
      node _io_seg7_seg7_T = eq(UInt<1>("h1"), io.hexDigit) @[Mux.scala 81:61]
      node _io_seg7_seg7_T_1 = mux(_io_seg7_seg7_T, UInt<8>("h9f"), UInt<2>("h3")) @[Mux.scala 81:58]
      node _io_seg7_seg7_T_2 = eq(UInt<2>("h2"), io.hexDigit) @[Mux.scala 81:61]
      node _io_seg7_seg7_T_3 = mux(_io_seg7_seg7_T_2, UInt<6>("h25"), _io_seg7_seg7_T_1) @[Mux.scala 81:58]
      node _io_seg7_seg7_T_4 = eq(UInt<2>("h3"), io.hexDigit) @[Mux.scala 81:61]
      node _io_seg7_seg7_T_5 = mux(_io_seg7_seg7_T_4, UInt<4>("hd"), _io_seg7_seg7_T_3) @[Mux.scala 81:58]
      node _io_seg7_seg7_T_6 = eq(UInt<3>("h4"), io.hexDigit) @[Mux.scala 81:61]
      node _io_seg7_seg7_T_7 = mux(_io_seg7_seg7_T_6, UInt<8>("h99"), _io_seg7_seg7_T_5) @[Mux.scala 81:58]
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
      node _io_seg7_seg7_T_23 = mux(_io_seg7_seg7_T_22, UInt<7>("h63"), _io_seg7_seg7_T_21) @[Mux.scala 81:58]
      node _io_seg7_seg7_T_24 = eq(UInt<4>("hd"), io.hexDigit) @[Mux.scala 81:61]
      node _io_seg7_seg7_T_25 = mux(_io_seg7_seg7_T_24, UInt<8>("h85"), _io_seg7_seg7_T_23) @[Mux.scala 81:58]
      node _io_seg7_seg7_T_26 = eq(UInt<4>("he"), io.hexDigit) @[Mux.scala 81:61]
      node _io_seg7_seg7_T_27 = mux(_io_seg7_seg7_T_26, UInt<7>("h61"), _io_seg7_seg7_T_25) @[Mux.scala 81:58]
      node _io_seg7_seg7_T_28 = eq(UInt<4>("hf"), io.hexDigit) @[Mux.scala 81:61]
      node _io_seg7_seg7_T_29 = mux(_io_seg7_seg7_T_28, UInt<7>("h71"), _io_seg7_seg7_T_27) @[Mux.scala 81:58]
      io_seg7_seg7 <= _io_seg7_seg7_T_29 @[Seg7.scala 77:10]
      io.seg7 <= io_seg7_seg7 @[Seg7.scala 141:13]
    else :
      wire io_seg7_seg7_1 : UInt<8> @[Seg7.scala 101:20]
      node _io_seg7_T = eq(io.special, UInt<2>("h2")) @[Seg7.scala 103:18]
      when _io_seg7_T : @[Seg7.scala 103:31]
        node _io_seg7_seg7_T_30 = eq(UInt<1>("h0"), io.counter) @[Mux.scala 81:61]
        node _io_seg7_seg7_T_31 = mux(_io_seg7_seg7_T_30, UInt<7>("h71"), UInt<8>("hff")) @[Mux.scala 81:58]
        node _io_seg7_seg7_T_32 = eq(UInt<1>("h1"), io.counter) @[Mux.scala 81:61]
        node _io_seg7_seg7_T_33 = mux(_io_seg7_seg7_T_32, UInt<8>("hd5"), _io_seg7_seg7_T_31) @[Mux.scala 81:58]
        node _io_seg7_seg7_T_34 = eq(UInt<2>("h2"), io.counter) @[Mux.scala 81:61]
        node _io_seg7_seg7_T_35 = mux(_io_seg7_seg7_T_34, UInt<4>("hf"), _io_seg7_seg7_T_33) @[Mux.scala 81:58]
        io_seg7_seg7_1 <= _io_seg7_seg7_T_35 @[Seg7.scala 104:12]
      else :
        node _io_seg7_T_1 = eq(io.special, UInt<2>("h3")) @[Seg7.scala 111:24]
        when _io_seg7_T_1 : @[Seg7.scala 111:37]
          node _io_seg7_seg7_T_36 = eq(UInt<1>("h0"), io.counter) @[Mux.scala 81:61]
          node _io_seg7_seg7_T_37 = mux(_io_seg7_seg7_T_36, UInt<8>("hd5"), UInt<8>("hff")) @[Mux.scala 81:58]
          node _io_seg7_seg7_T_38 = eq(UInt<1>("h1"), io.counter) @[Mux.scala 81:61]
          node _io_seg7_seg7_T_39 = mux(_io_seg7_seg7_T_38, UInt<5>("h11"), _io_seg7_seg7_T_37) @[Mux.scala 81:58]
          node _io_seg7_seg7_T_40 = eq(UInt<2>("h2"), io.counter) @[Mux.scala 81:61]
          node _io_seg7_seg7_T_41 = mux(_io_seg7_seg7_T_40, UInt<8>("hd5"), _io_seg7_seg7_T_39) @[Mux.scala 81:58]
          io_seg7_seg7_1 <= _io_seg7_seg7_T_41 @[Seg7.scala 112:12]
        else :
          node _io_seg7_seg7_T_42 = eq(UInt<1>("h0"), io.counter) @[Mux.scala 81:61]
          node _io_seg7_seg7_T_43 = mux(_io_seg7_seg7_T_42, UInt<7>("h73"), UInt<8>("hff")) @[Mux.scala 81:58]
          node _io_seg7_seg7_T_44 = eq(UInt<1>("h1"), io.counter) @[Mux.scala 81:61]
          node _io_seg7_seg7_T_45 = mux(_io_seg7_seg7_T_44, UInt<8>("hc5"), _io_seg7_seg7_T_43) @[Mux.scala 81:58]
          node _io_seg7_seg7_T_46 = eq(UInt<2>("h2"), io.counter) @[Mux.scala 81:61]
          node _io_seg7_seg7_T_47 = mux(_io_seg7_seg7_T_46, UInt<7>("h73"), _io_seg7_seg7_T_45) @[Mux.scala 81:58]
          node _io_seg7_seg7_T_48 = eq(UInt<2>("h3"), io.counter) @[Mux.scala 81:61]
          node _io_seg7_seg7_T_49 = mux(_io_seg7_seg7_T_48, UInt<7>("h73"), _io_seg7_seg7_T_47) @[Mux.scala 81:58]
          node _io_seg7_seg7_T_50 = eq(UInt<3>("h4"), io.counter) @[Mux.scala 81:61]
          node _io_seg7_seg7_T_51 = mux(_io_seg7_seg7_T_50, UInt<7>("h61"), _io_seg7_seg7_T_49) @[Mux.scala 81:58]
          io_seg7_seg7_1 <= _io_seg7_seg7_T_51 @[Seg7.scala 120:12]
      io.seg7 <= io_seg7_seg7_1 @[Seg7.scala 143:13]

  module Seg7 :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { flip seg7 : UInt<32>}, board : { seg7 : UInt<8>, an : UInt<8>}}

    reg clock_counter : UInt<32>, clock with :
      reset => (reset, UInt<32>("h186a0")) @[Seg7.scala 25:30]
    node _clock_counter_T = sub(clock_counter, UInt<1>("h1")) @[Seg7.scala 27:34]
    node _clock_counter_T_1 = tail(_clock_counter_T, 1) @[Seg7.scala 27:34]
    clock_counter <= _clock_counter_T_1 @[Seg7.scala 27:17]
    reg counter : UInt<3>, clock with :
      reset => (reset, UInt<3>("h0")) @[Seg7.scala 29:24]
    node _T = eq(clock_counter, UInt<1>("h0")) @[Seg7.scala 31:22]
    when _T : @[Seg7.scala 31:31]
      clock_counter <= UInt<17>("h186a0") @[Seg7.scala 32:19]
      node _counter_T = add(counter, UInt<1>("h1")) @[Seg7.scala 33:24]
      node _counter_T_1 = tail(_counter_T, 1) @[Seg7.scala 33:24]
      counter <= _counter_T_1 @[Seg7.scala 33:13]
    else :
      skip
    reg special : UInt<2>, clock with :
      reset => (reset, UInt<2>("h0")) @[Seg7.scala 38:24]
    node _T_1 = eq(io.mmio.seg7, UInt<24>("h800000")) @[Seg7.scala 40:21]
    when _T_1 : @[Seg7.scala 40:44]
      special <= UInt<2>("h2") @[Seg7.scala 41:13]
    else :
      node _T_2 = eq(io.mmio.seg7, UInt<24>("hc00000")) @[Seg7.scala 42:27]
      when _T_2 : @[Seg7.scala 42:50]
        special <= UInt<2>("h3") @[Seg7.scala 43:13]
      else :
        node _T_3 = eq(io.mmio.seg7, UInt<23>("h400000")) @[Seg7.scala 44:27]
        when _T_3 : @[Seg7.scala 44:50]
          special <= UInt<1>("h1") @[Seg7.scala 45:13]
        else :
          special <= UInt<1>("h0") @[Seg7.scala 47:13]
    node _digits_T = bits(io.mmio.seg7, 3, 0) @[Seg7.scala 50:57]
    node _digits_T_1 = bits(io.mmio.seg7, 7, 4) @[Seg7.scala 50:57]
    node _digits_T_2 = bits(io.mmio.seg7, 11, 8) @[Seg7.scala 50:57]
    node _digits_T_3 = bits(io.mmio.seg7, 15, 12) @[Seg7.scala 50:57]
    node _digits_T_4 = bits(io.mmio.seg7, 19, 16) @[Seg7.scala 50:57]
    node _digits_T_5 = bits(io.mmio.seg7, 23, 20) @[Seg7.scala 50:57]
    node _digits_T_6 = bits(io.mmio.seg7, 27, 24) @[Seg7.scala 50:57]
    node _digits_T_7 = bits(io.mmio.seg7, 31, 28) @[Seg7.scala 50:57]
    wire digits : UInt<4>[8] @[Seg7.scala 50:23]
    digits[0] <= _digits_T @[Seg7.scala 50:23]
    digits[1] <= _digits_T_1 @[Seg7.scala 50:23]
    digits[2] <= _digits_T_2 @[Seg7.scala 50:23]
    digits[3] <= _digits_T_3 @[Seg7.scala 50:23]
    digits[4] <= _digits_T_4 @[Seg7.scala 50:23]
    digits[5] <= _digits_T_5 @[Seg7.scala 50:23]
    digits[6] <= _digits_T_6 @[Seg7.scala 50:23]
    digits[7] <= _digits_T_7 @[Seg7.scala 50:23]
    inst seg7Module of HexToSeg7 @[Seg7.scala 56:26]
    seg7Module.clock <= clock
    seg7Module.reset <= reset
    seg7Module.io.hexDigit <= digits[counter] @[Seg7.scala 57:26]
    seg7Module.io.special <= special @[Seg7.scala 58:25]
    seg7Module.io.counter <= counter @[Seg7.scala 59:25]
    io.board.seg7 <= seg7Module.io.seg7 @[Seg7.scala 60:17]
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
    io.board.an <= _io_board_an_T_13 @[Seg7.scala 62:15]

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

  extmodule vga :
    input clk : Clock
    input rst : UInt<1>
    input digit : UInt<32>
    output hsync : UInt<1>
    output vsync : UInt<1>
    output vga_rgb : UInt<12>
    defname = vga

  module VGA :
    input clock : Clock
    input reset : Reset
    output io : { board : { hsync : UInt<1>, vsync : UInt<1>, vga_rgb : UInt<12>}, mmio : { flip value : UInt<32>}}

    inst vga of vga @[VGA.scala 21:19]
    vga.vga_rgb is invalid
    vga.vsync is invalid
    vga.hsync is invalid
    vga.digit is invalid
    vga.rst is invalid
    vga.clk is invalid
    vga.clk <= clock @[VGA.scala 22:13]
    vga.rst <= reset @[VGA.scala 23:13]
    vga.digit <= io.mmio.value @[VGA.scala 24:16]
    io.board.hsync <= vga.hsync @[VGA.scala 25:18]
    io.board.vsync <= vga.vsync @[VGA.scala 26:18]
    io.board.vga_rgb <= vga.vga_rgb @[VGA.scala 27:20]

  module DeviceTop :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}, vga : { flip value : UInt<32>}}, board : { led : { led : UInt<24>}, seg7 : { seg7 : UInt<8>, an : UInt<8>}, btn : { flip button : UInt<5>}, switch : { flip switches : UInt<24>}, uart : { flip rx : UInt<1>, tx : UInt<1>}, vga : { hsync : UInt<1>, vsync : UInt<1>, vga_rgb : UInt<12>}}, external_signal : { load_data_mode : UInt<1>}}

    io.external_signal.load_data_mode <= UInt<1>("h0") @[DeviceTop.scala 44:37]
    inst led of Led @[DeviceTop.scala 46:19]
    led.clock <= clock
    led.reset <= reset
    inst seg7 of Seg7 @[DeviceTop.scala 47:20]
    seg7.clock <= clock
    seg7.reset <= reset
    inst btn of Button @[DeviceTop.scala 48:19]
    btn.clock <= clock
    btn.reset <= reset
    inst switches of Switches @[DeviceTop.scala 49:24]
    switches.clock <= clock
    switches.reset <= reset
    inst uart of UARTWrapper @[DeviceTop.scala 50:20]
    uart.clock <= clock
    uart.reset <= reset
    inst vga of VGA @[DeviceTop.scala 51:19]
    vga.clock <= clock
    vga.reset <= reset
    led.io.mmio.led <= io.mmio.led.led @[DeviceTop.scala 53:15]
    seg7.io.mmio.seg7 <= io.mmio.seg7.seg7 @[DeviceTop.scala 54:16]
    io.mmio.btn <= btn.io.mmio @[DeviceTop.scala 55:15]
    io.mmio.switches <= switches.io.mmio @[DeviceTop.scala 56:20]
    io.mmio.uart.txReady <= uart.io.mmio.txReady @[DeviceTop.scala 57:16]
    uart.io.mmio.txStart <= io.mmio.uart.txStart @[DeviceTop.scala 57:16]
    uart.io.mmio.txData <= io.mmio.uart.txData @[DeviceTop.scala 57:16]
    uart.io.mmio.rxReady <= io.mmio.uart.rxReady @[DeviceTop.scala 57:16]
    io.mmio.uart.rxValid <= uart.io.mmio.rxValid @[DeviceTop.scala 57:16]
    io.mmio.uart.rxData <= uart.io.mmio.rxData @[DeviceTop.scala 57:16]
    vga.io.mmio.value <= io.mmio.seg7.seg7 @[DeviceTop.scala 59:20]
    io.board.led <= led.io.board @[DeviceTop.scala 61:16]
    io.board.seg7 <= seg7.io.board @[DeviceTop.scala 62:17]
    btn.io.board.button <= io.board.btn.button @[DeviceTop.scala 63:16]
    switches.io.board.switches <= io.board.switch.switches @[DeviceTop.scala 64:21]
    io.board.uart.tx <= uart.io.board.tx @[DeviceTop.scala 65:17]
    uart.io.board.rx <= io.board.uart.rx @[DeviceTop.scala 65:17]
    io.board.vga <= vga.io.board @[DeviceTop.scala 66:16]
    node _io_external_signal_load_data_mode_T = bits(io.board.switch.switches, 23, 23) @[DeviceTop.scala 69:64]
    io.external_signal.load_data_mode <= _io_external_signal_load_data_mode_T @[DeviceTop.scala 69:37]

  module Top :
    input clock : Clock
    input reset : UInt<1>
    output io : { led : { led : UInt<24>}, seg7 : { seg7 : UInt<8>, an : UInt<8>}, btn : { flip button : UInt<5>}, switch : { flip switches : UInt<24>}, uart : { flip rx : UInt<1>, tx : UInt<1>}, vga : { hsync : UInt<1>, vsync : UInt<1>, vga_rgb : UInt<12>}}

    inst clockSeparator of ClockSeparator @[Top.scala 15:30]
    clockSeparator.clock <= clock
    clockSeparator.reset <= reset
    inst cpu of CoreTop @[Top.scala 17:11]
    cpu.clock <= clockSeparator.io.cpuClock
    cpu.reset <= reset
    inst device of DeviceTop @[Top.scala 19:22]
    device.clock <= clock
    device.reset <= reset
    io.vga <= device.io.board.vga @[Top.scala 22:18]
    io.uart.tx <= device.io.board.uart.tx @[Top.scala 22:18]
    device.io.board.uart.rx <= io.uart.rx @[Top.scala 22:18]
    device.io.board.switch.switches <= io.switch.switches @[Top.scala 22:18]
    device.io.board.btn.button <= io.btn.button @[Top.scala 22:18]
    io.seg7 <= device.io.board.seg7 @[Top.scala 22:18]
    io.led <= device.io.board.led @[Top.scala 22:18]
    cpu.io.external <= device.io.mmio @[Top.scala 25:18]
    cpu.io.external_signal <= device.io.external_signal @[Top.scala 26:25]

