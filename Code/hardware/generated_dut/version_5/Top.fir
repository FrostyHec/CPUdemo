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

  module ConflictController :
    input clock : Clock
    input reset : Reset
    output io : { flip uart_loading : UInt<1>, flip rs1_use : UInt<5>, flip rs2_use : UInt<5>, flip csr_use : UInt<12>, flip IDEX_reg_to_write : UInt<5>, flip IDEX_write_reg : UInt<1>, flip IDEX_csr_to_write : UInt<12>, flip IDEX_write_csr : UInt<1>, flip IDEX_reg_result_stage : UInt<1>, flip EXMEM_reg_to_write : UInt<5>, flip EXMEM_write_reg : UInt<1>, flip EXMEM_csr_to_write : UInt<12>, flip EXMEM_write_csr : UInt<1>, flip EXMEM_reg_result_stage : UInt<1>, rs1_forward_type : UInt<2>, rs2_forward_type : UInt<2>, csr_forward_type : UInt<2>, flip branch_type : UInt<2>, flip pc : UInt<32>, flip imm : UInt<32>, flip alu_result : UInt<32>, flip cmp_result : UInt<1>, flip predict_next_pc : UInt<32>, flip exception_occurs : UInt<1>, flip exception_new_pc : UInt<32>, new_pc : UInt<32>, next_control_signal : UInt<2>, IFID_control_signal : UInt<2>, IDEX_control_signal : UInt<2>, EXMEM_control_signal : UInt<2>, MEMWB_control_signal : UInt<2>, flip prediction_failure : { flip failure_occurs : UInt<1>, flip success_occurs : UInt<1>, flip address : UInt<32>}}

    io.new_pc <= UInt<1>("h0") @[ConflictController.scala 100:12]
    io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 69:14]
    io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 69:14]
    io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 69:14]
    io.next_control_signal <= UInt<1>("h0") @[ConflictController.scala 69:14]
    io.IFID_control_signal <= UInt<1>("h0") @[ConflictController.scala 69:14]
    io.IDEX_control_signal <= UInt<1>("h0") @[ConflictController.scala 69:14]
    io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 69:14]
    io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 69:14]
    io.prediction_failure.failure_occurs <= UInt<1>("h0") @[PCPrediction.scala 14:20]
    io.prediction_failure.success_occurs <= UInt<1>("h0") @[PCPrediction.scala 15:20]
    io.prediction_failure.address <= UInt<1>("h0") @[PCPrediction.scala 16:13]
    when io.uart_loading : @[ConflictController.scala 107:25]
      node _T = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T : @[ConflictController.scala 65:31]
        io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
      node _T_1 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_1 : @[ConflictController.scala 65:31]
        io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
      node _T_2 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_2 : @[ConflictController.scala 65:31]
        io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
      node _T_3 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_3 : @[ConflictController.scala 65:31]
        io.next_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
      node _T_4 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_4 : @[ConflictController.scala 65:31]
        io.IFID_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
      node _T_5 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_5 : @[ConflictController.scala 65:31]
        io.IDEX_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
      node _T_6 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_6 : @[ConflictController.scala 65:31]
        io.EXMEM_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
      node _T_7 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_7 : @[ConflictController.scala 65:31]
        io.MEMWB_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
    node _T_8 = eq(io.uart_loading, UInt<1>("h0")) @[ConflictController.scala 118:9]
    node _T_9 = and(_T_8, io.exception_occurs) @[ConflictController.scala 118:26]
    when _T_9 : @[ConflictController.scala 118:49]
      io.new_pc <= io.exception_new_pc @[ConflictController.scala 119:14]
      node _T_10 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_10 : @[ConflictController.scala 65:31]
        io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
      node _T_11 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_11 : @[ConflictController.scala 65:31]
        io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
      node _T_12 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_12 : @[ConflictController.scala 65:31]
        io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
      node _T_13 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_13 : @[ConflictController.scala 65:31]
        io.next_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
      node _T_14 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_14 : @[ConflictController.scala 65:31]
        io.IFID_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
      node _T_15 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_15 : @[ConflictController.scala 65:31]
        io.IDEX_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
      node _T_16 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_16 : @[ConflictController.scala 65:31]
        io.EXMEM_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
      node _T_17 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
      when _T_17 : @[ConflictController.scala 65:31]
        io.MEMWB_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
    wire control_hazard : UInt<1> @[ConflictController.scala 128:28]
    control_hazard <= UInt<1>("h0") @[ConflictController.scala 129:17]
    node _T_18 = eq(io.uart_loading, UInt<1>("h0")) @[ConflictController.scala 131:9]
    node _T_19 = eq(io.exception_occurs, UInt<1>("h0")) @[ConflictController.scala 131:29]
    node _T_20 = and(_T_18, _T_19) @[ConflictController.scala 131:26]
    when _T_20 : @[ConflictController.scala 131:52]
      node _T_21 = eq(UInt<1>("h1"), io.branch_type) @[ConflictController.scala 132:28]
      when _T_21 : @[ConflictController.scala 132:28]
        node _correct_pc_T = add(io.pc, io.imm) @[ConflictController.scala 134:50]
        node _correct_pc_T_1 = tail(_correct_pc_T, 1) @[ConflictController.scala 134:50]
        node _correct_pc_T_2 = add(io.pc, UInt<3>("h4")) @[ConflictController.scala 134:64]
        node _correct_pc_T_3 = tail(_correct_pc_T_2, 1) @[ConflictController.scala 134:64]
        node correct_pc = mux(io.cmp_result, _correct_pc_T_1, _correct_pc_T_3) @[ConflictController.scala 134:29]
        node _T_22 = neq(io.predict_next_pc, correct_pc) @[ConflictController.scala 135:33]
        when _T_22 : @[ConflictController.scala 135:49]
          io.prediction_failure.failure_occurs <= UInt<1>("h1") @[PCPrediction.scala 20:20]
          io.prediction_failure.success_occurs <= UInt<1>("h0") @[PCPrediction.scala 21:20]
          io.prediction_failure.address <= io.pc @[PCPrediction.scala 22:13]
          control_hazard <= UInt<1>("h1") @[ConflictController.scala 137:25]
          io.new_pc <= correct_pc @[ConflictController.scala 138:21]
          node _T_23 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_23 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_24 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_24 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_25 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_25 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_26 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_26 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_27 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_27 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_28 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_28 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_29 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_29 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_30 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_30 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
        else :
          io.prediction_failure.failure_occurs <= UInt<1>("h0") @[PCPrediction.scala 26:20]
          io.prediction_failure.success_occurs <= UInt<1>("h1") @[PCPrediction.scala 27:20]
          io.prediction_failure.address <= io.pc @[PCPrediction.scala 28:13]
      else :
        node _T_31 = eq(UInt<2>("h2"), io.branch_type) @[ConflictController.scala 132:28]
        when _T_31 : @[ConflictController.scala 132:28]
          node _T_32 = neq(io.predict_next_pc, io.alu_result) @[ConflictController.scala 153:33]
          when _T_32 : @[ConflictController.scala 153:49]
            control_hazard <= UInt<1>("h1") @[ConflictController.scala 154:25]
            io.new_pc <= io.alu_result @[ConflictController.scala 155:21]
            node _T_33 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
            when _T_33 : @[ConflictController.scala 65:31]
              io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
            node _T_34 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
            when _T_34 : @[ConflictController.scala 65:31]
              io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
            node _T_35 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
            when _T_35 : @[ConflictController.scala 65:31]
              io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
            node _T_36 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
            when _T_36 : @[ConflictController.scala 65:31]
              io.next_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
            node _T_37 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
            when _T_37 : @[ConflictController.scala 65:31]
              io.IFID_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
            node _T_38 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
            when _T_38 : @[ConflictController.scala 65:31]
              io.IDEX_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
            node _T_39 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
            when _T_39 : @[ConflictController.scala 65:31]
              io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
            node _T_40 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
            when _T_40 : @[ConflictController.scala 65:31]
              io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
    node _T_41 = eq(io.uart_loading, UInt<1>("h0")) @[ConflictController.scala 166:9]
    node _T_42 = eq(io.exception_occurs, UInt<1>("h0")) @[ConflictController.scala 167:8]
    node _T_43 = and(_T_41, _T_42) @[ConflictController.scala 167:5]
    node _T_44 = eq(control_hazard, UInt<1>("h0")) @[ConflictController.scala 168:8]
    node _T_45 = and(_T_43, _T_44) @[ConflictController.scala 168:5]
    when _T_45 : @[ConflictController.scala 169:5]
      node _T_46 = neq(io.IDEX_reg_to_write, UInt<1>("h0")) @[ConflictController.scala 172:31]
      node _T_47 = and(io.IDEX_write_reg, _T_46) @[ConflictController.scala 172:7]
      node _T_48 = eq(io.rs1_use, io.IDEX_reg_to_write) @[ConflictController.scala 173:21]
      node _T_49 = and(_T_47, _T_48) @[ConflictController.scala 173:7]
      when _T_49 : @[ConflictController.scala 173:47]
        node _T_50 = eq(io.IDEX_reg_result_stage, UInt<1>("h0")) @[ConflictController.scala 174:37]
        when _T_50 : @[ConflictController.scala 174:69]
          node _T_51 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_51 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_52 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_52 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_53 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_53 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_54 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_54 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_55 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_55 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_56 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_56 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_57 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_57 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_58 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_58 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
        else :
          node _T_59 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_59 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_60 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_60 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_61 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_61 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_62 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_62 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_63 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_63 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_64 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_64 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_65 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_65 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_66 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_66 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
      else :
        node _T_67 = neq(io.EXMEM_reg_to_write, UInt<1>("h0")) @[ConflictController.scala 186:32]
        node _T_68 = and(io.EXMEM_write_reg, _T_67) @[ConflictController.scala 186:7]
        node _T_69 = eq(io.rs1_use, io.EXMEM_reg_to_write) @[ConflictController.scala 187:21]
        node _T_70 = and(_T_68, _T_69) @[ConflictController.scala 187:7]
        when _T_70 : @[ConflictController.scala 187:48]
          node _T_71 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_71 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_72 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_72 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_73 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_73 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_74 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_74 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_75 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_75 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_76 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_76 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_77 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_77 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_78 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_78 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
      node _T_79 = neq(io.IDEX_reg_to_write, UInt<1>("h0")) @[ConflictController.scala 194:31]
      node _T_80 = and(io.IDEX_write_reg, _T_79) @[ConflictController.scala 194:7]
      node _T_81 = eq(io.rs2_use, io.IDEX_reg_to_write) @[ConflictController.scala 195:21]
      node _T_82 = and(_T_80, _T_81) @[ConflictController.scala 195:7]
      when _T_82 : @[ConflictController.scala 195:47]
        node _T_83 = eq(io.IDEX_reg_result_stage, UInt<1>("h0")) @[ConflictController.scala 196:37]
        when _T_83 : @[ConflictController.scala 196:69]
          node _T_84 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_84 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_85 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_85 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_86 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_86 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_87 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_87 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_88 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_88 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_89 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_89 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_90 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_90 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_91 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_91 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
        else :
          node _T_92 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_92 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_93 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_93 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_94 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_94 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_95 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_95 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_96 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_96 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_97 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_97 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_98 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_98 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_99 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_99 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
      else :
        node _T_100 = neq(io.EXMEM_reg_to_write, UInt<1>("h0")) @[ConflictController.scala 208:32]
        node _T_101 = and(io.EXMEM_write_reg, _T_100) @[ConflictController.scala 208:7]
        node _T_102 = eq(io.rs2_use, io.EXMEM_reg_to_write) @[ConflictController.scala 209:21]
        node _T_103 = and(_T_101, _T_102) @[ConflictController.scala 209:7]
        when _T_103 : @[ConflictController.scala 209:48]
          node _T_104 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_104 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_105 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_105 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_106 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_106 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_107 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_107 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_108 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_108 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_109 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_109 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_110 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_110 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_111 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_111 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
      node _T_112 = neq(io.IDEX_csr_to_write, UInt<1>("h0")) @[ConflictController.scala 216:31]
      node _T_113 = and(io.IDEX_write_csr, _T_112) @[ConflictController.scala 216:7]
      node _T_114 = eq(io.csr_use, io.IDEX_csr_to_write) @[ConflictController.scala 217:21]
      node _T_115 = and(_T_113, _T_114) @[ConflictController.scala 217:7]
      when _T_115 : @[ConflictController.scala 217:47]
        node _T_116 = eq(io.IDEX_reg_result_stage, UInt<1>("h0")) @[ConflictController.scala 218:37]
        when _T_116 : @[ConflictController.scala 218:69]
          node _T_117 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_117 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_118 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_118 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_119 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_119 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_120 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_120 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_121 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_121 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_122 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_122 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_123 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_123 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_124 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_124 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
        else :
          node _T_125 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_125 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_126 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_126 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_127 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_127 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_128 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_128 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_129 = neq(UInt<1>("h1"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_129 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h1") @[ConflictController.scala 66:16]
          node _T_130 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_130 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_131 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_131 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_132 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_132 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
      else :
        node _T_133 = neq(io.EXMEM_csr_to_write, UInt<1>("h0")) @[ConflictController.scala 230:32]
        node _T_134 = and(io.EXMEM_write_csr, _T_133) @[ConflictController.scala 230:7]
        node _T_135 = eq(io.csr_use, io.EXMEM_csr_to_write) @[ConflictController.scala 231:21]
        node _T_136 = and(_T_134, _T_135) @[ConflictController.scala 231:7]
        when _T_136 : @[ConflictController.scala 231:48]
          node _T_137 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_137 : @[ConflictController.scala 65:31]
            io.rs1_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_138 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_138 : @[ConflictController.scala 65:31]
            io.rs2_forward_type <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_139 = neq(UInt<2>("h2"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_139 : @[ConflictController.scala 65:31]
            io.csr_forward_type <= UInt<2>("h2") @[ConflictController.scala 66:16]
          node _T_140 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_140 : @[ConflictController.scala 65:31]
            io.next_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_141 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_141 : @[ConflictController.scala 65:31]
            io.IFID_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_142 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_142 : @[ConflictController.scala 65:31]
            io.IDEX_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_143 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_143 : @[ConflictController.scala 65:31]
            io.EXMEM_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]
          node _T_144 = neq(UInt<1>("h0"), UInt<1>("h0")) @[ConflictController.scala 65:18]
          when _T_144 : @[ConflictController.scala 65:31]
            io.MEMWB_control_signal <= UInt<1>("h0") @[ConflictController.scala 66:16]

  module CPUState :
    input clock : Clock
    input reset : Reset
    output io : { flip stall : UInt<1>, cpu_state : UInt<2>}

    reg state : UInt, clock with :
      reset => (reset, UInt<1>("h0")) @[CPUState.scala 15:22]
    io.cpu_state <= state @[CPUState.scala 16:15]
    node _T = eq(io.stall, UInt<1>("h0")) @[CPUState.scala 20:8]
    when _T : @[CPUState.scala 20:19]
      node _T_1 = eq(UInt<1>("h0"), state) @[CPUState.scala 21:18]
      when _T_1 : @[CPUState.scala 21:18]
        state <= UInt<1>("h1") @[CPUState.scala 23:14]
      else :
        node _T_2 = eq(UInt<1>("h1"), state) @[CPUState.scala 21:18]
        when _T_2 : @[CPUState.scala 21:18]
          state <= UInt<2>("h2") @[CPUState.scala 26:14]
        else :
          node _T_3 = eq(UInt<2>("h2"), state) @[CPUState.scala 21:18]
          when _T_3 : @[CPUState.scala 21:18]
            state <= UInt<1>("h0") @[CPUState.scala 29:14]

  module CSR :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, cur_privilege : UInt<2>, flip write : UInt<1>, flip csr_idx_write : UInt<12>, flip write_data : UInt<32>, flip csr_idx_read : UInt<12>, csr_val : UInt<32>, flip IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, flip ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}, flip MEM_fault : { mem_fault_type : UInt<3>, mtval : UInt<32>}, flip io_interruption : { io_fault_occur : UInt<1>, mtval : UInt<32>}, flip pc : UInt<32>, fault_occurs : UInt<1>, fault_new_PC : UInt<32>}

    reg cur_privilege : UInt<2>, clock with :
      reset => (reset, UInt<2>("h3")) @[CSR.scala 36:30]
    io.cur_privilege <= cur_privilege @[CSR.scala 37:20]
    reg mstatus : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 39:24]
    reg mie : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 40:20]
    reg mtvec : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 41:22]
    reg mscratch : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 43:25]
    reg mepc : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 44:21]
    reg mcause : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 45:23]
    reg mtval : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 46:22]
    reg mip : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[CSR.scala 47:20]
    node read_MIE = bits(mstatus, 3, 3) @[CSR.scala 49:25]
    node read_MEIE = bits(mie, 11, 11) @[CSR.scala 50:22]
    node read_MPIE = bits(mstatus, 7, 7) @[CSR.scala 51:26]
    node read_MPP = bits(mstatus, 12, 11) @[CSR.scala 52:25]
    node _mip_T = and(read_MIE, read_MEIE) @[CSR.scala 54:36]
    node _mip_T_1 = and(_mip_T, io.io_interruption.io_fault_occur) @[CSR.scala 54:48]
    node _mip_T_2 = dshl(UInt<1>("h1"), UInt<4>("hb")) @[CSR.scala 54:20]
    node _mip_T_3 = or(mip, _mip_T_2) @[CSR.scala 54:20]
    node _mip_T_4 = not(mip) @[CSR.scala 54:20]
    node _mip_T_5 = or(_mip_T_4, _mip_T_2) @[CSR.scala 54:20]
    node _mip_T_6 = not(_mip_T_5) @[CSR.scala 54:20]
    node _mip_T_7 = mux(_mip_T_1, _mip_T_3, _mip_T_6) @[CSR.scala 54:20]
    mip <= _mip_T_7 @[CSR.scala 54:7]
    node _no_fault_T = eq(io.IF_fault.IF_fault_type, UInt<1>("h0")) @[CSR.scala 56:45]
    node _no_fault_T_1 = eq(io.ID_fault.ID_fault_type, UInt<1>("h0")) @[CSR.scala 57:32]
    node _no_fault_T_2 = and(_no_fault_T, _no_fault_T_1) @[CSR.scala 56:73]
    node _no_fault_T_3 = eq(io.MEM_fault.mem_fault_type, UInt<1>("h0")) @[CSR.scala 58:34]
    node _no_fault_T_4 = and(_no_fault_T_2, _no_fault_T_3) @[CSR.scala 57:60]
    node _no_fault_T_5 = and(read_MIE, read_MEIE) @[CSR.scala 59:16]
    node _no_fault_T_6 = and(_no_fault_T_5, io.io_interruption.io_fault_occur) @[CSR.scala 59:28]
    node _no_fault_T_7 = eq(_no_fault_T_6, UInt<1>("h0")) @[CSR.scala 59:5]
    node no_fault = and(_no_fault_T_4, _no_fault_T_7) @[CSR.scala 58:63]
    io.csr_val is invalid @[CSR.scala 96:14]
    io.fault_occurs <= UInt<1>("h0") @[CSR.scala 97:19]
    io.fault_new_PC is invalid @[CSR.scala 98:19]
    node _T = eq(io.csr_idx_read, UInt<10>("h300")) @[CSR.scala 62:26]
    when _T : @[CSR.scala 62:35]
      io.csr_val <= mstatus @[CSR.scala 63:18]
    node _T_1 = eq(io.cpu_state, UInt<1>("h0")) @[CSR.scala 65:23]
    node _T_2 = and(_T_1, io.write) @[CSR.scala 66:7]
    node _T_3 = eq(io.csr_idx_write, UInt<10>("h300")) @[CSR.scala 67:26]
    node _T_4 = and(_T_2, _T_3) @[CSR.scala 67:7]
    when _T_4 : @[CSR.scala 67:35]
      mstatus <= io.write_data @[CSR.scala 68:11]
    node _T_5 = eq(io.csr_idx_read, UInt<10>("h304")) @[CSR.scala 62:26]
    when _T_5 : @[CSR.scala 62:35]
      io.csr_val <= mie @[CSR.scala 63:18]
    node _T_6 = eq(io.cpu_state, UInt<1>("h0")) @[CSR.scala 65:23]
    node _T_7 = and(_T_6, io.write) @[CSR.scala 66:7]
    node _T_8 = eq(io.csr_idx_write, UInt<10>("h304")) @[CSR.scala 67:26]
    node _T_9 = and(_T_7, _T_8) @[CSR.scala 67:7]
    when _T_9 : @[CSR.scala 67:35]
      mie <= io.write_data @[CSR.scala 68:11]
    node _T_10 = eq(io.csr_idx_read, UInt<10>("h305")) @[CSR.scala 62:26]
    when _T_10 : @[CSR.scala 62:35]
      io.csr_val <= mtvec @[CSR.scala 63:18]
    node _T_11 = eq(io.cpu_state, UInt<1>("h0")) @[CSR.scala 65:23]
    node _T_12 = and(_T_11, io.write) @[CSR.scala 66:7]
    node _T_13 = eq(io.csr_idx_write, UInt<10>("h305")) @[CSR.scala 67:26]
    node _T_14 = and(_T_12, _T_13) @[CSR.scala 67:7]
    when _T_14 : @[CSR.scala 67:35]
      mtvec <= io.write_data @[CSR.scala 68:11]
    node _T_15 = eq(io.csr_idx_read, UInt<10>("h340")) @[CSR.scala 62:26]
    when _T_15 : @[CSR.scala 62:35]
      io.csr_val <= mscratch @[CSR.scala 63:18]
    node _T_16 = eq(io.cpu_state, UInt<1>("h0")) @[CSR.scala 65:23]
    node _T_17 = and(_T_16, io.write) @[CSR.scala 66:7]
    node _T_18 = eq(io.csr_idx_write, UInt<10>("h340")) @[CSR.scala 67:26]
    node _T_19 = and(_T_17, _T_18) @[CSR.scala 67:7]
    when _T_19 : @[CSR.scala 67:35]
      mscratch <= io.write_data @[CSR.scala 68:11]
    node _T_20 = eq(io.csr_idx_read, UInt<10>("h341")) @[CSR.scala 62:26]
    when _T_20 : @[CSR.scala 62:35]
      io.csr_val <= mepc @[CSR.scala 63:18]
    node _T_21 = eq(io.cpu_state, UInt<1>("h0")) @[CSR.scala 65:23]
    node _T_22 = and(_T_21, io.write) @[CSR.scala 66:7]
    node _T_23 = eq(io.csr_idx_write, UInt<10>("h341")) @[CSR.scala 67:26]
    node _T_24 = and(_T_22, _T_23) @[CSR.scala 67:7]
    when _T_24 : @[CSR.scala 67:35]
      mepc <= io.write_data @[CSR.scala 68:11]
    node _T_25 = eq(io.csr_idx_read, UInt<10>("h342")) @[CSR.scala 62:26]
    when _T_25 : @[CSR.scala 62:35]
      io.csr_val <= mcause @[CSR.scala 63:18]
    node _T_26 = eq(io.cpu_state, UInt<1>("h0")) @[CSR.scala 65:23]
    node _T_27 = and(_T_26, io.write) @[CSR.scala 66:7]
    node _T_28 = eq(io.csr_idx_write, UInt<10>("h342")) @[CSR.scala 67:26]
    node _T_29 = and(_T_27, _T_28) @[CSR.scala 67:7]
    when _T_29 : @[CSR.scala 67:35]
      mcause <= io.write_data @[CSR.scala 68:11]
    node _T_30 = eq(io.csr_idx_read, UInt<10>("h343")) @[CSR.scala 62:26]
    when _T_30 : @[CSR.scala 62:35]
      io.csr_val <= mtval @[CSR.scala 63:18]
    node _T_31 = eq(io.cpu_state, UInt<1>("h0")) @[CSR.scala 65:23]
    node _T_32 = and(_T_31, io.write) @[CSR.scala 66:7]
    node _T_33 = eq(io.csr_idx_write, UInt<10>("h343")) @[CSR.scala 67:26]
    node _T_34 = and(_T_32, _T_33) @[CSR.scala 67:7]
    when _T_34 : @[CSR.scala 67:35]
      mtval <= io.write_data @[CSR.scala 68:11]
    node _T_35 = eq(io.csr_idx_read, UInt<10>("h344")) @[CSR.scala 62:26]
    when _T_35 : @[CSR.scala 62:35]
      io.csr_val <= mip @[CSR.scala 63:18]
    node _T_36 = eq(io.cpu_state, UInt<1>("h0")) @[CSR.scala 65:23]
    node _T_37 = and(_T_36, io.write) @[CSR.scala 66:7]
    node _T_38 = eq(io.csr_idx_write, UInt<10>("h344")) @[CSR.scala 67:26]
    node _T_39 = and(_T_37, _T_38) @[CSR.scala 67:7]
    when _T_39 : @[CSR.scala 67:35]
      mip <= io.write_data @[CSR.scala 68:11]
    node _T_40 = eq(no_fault, UInt<1>("h0")) @[CSR.scala 109:8]
    when _T_40 : @[CSR.scala 109:19]
      wire fault_type : UInt<1> @[CSR.scala 110:26]
      fault_type <= UInt<1>("h0") @[CSR.scala 111:16]
      node _T_41 = eq(io.ID_fault.ID_fault_type, UInt<3>("h4")) @[CSR.scala 112:36]
      when _T_41 : @[CSR.scala 112:66]
        fault_type <= UInt<1>("h1") @[CSR.scala 113:18]
      node _T_42 = eq(fault_type, UInt<1>("h0")) @[CSR.scala 117:21]
      when _T_42 : @[CSR.scala 117:62]
        node _T_43 = eq(io.MEM_fault.mem_fault_type, UInt<1>("h1")) @[CSR.scala 118:40]
        when _T_43 : @[CSR.scala 118:81]
          io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
          mepc <= io.pc @[CSR.scala 81:10]
          io.fault_new_PC <= mtvec @[CSR.scala 82:21]
          mcause <= UInt<3>("h4") @[CSR.scala 83:12]
          mtval <= io.MEM_fault.mtval @[CSR.scala 84:11]
          node _mstatus_T = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
          node _mstatus_T_1 = or(mstatus, _mstatus_T) @[CSR.scala 85:30]
          node _mstatus_T_2 = not(mstatus) @[CSR.scala 85:30]
          node _mstatus_T_3 = or(_mstatus_T_2, _mstatus_T) @[CSR.scala 85:30]
          node _mstatus_T_4 = not(_mstatus_T_3) @[CSR.scala 85:30]
          node _mstatus_T_5 = mux(UInt<1>("h0"), _mstatus_T_1, _mstatus_T_4) @[CSR.scala 85:30]
          mstatus <= _mstatus_T_5 @[CSR.scala 85:13]
          node _mstatus_T_6 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
          node _mstatus_T_7 = or(mstatus, _mstatus_T_6) @[CSR.scala 87:30]
          node _mstatus_T_8 = not(mstatus) @[CSR.scala 87:30]
          node _mstatus_T_9 = or(_mstatus_T_8, _mstatus_T_6) @[CSR.scala 87:30]
          node _mstatus_T_10 = not(_mstatus_T_9) @[CSR.scala 87:30]
          node _mstatus_T_11 = mux(read_MIE, _mstatus_T_7, _mstatus_T_10) @[CSR.scala 87:30]
          mstatus <= _mstatus_T_11 @[CSR.scala 87:13]
          node _mstatus_T_12 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
          node _mstatus_T_13 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
          node mstatus_hi = cat(_mstatus_T_12, cur_privilege) @[Cat.scala 33:92]
          node _mstatus_T_14 = cat(mstatus_hi, _mstatus_T_13) @[Cat.scala 33:92]
          mstatus <= _mstatus_T_14 @[CSR.scala 89:13]
          cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
        else :
          node _T_44 = eq(io.MEM_fault.mem_fault_type, UInt<2>("h2")) @[CSR.scala 120:46]
          when _T_44 : @[CSR.scala 120:82]
            io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
            mepc <= io.pc @[CSR.scala 81:10]
            io.fault_new_PC <= mtvec @[CSR.scala 82:21]
            mcause <= UInt<3>("h5") @[CSR.scala 83:12]
            mtval <= io.MEM_fault.mtval @[CSR.scala 84:11]
            node _mstatus_T_15 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
            node _mstatus_T_16 = or(mstatus, _mstatus_T_15) @[CSR.scala 85:30]
            node _mstatus_T_17 = not(mstatus) @[CSR.scala 85:30]
            node _mstatus_T_18 = or(_mstatus_T_17, _mstatus_T_15) @[CSR.scala 85:30]
            node _mstatus_T_19 = not(_mstatus_T_18) @[CSR.scala 85:30]
            node _mstatus_T_20 = mux(UInt<1>("h0"), _mstatus_T_16, _mstatus_T_19) @[CSR.scala 85:30]
            mstatus <= _mstatus_T_20 @[CSR.scala 85:13]
            node _mstatus_T_21 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
            node _mstatus_T_22 = or(mstatus, _mstatus_T_21) @[CSR.scala 87:30]
            node _mstatus_T_23 = not(mstatus) @[CSR.scala 87:30]
            node _mstatus_T_24 = or(_mstatus_T_23, _mstatus_T_21) @[CSR.scala 87:30]
            node _mstatus_T_25 = not(_mstatus_T_24) @[CSR.scala 87:30]
            node _mstatus_T_26 = mux(read_MIE, _mstatus_T_22, _mstatus_T_25) @[CSR.scala 87:30]
            mstatus <= _mstatus_T_26 @[CSR.scala 87:13]
            node _mstatus_T_27 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
            node _mstatus_T_28 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
            node mstatus_hi_1 = cat(_mstatus_T_27, cur_privilege) @[Cat.scala 33:92]
            node _mstatus_T_29 = cat(mstatus_hi_1, _mstatus_T_28) @[Cat.scala 33:92]
            mstatus <= _mstatus_T_29 @[CSR.scala 89:13]
            cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
          else :
            node _T_45 = eq(io.MEM_fault.mem_fault_type, UInt<2>("h3")) @[CSR.scala 122:48]
            when _T_45 : @[CSR.scala 122:90]
              io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
              mepc <= io.pc @[CSR.scala 81:10]
              io.fault_new_PC <= mtvec @[CSR.scala 82:21]
              mcause <= UInt<3>("h6") @[CSR.scala 83:12]
              mtval <= io.MEM_fault.mtval @[CSR.scala 84:11]
              node _mstatus_T_30 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
              node _mstatus_T_31 = or(mstatus, _mstatus_T_30) @[CSR.scala 85:30]
              node _mstatus_T_32 = not(mstatus) @[CSR.scala 85:30]
              node _mstatus_T_33 = or(_mstatus_T_32, _mstatus_T_30) @[CSR.scala 85:30]
              node _mstatus_T_34 = not(_mstatus_T_33) @[CSR.scala 85:30]
              node _mstatus_T_35 = mux(UInt<1>("h0"), _mstatus_T_31, _mstatus_T_34) @[CSR.scala 85:30]
              mstatus <= _mstatus_T_35 @[CSR.scala 85:13]
              node _mstatus_T_36 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
              node _mstatus_T_37 = or(mstatus, _mstatus_T_36) @[CSR.scala 87:30]
              node _mstatus_T_38 = not(mstatus) @[CSR.scala 87:30]
              node _mstatus_T_39 = or(_mstatus_T_38, _mstatus_T_36) @[CSR.scala 87:30]
              node _mstatus_T_40 = not(_mstatus_T_39) @[CSR.scala 87:30]
              node _mstatus_T_41 = mux(read_MIE, _mstatus_T_37, _mstatus_T_40) @[CSR.scala 87:30]
              mstatus <= _mstatus_T_41 @[CSR.scala 87:13]
              node _mstatus_T_42 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
              node _mstatus_T_43 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
              node mstatus_hi_2 = cat(_mstatus_T_42, cur_privilege) @[Cat.scala 33:92]
              node _mstatus_T_44 = cat(mstatus_hi_2, _mstatus_T_43) @[Cat.scala 33:92]
              mstatus <= _mstatus_T_44 @[CSR.scala 89:13]
              cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
            else :
              node _T_46 = eq(io.MEM_fault.mem_fault_type, UInt<3>("h4")) @[CSR.scala 124:48]
              when _T_46 : @[CSR.scala 124:85]
                io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
                mepc <= io.pc @[CSR.scala 81:10]
                io.fault_new_PC <= mtvec @[CSR.scala 82:21]
                mcause <= UInt<3>("h7") @[CSR.scala 83:12]
                mtval <= io.MEM_fault.mtval @[CSR.scala 84:11]
                node _mstatus_T_45 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
                node _mstatus_T_46 = or(mstatus, _mstatus_T_45) @[CSR.scala 85:30]
                node _mstatus_T_47 = not(mstatus) @[CSR.scala 85:30]
                node _mstatus_T_48 = or(_mstatus_T_47, _mstatus_T_45) @[CSR.scala 85:30]
                node _mstatus_T_49 = not(_mstatus_T_48) @[CSR.scala 85:30]
                node _mstatus_T_50 = mux(UInt<1>("h0"), _mstatus_T_46, _mstatus_T_49) @[CSR.scala 85:30]
                mstatus <= _mstatus_T_50 @[CSR.scala 85:13]
                node _mstatus_T_51 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
                node _mstatus_T_52 = or(mstatus, _mstatus_T_51) @[CSR.scala 87:30]
                node _mstatus_T_53 = not(mstatus) @[CSR.scala 87:30]
                node _mstatus_T_54 = or(_mstatus_T_53, _mstatus_T_51) @[CSR.scala 87:30]
                node _mstatus_T_55 = not(_mstatus_T_54) @[CSR.scala 87:30]
                node _mstatus_T_56 = mux(read_MIE, _mstatus_T_52, _mstatus_T_55) @[CSR.scala 87:30]
                mstatus <= _mstatus_T_56 @[CSR.scala 87:13]
                node _mstatus_T_57 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
                node _mstatus_T_58 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
                node mstatus_hi_3 = cat(_mstatus_T_57, cur_privilege) @[Cat.scala 33:92]
                node _mstatus_T_59 = cat(mstatus_hi_3, _mstatus_T_58) @[Cat.scala 33:92]
                mstatus <= _mstatus_T_59 @[CSR.scala 89:13]
                cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
              else :
                node _T_47 = eq(io.ID_fault.ID_fault_type, UInt<1>("h1")) @[CSR.scala 128:45]
                when _T_47 : @[CSR.scala 128:81]
                  io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
                  mepc <= io.pc @[CSR.scala 81:10]
                  io.fault_new_PC <= mtvec @[CSR.scala 82:21]
                  mcause <= UInt<2>("h2") @[CSR.scala 83:12]
                  mtval <= io.ID_fault.mtval @[CSR.scala 84:11]
                  node _mstatus_T_60 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
                  node _mstatus_T_61 = or(mstatus, _mstatus_T_60) @[CSR.scala 85:30]
                  node _mstatus_T_62 = not(mstatus) @[CSR.scala 85:30]
                  node _mstatus_T_63 = or(_mstatus_T_62, _mstatus_T_60) @[CSR.scala 85:30]
                  node _mstatus_T_64 = not(_mstatus_T_63) @[CSR.scala 85:30]
                  node _mstatus_T_65 = mux(UInt<1>("h0"), _mstatus_T_61, _mstatus_T_64) @[CSR.scala 85:30]
                  mstatus <= _mstatus_T_65 @[CSR.scala 85:13]
                  node _mstatus_T_66 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
                  node _mstatus_T_67 = or(mstatus, _mstatus_T_66) @[CSR.scala 87:30]
                  node _mstatus_T_68 = not(mstatus) @[CSR.scala 87:30]
                  node _mstatus_T_69 = or(_mstatus_T_68, _mstatus_T_66) @[CSR.scala 87:30]
                  node _mstatus_T_70 = not(_mstatus_T_69) @[CSR.scala 87:30]
                  node _mstatus_T_71 = mux(read_MIE, _mstatus_T_67, _mstatus_T_70) @[CSR.scala 87:30]
                  mstatus <= _mstatus_T_71 @[CSR.scala 87:13]
                  node _mstatus_T_72 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
                  node _mstatus_T_73 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
                  node mstatus_hi_4 = cat(_mstatus_T_72, cur_privilege) @[Cat.scala 33:92]
                  node _mstatus_T_74 = cat(mstatus_hi_4, _mstatus_T_73) @[Cat.scala 33:92]
                  mstatus <= _mstatus_T_74 @[CSR.scala 89:13]
                  cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
                else :
                  node _T_48 = eq(io.ID_fault.ID_fault_type, UInt<2>("h2")) @[CSR.scala 130:46]
                  when _T_48 : @[CSR.scala 130:82]
                    io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
                    mepc <= io.pc @[CSR.scala 81:10]
                    io.fault_new_PC <= mtvec @[CSR.scala 82:21]
                    mcause <= UInt<2>("h3") @[CSR.scala 83:12]
                    mtval <= io.ID_fault.mtval @[CSR.scala 84:11]
                    node _mstatus_T_75 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
                    node _mstatus_T_76 = or(mstatus, _mstatus_T_75) @[CSR.scala 85:30]
                    node _mstatus_T_77 = not(mstatus) @[CSR.scala 85:30]
                    node _mstatus_T_78 = or(_mstatus_T_77, _mstatus_T_75) @[CSR.scala 85:30]
                    node _mstatus_T_79 = not(_mstatus_T_78) @[CSR.scala 85:30]
                    node _mstatus_T_80 = mux(UInt<1>("h0"), _mstatus_T_76, _mstatus_T_79) @[CSR.scala 85:30]
                    mstatus <= _mstatus_T_80 @[CSR.scala 85:13]
                    node _mstatus_T_81 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
                    node _mstatus_T_82 = or(mstatus, _mstatus_T_81) @[CSR.scala 87:30]
                    node _mstatus_T_83 = not(mstatus) @[CSR.scala 87:30]
                    node _mstatus_T_84 = or(_mstatus_T_83, _mstatus_T_81) @[CSR.scala 87:30]
                    node _mstatus_T_85 = not(_mstatus_T_84) @[CSR.scala 87:30]
                    node _mstatus_T_86 = mux(read_MIE, _mstatus_T_82, _mstatus_T_85) @[CSR.scala 87:30]
                    mstatus <= _mstatus_T_86 @[CSR.scala 87:13]
                    node _mstatus_T_87 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
                    node _mstatus_T_88 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
                    node mstatus_hi_5 = cat(_mstatus_T_87, cur_privilege) @[Cat.scala 33:92]
                    node _mstatus_T_89 = cat(mstatus_hi_5, _mstatus_T_88) @[Cat.scala 33:92]
                    mstatus <= _mstatus_T_89 @[CSR.scala 89:13]
                    cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
                  else :
                    node _T_49 = eq(io.ID_fault.ID_fault_type, UInt<2>("h3")) @[CSR.scala 132:46]
                    when _T_49 : @[CSR.scala 132:78]
                      io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
                      mepc <= io.pc @[CSR.scala 81:10]
                      io.fault_new_PC <= mtvec @[CSR.scala 82:21]
                      mcause <= UInt<4>("hb") @[CSR.scala 83:12]
                      mtval <= io.ID_fault.mtval @[CSR.scala 84:11]
                      node _mstatus_T_90 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
                      node _mstatus_T_91 = or(mstatus, _mstatus_T_90) @[CSR.scala 85:30]
                      node _mstatus_T_92 = not(mstatus) @[CSR.scala 85:30]
                      node _mstatus_T_93 = or(_mstatus_T_92, _mstatus_T_90) @[CSR.scala 85:30]
                      node _mstatus_T_94 = not(_mstatus_T_93) @[CSR.scala 85:30]
                      node _mstatus_T_95 = mux(UInt<1>("h0"), _mstatus_T_91, _mstatus_T_94) @[CSR.scala 85:30]
                      mstatus <= _mstatus_T_95 @[CSR.scala 85:13]
                      node _mstatus_T_96 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
                      node _mstatus_T_97 = or(mstatus, _mstatus_T_96) @[CSR.scala 87:30]
                      node _mstatus_T_98 = not(mstatus) @[CSR.scala 87:30]
                      node _mstatus_T_99 = or(_mstatus_T_98, _mstatus_T_96) @[CSR.scala 87:30]
                      node _mstatus_T_100 = not(_mstatus_T_99) @[CSR.scala 87:30]
                      node _mstatus_T_101 = mux(read_MIE, _mstatus_T_97, _mstatus_T_100) @[CSR.scala 87:30]
                      mstatus <= _mstatus_T_101 @[CSR.scala 87:13]
                      node _mstatus_T_102 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
                      node _mstatus_T_103 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
                      node mstatus_hi_6 = cat(_mstatus_T_102, cur_privilege) @[Cat.scala 33:92]
                      node _mstatus_T_104 = cat(mstatus_hi_6, _mstatus_T_103) @[Cat.scala 33:92]
                      mstatus <= _mstatus_T_104 @[CSR.scala 89:13]
                      cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
                    else :
                      node _T_50 = eq(io.IF_fault.IF_fault_type, UInt<1>("h1")) @[CSR.scala 136:45]
                      when _T_50 : @[CSR.scala 136:84]
                        io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
                        mepc <= io.pc @[CSR.scala 81:10]
                        io.fault_new_PC <= mtvec @[CSR.scala 82:21]
                        mcause <= UInt<1>("h0") @[CSR.scala 83:12]
                        mtval <= io.IF_fault.mtval @[CSR.scala 84:11]
                        node _mstatus_T_105 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
                        node _mstatus_T_106 = or(mstatus, _mstatus_T_105) @[CSR.scala 85:30]
                        node _mstatus_T_107 = not(mstatus) @[CSR.scala 85:30]
                        node _mstatus_T_108 = or(_mstatus_T_107, _mstatus_T_105) @[CSR.scala 85:30]
                        node _mstatus_T_109 = not(_mstatus_T_108) @[CSR.scala 85:30]
                        node _mstatus_T_110 = mux(UInt<1>("h0"), _mstatus_T_106, _mstatus_T_109) @[CSR.scala 85:30]
                        mstatus <= _mstatus_T_110 @[CSR.scala 85:13]
                        node _mstatus_T_111 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
                        node _mstatus_T_112 = or(mstatus, _mstatus_T_111) @[CSR.scala 87:30]
                        node _mstatus_T_113 = not(mstatus) @[CSR.scala 87:30]
                        node _mstatus_T_114 = or(_mstatus_T_113, _mstatus_T_111) @[CSR.scala 87:30]
                        node _mstatus_T_115 = not(_mstatus_T_114) @[CSR.scala 87:30]
                        node _mstatus_T_116 = mux(read_MIE, _mstatus_T_112, _mstatus_T_115) @[CSR.scala 87:30]
                        mstatus <= _mstatus_T_116 @[CSR.scala 87:13]
                        node _mstatus_T_117 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
                        node _mstatus_T_118 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
                        node mstatus_hi_7 = cat(_mstatus_T_117, cur_privilege) @[Cat.scala 33:92]
                        node _mstatus_T_119 = cat(mstatus_hi_7, _mstatus_T_118) @[Cat.scala 33:92]
                        mstatus <= _mstatus_T_119 @[CSR.scala 89:13]
                        cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
                      else :
                        node _T_51 = eq(io.IF_fault.IF_fault_type, UInt<2>("h2")) @[CSR.scala 139:45]
                        when _T_51 : @[CSR.scala 139:79]
                          io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
                          mepc <= io.pc @[CSR.scala 81:10]
                          io.fault_new_PC <= mtvec @[CSR.scala 82:21]
                          mcause <= UInt<1>("h1") @[CSR.scala 83:12]
                          mtval <= io.IF_fault.mtval @[CSR.scala 84:11]
                          node _mstatus_T_120 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
                          node _mstatus_T_121 = or(mstatus, _mstatus_T_120) @[CSR.scala 85:30]
                          node _mstatus_T_122 = not(mstatus) @[CSR.scala 85:30]
                          node _mstatus_T_123 = or(_mstatus_T_122, _mstatus_T_120) @[CSR.scala 85:30]
                          node _mstatus_T_124 = not(_mstatus_T_123) @[CSR.scala 85:30]
                          node _mstatus_T_125 = mux(UInt<1>("h0"), _mstatus_T_121, _mstatus_T_124) @[CSR.scala 85:30]
                          mstatus <= _mstatus_T_125 @[CSR.scala 85:13]
                          node _mstatus_T_126 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
                          node _mstatus_T_127 = or(mstatus, _mstatus_T_126) @[CSR.scala 87:30]
                          node _mstatus_T_128 = not(mstatus) @[CSR.scala 87:30]
                          node _mstatus_T_129 = or(_mstatus_T_128, _mstatus_T_126) @[CSR.scala 87:30]
                          node _mstatus_T_130 = not(_mstatus_T_129) @[CSR.scala 87:30]
                          node _mstatus_T_131 = mux(read_MIE, _mstatus_T_127, _mstatus_T_130) @[CSR.scala 87:30]
                          mstatus <= _mstatus_T_131 @[CSR.scala 87:13]
                          node _mstatus_T_132 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
                          node _mstatus_T_133 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
                          node mstatus_hi_8 = cat(_mstatus_T_132, cur_privilege) @[Cat.scala 33:92]
                          node _mstatus_T_134 = cat(mstatus_hi_8, _mstatus_T_133) @[Cat.scala 33:92]
                          mstatus <= _mstatus_T_134 @[CSR.scala 89:13]
                          cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
                        else :
                          when io.io_interruption.io_fault_occur : @[CSR.scala 143:54]
                            node _T_52 = dshl(UInt<1>("h1"), UInt<5>("h1f")) @[CSR.scala 144:29]
                            node _T_53 = add(_T_52, UInt<4>("hb")) @[CSR.scala 144:45]
                            node _T_54 = tail(_T_53, 1) @[CSR.scala 144:45]
                            io.fault_occurs <= UInt<1>("h1") @[CSR.scala 80:21]
                            mepc <= io.pc @[CSR.scala 81:10]
                            io.fault_new_PC <= mtvec @[CSR.scala 82:21]
                            mcause <= _T_54 @[CSR.scala 83:12]
                            mtval <= io.io_interruption.mtval @[CSR.scala 84:11]
                            node _mstatus_T_135 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 85:30]
                            node _mstatus_T_136 = or(mstatus, _mstatus_T_135) @[CSR.scala 85:30]
                            node _mstatus_T_137 = not(mstatus) @[CSR.scala 85:30]
                            node _mstatus_T_138 = or(_mstatus_T_137, _mstatus_T_135) @[CSR.scala 85:30]
                            node _mstatus_T_139 = not(_mstatus_T_138) @[CSR.scala 85:30]
                            node _mstatus_T_140 = mux(UInt<1>("h0"), _mstatus_T_136, _mstatus_T_139) @[CSR.scala 85:30]
                            mstatus <= _mstatus_T_140 @[CSR.scala 85:13]
                            node _mstatus_T_141 = dshl(UInt<1>("h1"), UInt<3>("h7")) @[CSR.scala 87:30]
                            node _mstatus_T_142 = or(mstatus, _mstatus_T_141) @[CSR.scala 87:30]
                            node _mstatus_T_143 = not(mstatus) @[CSR.scala 87:30]
                            node _mstatus_T_144 = or(_mstatus_T_143, _mstatus_T_141) @[CSR.scala 87:30]
                            node _mstatus_T_145 = not(_mstatus_T_144) @[CSR.scala 87:30]
                            node _mstatus_T_146 = mux(read_MIE, _mstatus_T_142, _mstatus_T_145) @[CSR.scala 87:30]
                            mstatus <= _mstatus_T_146 @[CSR.scala 87:13]
                            node _mstatus_T_147 = bits(mstatus, 31, 13) @[CSR.scala 89:27]
                            node _mstatus_T_148 = bits(mstatus, 10, 0) @[CSR.scala 89:59]
                            node mstatus_hi_9 = cat(_mstatus_T_147, cur_privilege) @[Cat.scala 33:92]
                            node _mstatus_T_149 = cat(mstatus_hi_9, _mstatus_T_148) @[Cat.scala 33:92]
                            mstatus <= _mstatus_T_149 @[CSR.scala 89:13]
                            cur_privilege <= UInt<2>("h3") @[CSR.scala 90:19]
      else :
        io.fault_occurs <= UInt<1>("h1") @[CSR.scala 147:23]
        io.fault_new_PC <= mepc @[CSR.scala 148:23]
        node _mstatus_T_150 = dshl(UInt<1>("h1"), UInt<2>("h3")) @[CSR.scala 150:32]
        node _mstatus_T_151 = or(mstatus, _mstatus_T_150) @[CSR.scala 150:32]
        node _mstatus_T_152 = not(mstatus) @[CSR.scala 150:32]
        node _mstatus_T_153 = or(_mstatus_T_152, _mstatus_T_150) @[CSR.scala 150:32]
        node _mstatus_T_154 = not(_mstatus_T_153) @[CSR.scala 150:32]
        node _mstatus_T_155 = mux(read_MPIE, _mstatus_T_151, _mstatus_T_154) @[CSR.scala 150:32]
        mstatus <= _mstatus_T_155 @[CSR.scala 150:15]
        cur_privilege <= read_MPP @[CSR.scala 151:21]

  module PLIC :
    input clock : Clock
    input reset : Reset
    output io : { interruption : { io_fault_occur : UInt<1>, mtval : UInt<32>}}

    io.interruption.io_fault_occur <= UInt<1>("h0") @[PLIC.scala 12:34]
    io.interruption.mtval is invalid @[PLIC.scala 13:25]

  module PC :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip next_addr : UInt<32>, addr : UInt<32>}

    reg pc : UInt<32>, clock with :
      reset => (reset, UInt<32>("h0")) @[PC.scala 17:17]
    io.addr <= pc @[PC.scala 18:10]
    node _T = eq(io.cpu_state, UInt<2>("h2")) @[PC.scala 19:20]
    when _T : @[PC.scala 19:57]
      pc <= io.next_addr @[PC.scala 20:7]

  module MaxPeriodFibonacciLFSR :
    input clock : Clock
    input reset : Reset
    output io : { flip seed : { valid : UInt<1>, bits : UInt<1>[4]}, flip increment : UInt<1>, out : UInt<1>[4]}

    wire _state_WIRE : UInt<1>[4] @[PRNG.scala 46:28]
    _state_WIRE[0] <= UInt<1>("h1") @[PRNG.scala 46:28]
    _state_WIRE[1] <= UInt<1>("h0") @[PRNG.scala 46:28]
    _state_WIRE[2] <= UInt<1>("h0") @[PRNG.scala 46:28]
    _state_WIRE[3] <= UInt<1>("h0") @[PRNG.scala 46:28]
    reg state : UInt<1>[4], clock with :
      reset => (reset, _state_WIRE) @[PRNG.scala 55:49]
    when io.increment : @[PRNG.scala 69:22]
      node _T = xor(state[3], state[2]) @[LFSR.scala 15:41]
      state[0] <= _T @[PRNG.scala 70:11]
      state[1] <= state[0] @[PRNG.scala 70:11]
      state[2] <= state[1] @[PRNG.scala 70:11]
      state[3] <= state[2] @[PRNG.scala 70:11]
    when io.seed.valid : @[PRNG.scala 73:22]
      state[0] <= io.seed.bits[0] @[PRNG.scala 74:11]
      state[1] <= io.seed.bits[1] @[PRNG.scala 74:11]
      state[2] <= io.seed.bits[2] @[PRNG.scala 74:11]
      state[3] <= io.seed.bits[3] @[PRNG.scala 74:11]
    io.out <= state @[PRNG.scala 78:10]

  module PRNGWrapper :
    input clock : Clock
    input reset : Reset
    output io : { random : UInt<4>}

    inst lfsr of MaxPeriodFibonacciLFSR @[PRNGWrapper.scala 14:22]
    lfsr.clock <= clock
    lfsr.reset <= reset
    node io_random_lo = cat(lfsr.io.out[1], lfsr.io.out[0]) @[PRNGWrapper.scala 16:30]
    node io_random_hi = cat(lfsr.io.out[3], lfsr.io.out[2]) @[PRNGWrapper.scala 16:30]
    node _io_random_T = cat(io_random_hi, io_random_lo) @[PRNGWrapper.scala 16:30]
    io.random <= _io_random_T @[PRNGWrapper.scala 16:15]
    wire seedWire : { valid : UInt<1>, bits : UInt<1>[4]} @[PRNGWrapper.scala 18:24]
    seedWire.valid <= UInt<1>("h1") @[PRNGWrapper.scala 19:19]
    node _T = mux(UInt<1>("h0"), UInt<3>("h7"), UInt<3>("h0")) @[Bitwise.scala 77:12]
    node _T_1 = cat(_T, UInt<1>("h1")) @[Cat.scala 33:92]
    node _T_2 = bits(_T_1, 0, 0) @[PRNGWrapper.scala 20:51]
    node _T_3 = bits(_T_1, 1, 1) @[PRNGWrapper.scala 20:51]
    node _T_4 = bits(_T_1, 2, 2) @[PRNGWrapper.scala 20:51]
    node _T_5 = bits(_T_1, 3, 3) @[PRNGWrapper.scala 20:51]
    seedWire.bits[0] <= _T_2 @[PRNGWrapper.scala 20:18]
    seedWire.bits[1] <= _T_3 @[PRNGWrapper.scala 20:18]
    seedWire.bits[2] <= _T_4 @[PRNGWrapper.scala 20:18]
    seedWire.bits[3] <= _T_5 @[PRNGWrapper.scala 20:18]
    lfsr.io.seed.bits[0] <= seedWire.bits[0] @[PRNGWrapper.scala 21:18]
    lfsr.io.seed.bits[1] <= seedWire.bits[1] @[PRNGWrapper.scala 21:18]
    lfsr.io.seed.bits[2] <= seedWire.bits[2] @[PRNGWrapper.scala 21:18]
    lfsr.io.seed.bits[3] <= seedWire.bits[3] @[PRNGWrapper.scala 21:18]
    lfsr.io.seed.valid <= seedWire.valid @[PRNGWrapper.scala 21:18]
    lfsr.io.increment <= UInt<1>("h1") @[PRNGWrapper.scala 22:23]

  module PredictionCache :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state_type : UInt<2>, flip addr_read : UInt<32>, flip read_en : UInt<1>, flip addr_write : UInt<32>, flip dataIn : UInt<2>, flip write_en : UInt<1>, write_data_out : UInt<2>, dataOut : UInt<2>}

    wire _cache_WIRE : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_1 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_1.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_1.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_1.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_2 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_2.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_2.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_2.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_3 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_3.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_3.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_3.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_4 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_4.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_4.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_4.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_5 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_5.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_5.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_5.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_6 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_6.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_6.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_6.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_7 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_7.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_7.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_7.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_8 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_8.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_8.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_8.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_9 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_9.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_9.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_9.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_10 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_10.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_10.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_10.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_11 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_11.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_11.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_11.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_12 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_12.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_12.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_12.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_13 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_13.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_13.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_13.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_14 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_14.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_14.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_14.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_15 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>} @[PredictionCache.scala 31:63]
    _cache_WIRE_15.data <= UInt<2>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_15.address <= UInt<32>("h0") @[PredictionCache.scala 31:63]
    _cache_WIRE_15.valid <= UInt<1>("h0") @[PredictionCache.scala 31:63]
    wire _cache_WIRE_16 : { valid : UInt<1>, address : UInt<32>, data : UInt<2>}[16] @[PredictionCache.scala 31:30]
    _cache_WIRE_16[0] <= _cache_WIRE @[PredictionCache.scala 31:30]
    _cache_WIRE_16[1] <= _cache_WIRE_1 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[2] <= _cache_WIRE_2 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[3] <= _cache_WIRE_3 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[4] <= _cache_WIRE_4 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[5] <= _cache_WIRE_5 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[6] <= _cache_WIRE_6 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[7] <= _cache_WIRE_7 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[8] <= _cache_WIRE_8 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[9] <= _cache_WIRE_9 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[10] <= _cache_WIRE_10 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[11] <= _cache_WIRE_11 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[12] <= _cache_WIRE_12 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[13] <= _cache_WIRE_13 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[14] <= _cache_WIRE_14 @[PredictionCache.scala 31:30]
    _cache_WIRE_16[15] <= _cache_WIRE_15 @[PredictionCache.scala 31:30]
    reg cache : { valid : UInt<1>, address : UInt<32>, data : UInt<2>}[16], clock with :
      reset => (reset, _cache_WIRE_16) @[PredictionCache.scala 31:22]
    inst random_gen of PRNGWrapper @[PredictionCache.scala 32:26]
    random_gen.clock <= clock
    random_gen.reset <= reset
    io.write_data_out <= UInt<1>("h1") @[PredictionCache.scala 34:21]
    io.dataOut <= UInt<1>("h1") @[PredictionCache.scala 35:14]
    when io.write_en : @[PredictionCache.scala 38:21]
      wire write_idx : UInt<4> @[PredictionCache.scala 39:25]
      write_idx <= random_gen.io.random @[PredictionCache.scala 40:15]
      wire cache_hit : UInt<1> @[PredictionCache.scala 42:25]
      cache_hit <= UInt<1>("h0") @[PredictionCache.scala 43:15]
      node _T = eq(cache[15].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T : @[PredictionCache.scala 45:48]
        write_idx <= UInt<4>("hf") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_1 = eq(cache[14].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_1 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<4>("he") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_2 = eq(cache[13].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_2 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<4>("hd") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_3 = eq(cache[12].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_3 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<4>("hc") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_4 = eq(cache[11].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_4 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<4>("hb") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_5 = eq(cache[10].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_5 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<4>("ha") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_6 = eq(cache[9].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_6 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<4>("h9") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_7 = eq(cache[8].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_7 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<4>("h8") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_8 = eq(cache[7].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_8 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<3>("h7") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_9 = eq(cache[6].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_9 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<3>("h6") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_10 = eq(cache[5].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_10 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<3>("h5") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_11 = eq(cache[4].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_11 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<3>("h4") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_12 = eq(cache[3].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_12 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<2>("h3") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_13 = eq(cache[2].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_13 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<2>("h2") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_14 = eq(cache[1].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_14 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<1>("h1") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_15 = eq(cache[0].address, io.addr_write) @[PredictionCache.scala 45:29]
      when _T_15 : @[PredictionCache.scala 45:48]
        write_idx <= UInt<1>("h0") @[PredictionCache.scala 47:19]
        cache_hit <= UInt<1>("h1") @[PredictionCache.scala 48:19]
      node _T_16 = eq(cache_hit, UInt<1>("h0")) @[PredictionCache.scala 51:10]
      when _T_16 : @[PredictionCache.scala 51:22]
        node _T_17 = eq(cache[15].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_17 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<4>("hf") @[PredictionCache.scala 55:21]
        node _T_18 = eq(cache[14].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_18 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<4>("he") @[PredictionCache.scala 55:21]
        node _T_19 = eq(cache[13].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_19 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<4>("hd") @[PredictionCache.scala 55:21]
        node _T_20 = eq(cache[12].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_20 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<4>("hc") @[PredictionCache.scala 55:21]
        node _T_21 = eq(cache[11].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_21 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<4>("hb") @[PredictionCache.scala 55:21]
        node _T_22 = eq(cache[10].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_22 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<4>("ha") @[PredictionCache.scala 55:21]
        node _T_23 = eq(cache[9].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_23 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<4>("h9") @[PredictionCache.scala 55:21]
        node _T_24 = eq(cache[8].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_24 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<4>("h8") @[PredictionCache.scala 55:21]
        node _T_25 = eq(cache[7].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_25 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<3>("h7") @[PredictionCache.scala 55:21]
        node _T_26 = eq(cache[6].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_26 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<3>("h6") @[PredictionCache.scala 55:21]
        node _T_27 = eq(cache[5].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_27 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<3>("h5") @[PredictionCache.scala 55:21]
        node _T_28 = eq(cache[4].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_28 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<3>("h4") @[PredictionCache.scala 55:21]
        node _T_29 = eq(cache[3].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_29 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<2>("h3") @[PredictionCache.scala 55:21]
        node _T_30 = eq(cache[2].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_30 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<2>("h2") @[PredictionCache.scala 55:21]
        node _T_31 = eq(cache[1].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_31 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<1>("h1") @[PredictionCache.scala 55:21]
        node _T_32 = eq(cache[0].valid, UInt<1>("h0")) @[PredictionCache.scala 54:14]
        when _T_32 : @[PredictionCache.scala 54:31]
          write_idx <= UInt<1>("h0") @[PredictionCache.scala 55:21]
      when cache_hit : @[PredictionCache.scala 59:21]
        io.write_data_out <= cache[write_idx].data @[PredictionCache.scala 60:25]
      else :
        io.write_data_out <= UInt<1>("h1") @[PredictionCache.scala 62:25]
      node _T_33 = eq(io.cpu_state_type, UInt<1>("h1")) @[PredictionCache.scala 64:28]
      when _T_33 : @[PredictionCache.scala 64:67]
        cache[write_idx].valid <= UInt<1>("h1") @[PredictionCache.scala 66:30]
        cache[write_idx].address <= io.addr_write @[PredictionCache.scala 67:32]
        cache[write_idx].data <= io.dataIn @[PredictionCache.scala 68:29]
    when io.read_en : @[PredictionCache.scala 72:20]
      wire cache_hit_read : UInt<1> @[PredictionCache.scala 73:30]
      cache_hit_read <= UInt<1>("h0") @[PredictionCache.scala 74:20]
      node _T_34 = eq(cache[0].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_35 = and(cache[0].valid, _T_34) @[PredictionCache.scala 76:27]
      when _T_35 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[0].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_36 = eq(cache[1].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_37 = and(cache[1].valid, _T_36) @[PredictionCache.scala 76:27]
      when _T_37 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[1].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_38 = eq(cache[2].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_39 = and(cache[2].valid, _T_38) @[PredictionCache.scala 76:27]
      when _T_39 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[2].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_40 = eq(cache[3].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_41 = and(cache[3].valid, _T_40) @[PredictionCache.scala 76:27]
      when _T_41 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[3].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_42 = eq(cache[4].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_43 = and(cache[4].valid, _T_42) @[PredictionCache.scala 76:27]
      when _T_43 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[4].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_44 = eq(cache[5].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_45 = and(cache[5].valid, _T_44) @[PredictionCache.scala 76:27]
      when _T_45 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[5].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_46 = eq(cache[6].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_47 = and(cache[6].valid, _T_46) @[PredictionCache.scala 76:27]
      when _T_47 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[6].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_48 = eq(cache[7].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_49 = and(cache[7].valid, _T_48) @[PredictionCache.scala 76:27]
      when _T_49 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[7].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_50 = eq(cache[8].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_51 = and(cache[8].valid, _T_50) @[PredictionCache.scala 76:27]
      when _T_51 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[8].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_52 = eq(cache[9].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_53 = and(cache[9].valid, _T_52) @[PredictionCache.scala 76:27]
      when _T_53 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[9].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_54 = eq(cache[10].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_55 = and(cache[10].valid, _T_54) @[PredictionCache.scala 76:27]
      when _T_55 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[10].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_56 = eq(cache[11].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_57 = and(cache[11].valid, _T_56) @[PredictionCache.scala 76:27]
      when _T_57 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[11].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_58 = eq(cache[12].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_59 = and(cache[12].valid, _T_58) @[PredictionCache.scala 76:27]
      when _T_59 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[12].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_60 = eq(cache[13].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_61 = and(cache[13].valid, _T_60) @[PredictionCache.scala 76:27]
      when _T_61 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[13].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_62 = eq(cache[14].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_63 = and(cache[14].valid, _T_62) @[PredictionCache.scala 76:27]
      when _T_63 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[14].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_64 = eq(cache[15].address, io.addr_read) @[PredictionCache.scala 76:47]
      node _T_65 = and(cache[15].valid, _T_64) @[PredictionCache.scala 76:27]
      when _T_65 : @[PredictionCache.scala 76:65]
        io.dataOut <= cache[15].data @[PredictionCache.scala 77:20]
        cache_hit_read <= UInt<1>("h1") @[PredictionCache.scala 78:24]
      node _T_66 = eq(cache_hit_read, UInt<1>("h0")) @[PredictionCache.scala 81:10]
      when _T_66 : @[PredictionCache.scala 81:27]
        wire write_idx_read : UInt<4> @[PredictionCache.scala 83:32]
        write_idx_read <= random_gen.io.random @[PredictionCache.scala 84:22]
        node _T_67 = eq(cache[15].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_67 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<4>("hf") @[PredictionCache.scala 88:26]
        node _T_68 = eq(cache[14].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_68 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<4>("he") @[PredictionCache.scala 88:26]
        node _T_69 = eq(cache[13].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_69 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<4>("hd") @[PredictionCache.scala 88:26]
        node _T_70 = eq(cache[12].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_70 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<4>("hc") @[PredictionCache.scala 88:26]
        node _T_71 = eq(cache[11].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_71 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<4>("hb") @[PredictionCache.scala 88:26]
        node _T_72 = eq(cache[10].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_72 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<4>("ha") @[PredictionCache.scala 88:26]
        node _T_73 = eq(cache[9].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_73 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<4>("h9") @[PredictionCache.scala 88:26]
        node _T_74 = eq(cache[8].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_74 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<4>("h8") @[PredictionCache.scala 88:26]
        node _T_75 = eq(cache[7].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_75 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<3>("h7") @[PredictionCache.scala 88:26]
        node _T_76 = eq(cache[6].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_76 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<3>("h6") @[PredictionCache.scala 88:26]
        node _T_77 = eq(cache[5].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_77 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<3>("h5") @[PredictionCache.scala 88:26]
        node _T_78 = eq(cache[4].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_78 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<3>("h4") @[PredictionCache.scala 88:26]
        node _T_79 = eq(cache[3].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_79 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<2>("h3") @[PredictionCache.scala 88:26]
        node _T_80 = eq(cache[2].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_80 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<2>("h2") @[PredictionCache.scala 88:26]
        node _T_81 = eq(cache[1].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_81 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<1>("h1") @[PredictionCache.scala 88:26]
        node _T_82 = eq(cache[0].valid, UInt<1>("h0")) @[PredictionCache.scala 87:14]
        when _T_82 : @[PredictionCache.scala 87:31]
          write_idx_read <= UInt<1>("h0") @[PredictionCache.scala 88:26]
        node _T_83 = eq(io.cpu_state_type, UInt<1>("h1")) @[PredictionCache.scala 91:30]
        when _T_83 : @[PredictionCache.scala 91:69]
          cache[write_idx_read].valid <= UInt<1>("h1") @[PredictionCache.scala 93:37]
          cache[write_idx_read].address <= io.addr_read @[PredictionCache.scala 94:39]
          cache[write_idx_read].data <= UInt<1>("h1") @[PredictionCache.scala 95:36]

  module PCPrediction :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state_type : UInt<2>, flip pc : UInt<32>, flip instruction : UInt<32>, prediction_failure : { flip failure_occurs : UInt<1>, flip success_occurs : UInt<1>, flip address : UInt<32>}, predict_new_pc : UInt<32>}

    node _io_predict_new_pc_T = add(io.pc, UInt<3>("h4")) @[PCPrediction.scala 42:30]
    node _io_predict_new_pc_T_1 = tail(_io_predict_new_pc_T, 1) @[PCPrediction.scala 42:30]
    io.predict_new_pc <= _io_predict_new_pc_T_1 @[PCPrediction.scala 42:21]
    node _T = bits(io.instruction, 6, 0) @[PCPrediction.scala 44:22]
    node _T_1 = eq(_T, UInt<7>("h6f")) @[PCPrediction.scala 44:29]
    when _T_1 : @[PCPrediction.scala 44:48]
      node _raw_imm_T = bits(io.instruction, 31, 31) @[PCPrediction.scala 45:46]
      node _raw_imm_T_1 = bits(_raw_imm_T, 0, 0) @[Bitwise.scala 77:15]
      node _raw_imm_T_2 = mux(_raw_imm_T_1, UInt<11>("h7ff"), UInt<11>("h0")) @[Bitwise.scala 77:12]
      node _raw_imm_T_3 = bits(io.instruction, 31, 31) @[PCPrediction.scala 46:21]
      node _raw_imm_T_4 = bits(io.instruction, 19, 12) @[PCPrediction.scala 46:45]
      node _raw_imm_T_5 = bits(io.instruction, 20, 20) @[PCPrediction.scala 47:21]
      node _raw_imm_T_6 = bits(io.instruction, 30, 21) @[PCPrediction.scala 47:45]
      node raw_imm_lo = cat(_raw_imm_T_5, _raw_imm_T_6) @[Cat.scala 33:92]
      node raw_imm_hi_hi = cat(_raw_imm_T_2, _raw_imm_T_3) @[Cat.scala 33:92]
      node raw_imm_hi = cat(raw_imm_hi_hi, _raw_imm_T_4) @[Cat.scala 33:92]
      node raw_imm = cat(raw_imm_hi, raw_imm_lo) @[Cat.scala 33:92]
      node _io_predict_new_pc_T_2 = shl(raw_imm, 1) @[PCPrediction.scala 48:43]
      node _io_predict_new_pc_T_3 = add(io.pc, _io_predict_new_pc_T_2) @[PCPrediction.scala 48:32]
      node _io_predict_new_pc_T_4 = tail(_io_predict_new_pc_T_3, 1) @[PCPrediction.scala 48:32]
      io.predict_new_pc <= _io_predict_new_pc_T_4 @[PCPrediction.scala 48:23]
    inst cache of PredictionCache @[PCPrediction.scala 63:23]
    cache.clock <= clock
    cache.reset <= reset
    cache.io.cpu_state_type <= io.cpu_state_type @[PCPrediction.scala 70:28]
    cache.io.addr_read <= io.pc @[PCPrediction.scala 71:24]
    cache.io.read_en <= UInt<1>("h0") @[PCPrediction.scala 72:22]
    cache.io.addr_write <= io.prediction_failure.address @[PCPrediction.scala 74:25]
    cache.io.dataIn <= cache.io.write_data_out @[PCPrediction.scala 75:21]
    node prediction_result_get = or(io.prediction_failure.failure_occurs, io.prediction_failure.success_occurs) @[PCPrediction.scala 76:70]
    cache.io.write_en <= prediction_result_get @[PCPrediction.scala 77:23]
    node _T_2 = bits(io.instruction, 6, 0) @[PCPrediction.scala 80:24]
    node _T_3 = eq(_T_2, UInt<7>("h63")) @[PCPrediction.scala 80:31]
    when _T_3 : @[PCPrediction.scala 80:49]
      node _imm_T = bits(io.instruction, 31, 31) @[PCPrediction.scala 81:44]
      node _imm_T_1 = bits(_imm_T, 0, 0) @[Bitwise.scala 77:15]
      node _imm_T_2 = mux(_imm_T_1, UInt<20>("hfffff"), UInt<20>("h0")) @[Bitwise.scala 77:12]
      node _imm_T_3 = bits(io.instruction, 31, 31) @[PCPrediction.scala 82:23]
      node _imm_T_4 = bits(io.instruction, 7, 7) @[PCPrediction.scala 82:47]
      node _imm_T_5 = bits(io.instruction, 30, 25) @[PCPrediction.scala 83:23]
      node _imm_T_6 = bits(io.instruction, 11, 8) @[PCPrediction.scala 83:47]
      node imm_lo = cat(_imm_T_5, _imm_T_6) @[Cat.scala 33:92]
      node imm_hi_hi = cat(_imm_T_2, _imm_T_3) @[Cat.scala 33:92]
      node imm_hi = cat(imm_hi_hi, _imm_T_4) @[Cat.scala 33:92]
      node imm = cat(imm_hi, imm_lo) @[Cat.scala 33:92]
      cache.io.read_en <= UInt<1>("h1") @[PCPrediction.scala 84:24]
      node _T_4 = leq(cache.io.dataOut, UInt<1>("h1")) @[PCPrediction.scala 85:29]
      when _T_4 : @[PCPrediction.scala 85:49]
        node _io_predict_new_pc_T_5 = shl(imm, 1) @[PCPrediction.scala 86:43]
        node _io_predict_new_pc_T_6 = add(io.pc, _io_predict_new_pc_T_5) @[PCPrediction.scala 86:36]
        node _io_predict_new_pc_T_7 = tail(_io_predict_new_pc_T_6, 1) @[PCPrediction.scala 86:36]
        io.predict_new_pc <= _io_predict_new_pc_T_7 @[PCPrediction.scala 86:27]
      else :
        node _io_predict_new_pc_T_8 = add(io.pc, UInt<3>("h4")) @[PCPrediction.scala 88:36]
        node _io_predict_new_pc_T_9 = tail(_io_predict_new_pc_T_8, 1) @[PCPrediction.scala 88:36]
        io.predict_new_pc <= _io_predict_new_pc_T_9 @[PCPrediction.scala 88:27]
    when prediction_result_get : @[PCPrediction.scala 92:33]
      when io.prediction_failure.failure_occurs : @[PCPrediction.scala 93:50]
        node _T_5 = leq(cache.io.write_data_out, UInt<1>("h1")) @[PCPrediction.scala 94:38]
        when _T_5 : @[PCPrediction.scala 94:58]
          node _cache_io_dataIn_T = add(cache.io.write_data_out, UInt<1>("h1")) @[PCPrediction.scala 95:54]
          node _cache_io_dataIn_T_1 = tail(_cache_io_dataIn_T, 1) @[PCPrediction.scala 95:54]
          cache.io.dataIn <= _cache_io_dataIn_T_1 @[PCPrediction.scala 95:27]
        else :
          node _cache_io_dataIn_T_2 = sub(cache.io.write_data_out, UInt<1>("h1")) @[PCPrediction.scala 97:54]
          node _cache_io_dataIn_T_3 = tail(_cache_io_dataIn_T_2, 1) @[PCPrediction.scala 97:54]
          cache.io.dataIn <= _cache_io_dataIn_T_3 @[PCPrediction.scala 97:27]
      else :
        node _T_6 = leq(cache.io.write_data_out, UInt<1>("h1")) @[PCPrediction.scala 101:38]
        when _T_6 : @[PCPrediction.scala 101:58]
          node _T_7 = gt(cache.io.write_data_out, UInt<1>("h0")) @[PCPrediction.scala 102:40]
          when _T_7 : @[PCPrediction.scala 102:47]
            node _cache_io_dataIn_T_4 = sub(cache.io.write_data_out, UInt<1>("h1")) @[PCPrediction.scala 103:56]
            node _cache_io_dataIn_T_5 = tail(_cache_io_dataIn_T_4, 1) @[PCPrediction.scala 103:56]
            cache.io.dataIn <= _cache_io_dataIn_T_5 @[PCPrediction.scala 103:29]
        else :
          node _T_8 = lt(cache.io.write_data_out, UInt<2>("h3")) @[PCPrediction.scala 106:40]
          when _T_8 : @[PCPrediction.scala 106:64]
            node _cache_io_dataIn_T_6 = add(cache.io.write_data_out, UInt<1>("h1")) @[PCPrediction.scala 107:56]
            node _cache_io_dataIn_T_7 = tail(_cache_io_dataIn_T_6, 1) @[PCPrediction.scala 107:56]
            cache.io.dataIn <= _cache_io_dataIn_T_7 @[PCPrediction.scala 107:29]

  module NextPCGen :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state_type : UInt<2>, flip nextPC_control_signal : UInt<2>, flip instruction : UInt<32>, prediction_failure : { flip failure_occurs : UInt<1>, flip success_occurs : UInt<1>, flip address : UInt<32>}, flip pc : UInt<32>, flip new_pc : UInt<32>, nextPC : UInt<32>}

    inst predictor of PCPrediction @[NextPCGen.scala 21:25]
    predictor.clock <= clock
    predictor.reset <= reset
    predictor.io.cpu_state_type <= io.cpu_state_type @[NextPCGen.scala 22:30]
    predictor.io.pc <= io.pc @[NextPCGen.scala 23:18]
    predictor.io.instruction <= io.instruction @[NextPCGen.scala 24:27]
    predictor.io.prediction_failure.address <= io.prediction_failure.address @[NextPCGen.scala 25:34]
    predictor.io.prediction_failure.success_occurs <= io.prediction_failure.success_occurs @[NextPCGen.scala 25:34]
    predictor.io.prediction_failure.failure_occurs <= io.prediction_failure.failure_occurs @[NextPCGen.scala 25:34]
    io.nextPC <= io.pc @[NextPCGen.scala 27:12]
    node _T = eq(UInt<1>("h0"), io.nextPC_control_signal) @[NextPCGen.scala 28:35]
    when _T : @[NextPCGen.scala 28:35]
      io.nextPC <= predictor.io.predict_new_pc @[NextPCGen.scala 30:16]
    else :
      node _T_1 = eq(UInt<1>("h1"), io.nextPC_control_signal) @[NextPCGen.scala 28:35]
      when _T_1 : @[NextPCGen.scala 28:35]
        io.nextPC <= io.pc @[NextPCGen.scala 33:16]
      else :
        node _T_2 = eq(UInt<2>("h2"), io.nextPC_control_signal) @[NextPCGen.scala 28:35]
        when _T_2 : @[NextPCGen.scala 28:35]
          io.nextPC <= io.new_pc @[NextPCGen.scala 36:16]

  module IFID :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip signal : UInt<2>, flip in : { pc : UInt<32>, ins : UInt<32>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}}, out : { pc : UInt<32>, ins : UInt<32>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}}}

    wire _regs_WIRE : { pc : UInt<32>, ins : UInt<32>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}}
    _regs_WIRE.IF_fault.mtval is invalid
    _regs_WIRE.IF_fault.IF_fault_type <= UInt<1>("h0")
    _regs_WIRE.ins <= UInt<5>("h13")
    _regs_WIRE.pc <= UInt<1>("h0")
    reg regs : { pc : UInt<32>, ins : UInt<32>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}}, clock with :
      reset => (reset, _regs_WIRE) @[IFID.scala 28:21]
    io.out <= regs @[IFID.scala 29:10]
    node _T = eq(io.cpu_state, UInt<2>("h2")) @[IFID.scala 30:21]
    when _T : @[IFID.scala 30:60]
      node _T_1 = eq(UInt<1>("h0"), io.signal) @[IFID.scala 31:23]
      when _T_1 : @[IFID.scala 31:23]
        node _T_2 = neq(io.in.IF_fault.IF_fault_type, UInt<1>("h0")) @[IFID.scala 33:43]
        when _T_2 : @[IFID.scala 33:71]
          regs.IF_fault.mtval is invalid @[IFID.scala 34:16]
          regs.IF_fault.IF_fault_type <= UInt<1>("h0") @[IFID.scala 34:16]
          regs.ins <= UInt<5>("h13") @[IFID.scala 34:16]
          regs.pc <= UInt<1>("h0") @[IFID.scala 34:16]
          regs.IF_fault <= io.in.IF_fault @[IFID.scala 35:25]
          regs.pc <= io.in.pc @[IFID.scala 36:19]
        else :
          regs <= io.in @[IFID.scala 38:16]
      else :
        node _T_3 = eq(UInt<1>("h1"), io.signal) @[IFID.scala 31:23]
        when _T_3 : @[IFID.scala 31:23]
          skip
        else :
          node _T_4 = eq(UInt<2>("h2"), io.signal) @[IFID.scala 31:23]
          when _T_4 : @[IFID.scala 31:23]
            regs.IF_fault.mtval is invalid @[IFID.scala 45:14]
            regs.IF_fault.IF_fault_type <= UInt<1>("h0") @[IFID.scala 45:14]
            regs.ins <= UInt<5>("h13") @[IFID.scala 45:14]
            regs.pc <= UInt<1>("h0") @[IFID.scala 45:14]

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
    output io : { flip instruction : UInt<32>, flip opcode : UInt<7>, flip func3 : UInt<3>, flip func7 : UInt<7>, flip rs1 : UInt<5>, flip rs2 : UInt<5>, flip rd : UInt<5>, flip raw_imm : UInt<20>, flip csr : UInt<12>, flip cur_privilege : UInt<2>, rs1_out : UInt<5>, rs2_out : UInt<5>, rd_out : UInt<5>, raw_imm_out : UInt<20>, csr_out : UInt<12>, alu_type : UInt<4>, cmp_type : UInt<2>, unsigned : UInt<1>, nextPC_type : UInt<2>, regs_write : UInt<1>, imm_width_type : UInt<2>, operand2_type : UInt<1>, au_type : UInt<1>, write_back_type : UInt<3>, memory_read : UInt<1>, memory_write : UInt<1>, data_width : UInt<2>, csr_write : UInt<1>, operand1_type : UInt<1>, result_stage : UInt<1>, branch_type : UInt<2>, fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}}

    io.fault.ID_fault_type <= UInt<1>("h0") @[ControlUnit.scala 54:26]
    io.fault.mtval <= UInt<1>("h0") @[ControlUnit.scala 55:18]
    io.csr_out <= io.csr @[ControlUnit.scala 57:14]
    io.rs1_out <= io.rs1 @[ControlUnit.scala 58:14]
    io.rs2_out <= io.rs2 @[ControlUnit.scala 59:14]
    io.rd_out <= io.rd @[ControlUnit.scala 60:13]
    io.raw_imm_out <= io.raw_imm @[ControlUnit.scala 61:18]
    io.alu_type is invalid @[ControlUnit.scala 63:15]
    io.cmp_type is invalid @[ControlUnit.scala 64:15]
    io.unsigned is invalid @[ControlUnit.scala 65:15]
    io.nextPC_type is invalid @[ControlUnit.scala 66:18]
    io.regs_write is invalid @[ControlUnit.scala 67:17]
    io.imm_width_type is invalid @[ControlUnit.scala 68:21]
    io.operand2_type is invalid @[ControlUnit.scala 69:20]
    io.au_type is invalid @[ControlUnit.scala 70:14]
    io.write_back_type is invalid @[ControlUnit.scala 71:22]
    io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 72:18]
    io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 73:19]
    io.data_width is invalid @[ControlUnit.scala 74:17]
    io.csr_write <= UInt<1>("h0") @[ControlUnit.scala 100:16]
    io.operand1_type <= UInt<1>("h0") @[ControlUnit.scala 101:20]
    io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
    io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
    node _T = eq(UInt<6>("h33"), io.opcode) @[ControlUnit.scala 105:21]
    when _T : @[ControlUnit.scala 105:21]
      io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
      io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
      io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
      io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 112:22]
      io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 113:21]
      io.au_type <= UInt<1>("h0") @[ControlUnit.scala 114:18]
      io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 115:22]
      io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 116:23]
      io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 117:24]
      io.write_back_type <= UInt<1>("h0") @[ControlUnit.scala 118:26]
      io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 119:19]
      node _T_1 = cat(io.func3, io.func7) @[Cat.scala 33:92]
      node _T_2 = eq(UInt<1>("h0"), _T_1) @[ControlUnit.scala 120:39]
      when _T_2 : @[ControlUnit.scala 120:39]
        io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 122:23]
      else :
        node _T_3 = eq(UInt<6>("h20"), _T_1) @[ControlUnit.scala 120:39]
        when _T_3 : @[ControlUnit.scala 120:39]
          io.alu_type <= UInt<1>("h1") @[ControlUnit.scala 125:23]
        else :
          node _T_4 = eq(UInt<10>("h200"), _T_1) @[ControlUnit.scala 120:39]
          when _T_4 : @[ControlUnit.scala 120:39]
            io.alu_type <= UInt<2>("h2") @[ControlUnit.scala 128:23]
          else :
            node _T_5 = eq(UInt<10>("h300"), _T_1) @[ControlUnit.scala 120:39]
            when _T_5 : @[ControlUnit.scala 120:39]
              io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 131:23]
            else :
              node _T_6 = eq(UInt<10>("h380"), _T_1) @[ControlUnit.scala 120:39]
              when _T_6 : @[ControlUnit.scala 120:39]
                io.alu_type <= UInt<3>("h4") @[ControlUnit.scala 134:23]
              else :
                node _T_7 = eq(UInt<8>("h80"), _T_1) @[ControlUnit.scala 120:39]
                when _T_7 : @[ControlUnit.scala 120:39]
                  io.alu_type <= UInt<3>("h5") @[ControlUnit.scala 137:23]
                else :
                  node _T_8 = eq(UInt<10>("h280"), _T_1) @[ControlUnit.scala 120:39]
                  when _T_8 : @[ControlUnit.scala 120:39]
                    io.alu_type <= UInt<3>("h6") @[ControlUnit.scala 140:23]
                  else :
                    node _T_9 = eq(UInt<10>("h2a0"), _T_1) @[ControlUnit.scala 120:39]
                    when _T_9 : @[ControlUnit.scala 120:39]
                      io.alu_type <= UInt<3>("h7") @[ControlUnit.scala 143:23]
                    else :
                      node _T_10 = eq(UInt<9>("h100"), _T_1) @[ControlUnit.scala 120:39]
                      when _T_10 : @[ControlUnit.scala 120:39]
                        io.au_type <= UInt<1>("h1") @[ControlUnit.scala 146:22]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 147:23]
                      else :
                        node _T_11 = eq(UInt<9>("h180"), _T_1) @[ControlUnit.scala 120:39]
                        when _T_11 : @[ControlUnit.scala 120:39]
                          io.au_type <= UInt<1>("h1") @[ControlUnit.scala 150:22]
                          io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 151:23]
                          io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 152:23]
    else :
      node _T_12 = eq(UInt<5>("h13"), io.opcode) @[ControlUnit.scala 105:21]
      when _T_12 : @[ControlUnit.scala 105:21]
        io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
        io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
        io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
        io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
        io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 162:22]
        io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 163:21]
        io.au_type <= UInt<1>("h0") @[ControlUnit.scala 164:18]
        io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 165:22]
        io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 166:23]
        io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 167:24]
        io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 168:25]
        io.write_back_type <= UInt<1>("h0") @[ControlUnit.scala 169:26]
        io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 170:19]
        node _T_13 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 171:24]
        when _T_13 : @[ControlUnit.scala 171:24]
          io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 173:23]
        else :
          node _T_14 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 171:24]
          when _T_14 : @[ControlUnit.scala 171:24]
            io.alu_type <= UInt<2>("h2") @[ControlUnit.scala 176:23]
          else :
            node _T_15 = eq(UInt<3>("h6"), io.func3) @[ControlUnit.scala 171:24]
            when _T_15 : @[ControlUnit.scala 171:24]
              io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 179:23]
            else :
              node _T_16 = eq(UInt<3>("h7"), io.func3) @[ControlUnit.scala 171:24]
              when _T_16 : @[ControlUnit.scala 171:24]
                io.alu_type <= UInt<3>("h4") @[ControlUnit.scala 182:23]
              else :
                node _T_17 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 171:24]
                when _T_17 : @[ControlUnit.scala 171:24]
                  io.alu_type <= UInt<3>("h5") @[ControlUnit.scala 185:23]
                else :
                  node _T_18 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 171:24]
                  when _T_18 : @[ControlUnit.scala 171:24]
                    node _T_19 = eq(UInt<1>("h0"), io.func7) @[ControlUnit.scala 188:28]
                    when _T_19 : @[ControlUnit.scala 188:28]
                      io.alu_type <= UInt<3>("h6") @[ControlUnit.scala 190:27]
                    else :
                      node _T_20 = eq(UInt<6>("h20"), io.func7) @[ControlUnit.scala 188:28]
                      when _T_20 : @[ControlUnit.scala 188:28]
                        io.alu_type <= UInt<3>("h7") @[ControlUnit.scala 193:27]
                  else :
                    node _T_21 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 171:24]
                    when _T_21 : @[ControlUnit.scala 171:24]
                      io.au_type <= UInt<1>("h1") @[ControlUnit.scala 198:22]
                      io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 199:23]
                    else :
                      node _T_22 = eq(UInt<2>("h3"), io.func3) @[ControlUnit.scala 171:24]
                      when _T_22 : @[ControlUnit.scala 171:24]
                        io.au_type <= UInt<1>("h1") @[ControlUnit.scala 202:22]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 203:23]
                        io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 204:23]
      else :
        node _T_23 = eq(UInt<2>("h3"), io.opcode) @[ControlUnit.scala 105:21]
        when _T_23 : @[ControlUnit.scala 105:21]
          io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
          io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
          io.result_stage <= UInt<1>("h1") @[ControlUnit.scala 96:21]
          io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
          io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 215:22]
          io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 216:21]
          io.au_type <= UInt<1>("h0") @[ControlUnit.scala 217:18]
          io.memory_read <= UInt<1>("h1") @[ControlUnit.scala 218:22]
          io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 219:23]
          io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 220:24]
          io.write_back_type <= UInt<1>("h1") @[ControlUnit.scala 221:26]
          io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 222:25]
          io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 223:19]
          io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 224:19]
          node _T_24 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 225:24]
          when _T_24 : @[ControlUnit.scala 225:24]
            io.data_width <= UInt<1>("h0") @[ControlUnit.scala 227:25]
          else :
            node _T_25 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 225:24]
            when _T_25 : @[ControlUnit.scala 225:24]
              io.data_width <= UInt<1>("h1") @[ControlUnit.scala 230:25]
            else :
              node _T_26 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 225:24]
              when _T_26 : @[ControlUnit.scala 225:24]
                io.data_width <= UInt<2>("h2") @[ControlUnit.scala 233:25]
              else :
                node _T_27 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 225:24]
                when _T_27 : @[ControlUnit.scala 225:24]
                  io.data_width <= UInt<1>("h0") @[ControlUnit.scala 236:25]
                  io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 237:23]
                else :
                  node _T_28 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 225:24]
                  when _T_28 : @[ControlUnit.scala 225:24]
                    io.data_width <= UInt<1>("h1") @[ControlUnit.scala 240:25]
                    io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 241:23]
        else :
          node _T_29 = eq(UInt<6>("h23"), io.opcode) @[ControlUnit.scala 105:21]
          when _T_29 : @[ControlUnit.scala 105:21]
            io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
            io.rd_out <= UInt<1>("h0") @[ControlUnit.scala 90:16]
            io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
            io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
            io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 252:22]
            io.regs_write <= UInt<1>("h0") @[ControlUnit.scala 253:21]
            io.au_type <= UInt<1>("h0") @[ControlUnit.scala 254:18]
            io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 255:22]
            io.memory_write <= UInt<1>("h1") @[ControlUnit.scala 256:23]
            io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 257:24]
            io.write_back_type is invalid @[ControlUnit.scala 258:26]
            io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 259:25]
            io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 260:19]
            io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 261:19]
            node _T_30 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 262:24]
            when _T_30 : @[ControlUnit.scala 262:24]
              io.data_width <= UInt<1>("h0") @[ControlUnit.scala 264:25]
            else :
              node _T_31 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 262:24]
              when _T_31 : @[ControlUnit.scala 262:24]
                io.data_width <= UInt<1>("h1") @[ControlUnit.scala 267:25]
              else :
                node _T_32 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 262:24]
                when _T_32 : @[ControlUnit.scala 262:24]
                  io.data_width <= UInt<2>("h2") @[ControlUnit.scala 270:25]
          else :
            node _T_33 = eq(UInt<7>("h63"), io.opcode) @[ControlUnit.scala 105:21]
            when _T_33 : @[ControlUnit.scala 105:21]
              io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
              io.rd_out <= UInt<1>("h0") @[ControlUnit.scala 90:16]
              io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
              io.branch_type <= UInt<1>("h1") @[ControlUnit.scala 97:20]
              io.nextPC_type <= UInt<1>("h1") @[ControlUnit.scala 281:22]
              io.regs_write <= UInt<1>("h0") @[ControlUnit.scala 282:21]
              io.au_type <= UInt<1>("h1") @[ControlUnit.scala 283:18]
              io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 284:22]
              io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 285:23]
              io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 286:24]
              io.write_back_type is invalid @[ControlUnit.scala 287:26]
              io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 288:19]
              io.imm_width_type <= UInt<1>("h1") @[ControlUnit.scala 289:25]
              io.alu_type is invalid @[ControlUnit.scala 290:19]
              node _T_34 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 291:24]
              when _T_34 : @[ControlUnit.scala 291:24]
                io.cmp_type <= UInt<2>("h2") @[ControlUnit.scala 293:23]
              else :
                node _T_35 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 291:24]
                when _T_35 : @[ControlUnit.scala 291:24]
                  io.cmp_type <= UInt<2>("h3") @[ControlUnit.scala 296:23]
                else :
                  node _T_36 = eq(UInt<3>("h4"), io.func3) @[ControlUnit.scala 291:24]
                  when _T_36 : @[ControlUnit.scala 291:24]
                    io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 299:23]
                  else :
                    node _T_37 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 291:24]
                    when _T_37 : @[ControlUnit.scala 291:24]
                      io.cmp_type <= UInt<1>("h1") @[ControlUnit.scala 302:23]
                    else :
                      node _T_38 = eq(UInt<3>("h6"), io.func3) @[ControlUnit.scala 291:24]
                      when _T_38 : @[ControlUnit.scala 291:24]
                        io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 305:23]
                        io.cmp_type <= UInt<1>("h0") @[ControlUnit.scala 306:23]
                      else :
                        node _T_39 = eq(UInt<3>("h7"), io.func3) @[ControlUnit.scala 291:24]
                        when _T_39 : @[ControlUnit.scala 291:24]
                          io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 309:23]
                          io.cmp_type <= UInt<1>("h1") @[ControlUnit.scala 310:23]
            else :
              node _T_40 = eq(UInt<7>("h6f"), io.opcode) @[ControlUnit.scala 105:21]
              when _T_40 : @[ControlUnit.scala 105:21]
                io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
                io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
                io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
                io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
                io.nextPC_type <= UInt<2>("h3") @[ControlUnit.scala 321:22]
                io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 322:21]
                io.au_type <= UInt<1>("h0") @[ControlUnit.scala 323:18]
                io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 324:22]
                io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 325:23]
                io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 326:24]
                io.write_back_type <= UInt<2>("h2") @[ControlUnit.scala 327:26]
                io.imm_width_type <= UInt<1>("h1") @[ControlUnit.scala 328:25]
                io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 329:19]
                io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 330:19]
              else :
                node _T_41 = eq(UInt<7>("h67"), io.opcode) @[ControlUnit.scala 105:21]
                when _T_41 : @[ControlUnit.scala 105:21]
                  io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
                  io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
                  io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
                  io.branch_type <= UInt<2>("h2") @[ControlUnit.scala 97:20]
                  io.nextPC_type <= UInt<2>("h2") @[ControlUnit.scala 339:22]
                  io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 340:21]
                  io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 341:22]
                  io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 342:23]
                  io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 343:24]
                  io.write_back_type <= UInt<2>("h2") @[ControlUnit.scala 344:26]
                  io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 345:25]
                  io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 346:19]
                  io.au_type <= UInt<1>("h0") @[ControlUnit.scala 347:18]
                  io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 348:19]
                else :
                  node _T_42 = eq(UInt<6>("h37"), io.opcode) @[ControlUnit.scala 105:21]
                  when _T_42 : @[ControlUnit.scala 105:21]
                    io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                    io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
                    io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
                    io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
                    io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
                    io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 357:22]
                    io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 358:21]
                    io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 359:22]
                    io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 360:23]
                    io.imm_width_type <= UInt<2>("h3") @[ControlUnit.scala 361:25]
                    io.write_back_type <= UInt<2>("h3") @[ControlUnit.scala 362:26]
                    io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 363:19]
                    io.au_type is invalid @[ControlUnit.scala 364:18]
                    io.alu_type is invalid @[ControlUnit.scala 365:19]
                  else :
                    node _T_43 = eq(UInt<5>("h17"), io.opcode) @[ControlUnit.scala 105:21]
                    when _T_43 : @[ControlUnit.scala 105:21]
                      io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                      io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
                      io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
                      io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
                      io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
                      io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 374:22]
                      io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 375:21]
                      io.memory_read <= UInt<1>("h0") @[ControlUnit.scala 376:22]
                      io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 377:23]
                      io.imm_width_type <= UInt<2>("h3") @[ControlUnit.scala 378:25]
                      io.write_back_type <= UInt<3>("h4") @[ControlUnit.scala 379:26]
                      io.unsigned <= UInt<1>("h0") @[ControlUnit.scala 380:19]
                      io.au_type is invalid @[ControlUnit.scala 381:18]
                      io.alu_type is invalid @[ControlUnit.scala 382:19]
                    else :
                      node _T_44 = eq(UInt<7>("h73"), io.opcode) @[ControlUnit.scala 105:21]
                      when _T_44 : @[ControlUnit.scala 105:21]
                        io.result_stage <= UInt<1>("h0") @[ControlUnit.scala 96:21]
                        io.branch_type <= UInt<1>("h0") @[ControlUnit.scala 97:20]
                        io.unsigned <= UInt<1>("h1") @[ControlUnit.scala 389:19]
                        io.nextPC_type <= UInt<1>("h0") @[ControlUnit.scala 390:22]
                        io.au_type <= UInt<1>("h0") @[ControlUnit.scala 391:18]
                        io.imm_width_type <= UInt<1>("h0") @[ControlUnit.scala 392:25]
                        io.regs_write <= UInt<1>("h1") @[ControlUnit.scala 393:21]
                        io.write_back_type <= UInt<3>("h5") @[ControlUnit.scala 394:26]
                        io.memory_write <= UInt<1>("h0") @[ControlUnit.scala 395:23]
                        io.csr_write <= UInt<1>("h1") @[ControlUnit.scala 396:20]
                        io.operand1_type <= UInt<1>("h1") @[ControlUnit.scala 397:24]
                        io.rs2_out <= io.rs1 @[ControlUnit.scala 398:18]
                        node _T_45 = eq(UInt<1>("h1"), io.func3) @[ControlUnit.scala 400:24]
                        when _T_45 : @[ControlUnit.scala 400:24]
                          io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                          node _T_46 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 403:33]
                          when _T_46 : @[ControlUnit.scala 403:46]
                            io.fault.ID_fault_type <= UInt<1>("h1") @[ControlUnit.scala 404:36]
                            io.fault.mtval <= io.instruction @[ControlUnit.scala 405:28]
                          else :
                            io.operand1_type <= UInt<1>("h0") @[ControlUnit.scala 407:30]
                            io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 408:24]
                            io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 409:25]
                            io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 410:30]
                        else :
                          node _T_47 = eq(UInt<2>("h2"), io.func3) @[ControlUnit.scala 400:24]
                          when _T_47 : @[ControlUnit.scala 400:24]
                            io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                            node _T_48 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 415:33]
                            when _T_48 : @[ControlUnit.scala 415:46]
                              io.fault.ID_fault_type <= UInt<1>("h1") @[ControlUnit.scala 416:36]
                              io.fault.mtval <= io.instruction @[ControlUnit.scala 417:28]
                            else :
                              io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 419:25]
                              io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 420:30]
                          else :
                            node _T_49 = eq(UInt<2>("h3"), io.func3) @[ControlUnit.scala 400:24]
                            when _T_49 : @[ControlUnit.scala 400:24]
                              io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                              node _T_50 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 425:33]
                              when _T_50 : @[ControlUnit.scala 425:46]
                                io.fault.ID_fault_type <= UInt<1>("h1") @[ControlUnit.scala 426:36]
                                io.fault.mtval <= io.instruction @[ControlUnit.scala 427:28]
                              else :
                                io.alu_type <= UInt<4>("h8") @[ControlUnit.scala 429:25]
                                io.operand2_type <= UInt<1>("h1") @[ControlUnit.scala 430:30]
                            else :
                              node _T_51 = eq(UInt<3>("h5"), io.func3) @[ControlUnit.scala 400:24]
                              when _T_51 : @[ControlUnit.scala 400:24]
                                io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                                io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
                                node _T_52 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 435:33]
                                when _T_52 : @[ControlUnit.scala 435:46]
                                  io.fault.ID_fault_type <= UInt<1>("h1") @[ControlUnit.scala 436:36]
                                  io.fault.mtval <= io.instruction @[ControlUnit.scala 437:28]
                                else :
                                  io.operand1_type <= UInt<1>("h0") @[ControlUnit.scala 439:30]
                                  io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 440:24]
                                  node _io_raw_imm_out_T = cat(UInt<27>("h0"), io.rs1) @[Cat.scala 33:92]
                                  io.raw_imm_out <= _io_raw_imm_out_T @[ControlUnit.scala 441:27]
                                  io.alu_type <= UInt<1>("h0") @[ControlUnit.scala 442:25]
                                  io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 443:30]
                              else :
                                node _T_53 = eq(UInt<3>("h6"), io.func3) @[ControlUnit.scala 400:24]
                                when _T_53 : @[ControlUnit.scala 400:24]
                                  io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                                  io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
                                  node _T_54 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 448:33]
                                  when _T_54 : @[ControlUnit.scala 448:46]
                                    io.fault.ID_fault_type <= UInt<1>("h1") @[ControlUnit.scala 449:36]
                                    io.fault.mtval <= io.instruction @[ControlUnit.scala 450:28]
                                  else :
                                    node _io_raw_imm_out_T_1 = cat(UInt<27>("h0"), io.rs1) @[Cat.scala 33:92]
                                    io.raw_imm_out <= _io_raw_imm_out_T_1 @[ControlUnit.scala 452:27]
                                    io.alu_type <= UInt<2>("h3") @[ControlUnit.scala 453:25]
                                    io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 454:30]
                                else :
                                  node _T_55 = eq(UInt<3>("h7"), io.func3) @[ControlUnit.scala 400:24]
                                  when _T_55 : @[ControlUnit.scala 400:24]
                                    io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                                    io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
                                    node _T_56 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 459:33]
                                    when _T_56 : @[ControlUnit.scala 459:46]
                                      io.fault.ID_fault_type <= UInt<1>("h1") @[ControlUnit.scala 460:36]
                                      io.fault.mtval <= io.instruction @[ControlUnit.scala 461:28]
                                    else :
                                      node _io_raw_imm_out_T_2 = cat(UInt<27>("h0"), io.rs1) @[Cat.scala 33:92]
                                      io.raw_imm_out <= _io_raw_imm_out_T_2 @[ControlUnit.scala 463:27]
                                      io.alu_type <= UInt<4>("h8") @[ControlUnit.scala 464:25]
                                      io.operand2_type <= UInt<1>("h0") @[ControlUnit.scala 465:30]
                                  else :
                                    node _T_57 = eq(UInt<1>("h0"), io.func3) @[ControlUnit.scala 400:24]
                                    when _T_57 : @[ControlUnit.scala 400:24]
                                      io.rs1_out <= UInt<1>("h0") @[ControlUnit.scala 81:17]
                                      io.rs2_out <= UInt<1>("h0") @[ControlUnit.scala 84:17]
                                      io.csr_out <= UInt<1>("h0") @[ControlUnit.scala 87:17]
                                      io.rd_out <= UInt<1>("h0") @[ControlUnit.scala 90:16]
                                      node _T_58 = eq(UInt<1>("h0"), io.raw_imm) @[ControlUnit.scala 470:30]
                                      when _T_58 : @[ControlUnit.scala 470:30]
                                        io.fault.ID_fault_type <= UInt<2>("h3") @[ControlUnit.scala 472:38]
                                      else :
                                        node _T_59 = eq(UInt<1>("h1"), io.raw_imm) @[ControlUnit.scala 470:30]
                                        when _T_59 : @[ControlUnit.scala 470:30]
                                          io.fault.ID_fault_type <= UInt<2>("h2") @[ControlUnit.scala 475:38]
                                        else :
                                          node _T_60 = eq(UInt<10>("h302"), io.raw_imm) @[ControlUnit.scala 470:30]
                                          when _T_60 : @[ControlUnit.scala 470:30]
                                            node _T_61 = eq(io.cur_privilege, UInt<1>("h0")) @[ControlUnit.scala 478:37]
                                            when _T_61 : @[ControlUnit.scala 478:50]
                                              io.fault.ID_fault_type <= UInt<1>("h1") @[ControlUnit.scala 479:40]
                                              io.fault.mtval <= io.instruction @[ControlUnit.scala 480:32]
                                            else :
                                              io.fault.ID_fault_type <= UInt<3>("h4") @[ControlUnit.scala 482:40]

  module Registers :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip write : UInt<1>, flip rs1 : UInt<5>, flip rs2 : UInt<5>, flip rd : UInt<5>, flip write_data : UInt<32>, rs1_val : UInt<32>, rs2_val : UInt<32>}

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
    node _T = eq(io.cpu_state, UInt<1>("h0")) @[Registers.scala 27:21]
    node _T_1 = and(_T, io.write) @[Registers.scala 28:5]
    node _T_2 = neq(io.rd, UInt<1>("h0")) @[Registers.scala 29:14]
    node _T_3 = and(_T_1, _T_2) @[Registers.scala 29:5]
    when _T_3 : @[Registers.scala 30:5]
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

  module ForwardSelector :
    input clock : Clock
    input reset : Reset
    output io : { flip forward_type : UInt<2>, flip origin_val : UInt<32>, flip ex_forward : UInt<32>, flip mem_forward : UInt<32>, real_val : UInt<32>}

    io.real_val <= io.origin_val @[ForwardSelector.scala 16:15]
    node _T = eq(UInt<1>("h1"), io.forward_type) @[ForwardSelector.scala 17:27]
    when _T : @[ForwardSelector.scala 17:27]
      io.real_val <= io.ex_forward @[ForwardSelector.scala 19:19]
    else :
      node _T_1 = eq(UInt<2>("h2"), io.forward_type) @[ForwardSelector.scala 17:27]
      when _T_1 : @[ForwardSelector.scala 17:27]
        io.real_val <= io.mem_forward @[ForwardSelector.scala 22:19]
      else :
        node _T_2 = eq(UInt<1>("h0"), io.forward_type) @[ForwardSelector.scala 17:27]
        when _T_2 : @[ForwardSelector.scala 17:27]
          io.real_val <= io.origin_val @[ForwardSelector.scala 25:19]

  module ForwardSelector_1 :
    input clock : Clock
    input reset : Reset
    output io : { flip forward_type : UInt<2>, flip origin_val : UInt<32>, flip ex_forward : UInt<32>, flip mem_forward : UInt<32>, real_val : UInt<32>}

    io.real_val <= io.origin_val @[ForwardSelector.scala 16:15]
    node _T = eq(UInt<1>("h1"), io.forward_type) @[ForwardSelector.scala 17:27]
    when _T : @[ForwardSelector.scala 17:27]
      io.real_val <= io.ex_forward @[ForwardSelector.scala 19:19]
    else :
      node _T_1 = eq(UInt<2>("h2"), io.forward_type) @[ForwardSelector.scala 17:27]
      when _T_1 : @[ForwardSelector.scala 17:27]
        io.real_val <= io.mem_forward @[ForwardSelector.scala 22:19]
      else :
        node _T_2 = eq(UInt<1>("h0"), io.forward_type) @[ForwardSelector.scala 17:27]
        when _T_2 : @[ForwardSelector.scala 17:27]
          io.real_val <= io.origin_val @[ForwardSelector.scala 25:19]

  module ForwardSelector_2 :
    input clock : Clock
    input reset : Reset
    output io : { flip forward_type : UInt<2>, flip origin_val : UInt<32>, flip ex_forward : UInt<32>, flip mem_forward : UInt<32>, real_val : UInt<32>}

    io.real_val <= io.origin_val @[ForwardSelector.scala 16:15]
    node _T = eq(UInt<1>("h1"), io.forward_type) @[ForwardSelector.scala 17:27]
    when _T : @[ForwardSelector.scala 17:27]
      io.real_val <= io.ex_forward @[ForwardSelector.scala 19:19]
    else :
      node _T_1 = eq(UInt<2>("h2"), io.forward_type) @[ForwardSelector.scala 17:27]
      when _T_1 : @[ForwardSelector.scala 17:27]
        io.real_val <= io.mem_forward @[ForwardSelector.scala 22:19]
      else :
        node _T_2 = eq(UInt<1>("h0"), io.forward_type) @[ForwardSelector.scala 17:27]
        when _T_2 : @[ForwardSelector.scala 17:27]
          io.real_val <= io.origin_val @[ForwardSelector.scala 25:19]

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

  module WBSelectorTop :
    input clock : Clock
    input reset : Reset
    output io : { flip au_type : UInt<1>, flip alu_result : UInt<32>, flip cmp_result : UInt<1>, flip write_back_type : UInt<3>, flip imm : UInt<32>, flip mem_out : UInt<32>, flip pc4 : UInt<32>, flip pcImm : UInt<32>, flip csr_val : UInt<32>, write_data : UInt<32>}

    inst au_selector of AUSelector @[WBSelectorTop.scala 24:35]
    au_selector.clock <= clock
    au_selector.reset <= reset
    inst wb_selector of WriteDataSelector @[WBSelectorTop.scala 25:35]
    wb_selector.clock <= clock
    wb_selector.reset <= reset
    au_selector.io.au_type <= io.au_type @[WBSelectorTop.scala 27:26]
    au_selector.io.alu_result <= io.alu_result @[WBSelectorTop.scala 28:29]
    au_selector.io.cmp_result <= io.cmp_result @[WBSelectorTop.scala 29:29]
    wb_selector.io.au_out <= au_selector.io.au_out @[WBSelectorTop.scala 30:25]
    wb_selector.io.write_back_type <= io.write_back_type @[WBSelectorTop.scala 33:34]
    wb_selector.io.imm <= io.imm @[WBSelectorTop.scala 34:22]
    wb_selector.io.mem_out <= io.mem_out @[WBSelectorTop.scala 35:26]
    wb_selector.io.pc4 <= io.pc4 @[WBSelectorTop.scala 36:22]
    wb_selector.io.pcImm <= io.pcImm @[WBSelectorTop.scala 37:24]
    wb_selector.io.csr <= io.csr_val @[WBSelectorTop.scala 38:22]
    io.write_data <= wb_selector.io.write_data @[WBSelectorTop.scala 39:17]

  module AUSelector_1 :
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

  module WriteDataSelector_1 :
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

  module WBSelectorTop_1 :
    input clock : Clock
    input reset : Reset
    output io : { flip au_type : UInt<1>, flip alu_result : UInt<32>, flip cmp_result : UInt<1>, flip write_back_type : UInt<3>, flip imm : UInt<32>, flip mem_out : UInt<32>, flip pc4 : UInt<32>, flip pcImm : UInt<32>, flip csr_val : UInt<32>, write_data : UInt<32>}

    inst au_selector of AUSelector_1 @[WBSelectorTop.scala 24:35]
    au_selector.clock <= clock
    au_selector.reset <= reset
    inst wb_selector of WriteDataSelector_1 @[WBSelectorTop.scala 25:35]
    wb_selector.clock <= clock
    wb_selector.reset <= reset
    au_selector.io.au_type <= io.au_type @[WBSelectorTop.scala 27:26]
    au_selector.io.alu_result <= io.alu_result @[WBSelectorTop.scala 28:29]
    au_selector.io.cmp_result <= io.cmp_result @[WBSelectorTop.scala 29:29]
    wb_selector.io.au_out <= au_selector.io.au_out @[WBSelectorTop.scala 30:25]
    wb_selector.io.write_back_type <= io.write_back_type @[WBSelectorTop.scala 33:34]
    wb_selector.io.imm <= io.imm @[WBSelectorTop.scala 34:22]
    wb_selector.io.mem_out <= io.mem_out @[WBSelectorTop.scala 35:26]
    wb_selector.io.pc4 <= io.pc4 @[WBSelectorTop.scala 36:22]
    wb_selector.io.pcImm <= io.pcImm @[WBSelectorTop.scala 37:24]
    wb_selector.io.csr <= io.csr_val @[WBSelectorTop.scala 38:22]
    io.write_data <= wb_selector.io.write_data @[WBSelectorTop.scala 39:17]

  module IDEX :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip signal : UInt<2>, flip in : { pc : UInt<32>, ins : UInt<32>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, op1 : UInt<32>, op2 : UInt<32>, csr_val_to_reg : UInt<32>, alu_op : UInt<4>, cmp_op : UInt<2>, unsigned : UInt<1>, mem_signal : { read_data_en : UInt<1>, write_data_en : UInt<1>, data_width : UInt<2>, data_to_write : UInt<32>}, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, result_stage : UInt<1>, branch_type : UInt<2>, ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}}, out : { pc : UInt<32>, ins : UInt<32>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, op1 : UInt<32>, op2 : UInt<32>, csr_val_to_reg : UInt<32>, alu_op : UInt<4>, cmp_op : UInt<2>, unsigned : UInt<1>, mem_signal : { read_data_en : UInt<1>, write_data_en : UInt<1>, data_width : UInt<2>, data_to_write : UInt<32>}, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, result_stage : UInt<1>, branch_type : UInt<2>, ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}}}

    wire _regs_WIRE : { pc : UInt<32>, ins : UInt<32>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, op1 : UInt<32>, op2 : UInt<32>, csr_val_to_reg : UInt<32>, alu_op : UInt<4>, cmp_op : UInt<2>, unsigned : UInt<1>, mem_signal : { read_data_en : UInt<1>, write_data_en : UInt<1>, data_width : UInt<2>, data_to_write : UInt<32>}, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, result_stage : UInt<1>, branch_type : UInt<2>, ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}}
    _regs_WIRE.ID_fault.mtval is invalid
    _regs_WIRE.ID_fault.ID_fault_type <= UInt<1>("h0")
    _regs_WIRE.branch_type <= UInt<1>("h0")
    _regs_WIRE.result_stage <= UInt<1>("h0")
    _regs_WIRE.wb_signal.csr_write <= UInt<1>("h0")
    _regs_WIRE.wb_signal.csr_idx <= UInt<1>("h0")
    _regs_WIRE.wb_signal.write_reg <= UInt<1>("h0")
    _regs_WIRE.wb_signal.rd <= UInt<1>("h0")
    _regs_WIRE.wb_signal.imm <= UInt<1>("h0")
    _regs_WIRE.wb_signal.wb_type <= UInt<1>("h0")
    _regs_WIRE.wb_signal.au_type <= UInt<1>("h0")
    _regs_WIRE.mem_signal.data_to_write <= UInt<1>("h0")
    _regs_WIRE.mem_signal.data_width <= UInt<1>("h0")
    _regs_WIRE.mem_signal.write_data_en <= UInt<1>("h0")
    _regs_WIRE.mem_signal.read_data_en <= UInt<1>("h0")
    _regs_WIRE.unsigned <= UInt<1>("h0")
    _regs_WIRE.cmp_op <= UInt<2>("h2")
    _regs_WIRE.alu_op <= UInt<1>("h0")
    _regs_WIRE.csr_val_to_reg <= UInt<1>("h0")
    _regs_WIRE.op2 <= UInt<1>("h0")
    _regs_WIRE.op1 <= UInt<1>("h0")
    _regs_WIRE.IF_fault.mtval <= UInt<1>("h0")
    _regs_WIRE.IF_fault.IF_fault_type <= UInt<1>("h0")
    _regs_WIRE.ins <= UInt<1>("h0")
    _regs_WIRE.pc <= UInt<1>("h0")
    reg regs : { pc : UInt<32>, ins : UInt<32>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, op1 : UInt<32>, op2 : UInt<32>, csr_val_to_reg : UInt<32>, alu_op : UInt<4>, cmp_op : UInt<2>, unsigned : UInt<1>, mem_signal : { read_data_en : UInt<1>, write_data_en : UInt<1>, data_width : UInt<2>, data_to_write : UInt<32>}, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, result_stage : UInt<1>, branch_type : UInt<2>, ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}}, clock with :
      reset => (reset, _regs_WIRE) @[IDEX.scala 93:21]
    io.out <= regs @[IDEX.scala 94:10]
    node _T = eq(io.cpu_state, UInt<2>("h2")) @[IDEX.scala 95:21]
    when _T : @[IDEX.scala 95:60]
      node _T_1 = eq(UInt<1>("h0"), io.signal) @[IDEX.scala 96:23]
      when _T_1 : @[IDEX.scala 96:23]
        node _T_2 = neq(io.in.IF_fault.IF_fault_type, UInt<1>("h0")) @[IDEX.scala 98:43]
        node _T_3 = neq(io.in.ID_fault.ID_fault_type, UInt<1>("h0")) @[IDEX.scala 99:40]
        node _T_4 = or(_T_2, _T_3) @[IDEX.scala 98:70]
        when _T_4 : @[IDEX.scala 99:68]
          regs.ID_fault.mtval is invalid @[IDEX.scala 100:16]
          regs.ID_fault.ID_fault_type <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.branch_type <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.result_stage <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.wb_signal.csr_write <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.wb_signal.csr_idx <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.wb_signal.write_reg <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.wb_signal.rd <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.wb_signal.imm <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.wb_signal.wb_type <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.wb_signal.au_type <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.mem_signal.data_to_write <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.mem_signal.data_width <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.mem_signal.write_data_en <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.mem_signal.read_data_en <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.unsigned <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.cmp_op <= UInt<2>("h2") @[IDEX.scala 100:16]
          regs.alu_op <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.csr_val_to_reg <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.op2 <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.op1 <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.IF_fault.mtval <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.IF_fault.IF_fault_type <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.ins <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.pc <= UInt<1>("h0") @[IDEX.scala 100:16]
          regs.IF_fault <= io.in.IF_fault @[IDEX.scala 101:25]
          regs.ID_fault <= io.in.ID_fault @[IDEX.scala 102:25]
          regs.pc <= io.in.pc @[IDEX.scala 103:19]
        else :
          regs <= io.in @[IDEX.scala 105:16]
      else :
        node _T_5 = eq(UInt<1>("h1"), io.signal) @[IDEX.scala 96:23]
        when _T_5 : @[IDEX.scala 96:23]
          skip
        else :
          node _T_6 = eq(UInt<2>("h2"), io.signal) @[IDEX.scala 96:23]
          when _T_6 : @[IDEX.scala 96:23]
            regs.ID_fault.mtval is invalid @[IDEX.scala 112:14]
            regs.ID_fault.ID_fault_type <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.branch_type <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.result_stage <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.wb_signal.csr_write <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.wb_signal.csr_idx <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.wb_signal.write_reg <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.wb_signal.rd <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.wb_signal.imm <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.wb_signal.wb_type <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.wb_signal.au_type <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.mem_signal.data_to_write <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.mem_signal.data_width <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.mem_signal.write_data_en <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.mem_signal.read_data_en <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.unsigned <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.cmp_op <= UInt<2>("h2") @[IDEX.scala 112:14]
            regs.alu_op <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.csr_val_to_reg <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.op2 <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.op1 <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.IF_fault.mtval <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.IF_fault.IF_fault_type <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.ins <= UInt<1>("h0") @[IDEX.scala 112:14]
            regs.pc <= UInt<1>("h0") @[IDEX.scala 112:14]

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

  module EXMEM :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip signal : UInt<2>, flip in : { pc : UInt<32>, ins : UInt<32>, resultStage : UInt<1>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}, csr_val_to_reg : UInt<32>, unsigned : UInt<1>, mem_signal : { read_data_en : UInt<1>, write_data_en : UInt<1>, data_width : UInt<2>, data_to_write : UInt<32>}, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, alu_out : UInt<32>, cmp_out : UInt<1>}, out : { pc : UInt<32>, ins : UInt<32>, resultStage : UInt<1>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}, csr_val_to_reg : UInt<32>, unsigned : UInt<1>, mem_signal : { read_data_en : UInt<1>, write_data_en : UInt<1>, data_width : UInt<2>, data_to_write : UInt<32>}, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, alu_out : UInt<32>, cmp_out : UInt<1>}}

    wire _regs_WIRE : { pc : UInt<32>, ins : UInt<32>, resultStage : UInt<1>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}, csr_val_to_reg : UInt<32>, unsigned : UInt<1>, mem_signal : { read_data_en : UInt<1>, write_data_en : UInt<1>, data_width : UInt<2>, data_to_write : UInt<32>}, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, alu_out : UInt<32>, cmp_out : UInt<1>}
    _regs_WIRE.cmp_out <= UInt<1>("h0")
    _regs_WIRE.alu_out <= UInt<1>("h0")
    _regs_WIRE.wb_signal.csr_write <= UInt<1>("h0")
    _regs_WIRE.wb_signal.csr_idx <= UInt<1>("h0")
    _regs_WIRE.wb_signal.write_reg <= UInt<1>("h0")
    _regs_WIRE.wb_signal.rd <= UInt<1>("h0")
    _regs_WIRE.wb_signal.imm <= UInt<1>("h0")
    _regs_WIRE.wb_signal.wb_type <= UInt<1>("h0")
    _regs_WIRE.wb_signal.au_type <= UInt<1>("h0")
    _regs_WIRE.mem_signal.data_to_write <= UInt<1>("h0")
    _regs_WIRE.mem_signal.data_width <= UInt<1>("h0")
    _regs_WIRE.mem_signal.write_data_en <= UInt<1>("h0")
    _regs_WIRE.mem_signal.read_data_en <= UInt<1>("h0")
    _regs_WIRE.unsigned <= UInt<1>("h0")
    _regs_WIRE.csr_val_to_reg <= UInt<1>("h0")
    _regs_WIRE.ID_fault.mtval <= UInt<1>("h0")
    _regs_WIRE.ID_fault.ID_fault_type <= UInt<1>("h0")
    _regs_WIRE.IF_fault.mtval <= UInt<1>("h0")
    _regs_WIRE.IF_fault.IF_fault_type <= UInt<1>("h0")
    _regs_WIRE.resultStage <= UInt<1>("h0")
    _regs_WIRE.ins <= UInt<1>("h0")
    _regs_WIRE.pc <= UInt<1>("h0")
    reg regs : { pc : UInt<32>, ins : UInt<32>, resultStage : UInt<1>, IF_fault : { IF_fault_type : UInt<2>, mtval : UInt<32>}, ID_fault : { ID_fault_type : UInt<3>, mtval : UInt<32>}, csr_val_to_reg : UInt<32>, unsigned : UInt<1>, mem_signal : { read_data_en : UInt<1>, write_data_en : UInt<1>, data_width : UInt<2>, data_to_write : UInt<32>}, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, alu_out : UInt<32>, cmp_out : UInt<1>}, clock with :
      reset => (reset, _regs_WIRE) @[EXMEM.scala 63:21]
    io.out <= regs @[EXMEM.scala 64:10]
    node _T = eq(io.cpu_state, UInt<2>("h2")) @[EXMEM.scala 65:21]
    when _T : @[EXMEM.scala 65:60]
      node _T_1 = eq(UInt<1>("h0"), io.signal) @[EXMEM.scala 66:23]
      when _T_1 : @[EXMEM.scala 66:23]
        node _T_2 = neq(io.in.IF_fault.IF_fault_type, UInt<1>("h0")) @[EXMEM.scala 68:43]
        node _T_3 = neq(io.in.ID_fault.ID_fault_type, UInt<1>("h0")) @[EXMEM.scala 69:40]
        node _T_4 = or(_T_2, _T_3) @[EXMEM.scala 68:70]
        when _T_4 : @[EXMEM.scala 69:68]
          regs.cmp_out <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.alu_out <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.wb_signal.csr_write <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.wb_signal.csr_idx <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.wb_signal.write_reg <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.wb_signal.rd <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.wb_signal.imm <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.wb_signal.wb_type <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.wb_signal.au_type <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.mem_signal.data_to_write <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.mem_signal.data_width <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.mem_signal.write_data_en <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.mem_signal.read_data_en <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.unsigned <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.csr_val_to_reg <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.ID_fault.mtval <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.ID_fault.ID_fault_type <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.IF_fault.mtval <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.IF_fault.IF_fault_type <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.resultStage <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.ins <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.pc <= UInt<1>("h0") @[EXMEM.scala 70:16]
          regs.IF_fault <= io.in.IF_fault @[EXMEM.scala 71:25]
          regs.ID_fault <= io.in.ID_fault @[EXMEM.scala 72:25]
          regs.pc <= io.in.pc @[EXMEM.scala 73:19]
        else :
          regs <= io.in @[EXMEM.scala 75:16]
      else :
        node _T_5 = eq(UInt<1>("h1"), io.signal) @[EXMEM.scala 66:23]
        when _T_5 : @[EXMEM.scala 66:23]
          skip
        else :
          node _T_6 = eq(UInt<2>("h2"), io.signal) @[EXMEM.scala 66:23]
          when _T_6 : @[EXMEM.scala 66:23]
            regs.cmp_out <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.alu_out <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.wb_signal.csr_write <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.wb_signal.csr_idx <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.wb_signal.write_reg <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.wb_signal.rd <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.wb_signal.imm <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.wb_signal.wb_type <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.wb_signal.au_type <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.mem_signal.data_to_write <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.mem_signal.data_width <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.mem_signal.write_data_en <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.mem_signal.read_data_en <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.unsigned <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.csr_val_to_reg <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.ID_fault.mtval <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.ID_fault.ID_fault_type <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.IF_fault.mtval <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.IF_fault.IF_fault_type <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.resultStage <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.ins <= UInt<1>("h0") @[EXMEM.scala 82:14]
            regs.pc <= UInt<1>("h0") @[EXMEM.scala 82:14]

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

  module MemWriteSelector :
    input clock : Clock
    input reset : Reset
    output io : { flip uart_load : UInt<1>, flip uart_in : { mem_read : UInt<1>, mem_write : UInt<1>, data_to_write : UInt<32>, data_addr : UInt<32>, data_width : UInt<2>}, flip cpu_read_data : UInt<1>, flip cpu_write_data : UInt<1>, flip cpu_data_width : UInt<2>, flip cpu_data_addr : UInt<32>, flip cpu_data_write : UInt<32>, read_data : UInt<1>, write_data : UInt<1>, data_width : UInt<2>, data_addr : UInt<32>, data_write : UInt<32>}

    when io.uart_load : @[MemWriteSelector.scala 29:21]
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

  module MEMWB :
    input clock : Clock
    input reset : Reset
    output io : { flip cpu_state : UInt<2>, flip signal : UInt<2>, flip in : { pc : UInt<32>, ins : UInt<32>, csr_val_to_reg : UInt<32>, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, alu_out : UInt<32>, cmp_out : UInt<1>, data_out : UInt<32>}, out : { pc : UInt<32>, ins : UInt<32>, csr_val_to_reg : UInt<32>, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, alu_out : UInt<32>, cmp_out : UInt<1>, data_out : UInt<32>}}

    wire _regs_WIRE : { pc : UInt<32>, ins : UInt<32>, csr_val_to_reg : UInt<32>, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, alu_out : UInt<32>, cmp_out : UInt<1>, data_out : UInt<32>}
    _regs_WIRE.data_out <= UInt<1>("h0")
    _regs_WIRE.cmp_out <= UInt<1>("h0")
    _regs_WIRE.alu_out <= UInt<1>("h0")
    _regs_WIRE.wb_signal.csr_write <= UInt<1>("h0")
    _regs_WIRE.wb_signal.csr_idx <= UInt<1>("h0")
    _regs_WIRE.wb_signal.write_reg <= UInt<1>("h0")
    _regs_WIRE.wb_signal.rd <= UInt<1>("h0")
    _regs_WIRE.wb_signal.imm <= UInt<1>("h0")
    _regs_WIRE.wb_signal.wb_type <= UInt<1>("h0")
    _regs_WIRE.wb_signal.au_type <= UInt<1>("h0")
    _regs_WIRE.csr_val_to_reg <= UInt<1>("h0")
    _regs_WIRE.ins <= UInt<1>("h0")
    _regs_WIRE.pc <= UInt<1>("h0")
    reg regs : { pc : UInt<32>, ins : UInt<32>, csr_val_to_reg : UInt<32>, wb_signal : { au_type : UInt<1>, wb_type : UInt<3>, imm : UInt<32>, rd : UInt<5>, write_reg : UInt<1>, csr_idx : UInt<12>, csr_write : UInt<1>}, alu_out : UInt<32>, cmp_out : UInt<1>, data_out : UInt<32>}, clock with :
      reset => (reset, _regs_WIRE) @[MEMWB.scala 45:20]
    io.out <= regs @[MEMWB.scala 46:9]
    node _T = eq(io.cpu_state, UInt<2>("h2")) @[MEMWB.scala 47:21]
    when _T : @[MEMWB.scala 47:60]
      node _T_1 = eq(UInt<1>("h0"), io.signal) @[MEMWB.scala 48:23]
      when _T_1 : @[MEMWB.scala 48:23]
        regs <= io.in @[MEMWB.scala 50:14]
      else :
        node _T_2 = eq(UInt<1>("h1"), io.signal) @[MEMWB.scala 48:23]
        when _T_2 : @[MEMWB.scala 48:23]
          skip
        else :
          node _T_3 = eq(UInt<2>("h2"), io.signal) @[MEMWB.scala 48:23]
          when _T_3 : @[MEMWB.scala 48:23]
            regs.data_out <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.cmp_out <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.alu_out <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.wb_signal.csr_write <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.wb_signal.csr_idx <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.wb_signal.write_reg <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.wb_signal.rd <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.wb_signal.imm <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.wb_signal.wb_type <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.wb_signal.au_type <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.csr_val_to_reg <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.ins <= UInt<1>("h0") @[MEMWB.scala 56:14]
            regs.pc <= UInt<1>("h0") @[MEMWB.scala 56:14]

  module AUSelector_2 :
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

  module WriteDataSelector_2 :
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

  module WBSelectorTop_2 :
    input clock : Clock
    input reset : Reset
    output io : { flip au_type : UInt<1>, flip alu_result : UInt<32>, flip cmp_result : UInt<1>, flip write_back_type : UInt<3>, flip imm : UInt<32>, flip mem_out : UInt<32>, flip pc4 : UInt<32>, flip pcImm : UInt<32>, flip csr_val : UInt<32>, write_data : UInt<32>}

    inst au_selector of AUSelector_2 @[WBSelectorTop.scala 24:35]
    au_selector.clock <= clock
    au_selector.reset <= reset
    inst wb_selector of WriteDataSelector_2 @[WBSelectorTop.scala 25:35]
    wb_selector.clock <= clock
    wb_selector.reset <= reset
    au_selector.io.au_type <= io.au_type @[WBSelectorTop.scala 27:26]
    au_selector.io.alu_result <= io.alu_result @[WBSelectorTop.scala 28:29]
    au_selector.io.cmp_result <= io.cmp_result @[WBSelectorTop.scala 29:29]
    wb_selector.io.au_out <= au_selector.io.au_out @[WBSelectorTop.scala 30:25]
    wb_selector.io.write_back_type <= io.write_back_type @[WBSelectorTop.scala 33:34]
    wb_selector.io.imm <= io.imm @[WBSelectorTop.scala 34:22]
    wb_selector.io.mem_out <= io.mem_out @[WBSelectorTop.scala 35:26]
    wb_selector.io.pc4 <= io.pc4 @[WBSelectorTop.scala 36:22]
    wb_selector.io.pcImm <= io.pcImm @[WBSelectorTop.scala 37:24]
    wb_selector.io.csr <= io.csr_val @[WBSelectorTop.scala 38:22]
    io.write_data <= wb_selector.io.write_data @[WBSelectorTop.scala 39:17]

  module CoreTop :
    input clock : Clock
    input reset : Reset
    output io : { flip external : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}, vga : { flip value : UInt<32>}}, flip external_signal : { load_data_mode : UInt<1>}}

    inst conflictController of ConflictController @[CoreTop.scala 22:34]
    conflictController.clock <= clock
    conflictController.reset <= reset
    inst state of CPUState @[CoreTop.scala 23:21]
    state.clock <= clock
    state.reset <= reset
    inst CSR of CSR @[CoreTop.scala 26:19]
    CSR.clock <= clock
    CSR.reset <= reset
    inst PLIC of PLIC @[CoreTop.scala 27:20]
    PLIC.clock <= clock
    PLIC.reset <= reset
    inst pc of PC @[CoreTop.scala 31:18]
    pc.clock <= clock
    pc.reset <= reset
    inst nextPCGen of NextPCGen @[CoreTop.scala 32:25]
    nextPCGen.clock <= clock
    nextPCGen.reset <= reset
    inst IF_ID of IFID @[CoreTop.scala 34:21]
    IF_ID.clock <= clock
    IF_ID.reset <= reset
    inst insDecode of InstructionDecoder @[CoreTop.scala 35:25]
    insDecode.clock <= clock
    insDecode.reset <= reset
    inst CU of ControlUnit @[CoreTop.scala 36:18]
    CU.clock <= clock
    CU.reset <= reset
    inst regs of Registers @[CoreTop.scala 38:20]
    regs.clock <= clock
    regs.reset <= reset
    inst immGen of ImmGen @[CoreTop.scala 39:22]
    immGen.clock <= clock
    immGen.reset <= reset
    inst rs1_forward_selector of ForwardSelector @[CoreTop.scala 40:36]
    rs1_forward_selector.clock <= clock
    rs1_forward_selector.reset <= reset
    inst rs2_forward_selector of ForwardSelector_1 @[CoreTop.scala 41:36]
    rs2_forward_selector.clock <= clock
    rs2_forward_selector.reset <= reset
    inst csr_forward_selector of ForwardSelector_2 @[CoreTop.scala 42:36]
    csr_forward_selector.clock <= clock
    csr_forward_selector.reset <= reset
    inst operandSelector of OperandSelector @[CoreTop.scala 43:31]
    operandSelector.clock <= clock
    operandSelector.reset <= reset
    inst EXForwarding of WBSelectorTop @[CoreTop.scala 44:28]
    EXForwarding.clock <= clock
    EXForwarding.reset <= reset
    inst MEMForwarding of WBSelectorTop_1 @[CoreTop.scala 45:29]
    MEMForwarding.clock <= clock
    MEMForwarding.reset <= reset
    inst ID_EX of IDEX @[CoreTop.scala 47:21]
    ID_EX.clock <= clock
    ID_EX.reset <= reset
    inst ALU of ALU @[CoreTop.scala 48:19]
    ALU.clock <= clock
    ALU.reset <= reset
    inst CMP of CMP @[CoreTop.scala 49:19]
    CMP.clock <= clock
    CMP.reset <= reset
    inst EX_MEM of EXMEM @[CoreTop.scala 52:22]
    EX_MEM.clock <= clock
    EX_MEM.reset <= reset
    inst memory of MemoryDispatch @[CoreTop.scala 53:22]
    memory.clock <= clock
    memory.reset <= reset
    inst uartLoader of UARTLoader @[CoreTop.scala 54:26]
    uartLoader.clock <= clock
    uartLoader.reset <= reset
    inst memInSelector of MemWriteSelector @[CoreTop.scala 55:29]
    memInSelector.clock <= clock
    memInSelector.reset <= reset
    inst MEM_WB of MEMWB @[CoreTop.scala 58:22]
    MEM_WB.clock <= clock
    MEM_WB.reset <= reset
    inst wb_selector of WBSelectorTop_2 @[CoreTop.scala 59:27]
    wb_selector.clock <= clock
    wb_selector.reset <= reset
    state.io.stall <= io.external_signal.load_data_mode @[CoreTop.scala 62:18]
    node _nextPCGen_io_cpu_state_type_T = mux(io.external_signal.load_data_mode, UInt<1>("h0"), state.io.cpu_state) @[CoreTop.scala 65:36]
    nextPCGen.io.cpu_state_type <= _nextPCGen_io_cpu_state_type_T @[CoreTop.scala 65:30]
    nextPCGen.io.prediction_failure.address <= conflictController.io.prediction_failure.address @[CoreTop.scala 66:34]
    nextPCGen.io.prediction_failure.success_occurs <= conflictController.io.prediction_failure.success_occurs @[CoreTop.scala 66:34]
    nextPCGen.io.prediction_failure.failure_occurs <= conflictController.io.prediction_failure.failure_occurs @[CoreTop.scala 66:34]
    nextPCGen.io.nextPC_control_signal <= conflictController.io.next_control_signal @[CoreTop.scala 67:38]
    nextPCGen.io.pc <= pc.io.addr @[CoreTop.scala 68:19]
    nextPCGen.io.new_pc <= conflictController.io.new_pc @[CoreTop.scala 69:23]
    nextPCGen.io.instruction <= memory.io.ins_out @[CoreTop.scala 70:28]
    pc.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 72:19]
    pc.io.next_addr <= nextPCGen.io.nextPC @[CoreTop.scala 73:19]
    memory.io.ins_addr <= pc.io.addr @[CoreTop.scala 74:22]
    IF_ID.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 76:21]
    IF_ID.io.signal <= conflictController.io.IFID_control_signal @[CoreTop.scala 77:19]
    IF_ID.io.in.pc <= pc.io.addr @[CoreTop.scala 78:18]
    IF_ID.io.in.ins <= memory.io.ins_out @[CoreTop.scala 79:19]
    IF_ID.io.in.IF_fault.mtval <= memory.io.IF_fault.mtval @[CoreTop.scala 80:24]
    IF_ID.io.in.IF_fault.IF_fault_type <= memory.io.IF_fault.IF_fault_type @[CoreTop.scala 80:24]
    insDecode.io.instruction <= IF_ID.io.out.ins @[CoreTop.scala 83:28]
    CU.io.instruction <= IF_ID.io.out.ins @[CoreTop.scala 84:21]
    CU.io.csr <= insDecode.io.csr @[CoreTop.scala 85:13]
    CU.io.opcode <= insDecode.io.opcode @[CoreTop.scala 86:16]
    CU.io.func3 <= insDecode.io.func3 @[CoreTop.scala 87:15]
    CU.io.func7 <= insDecode.io.func7 @[CoreTop.scala 88:15]
    CU.io.rs1 <= insDecode.io.rs1 @[CoreTop.scala 89:13]
    CU.io.rs2 <= insDecode.io.rs2 @[CoreTop.scala 90:13]
    CU.io.rd <= insDecode.io.rd @[CoreTop.scala 91:12]
    CU.io.raw_imm <= insDecode.io.raw_imm @[CoreTop.scala 92:17]
    CU.io.cur_privilege <= CSR.io.cur_privilege @[CoreTop.scala 93:23]
    CSR.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 95:20]
    CSR.io.csr_idx_read <= CU.io.csr_out @[CoreTop.scala 96:23]
    regs.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 99:21]
    regs.io.rs1 <= CU.io.rs1_out @[CoreTop.scala 100:15]
    regs.io.rs2 <= CU.io.rs2_out @[CoreTop.scala 101:15]
    immGen.io.raw_imm <= CU.io.raw_imm_out @[CoreTop.scala 104:21]
    immGen.io.unsigned <= CU.io.unsigned @[CoreTop.scala 105:22]
    immGen.io.imm_width <= CU.io.imm_width_type @[CoreTop.scala 106:23]
    rs1_forward_selector.io.forward_type <= conflictController.io.rs1_forward_type @[CoreTop.scala 109:40]
    rs1_forward_selector.io.origin_val <= regs.io.rs1_val @[CoreTop.scala 110:38]
    rs1_forward_selector.io.ex_forward <= EXForwarding.io.write_data @[CoreTop.scala 111:38]
    rs1_forward_selector.io.mem_forward <= MEMForwarding.io.write_data @[CoreTop.scala 112:39]
    rs2_forward_selector.io.forward_type <= conflictController.io.rs2_forward_type @[CoreTop.scala 113:40]
    rs2_forward_selector.io.origin_val <= regs.io.rs2_val @[CoreTop.scala 114:38]
    rs2_forward_selector.io.ex_forward <= EXForwarding.io.write_data @[CoreTop.scala 115:38]
    rs2_forward_selector.io.mem_forward <= MEMForwarding.io.write_data @[CoreTop.scala 116:39]
    csr_forward_selector.io.forward_type <= conflictController.io.csr_forward_type @[CoreTop.scala 118:40]
    csr_forward_selector.io.origin_val <= CSR.io.csr_val @[CoreTop.scala 119:38]
    csr_forward_selector.io.ex_forward <= ALU.io.result @[CoreTop.scala 120:38]
    csr_forward_selector.io.mem_forward <= EX_MEM.io.out.alu_out @[CoreTop.scala 121:39]
    operandSelector.io.csr_val <= csr_forward_selector.io.real_val @[CoreTop.scala 127:30]
    operandSelector.io.rs1_val <= rs1_forward_selector.io.real_val @[CoreTop.scala 128:30]
    operandSelector.io.rs2_val <= rs2_forward_selector.io.real_val @[CoreTop.scala 129:30]
    operandSelector.io.real_imm <= immGen.io.real_imm @[CoreTop.scala 130:31]
    operandSelector.io.operand2Type <= CU.io.operand2_type @[CoreTop.scala 131:35]
    operandSelector.io.operand1Type <= CU.io.operand1_type @[CoreTop.scala 132:35]
    ID_EX.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 135:21]
    ID_EX.io.signal <= conflictController.io.IDEX_control_signal @[CoreTop.scala 136:19]
    ID_EX.io.in.pc <= IF_ID.io.out.pc @[CoreTop.scala 137:18]
    ID_EX.io.in.ins <= IF_ID.io.out.ins @[CoreTop.scala 138:19]
    ID_EX.io.in.IF_fault.mtval <= IF_ID.io.out.IF_fault.mtval @[CoreTop.scala 139:24]
    ID_EX.io.in.IF_fault.IF_fault_type <= IF_ID.io.out.IF_fault.IF_fault_type @[CoreTop.scala 139:24]
    ID_EX.io.in.op1 <= operandSelector.io.operand1 @[CoreTop.scala 141:19]
    ID_EX.io.in.op2 <= operandSelector.io.operand2 @[CoreTop.scala 142:19]
    ID_EX.io.in.csr_val_to_reg <= csr_forward_selector.io.real_val @[CoreTop.scala 143:30]
    ID_EX.io.in.alu_op <= CU.io.alu_type @[CoreTop.scala 145:22]
    ID_EX.io.in.cmp_op <= CU.io.cmp_type @[CoreTop.scala 146:22]
    ID_EX.io.in.unsigned <= CU.io.unsigned @[CoreTop.scala 147:24]
    ID_EX.io.in.mem_signal.read_data_en <= CU.io.memory_read @[CoreTop.scala 149:39]
    ID_EX.io.in.mem_signal.write_data_en <= CU.io.memory_write @[CoreTop.scala 150:40]
    ID_EX.io.in.mem_signal.data_width <= CU.io.data_width @[CoreTop.scala 151:37]
    ID_EX.io.in.mem_signal.data_to_write <= rs2_forward_selector.io.real_val @[CoreTop.scala 152:40]
    ID_EX.io.in.wb_signal.au_type <= CU.io.au_type @[CoreTop.scala 154:33]
    ID_EX.io.in.wb_signal.wb_type <= CU.io.write_back_type @[CoreTop.scala 155:33]
    ID_EX.io.in.wb_signal.imm <= immGen.io.real_imm @[CoreTop.scala 156:29]
    ID_EX.io.in.wb_signal.rd <= CU.io.rd_out @[CoreTop.scala 157:28]
    ID_EX.io.in.wb_signal.write_reg <= CU.io.regs_write @[CoreTop.scala 158:35]
    ID_EX.io.in.wb_signal.csr_idx <= CU.io.csr_out @[CoreTop.scala 159:33]
    ID_EX.io.in.wb_signal.csr_write <= CU.io.csr_write @[CoreTop.scala 160:35]
    ID_EX.io.in.result_stage <= CU.io.result_stage @[CoreTop.scala 162:28]
    ID_EX.io.in.branch_type <= CU.io.branch_type @[CoreTop.scala 163:27]
    ID_EX.io.in.ID_fault.mtval <= CU.io.fault.mtval @[CoreTop.scala 164:24]
    ID_EX.io.in.ID_fault.ID_fault_type <= CU.io.fault.ID_fault_type @[CoreTop.scala 164:24]
    ALU.io.operand1 <= ID_EX.io.out.op1 @[CoreTop.scala 168:19]
    ALU.io.operand2 <= ID_EX.io.out.op2 @[CoreTop.scala 169:19]
    ALU.io.alu_op <= ID_EX.io.out.alu_op @[CoreTop.scala 170:17]
    ALU.io.unsigned <= ID_EX.io.out.cmp_op @[CoreTop.scala 171:19]
    CMP.io.operand1 <= ID_EX.io.out.op1 @[CoreTop.scala 174:19]
    CMP.io.operand2 <= ID_EX.io.out.op2 @[CoreTop.scala 175:19]
    CMP.io.cmp_op <= ID_EX.io.out.cmp_op @[CoreTop.scala 176:17]
    CMP.io.unsigned <= ID_EX.io.out.unsigned @[CoreTop.scala 177:19]
    EXForwarding.io.au_type <= ID_EX.io.out.wb_signal.au_type @[CoreTop.scala 183:27]
    EXForwarding.io.alu_result <= ALU.io.result @[CoreTop.scala 184:30]
    EXForwarding.io.cmp_result <= CMP.io.result @[CoreTop.scala 186:30]
    EXForwarding.io.write_back_type <= ID_EX.io.out.wb_signal.wb_type @[CoreTop.scala 187:35]
    EXForwarding.io.imm <= ID_EX.io.out.wb_signal.imm @[CoreTop.scala 188:23]
    EXForwarding.io.mem_out is invalid @[CoreTop.scala 189:27]
    node _EXForwarding_io_pc4_T = add(ID_EX.io.out.pc, UInt<3>("h4")) @[CoreTop.scala 190:42]
    node _EXForwarding_io_pc4_T_1 = tail(_EXForwarding_io_pc4_T, 1) @[CoreTop.scala 190:42]
    EXForwarding.io.pc4 <= _EXForwarding_io_pc4_T_1 @[CoreTop.scala 190:23]
    node _EXForwarding_io_pcImm_T = add(ID_EX.io.out.pc, ID_EX.io.out.wb_signal.imm) @[CoreTop.scala 191:44]
    node _EXForwarding_io_pcImm_T_1 = tail(_EXForwarding_io_pcImm_T, 1) @[CoreTop.scala 191:44]
    EXForwarding.io.pcImm <= _EXForwarding_io_pcImm_T_1 @[CoreTop.scala 191:25]
    EXForwarding.io.csr_val <= ID_EX.io.out.csr_val_to_reg @[CoreTop.scala 192:27]
    EX_MEM.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 195:22]
    EX_MEM.io.signal <= conflictController.io.EXMEM_control_signal @[CoreTop.scala 196:20]
    EX_MEM.io.in.pc <= ID_EX.io.out.pc @[CoreTop.scala 197:19]
    EX_MEM.io.in.ins <= ID_EX.io.out.ins @[CoreTop.scala 198:20]
    EX_MEM.io.in.resultStage <= ID_EX.io.out.result_stage @[CoreTop.scala 199:28]
    EX_MEM.io.in.IF_fault.mtval <= ID_EX.io.out.IF_fault.mtval @[CoreTop.scala 200:25]
    EX_MEM.io.in.IF_fault.IF_fault_type <= ID_EX.io.out.IF_fault.IF_fault_type @[CoreTop.scala 200:25]
    EX_MEM.io.in.ID_fault.mtval <= ID_EX.io.out.ID_fault.mtval @[CoreTop.scala 201:25]
    EX_MEM.io.in.ID_fault.ID_fault_type <= ID_EX.io.out.ID_fault.ID_fault_type @[CoreTop.scala 201:25]
    EX_MEM.io.in.csr_val_to_reg <= ID_EX.io.out.csr_val_to_reg @[CoreTop.scala 202:31]
    EX_MEM.io.in.unsigned <= ID_EX.io.out.unsigned @[CoreTop.scala 203:25]
    EX_MEM.io.in.mem_signal <= ID_EX.io.out.mem_signal @[CoreTop.scala 204:27]
    EX_MEM.io.in.wb_signal <= ID_EX.io.out.wb_signal @[CoreTop.scala 205:26]
    EX_MEM.io.in.alu_out <= ALU.io.result @[CoreTop.scala 206:24]
    EX_MEM.io.in.cmp_out <= CMP.io.result @[CoreTop.scala 207:24]
    uartLoader.io.uart_load <= io.external_signal.load_data_mode @[CoreTop.scala 212:27]
    uartLoader.io.rxValid <= io.external.uart.rxValid @[CoreTop.scala 213:25]
    uartLoader.io.rxData <= io.external.uart.rxData @[CoreTop.scala 214:24]
    io.external.uart.rxReady <= uartLoader.io.rxReady @[CoreTop.scala 215:28]
    memInSelector.io.uart_load <= io.external_signal.load_data_mode @[CoreTop.scala 218:30]
    memInSelector.io.uart_in <= uartLoader.io.mem @[CoreTop.scala 219:28]
    memInSelector.io.cpu_read_data <= EX_MEM.io.out.mem_signal.read_data_en @[CoreTop.scala 220:34]
    memInSelector.io.cpu_write_data <= EX_MEM.io.out.mem_signal.write_data_en @[CoreTop.scala 221:35]
    memInSelector.io.cpu_data_width <= EX_MEM.io.out.mem_signal.data_width @[CoreTop.scala 222:35]
    memInSelector.io.cpu_data_addr <= EX_MEM.io.out.alu_out @[CoreTop.scala 223:34]
    memInSelector.io.cpu_data_write <= EX_MEM.io.out.mem_signal.data_to_write @[CoreTop.scala 224:35]
    memory.io.uart_load <= io.external_signal.load_data_mode @[CoreTop.scala 227:23]
    memory.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 228:23]
    memory.io.data_addr <= memInSelector.io.data_addr @[CoreTop.scala 229:23]
    memory.io.data_write <= memInSelector.io.data_write @[CoreTop.scala 230:24]
    memory.io.data_width <= memInSelector.io.data_width @[CoreTop.scala 231:24]
    memory.io.write_data <= memInSelector.io.write_data @[CoreTop.scala 232:24]
    memory.io.read_data <= memInSelector.io.read_data @[CoreTop.scala 233:23]
    memory.io.unsigned <= EX_MEM.io.out.unsigned @[CoreTop.scala 234:22]
    memory.io.external <= io.external @[CoreTop.scala 235:22]
    CSR.io.io_interruption <= PLIC.io.interruption @[CoreTop.scala 238:26]
    CSR.io.IF_fault <= EX_MEM.io.out.IF_fault @[CoreTop.scala 239:19]
    CSR.io.ID_fault <= EX_MEM.io.out.ID_fault @[CoreTop.scala 240:19]
    CSR.io.MEM_fault <= memory.io.MEM_fault @[CoreTop.scala 241:20]
    CSR.io.pc <= EX_MEM.io.out.pc @[CoreTop.scala 242:13]
    MEMForwarding.io.au_type <= EX_MEM.io.out.wb_signal.au_type @[CoreTop.scala 245:28]
    MEMForwarding.io.alu_result <= EX_MEM.io.out.alu_out @[CoreTop.scala 246:31]
    MEMForwarding.io.cmp_result <= EX_MEM.io.out.cmp_out @[CoreTop.scala 247:31]
    MEMForwarding.io.write_back_type <= EX_MEM.io.out.wb_signal.wb_type @[CoreTop.scala 248:36]
    MEMForwarding.io.imm <= EX_MEM.io.out.wb_signal.imm @[CoreTop.scala 249:24]
    MEMForwarding.io.mem_out <= memory.io.data_out @[CoreTop.scala 250:28]
    node _MEMForwarding_io_pc4_T = add(EX_MEM.io.out.pc, UInt<3>("h4")) @[CoreTop.scala 251:44]
    node _MEMForwarding_io_pc4_T_1 = tail(_MEMForwarding_io_pc4_T, 1) @[CoreTop.scala 251:44]
    MEMForwarding.io.pc4 <= _MEMForwarding_io_pc4_T_1 @[CoreTop.scala 251:24]
    node _MEMForwarding_io_pcImm_T = add(EX_MEM.io.out.pc, EX_MEM.io.out.wb_signal.imm) @[CoreTop.scala 252:46]
    node _MEMForwarding_io_pcImm_T_1 = tail(_MEMForwarding_io_pcImm_T, 1) @[CoreTop.scala 252:46]
    MEMForwarding.io.pcImm <= _MEMForwarding_io_pcImm_T_1 @[CoreTop.scala 252:26]
    MEMForwarding.io.csr_val <= EX_MEM.io.out.csr_val_to_reg @[CoreTop.scala 253:28]
    MEM_WB.io.cpu_state <= state.io.cpu_state @[CoreTop.scala 257:22]
    MEM_WB.io.signal <= conflictController.io.MEMWB_control_signal @[CoreTop.scala 258:20]
    MEM_WB.io.in.pc <= EX_MEM.io.out.pc @[CoreTop.scala 259:19]
    MEM_WB.io.in.ins <= EX_MEM.io.out.ins @[CoreTop.scala 260:20]
    MEM_WB.io.in.csr_val_to_reg <= EX_MEM.io.out.csr_val_to_reg @[CoreTop.scala 261:31]
    MEM_WB.io.in.wb_signal <= EX_MEM.io.out.wb_signal @[CoreTop.scala 262:26]
    MEM_WB.io.in.alu_out <= EX_MEM.io.out.alu_out @[CoreTop.scala 263:24]
    MEM_WB.io.in.cmp_out <= EX_MEM.io.out.cmp_out @[CoreTop.scala 264:24]
    MEM_WB.io.in.data_out <= memory.io.data_out @[CoreTop.scala 265:25]
    CSR.io.write <= MEM_WB.io.out.wb_signal.csr_write @[CoreTop.scala 269:16]
    CSR.io.csr_idx_write <= MEM_WB.io.out.wb_signal.csr_idx @[CoreTop.scala 270:24]
    CSR.io.write_data <= MEM_WB.io.out.alu_out @[CoreTop.scala 271:21]
    wb_selector.io.au_type <= MEM_WB.io.out.wb_signal.au_type @[CoreTop.scala 276:26]
    wb_selector.io.alu_result <= MEM_WB.io.out.alu_out @[CoreTop.scala 277:29]
    wb_selector.io.cmp_result <= MEM_WB.io.out.cmp_out @[CoreTop.scala 278:29]
    wb_selector.io.write_back_type <= MEM_WB.io.out.wb_signal.wb_type @[CoreTop.scala 279:34]
    wb_selector.io.imm <= MEM_WB.io.out.wb_signal.imm @[CoreTop.scala 280:22]
    wb_selector.io.mem_out <= MEM_WB.io.out.data_out @[CoreTop.scala 281:26]
    node _wb_selector_io_pc4_T = add(MEM_WB.io.out.pc, UInt<3>("h4")) @[CoreTop.scala 282:42]
    node _wb_selector_io_pc4_T_1 = tail(_wb_selector_io_pc4_T, 1) @[CoreTop.scala 282:42]
    wb_selector.io.pc4 <= _wb_selector_io_pc4_T_1 @[CoreTop.scala 282:22]
    node _wb_selector_io_pcImm_T = add(MEM_WB.io.out.pc, MEM_WB.io.out.wb_signal.imm) @[CoreTop.scala 283:44]
    node _wb_selector_io_pcImm_T_1 = tail(_wb_selector_io_pcImm_T, 1) @[CoreTop.scala 283:44]
    wb_selector.io.pcImm <= _wb_selector_io_pcImm_T_1 @[CoreTop.scala 283:24]
    wb_selector.io.csr_val <= MEM_WB.io.out.csr_val_to_reg @[CoreTop.scala 284:26]
    regs.io.write <= MEM_WB.io.out.wb_signal.write_reg @[CoreTop.scala 287:17]
    regs.io.rd <= MEM_WB.io.out.wb_signal.rd @[CoreTop.scala 288:14]
    regs.io.write_data <= wb_selector.io.write_data @[CoreTop.scala 289:22]
    conflictController.io.uart_loading <= io.external_signal.load_data_mode @[CoreTop.scala 292:38]
    conflictController.io.rs1_use <= CU.io.rs1_out @[CoreTop.scala 294:33]
    conflictController.io.rs2_use <= CU.io.rs2_out @[CoreTop.scala 295:33]
    conflictController.io.csr_use <= CU.io.csr_out @[CoreTop.scala 296:33]
    conflictController.io.IDEX_reg_to_write <= ID_EX.io.out.wb_signal.rd @[CoreTop.scala 298:43]
    conflictController.io.IDEX_write_reg <= ID_EX.io.out.wb_signal.write_reg @[CoreTop.scala 299:40]
    conflictController.io.IDEX_csr_to_write <= ID_EX.io.out.wb_signal.csr_idx @[CoreTop.scala 300:43]
    conflictController.io.IDEX_write_csr <= ID_EX.io.out.wb_signal.csr_write @[CoreTop.scala 301:40]
    conflictController.io.IDEX_reg_result_stage <= ID_EX.io.out.result_stage @[CoreTop.scala 302:47]
    conflictController.io.EXMEM_reg_to_write <= EX_MEM.io.out.wb_signal.rd @[CoreTop.scala 304:44]
    conflictController.io.EXMEM_write_reg <= EX_MEM.io.out.wb_signal.write_reg @[CoreTop.scala 305:41]
    conflictController.io.EXMEM_csr_to_write <= EX_MEM.io.out.wb_signal.csr_idx @[CoreTop.scala 306:44]
    conflictController.io.EXMEM_write_csr <= EX_MEM.io.out.wb_signal.csr_write @[CoreTop.scala 307:41]
    conflictController.io.EXMEM_reg_result_stage <= EX_MEM.io.out.resultStage @[CoreTop.scala 308:48]
    conflictController.io.branch_type <= ID_EX.io.out.branch_type @[CoreTop.scala 311:37]
    conflictController.io.pc <= ID_EX.io.out.pc @[CoreTop.scala 312:28]
    conflictController.io.imm <= ID_EX.io.out.wb_signal.imm @[CoreTop.scala 313:29]
    conflictController.io.alu_result <= ALU.io.result @[CoreTop.scala 314:36]
    conflictController.io.cmp_result <= CMP.io.result @[CoreTop.scala 315:36]
    conflictController.io.predict_next_pc <= IF_ID.io.out.pc @[CoreTop.scala 316:41]
    conflictController.io.exception_occurs <= CSR.io.fault_occurs @[CoreTop.scala 319:42]
    conflictController.io.exception_new_pc <= CSR.io.fault_new_PC @[CoreTop.scala 320:42]
    node _T = eq(io.external_signal.load_data_mode, UInt<1>("h0")) @[CoreTop.scala 339:8]
    node _T_1 = or(_T, memInSelector.io.write_data) @[CoreTop.scala 339:43]
    when _T_1 : @[CoreTop.scala 339:75]
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

  module VGA :
    input clock : Clock
    input reset : Reset
    output io : { board : { }, mmio : { flip value : UInt<32>}}

    skip

  module DeviceTop :
    input clock : Clock
    input reset : Reset
    output io : { mmio : { led : { flip led : UInt<32>}, seg7 : { flip seg7 : UInt<32>}, btn : { button : UInt<32>}, switches : { switches : UInt<32>}, uart : { rxData : UInt<8>, rxValid : UInt<1>, flip rxReady : UInt<1>, flip txData : UInt<8>, flip txStart : UInt<1>, txReady : UInt<1>}, vga : { flip value : UInt<32>}}, board : { led : { led : UInt<24>}, seg7 : { seg7 : UInt<8>, an : UInt<8>}, btn : { flip button : UInt<5>}, switch : { flip switches : UInt<24>}, uart : { flip rx : UInt<1>, tx : UInt<1>}, vga : { flip value : UInt<32>}}, external_signal : { load_data_mode : UInt<1>}}

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
    vga.io.mmio.value <= io.mmio.vga.value @[DeviceTop.scala 58:15]
    io.board.led <= led.io.board @[DeviceTop.scala 61:16]
    io.board.seg7 <= seg7.io.board @[DeviceTop.scala 62:17]
    btn.io.board.button <= io.board.btn.button @[DeviceTop.scala 63:16]
    switches.io.board.switches <= io.board.switch.switches @[DeviceTop.scala 64:21]
    io.board.uart.tx <= uart.io.board.tx @[DeviceTop.scala 65:17]
    uart.io.board.rx <= io.board.uart.rx @[DeviceTop.scala 65:17]
    node _io_external_signal_load_data_mode_T = bits(io.board.switch.switches, 23, 23) @[DeviceTop.scala 69:64]
    io.external_signal.load_data_mode <= _io_external_signal_load_data_mode_T @[DeviceTop.scala 69:37]

  module Top :
    input clock : Clock
    input reset : UInt<1>
    output io : { led : { led : UInt<24>}, seg7 : { seg7 : UInt<8>, an : UInt<8>}, btn : { flip button : UInt<5>}, switch : { flip switches : UInt<24>}, uart : { flip rx : UInt<1>, tx : UInt<1>}, vga : { flip value : UInt<32>}}

    inst clockSeparator of ClockSeparator @[Top.scala 15:30]
    clockSeparator.clock <= clock
    clockSeparator.reset <= reset
    inst cpu of CoreTop @[Top.scala 17:11]
    cpu.clock <= clockSeparator.io.cpuClock
    cpu.reset <= reset
    inst device of DeviceTop @[Top.scala 19:22]
    device.clock <= clock
    device.reset <= reset
    device.io.board.vga.value <= io.vga.value @[Top.scala 22:18]
    io.uart.tx <= device.io.board.uart.tx @[Top.scala 22:18]
    device.io.board.uart.rx <= io.uart.rx @[Top.scala 22:18]
    device.io.board.switch.switches <= io.switch.switches @[Top.scala 22:18]
    device.io.board.btn.button <= io.btn.button @[Top.scala 22:18]
    io.seg7 <= device.io.board.seg7 @[Top.scala 22:18]
    io.led <= device.io.board.led @[Top.scala 22:18]
    cpu.io.external <= device.io.mmio @[Top.scala 25:18]
    cpu.io.external_signal <= device.io.external_signal @[Top.scala 26:25]

