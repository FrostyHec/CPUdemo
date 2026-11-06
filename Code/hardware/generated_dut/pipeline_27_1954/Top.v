module ClockSeparator(
  input   clock,
  output  io_cpuClock
);
  wire  ip_clock_clk_in1; // @[ClockSeparator.scala 14:26]
  wire  ip_clock_reset; // @[ClockSeparator.scala 14:26]
  wire  ip_clock_clk_out1; // @[ClockSeparator.scala 14:26]
  wire  ip_clock_clk_out2; // @[ClockSeparator.scala 14:26]
  clk_wiz_0 ip_clock ( // @[ClockSeparator.scala 14:26]
    .clk_in1(ip_clock_clk_in1),
    .reset(ip_clock_reset),
    .clk_out1(ip_clock_clk_out1),
    .clk_out2(ip_clock_clk_out2)
  );
  assign io_cpuClock = ip_clock_clk_out2; // @[ClockSeparator.scala 17:17]
  assign ip_clock_clk_in1 = clock; // @[ClockSeparator.scala 15:25]
  assign ip_clock_reset = 1'h0; // @[ClockSeparator.scala 16:22]
endmodule
module ConflictController(
  input         io_uart_loading,
  input  [4:0]  io_rs1_use,
  input  [4:0]  io_rs2_use,
  input  [11:0] io_csr_use,
  input  [4:0]  io_IDEX_reg_to_write,
  input         io_IDEX_write_reg,
  input  [11:0] io_IDEX_csr_to_write,
  input         io_IDEX_write_csr,
  input         io_IDEX_reg_result_stage,
  input  [4:0]  io_EXMEM_reg_to_write,
  input         io_EXMEM_write_reg,
  input  [11:0] io_EXMEM_csr_to_write,
  input         io_EXMEM_write_csr,
  output [1:0]  io_rs1_forward_type,
  output [1:0]  io_rs2_forward_type,
  output [1:0]  io_csr_forward_type,
  input  [1:0]  io_branch_type,
  input  [31:0] io_pc,
  input  [31:0] io_imm,
  input  [31:0] io_alu_result,
  input         io_cmp_result,
  input  [31:0] io_predict_next_pc,
  input         io_exception_occurs,
  input  [31:0] io_exception_new_pc,
  output [31:0] io_new_pc,
  output [1:0]  io_next_control_signal,
  output [1:0]  io_IFID_control_signal,
  output [1:0]  io_IDEX_control_signal,
  output [1:0]  io_EXMEM_control_signal,
  output [1:0]  io_MEMWB_control_signal,
  output        io_prediction_failure_failure_occurs,
  output        io_prediction_failure_success_occurs,
  output [31:0] io_prediction_failure_address
);
  wire  _T_8 = ~io_uart_loading; // @[ConflictController.scala 118:9]
  wire [31:0] _GEN_24 = ~io_uart_loading & io_exception_occurs ? io_exception_new_pc : 32'h0; // @[ConflictController.scala 100:12 118:49 119:14]
  wire [1:0] _GEN_28 = ~io_uart_loading & io_exception_occurs ? 2'h2 : {{1'd0}, io_uart_loading}; // @[ConflictController.scala 118:49]
  wire  _T_20 = _T_8 & ~io_exception_occurs; // @[ConflictController.scala 131:26]
  wire [31:0] _correct_pc_T_1 = io_pc + io_imm; // @[ConflictController.scala 134:50]
  wire [31:0] _correct_pc_T_3 = io_pc + 32'h4; // @[ConflictController.scala 134:64]
  wire [31:0] correct_pc = io_cmp_result ? _correct_pc_T_1 : _correct_pc_T_3; // @[ConflictController.scala 134:29]
  wire  _T_22 = io_predict_next_pc != correct_pc; // @[ConflictController.scala 135:33]
  wire  _GEN_42 = io_predict_next_pc != correct_pc ? 1'h0 : 1'h1; // @[ConflictController.scala 135:49 PCPrediction.scala 21:20 27:20]
  wire [31:0] _GEN_44 = io_predict_next_pc != correct_pc ? correct_pc : _GEN_24; // @[ConflictController.scala 135:49 138:21]
  wire [1:0] _GEN_48 = io_predict_next_pc != correct_pc ? 2'h2 : _GEN_28; // @[ConflictController.scala 135:49]
  wire  _T_32 = io_predict_next_pc != io_alu_result; // @[ConflictController.scala 153:33]
  wire [31:0] _GEN_62 = io_predict_next_pc != io_alu_result ? io_alu_result : _GEN_24; // @[ConflictController.scala 153:49 155:21]
  wire [1:0] _GEN_66 = io_predict_next_pc != io_alu_result ? 2'h2 : _GEN_28; // @[ConflictController.scala 153:49]
  wire  _GEN_71 = 2'h2 == io_branch_type & _T_32; // @[ConflictController.scala 129:17 132:28]
  wire [31:0] _GEN_72 = 2'h2 == io_branch_type ? _GEN_62 : _GEN_24; // @[ConflictController.scala 132:28]
  wire [1:0] _GEN_76 = 2'h2 == io_branch_type ? _GEN_66 : _GEN_28; // @[ConflictController.scala 132:28]
  wire  _GEN_81 = 2'h1 == io_branch_type & _T_22; // @[ConflictController.scala 132:28 PCPrediction.scala 14:20]
  wire  _GEN_82 = 2'h1 == io_branch_type & _GEN_42; // @[ConflictController.scala 132:28 PCPrediction.scala 15:20]
  wire [31:0] _GEN_83 = 2'h1 == io_branch_type ? io_pc : 32'h0; // @[ConflictController.scala 132:28 PCPrediction.scala 16:13]
  wire  _GEN_84 = 2'h1 == io_branch_type ? _T_22 : _GEN_71; // @[ConflictController.scala 132:28]
  wire [31:0] _GEN_85 = 2'h1 == io_branch_type ? _GEN_44 : _GEN_72; // @[ConflictController.scala 132:28]
  wire [1:0] _GEN_89 = 2'h1 == io_branch_type ? _GEN_48 : _GEN_76; // @[ConflictController.scala 132:28]
  wire  control_hazard = _T_8 & ~io_exception_occurs & _GEN_84; // @[ConflictController.scala 129:17 131:52]
  wire [1:0] _GEN_102 = _T_8 & ~io_exception_occurs ? _GEN_89 : _GEN_28; // @[ConflictController.scala 131:52]
  wire  _T_45 = _T_20 & ~control_hazard; // @[ConflictController.scala 168:5]
  wire  _T_47 = io_IDEX_write_reg & io_IDEX_reg_to_write != 5'h0; // @[ConflictController.scala 172:7]
  wire  _T_50 = ~io_IDEX_reg_result_stage; // @[ConflictController.scala 174:37]
  wire [1:0] _GEN_126 = ~io_IDEX_reg_result_stage ? _GEN_102 : 2'h1; // @[ConflictController.scala 174:69]
  wire [1:0] _GEN_128 = ~io_IDEX_reg_result_stage ? _GEN_102 : 2'h2; // @[ConflictController.scala 174:69]
  wire  _T_68 = io_EXMEM_write_reg & io_EXMEM_reg_to_write != 5'h0; // @[ConflictController.scala 186:7]
  wire [1:0] _GEN_139 = _T_68 & io_rs1_use == io_EXMEM_reg_to_write ? 2'h2 : 2'h0; // @[ConflictController.scala 187:48]
  wire [1:0] _GEN_147 = _T_47 & io_rs1_use == io_IDEX_reg_to_write ? {{1'd0}, _T_50} : _GEN_139; // @[ConflictController.scala 173:47]
  wire [1:0] _GEN_150 = _T_47 & io_rs1_use == io_IDEX_reg_to_write ? _GEN_126 : _GEN_102; // @[ConflictController.scala 173:47]
  wire [1:0] _GEN_152 = _T_47 & io_rs1_use == io_IDEX_reg_to_write ? _GEN_128 : _GEN_102; // @[ConflictController.scala 173:47]
  wire [1:0] _GEN_174 = _T_50 ? _GEN_150 : 2'h1; // @[ConflictController.scala 196:69]
  wire [1:0] _GEN_176 = _T_50 ? _GEN_152 : 2'h2; // @[ConflictController.scala 196:69]
  wire [1:0] _GEN_188 = _T_68 & io_rs2_use == io_EXMEM_reg_to_write ? 2'h2 : 2'h0; // @[ConflictController.scala 209:48]
  wire [1:0] _GEN_196 = _T_47 & io_rs2_use == io_IDEX_reg_to_write ? {{1'd0}, _T_50} : _GEN_188; // @[ConflictController.scala 195:47]
  wire [1:0] _GEN_198 = _T_47 & io_rs2_use == io_IDEX_reg_to_write ? _GEN_174 : _GEN_150; // @[ConflictController.scala 195:47]
  wire [1:0] _GEN_200 = _T_47 & io_rs2_use == io_IDEX_reg_to_write ? _GEN_176 : _GEN_152; // @[ConflictController.scala 195:47]
  wire  _T_113 = io_IDEX_write_csr & io_IDEX_csr_to_write != 12'h0; // @[ConflictController.scala 216:7]
  wire [1:0] _GEN_222 = _T_50 ? _GEN_198 : 2'h1; // @[ConflictController.scala 218:69]
  wire [1:0] _GEN_224 = _T_50 ? _GEN_200 : 2'h2; // @[ConflictController.scala 218:69]
  wire  _T_134 = io_EXMEM_write_csr & io_EXMEM_csr_to_write != 12'h0; // @[ConflictController.scala 230:7]
  wire [1:0] _GEN_237 = _T_134 & io_csr_use == io_EXMEM_csr_to_write ? 2'h2 : 2'h0; // @[ConflictController.scala 231:48]
  wire [1:0] _GEN_245 = _T_113 & io_csr_use == io_IDEX_csr_to_write ? {{1'd0}, _T_50} : _GEN_237; // @[ConflictController.scala 217:47]
  wire [1:0] _GEN_246 = _T_113 & io_csr_use == io_IDEX_csr_to_write ? _GEN_222 : _GEN_198; // @[ConflictController.scala 217:47]
  wire [1:0] _GEN_248 = _T_113 & io_csr_use == io_IDEX_csr_to_write ? _GEN_224 : _GEN_200; // @[ConflictController.scala 217:47]
  assign io_rs1_forward_type = _T_45 ? _GEN_147 : 2'h0; // @[ConflictController.scala 169:5]
  assign io_rs2_forward_type = _T_45 ? _GEN_196 : 2'h0; // @[ConflictController.scala 169:5]
  assign io_csr_forward_type = _T_45 ? _GEN_245 : 2'h0; // @[ConflictController.scala 169:5]
  assign io_new_pc = _T_8 & ~io_exception_occurs ? _GEN_85 : _GEN_24; // @[ConflictController.scala 131:52]
  assign io_next_control_signal = _T_45 ? _GEN_246 : _GEN_102; // @[ConflictController.scala 169:5]
  assign io_IFID_control_signal = _T_45 ? _GEN_246 : _GEN_102; // @[ConflictController.scala 169:5]
  assign io_IDEX_control_signal = _T_45 ? _GEN_248 : _GEN_102; // @[ConflictController.scala 169:5]
  assign io_EXMEM_control_signal = ~io_uart_loading & io_exception_occurs ? 2'h2 : {{1'd0}, io_uart_loading}; // @[ConflictController.scala 118:49]
  assign io_MEMWB_control_signal = ~io_uart_loading & io_exception_occurs ? 2'h2 : {{1'd0}, io_uart_loading}; // @[ConflictController.scala 118:49]
  assign io_prediction_failure_failure_occurs = _T_8 & ~io_exception_occurs & _GEN_81; // @[ConflictController.scala 131:52 PCPrediction.scala 14:20]
  assign io_prediction_failure_success_occurs = _T_8 & ~io_exception_occurs & _GEN_82; // @[ConflictController.scala 131:52 PCPrediction.scala 15:20]
  assign io_prediction_failure_address = _T_8 & ~io_exception_occurs ? _GEN_83 : 32'h0; // @[ConflictController.scala 131:52 PCPrediction.scala 16:13]
endmodule
module CPUState(
  input        clock,
  input        reset,
  input        io_stall,
  output [1:0] io_cpu_state
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  reg [1:0] state; // @[CPUState.scala 15:22]
  wire [1:0] _GEN_0 = 2'h2 == state ? 2'h0 : state; // @[CPUState.scala 21:18 29:14 15:22]
  assign io_cpu_state = state; // @[CPUState.scala 16:15]
  always @(posedge clock) begin
    if (reset) begin // @[CPUState.scala 15:22]
      state <= 2'h0; // @[CPUState.scala 15:22]
    end else if (~io_stall) begin // @[CPUState.scala 20:19]
      if (2'h0 == state) begin // @[CPUState.scala 21:18]
        state <= 2'h1; // @[CPUState.scala 23:14]
      end else if (2'h1 == state) begin // @[CPUState.scala 21:18]
        state <= 2'h2; // @[CPUState.scala 26:14]
      end else begin
        state <= _GEN_0;
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  state = _RAND_0[1:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module CSR(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  output [1:0]  io_cur_privilege,
  input         io_write,
  input  [11:0] io_csr_idx_write,
  input  [31:0] io_write_data,
  input  [11:0] io_csr_idx_read,
  output [31:0] io_csr_val,
  input  [1:0]  io_IF_fault_IF_fault_type,
  input  [31:0] io_IF_fault_mtval,
  input  [2:0]  io_ID_fault_ID_fault_type,
  input  [31:0] io_ID_fault_mtval,
  input  [2:0]  io_MEM_fault_mem_fault_type,
  input  [31:0] io_MEM_fault_mtval,
  input  [31:0] io_pc,
  output        io_fault_occurs,
  output [31:0] io_fault_new_PC
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
`endif // RANDOMIZE_REG_INIT
  reg [1:0] cur_privilege; // @[CSR.scala 36:30]
  reg [31:0] mstatus; // @[CSR.scala 39:24]
  reg [31:0] mie; // @[CSR.scala 40:20]
  reg [31:0] mtvec; // @[CSR.scala 41:22]
  reg [31:0] mscratch; // @[CSR.scala 43:25]
  reg [31:0] mepc; // @[CSR.scala 44:21]
  reg [31:0] mcause; // @[CSR.scala 45:23]
  reg [31:0] mtval; // @[CSR.scala 46:22]
  reg [31:0] mip; // @[CSR.scala 47:20]
  wire  read_MPIE = mstatus[7]; // @[CSR.scala 51:26]
  wire [1:0] read_MPP = mstatus[12:11]; // @[CSR.scala 52:25]
  wire [31:0] _mip_T_4 = ~mip; // @[CSR.scala 54:20]
  wire [31:0] _mip_T_5 = _mip_T_4 | 32'h800; // @[CSR.scala 54:20]
  wire [31:0] _mip_T_6 = ~_mip_T_5; // @[CSR.scala 54:20]
  wire  _no_fault_T_1 = io_ID_fault_ID_fault_type == 3'h0; // @[CSR.scala 57:32]
  wire  _no_fault_T_2 = io_IF_fault_IF_fault_type == 2'h0 & _no_fault_T_1; // @[CSR.scala 56:73]
  wire  _no_fault_T_3 = io_MEM_fault_mem_fault_type == 3'h0; // @[CSR.scala 58:34]
  wire  no_fault = _no_fault_T_2 & _no_fault_T_3; // @[CSR.scala 57:60]
  wire  _T_1 = io_cpu_state == 2'h0; // @[CSR.scala 65:23]
  wire  _T_2 = _T_1 & io_write; // @[CSR.scala 66:7]
  wire [31:0] _GEN_1 = _T_2 & io_csr_idx_write == 12'h300 ? io_write_data : mstatus; // @[CSR.scala 67:35 68:11 39:24]
  wire [31:0] _GEN_2 = io_csr_idx_read == 12'h304 ? mie : mstatus; // @[CSR.scala 62:35 63:18]
  wire [31:0] _GEN_4 = io_csr_idx_read == 12'h305 ? mtvec : _GEN_2; // @[CSR.scala 62:35 63:18]
  wire [31:0] _GEN_6 = io_csr_idx_read == 12'h340 ? mscratch : _GEN_4; // @[CSR.scala 62:35 63:18]
  wire [31:0] _GEN_8 = io_csr_idx_read == 12'h341 ? mepc : _GEN_6; // @[CSR.scala 62:35 63:18]
  wire [31:0] _GEN_9 = _T_2 & io_csr_idx_write == 12'h341 ? io_write_data : mepc; // @[CSR.scala 67:35 68:11 44:21]
  wire [31:0] _GEN_10 = io_csr_idx_read == 12'h342 ? mcause : _GEN_8; // @[CSR.scala 62:35 63:18]
  wire [31:0] _GEN_11 = _T_2 & io_csr_idx_write == 12'h342 ? io_write_data : mcause; // @[CSR.scala 67:35 68:11 45:23]
  wire [31:0] _GEN_12 = io_csr_idx_read == 12'h343 ? mtval : _GEN_10; // @[CSR.scala 62:35 63:18]
  wire [31:0] _GEN_13 = _T_2 & io_csr_idx_write == 12'h343 ? io_write_data : mtval; // @[CSR.scala 67:35 68:11 46:22]
  wire  fault_type = io_ID_fault_ID_fault_type == 3'h4; // @[CSR.scala 112:36]
  wire [31:0] _mstatus_T_14 = {mstatus[31:13],cur_privilege,mstatus[10:0]}; // @[Cat.scala 33:92]
  wire  _T_51 = io_IF_fault_IF_fault_type == 2'h2; // @[CSR.scala 139:45]
  wire [31:0] _GEN_25 = io_IF_fault_IF_fault_type == 2'h2 ? io_pc : _GEN_9; // @[CSR.scala 139:79 81:10]
  wire [31:0] _GEN_27 = io_IF_fault_IF_fault_type == 2'h2 ? 32'h1 : _GEN_11; // @[CSR.scala 139:79 83:12]
  wire [31:0] _GEN_28 = io_IF_fault_IF_fault_type == 2'h2 ? io_IF_fault_mtval : _GEN_13; // @[CSR.scala 139:79 84:11]
  wire [31:0] _GEN_29 = io_IF_fault_IF_fault_type == 2'h2 ? _mstatus_T_14 : _GEN_1; // @[CSR.scala 139:79 89:13]
  wire [1:0] _GEN_30 = io_IF_fault_IF_fault_type == 2'h2 ? 2'h3 : cur_privilege; // @[CSR.scala 139:79 90:19]
  wire  _GEN_31 = io_IF_fault_IF_fault_type == 2'h1 | _T_51; // @[CSR.scala 136:84 80:21]
  wire [31:0] _GEN_32 = io_IF_fault_IF_fault_type == 2'h1 ? io_pc : _GEN_25; // @[CSR.scala 136:84 81:10]
  wire [31:0] _GEN_34 = io_IF_fault_IF_fault_type == 2'h1 ? 32'h0 : _GEN_27; // @[CSR.scala 136:84 83:12]
  wire [31:0] _GEN_35 = io_IF_fault_IF_fault_type == 2'h1 ? io_IF_fault_mtval : _GEN_28; // @[CSR.scala 136:84 84:11]
  wire [31:0] _GEN_36 = io_IF_fault_IF_fault_type == 2'h1 ? _mstatus_T_14 : _GEN_29; // @[CSR.scala 136:84 89:13]
  wire [1:0] _GEN_37 = io_IF_fault_IF_fault_type == 2'h1 ? 2'h3 : _GEN_30; // @[CSR.scala 136:84 90:19]
  wire  _GEN_38 = io_ID_fault_ID_fault_type == 3'h3 | _GEN_31; // @[CSR.scala 132:78 80:21]
  wire [31:0] _GEN_39 = io_ID_fault_ID_fault_type == 3'h3 ? io_pc : _GEN_32; // @[CSR.scala 132:78 81:10]
  wire [31:0] _GEN_41 = io_ID_fault_ID_fault_type == 3'h3 ? 32'hb : _GEN_34; // @[CSR.scala 132:78 83:12]
  wire [31:0] _GEN_42 = io_ID_fault_ID_fault_type == 3'h3 ? io_ID_fault_mtval : _GEN_35; // @[CSR.scala 132:78 84:11]
  wire [31:0] _GEN_43 = io_ID_fault_ID_fault_type == 3'h3 ? _mstatus_T_14 : _GEN_36; // @[CSR.scala 132:78 89:13]
  wire [1:0] _GEN_44 = io_ID_fault_ID_fault_type == 3'h3 ? 2'h3 : _GEN_37; // @[CSR.scala 132:78 90:19]
  wire  _GEN_45 = io_ID_fault_ID_fault_type == 3'h2 | _GEN_38; // @[CSR.scala 130:82 80:21]
  wire [31:0] _GEN_46 = io_ID_fault_ID_fault_type == 3'h2 ? io_pc : _GEN_39; // @[CSR.scala 130:82 81:10]
  wire [31:0] _GEN_48 = io_ID_fault_ID_fault_type == 3'h2 ? 32'h3 : _GEN_41; // @[CSR.scala 130:82 83:12]
  wire [31:0] _GEN_49 = io_ID_fault_ID_fault_type == 3'h2 ? io_ID_fault_mtval : _GEN_42; // @[CSR.scala 130:82 84:11]
  wire [31:0] _GEN_50 = io_ID_fault_ID_fault_type == 3'h2 ? _mstatus_T_14 : _GEN_43; // @[CSR.scala 130:82 89:13]
  wire [1:0] _GEN_51 = io_ID_fault_ID_fault_type == 3'h2 ? 2'h3 : _GEN_44; // @[CSR.scala 130:82 90:19]
  wire  _GEN_52 = io_ID_fault_ID_fault_type == 3'h1 | _GEN_45; // @[CSR.scala 128:81 80:21]
  wire [31:0] _GEN_53 = io_ID_fault_ID_fault_type == 3'h1 ? io_pc : _GEN_46; // @[CSR.scala 128:81 81:10]
  wire [31:0] _GEN_55 = io_ID_fault_ID_fault_type == 3'h1 ? 32'h2 : _GEN_48; // @[CSR.scala 128:81 83:12]
  wire [31:0] _GEN_56 = io_ID_fault_ID_fault_type == 3'h1 ? io_ID_fault_mtval : _GEN_49; // @[CSR.scala 128:81 84:11]
  wire [31:0] _GEN_57 = io_ID_fault_ID_fault_type == 3'h1 ? _mstatus_T_14 : _GEN_50; // @[CSR.scala 128:81 89:13]
  wire [1:0] _GEN_58 = io_ID_fault_ID_fault_type == 3'h1 ? 2'h3 : _GEN_51; // @[CSR.scala 128:81 90:19]
  wire  _GEN_59 = io_MEM_fault_mem_fault_type == 3'h4 | _GEN_52; // @[CSR.scala 124:85 80:21]
  wire [31:0] _GEN_60 = io_MEM_fault_mem_fault_type == 3'h4 ? io_pc : _GEN_53; // @[CSR.scala 124:85 81:10]
  wire [31:0] _GEN_62 = io_MEM_fault_mem_fault_type == 3'h4 ? 32'h7 : _GEN_55; // @[CSR.scala 124:85 83:12]
  wire [31:0] _GEN_63 = io_MEM_fault_mem_fault_type == 3'h4 ? io_MEM_fault_mtval : _GEN_56; // @[CSR.scala 124:85 84:11]
  wire [31:0] _GEN_64 = io_MEM_fault_mem_fault_type == 3'h4 ? _mstatus_T_14 : _GEN_57; // @[CSR.scala 124:85 89:13]
  wire [1:0] _GEN_65 = io_MEM_fault_mem_fault_type == 3'h4 ? 2'h3 : _GEN_58; // @[CSR.scala 124:85 90:19]
  wire  _GEN_66 = io_MEM_fault_mem_fault_type == 3'h3 | _GEN_59; // @[CSR.scala 122:90 80:21]
  wire [31:0] _GEN_67 = io_MEM_fault_mem_fault_type == 3'h3 ? io_pc : _GEN_60; // @[CSR.scala 122:90 81:10]
  wire [31:0] _GEN_69 = io_MEM_fault_mem_fault_type == 3'h3 ? 32'h6 : _GEN_62; // @[CSR.scala 122:90 83:12]
  wire [31:0] _GEN_70 = io_MEM_fault_mem_fault_type == 3'h3 ? io_MEM_fault_mtval : _GEN_63; // @[CSR.scala 122:90 84:11]
  wire [31:0] _GEN_71 = io_MEM_fault_mem_fault_type == 3'h3 ? _mstatus_T_14 : _GEN_64; // @[CSR.scala 122:90 89:13]
  wire [1:0] _GEN_72 = io_MEM_fault_mem_fault_type == 3'h3 ? 2'h3 : _GEN_65; // @[CSR.scala 122:90 90:19]
  wire  _GEN_73 = io_MEM_fault_mem_fault_type == 3'h2 | _GEN_66; // @[CSR.scala 120:82 80:21]
  wire [31:0] _GEN_74 = io_MEM_fault_mem_fault_type == 3'h2 ? io_pc : _GEN_67; // @[CSR.scala 120:82 81:10]
  wire [31:0] _GEN_76 = io_MEM_fault_mem_fault_type == 3'h2 ? 32'h5 : _GEN_69; // @[CSR.scala 120:82 83:12]
  wire [31:0] _GEN_77 = io_MEM_fault_mem_fault_type == 3'h2 ? io_MEM_fault_mtval : _GEN_70; // @[CSR.scala 120:82 84:11]
  wire [31:0] _GEN_78 = io_MEM_fault_mem_fault_type == 3'h2 ? _mstatus_T_14 : _GEN_71; // @[CSR.scala 120:82 89:13]
  wire [1:0] _GEN_79 = io_MEM_fault_mem_fault_type == 3'h2 ? 2'h3 : _GEN_72; // @[CSR.scala 120:82 90:19]
  wire  _GEN_80 = io_MEM_fault_mem_fault_type == 3'h1 | _GEN_73; // @[CSR.scala 118:81 80:21]
  wire [31:0] _mstatus_T_151 = mstatus | 32'h8; // @[CSR.scala 150:32]
  wire [31:0] _mstatus_T_152 = ~mstatus; // @[CSR.scala 150:32]
  wire [31:0] _mstatus_T_153 = _mstatus_T_152 | 32'h8; // @[CSR.scala 150:32]
  wire [31:0] _mstatus_T_154 = ~_mstatus_T_153; // @[CSR.scala 150:32]
  wire  _GEN_87 = ~fault_type ? _GEN_80 : 1'h1; // @[CSR.scala 117:62 147:23]
  assign io_cur_privilege = cur_privilege; // @[CSR.scala 37:20]
  assign io_csr_val = io_csr_idx_read == 12'h344 ? mip : _GEN_12; // @[CSR.scala 62:35 63:18]
  assign io_fault_occurs = ~no_fault & _GEN_87; // @[CSR.scala 109:19 97:19]
  assign io_fault_new_PC = ~fault_type ? mtvec : mepc; // @[CSR.scala 117:62 148:23]
  always @(posedge clock) begin
    if (reset) begin // @[CSR.scala 36:30]
      cur_privilege <= 2'h3; // @[CSR.scala 36:30]
    end else if (~no_fault) begin // @[CSR.scala 109:19]
      if (~fault_type) begin // @[CSR.scala 117:62]
        if (io_MEM_fault_mem_fault_type == 3'h1) begin // @[CSR.scala 118:81]
          cur_privilege <= 2'h3; // @[CSR.scala 90:19]
        end else begin
          cur_privilege <= _GEN_79;
        end
      end else begin
        cur_privilege <= read_MPP; // @[CSR.scala 151:21]
      end
    end
    if (reset) begin // @[CSR.scala 39:24]
      mstatus <= 32'h0; // @[CSR.scala 39:24]
    end else if (~no_fault) begin // @[CSR.scala 109:19]
      if (~fault_type) begin // @[CSR.scala 117:62]
        if (io_MEM_fault_mem_fault_type == 3'h1) begin // @[CSR.scala 118:81]
          mstatus <= _mstatus_T_14; // @[CSR.scala 89:13]
        end else begin
          mstatus <= _GEN_78;
        end
      end else if (read_MPIE) begin // @[CSR.scala 150:32]
        mstatus <= _mstatus_T_151;
      end else begin
        mstatus <= _mstatus_T_154;
      end
    end else if (_T_2 & io_csr_idx_write == 12'h300) begin // @[CSR.scala 67:35]
      mstatus <= io_write_data; // @[CSR.scala 68:11]
    end
    if (reset) begin // @[CSR.scala 40:20]
      mie <= 32'h0; // @[CSR.scala 40:20]
    end else if (_T_2 & io_csr_idx_write == 12'h304) begin // @[CSR.scala 67:35]
      mie <= io_write_data; // @[CSR.scala 68:11]
    end
    if (reset) begin // @[CSR.scala 41:22]
      mtvec <= 32'h0; // @[CSR.scala 41:22]
    end else if (_T_2 & io_csr_idx_write == 12'h305) begin // @[CSR.scala 67:35]
      mtvec <= io_write_data; // @[CSR.scala 68:11]
    end
    if (reset) begin // @[CSR.scala 43:25]
      mscratch <= 32'h0; // @[CSR.scala 43:25]
    end else if (_T_2 & io_csr_idx_write == 12'h340) begin // @[CSR.scala 67:35]
      mscratch <= io_write_data; // @[CSR.scala 68:11]
    end
    if (reset) begin // @[CSR.scala 44:21]
      mepc <= 32'h0; // @[CSR.scala 44:21]
    end else if (~no_fault) begin // @[CSR.scala 109:19]
      if (~fault_type) begin // @[CSR.scala 117:62]
        if (io_MEM_fault_mem_fault_type == 3'h1) begin // @[CSR.scala 118:81]
          mepc <= io_pc; // @[CSR.scala 81:10]
        end else begin
          mepc <= _GEN_74;
        end
      end else begin
        mepc <= _GEN_9;
      end
    end else begin
      mepc <= _GEN_9;
    end
    if (reset) begin // @[CSR.scala 45:23]
      mcause <= 32'h0; // @[CSR.scala 45:23]
    end else if (~no_fault) begin // @[CSR.scala 109:19]
      if (~fault_type) begin // @[CSR.scala 117:62]
        if (io_MEM_fault_mem_fault_type == 3'h1) begin // @[CSR.scala 118:81]
          mcause <= 32'h4; // @[CSR.scala 83:12]
        end else begin
          mcause <= _GEN_76;
        end
      end else begin
        mcause <= _GEN_11;
      end
    end else begin
      mcause <= _GEN_11;
    end
    if (reset) begin // @[CSR.scala 46:22]
      mtval <= 32'h0; // @[CSR.scala 46:22]
    end else if (~no_fault) begin // @[CSR.scala 109:19]
      if (~fault_type) begin // @[CSR.scala 117:62]
        if (io_MEM_fault_mem_fault_type == 3'h1) begin // @[CSR.scala 118:81]
          mtval <= io_MEM_fault_mtval; // @[CSR.scala 84:11]
        end else begin
          mtval <= _GEN_77;
        end
      end else begin
        mtval <= _GEN_13;
      end
    end else begin
      mtval <= _GEN_13;
    end
    if (reset) begin // @[CSR.scala 47:20]
      mip <= 32'h0; // @[CSR.scala 47:20]
    end else if (_T_2 & io_csr_idx_write == 12'h344) begin // @[CSR.scala 67:35]
      mip <= io_write_data; // @[CSR.scala 68:11]
    end else begin
      mip <= _mip_T_6; // @[CSR.scala 54:7]
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  cur_privilege = _RAND_0[1:0];
  _RAND_1 = {1{`RANDOM}};
  mstatus = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  mie = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  mtvec = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  mscratch = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  mepc = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  mcause = _RAND_6[31:0];
  _RAND_7 = {1{`RANDOM}};
  mtval = _RAND_7[31:0];
  _RAND_8 = {1{`RANDOM}};
  mip = _RAND_8[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module PC(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  input  [31:0] io_next_addr,
  output [31:0] io_addr
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] pc; // @[PC.scala 17:17]
  assign io_addr = pc; // @[PC.scala 18:10]
  always @(posedge clock) begin
    if (reset) begin // @[PC.scala 17:17]
      pc <= 32'h0; // @[PC.scala 17:17]
    end else if (io_cpu_state == 2'h2) begin // @[PC.scala 19:57]
      pc <= io_next_addr; // @[PC.scala 20:7]
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  pc = _RAND_0[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module PredictionCache(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state_type,
  input  [31:0] io_addr_read,
  input         io_read_en,
  input  [31:0] io_addr_write,
  input  [1:0]  io_dataIn,
  input         io_write_en,
  output [1:0]  io_write_data_out,
  output [1:0]  io_dataOut
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [31:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [31:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
  reg [31:0] _RAND_20;
  reg [31:0] _RAND_21;
  reg [31:0] _RAND_22;
  reg [31:0] _RAND_23;
  reg [31:0] _RAND_24;
  reg [31:0] _RAND_25;
  reg [31:0] _RAND_26;
  reg [31:0] _RAND_27;
  reg [31:0] _RAND_28;
  reg [31:0] _RAND_29;
  reg [31:0] _RAND_30;
  reg [31:0] _RAND_31;
  reg [31:0] _RAND_32;
  reg [31:0] _RAND_33;
  reg [31:0] _RAND_34;
  reg [31:0] _RAND_35;
  reg [31:0] _RAND_36;
  reg [31:0] _RAND_37;
  reg [31:0] _RAND_38;
  reg [31:0] _RAND_39;
  reg [31:0] _RAND_40;
  reg [31:0] _RAND_41;
  reg [31:0] _RAND_42;
  reg [31:0] _RAND_43;
  reg [31:0] _RAND_44;
  reg [31:0] _RAND_45;
  reg [31:0] _RAND_46;
  reg [31:0] _RAND_47;
`endif // RANDOMIZE_REG_INIT
  reg  cache_0_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_0_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_0_data; // @[PredictionCache.scala 31:22]
  reg  cache_1_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_1_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_1_data; // @[PredictionCache.scala 31:22]
  reg  cache_2_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_2_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_2_data; // @[PredictionCache.scala 31:22]
  reg  cache_3_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_3_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_3_data; // @[PredictionCache.scala 31:22]
  reg  cache_4_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_4_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_4_data; // @[PredictionCache.scala 31:22]
  reg  cache_5_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_5_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_5_data; // @[PredictionCache.scala 31:22]
  reg  cache_6_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_6_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_6_data; // @[PredictionCache.scala 31:22]
  reg  cache_7_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_7_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_7_data; // @[PredictionCache.scala 31:22]
  reg  cache_8_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_8_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_8_data; // @[PredictionCache.scala 31:22]
  reg  cache_9_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_9_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_9_data; // @[PredictionCache.scala 31:22]
  reg  cache_10_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_10_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_10_data; // @[PredictionCache.scala 31:22]
  reg  cache_11_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_11_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_11_data; // @[PredictionCache.scala 31:22]
  reg  cache_12_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_12_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_12_data; // @[PredictionCache.scala 31:22]
  reg  cache_13_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_13_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_13_data; // @[PredictionCache.scala 31:22]
  reg  cache_14_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_14_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_14_data; // @[PredictionCache.scala 31:22]
  reg  cache_15_valid; // @[PredictionCache.scala 31:22]
  reg [31:0] cache_15_address; // @[PredictionCache.scala 31:22]
  reg [1:0] cache_15_data; // @[PredictionCache.scala 31:22]
  wire [3:0] _GEN_0 = cache_15_address == io_addr_write ? 4'hf : 4'h1; // @[PredictionCache.scala 40:15 45:48 47:19]
  wire [3:0] _GEN_2 = cache_14_address == io_addr_write ? 4'he : _GEN_0; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_4 = cache_13_address == io_addr_write ? 4'hd : _GEN_2; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_6 = cache_12_address == io_addr_write ? 4'hc : _GEN_4; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_8 = cache_11_address == io_addr_write ? 4'hb : _GEN_6; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_10 = cache_10_address == io_addr_write ? 4'ha : _GEN_8; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_12 = cache_9_address == io_addr_write ? 4'h9 : _GEN_10; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_14 = cache_8_address == io_addr_write ? 4'h8 : _GEN_12; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_16 = cache_7_address == io_addr_write ? 4'h7 : _GEN_14; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_18 = cache_6_address == io_addr_write ? 4'h6 : _GEN_16; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_20 = cache_5_address == io_addr_write ? 4'h5 : _GEN_18; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_22 = cache_4_address == io_addr_write ? 4'h4 : _GEN_20; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_24 = cache_3_address == io_addr_write ? 4'h3 : _GEN_22; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_26 = cache_2_address == io_addr_write ? 4'h2 : _GEN_24; // @[PredictionCache.scala 45:48 47:19]
  wire [3:0] _GEN_28 = cache_1_address == io_addr_write ? 4'h1 : _GEN_26; // @[PredictionCache.scala 45:48 47:19]
  wire  _GEN_29 = cache_1_address == io_addr_write | (cache_2_address == io_addr_write | (cache_3_address ==
    io_addr_write | (cache_4_address == io_addr_write | (cache_5_address == io_addr_write | (cache_6_address ==
    io_addr_write | (cache_7_address == io_addr_write | (cache_8_address == io_addr_write | (cache_9_address ==
    io_addr_write | (cache_10_address == io_addr_write | (cache_11_address == io_addr_write | (cache_12_address ==
    io_addr_write | (cache_13_address == io_addr_write | (cache_14_address == io_addr_write | cache_15_address ==
    io_addr_write))))))))))))); // @[PredictionCache.scala 45:48 48:19]
  wire [3:0] _GEN_30 = cache_0_address == io_addr_write ? 4'h0 : _GEN_28; // @[PredictionCache.scala 45:48 47:19]
  wire  cache_hit = cache_0_address == io_addr_write | _GEN_29; // @[PredictionCache.scala 45:48 48:19]
  wire  _T_17 = ~cache_15_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_32 = ~cache_15_valid ? 4'hf : _GEN_30; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_18 = ~cache_14_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_33 = ~cache_14_valid ? 4'he : _GEN_32; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_19 = ~cache_13_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_34 = ~cache_13_valid ? 4'hd : _GEN_33; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_20 = ~cache_12_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_35 = ~cache_12_valid ? 4'hc : _GEN_34; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_21 = ~cache_11_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_36 = ~cache_11_valid ? 4'hb : _GEN_35; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_22 = ~cache_10_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_37 = ~cache_10_valid ? 4'ha : _GEN_36; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_23 = ~cache_9_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_38 = ~cache_9_valid ? 4'h9 : _GEN_37; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_24 = ~cache_8_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_39 = ~cache_8_valid ? 4'h8 : _GEN_38; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_25 = ~cache_7_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_40 = ~cache_7_valid ? 4'h7 : _GEN_39; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_26 = ~cache_6_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_41 = ~cache_6_valid ? 4'h6 : _GEN_40; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_27 = ~cache_5_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_42 = ~cache_5_valid ? 4'h5 : _GEN_41; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_28 = ~cache_4_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_43 = ~cache_4_valid ? 4'h4 : _GEN_42; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_29 = ~cache_3_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_44 = ~cache_3_valid ? 4'h3 : _GEN_43; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_30 = ~cache_2_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_45 = ~cache_2_valid ? 4'h2 : _GEN_44; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_31 = ~cache_1_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_46 = ~cache_1_valid ? 4'h1 : _GEN_45; // @[PredictionCache.scala 54:31 55:21]
  wire  _T_32 = ~cache_0_valid; // @[PredictionCache.scala 54:14]
  wire [3:0] _GEN_47 = ~cache_0_valid ? 4'h0 : _GEN_46; // @[PredictionCache.scala 54:31 55:21]
  wire [3:0] write_idx = ~cache_hit ? _GEN_47 : _GEN_30; // @[PredictionCache.scala 51:22]
  wire [1:0] _GEN_50 = 4'h1 == write_idx ? cache_1_data : cache_0_data; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_51 = 4'h2 == write_idx ? cache_2_data : _GEN_50; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_52 = 4'h3 == write_idx ? cache_3_data : _GEN_51; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_53 = 4'h4 == write_idx ? cache_4_data : _GEN_52; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_54 = 4'h5 == write_idx ? cache_5_data : _GEN_53; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_55 = 4'h6 == write_idx ? cache_6_data : _GEN_54; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_56 = 4'h7 == write_idx ? cache_7_data : _GEN_55; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_57 = 4'h8 == write_idx ? cache_8_data : _GEN_56; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_58 = 4'h9 == write_idx ? cache_9_data : _GEN_57; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_59 = 4'ha == write_idx ? cache_10_data : _GEN_58; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_60 = 4'hb == write_idx ? cache_11_data : _GEN_59; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_61 = 4'hc == write_idx ? cache_12_data : _GEN_60; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_62 = 4'hd == write_idx ? cache_13_data : _GEN_61; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_63 = 4'he == write_idx ? cache_14_data : _GEN_62; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_64 = 4'hf == write_idx ? cache_15_data : _GEN_63; // @[PredictionCache.scala 60:{25,25}]
  wire [1:0] _GEN_65 = cache_hit ? _GEN_64 : 2'h1; // @[PredictionCache.scala 59:21 60:25 62:25]
  wire  _T_33 = io_cpu_state_type == 2'h1; // @[PredictionCache.scala 64:28]
  wire  _GEN_66 = 4'h0 == write_idx | cache_0_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_67 = 4'h1 == write_idx | cache_1_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_68 = 4'h2 == write_idx | cache_2_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_69 = 4'h3 == write_idx | cache_3_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_70 = 4'h4 == write_idx | cache_4_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_71 = 4'h5 == write_idx | cache_5_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_72 = 4'h6 == write_idx | cache_6_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_73 = 4'h7 == write_idx | cache_7_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_74 = 4'h8 == write_idx | cache_8_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_75 = 4'h9 == write_idx | cache_9_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_76 = 4'ha == write_idx | cache_10_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_77 = 4'hb == write_idx | cache_11_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_78 = 4'hc == write_idx | cache_12_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_79 = 4'hd == write_idx | cache_13_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_80 = 4'he == write_idx | cache_14_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire  _GEN_81 = 4'hf == write_idx | cache_15_valid; // @[PredictionCache.scala 31:22 66:{30,30}]
  wire [31:0] _GEN_82 = 4'h0 == write_idx ? io_addr_write : cache_0_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_83 = 4'h1 == write_idx ? io_addr_write : cache_1_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_84 = 4'h2 == write_idx ? io_addr_write : cache_2_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_85 = 4'h3 == write_idx ? io_addr_write : cache_3_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_86 = 4'h4 == write_idx ? io_addr_write : cache_4_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_87 = 4'h5 == write_idx ? io_addr_write : cache_5_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_88 = 4'h6 == write_idx ? io_addr_write : cache_6_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_89 = 4'h7 == write_idx ? io_addr_write : cache_7_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_90 = 4'h8 == write_idx ? io_addr_write : cache_8_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_91 = 4'h9 == write_idx ? io_addr_write : cache_9_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_92 = 4'ha == write_idx ? io_addr_write : cache_10_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_93 = 4'hb == write_idx ? io_addr_write : cache_11_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_94 = 4'hc == write_idx ? io_addr_write : cache_12_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_95 = 4'hd == write_idx ? io_addr_write : cache_13_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_96 = 4'he == write_idx ? io_addr_write : cache_14_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [31:0] _GEN_97 = 4'hf == write_idx ? io_addr_write : cache_15_address; // @[PredictionCache.scala 31:22 67:{32,32}]
  wire [1:0] _GEN_98 = 4'h0 == write_idx ? io_dataIn : cache_0_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_99 = 4'h1 == write_idx ? io_dataIn : cache_1_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_100 = 4'h2 == write_idx ? io_dataIn : cache_2_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_101 = 4'h3 == write_idx ? io_dataIn : cache_3_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_102 = 4'h4 == write_idx ? io_dataIn : cache_4_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_103 = 4'h5 == write_idx ? io_dataIn : cache_5_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_104 = 4'h6 == write_idx ? io_dataIn : cache_6_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_105 = 4'h7 == write_idx ? io_dataIn : cache_7_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_106 = 4'h8 == write_idx ? io_dataIn : cache_8_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_107 = 4'h9 == write_idx ? io_dataIn : cache_9_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_108 = 4'ha == write_idx ? io_dataIn : cache_10_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_109 = 4'hb == write_idx ? io_dataIn : cache_11_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_110 = 4'hc == write_idx ? io_dataIn : cache_12_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_111 = 4'hd == write_idx ? io_dataIn : cache_13_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_112 = 4'he == write_idx ? io_dataIn : cache_14_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire [1:0] _GEN_113 = 4'hf == write_idx ? io_dataIn : cache_15_data; // @[PredictionCache.scala 31:22 68:{29,29}]
  wire  _GEN_114 = io_cpu_state_type == 2'h1 ? _GEN_66 : cache_0_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_115 = io_cpu_state_type == 2'h1 ? _GEN_67 : cache_1_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_116 = io_cpu_state_type == 2'h1 ? _GEN_68 : cache_2_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_117 = io_cpu_state_type == 2'h1 ? _GEN_69 : cache_3_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_118 = io_cpu_state_type == 2'h1 ? _GEN_70 : cache_4_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_119 = io_cpu_state_type == 2'h1 ? _GEN_71 : cache_5_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_120 = io_cpu_state_type == 2'h1 ? _GEN_72 : cache_6_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_121 = io_cpu_state_type == 2'h1 ? _GEN_73 : cache_7_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_122 = io_cpu_state_type == 2'h1 ? _GEN_74 : cache_8_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_123 = io_cpu_state_type == 2'h1 ? _GEN_75 : cache_9_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_124 = io_cpu_state_type == 2'h1 ? _GEN_76 : cache_10_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_125 = io_cpu_state_type == 2'h1 ? _GEN_77 : cache_11_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_126 = io_cpu_state_type == 2'h1 ? _GEN_78 : cache_12_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_127 = io_cpu_state_type == 2'h1 ? _GEN_79 : cache_13_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_128 = io_cpu_state_type == 2'h1 ? _GEN_80 : cache_14_valid; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_129 = io_cpu_state_type == 2'h1 ? _GEN_81 : cache_15_valid; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_130 = io_cpu_state_type == 2'h1 ? _GEN_82 : cache_0_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_131 = io_cpu_state_type == 2'h1 ? _GEN_83 : cache_1_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_132 = io_cpu_state_type == 2'h1 ? _GEN_84 : cache_2_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_133 = io_cpu_state_type == 2'h1 ? _GEN_85 : cache_3_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_134 = io_cpu_state_type == 2'h1 ? _GEN_86 : cache_4_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_135 = io_cpu_state_type == 2'h1 ? _GEN_87 : cache_5_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_136 = io_cpu_state_type == 2'h1 ? _GEN_88 : cache_6_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_137 = io_cpu_state_type == 2'h1 ? _GEN_89 : cache_7_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_138 = io_cpu_state_type == 2'h1 ? _GEN_90 : cache_8_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_139 = io_cpu_state_type == 2'h1 ? _GEN_91 : cache_9_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_140 = io_cpu_state_type == 2'h1 ? _GEN_92 : cache_10_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_141 = io_cpu_state_type == 2'h1 ? _GEN_93 : cache_11_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_142 = io_cpu_state_type == 2'h1 ? _GEN_94 : cache_12_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_143 = io_cpu_state_type == 2'h1 ? _GEN_95 : cache_13_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_144 = io_cpu_state_type == 2'h1 ? _GEN_96 : cache_14_address; // @[PredictionCache.scala 31:22 64:67]
  wire [31:0] _GEN_145 = io_cpu_state_type == 2'h1 ? _GEN_97 : cache_15_address; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_146 = io_cpu_state_type == 2'h1 ? _GEN_98 : cache_0_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_147 = io_cpu_state_type == 2'h1 ? _GEN_99 : cache_1_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_148 = io_cpu_state_type == 2'h1 ? _GEN_100 : cache_2_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_149 = io_cpu_state_type == 2'h1 ? _GEN_101 : cache_3_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_150 = io_cpu_state_type == 2'h1 ? _GEN_102 : cache_4_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_151 = io_cpu_state_type == 2'h1 ? _GEN_103 : cache_5_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_152 = io_cpu_state_type == 2'h1 ? _GEN_104 : cache_6_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_153 = io_cpu_state_type == 2'h1 ? _GEN_105 : cache_7_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_154 = io_cpu_state_type == 2'h1 ? _GEN_106 : cache_8_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_155 = io_cpu_state_type == 2'h1 ? _GEN_107 : cache_9_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_156 = io_cpu_state_type == 2'h1 ? _GEN_108 : cache_10_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_157 = io_cpu_state_type == 2'h1 ? _GEN_109 : cache_11_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_158 = io_cpu_state_type == 2'h1 ? _GEN_110 : cache_12_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_159 = io_cpu_state_type == 2'h1 ? _GEN_111 : cache_13_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_160 = io_cpu_state_type == 2'h1 ? _GEN_112 : cache_14_data; // @[PredictionCache.scala 31:22 64:67]
  wire [1:0] _GEN_161 = io_cpu_state_type == 2'h1 ? _GEN_113 : cache_15_data; // @[PredictionCache.scala 31:22 64:67]
  wire  _GEN_163 = io_write_en ? _GEN_114 : cache_0_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_164 = io_write_en ? _GEN_115 : cache_1_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_165 = io_write_en ? _GEN_116 : cache_2_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_166 = io_write_en ? _GEN_117 : cache_3_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_167 = io_write_en ? _GEN_118 : cache_4_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_168 = io_write_en ? _GEN_119 : cache_5_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_169 = io_write_en ? _GEN_120 : cache_6_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_170 = io_write_en ? _GEN_121 : cache_7_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_171 = io_write_en ? _GEN_122 : cache_8_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_172 = io_write_en ? _GEN_123 : cache_9_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_173 = io_write_en ? _GEN_124 : cache_10_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_174 = io_write_en ? _GEN_125 : cache_11_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_175 = io_write_en ? _GEN_126 : cache_12_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_176 = io_write_en ? _GEN_127 : cache_13_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_177 = io_write_en ? _GEN_128 : cache_14_valid; // @[PredictionCache.scala 38:21 31:22]
  wire  _GEN_178 = io_write_en ? _GEN_129 : cache_15_valid; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_179 = io_write_en ? _GEN_130 : cache_0_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_180 = io_write_en ? _GEN_131 : cache_1_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_181 = io_write_en ? _GEN_132 : cache_2_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_182 = io_write_en ? _GEN_133 : cache_3_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_183 = io_write_en ? _GEN_134 : cache_4_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_184 = io_write_en ? _GEN_135 : cache_5_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_185 = io_write_en ? _GEN_136 : cache_6_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_186 = io_write_en ? _GEN_137 : cache_7_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_187 = io_write_en ? _GEN_138 : cache_8_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_188 = io_write_en ? _GEN_139 : cache_9_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_189 = io_write_en ? _GEN_140 : cache_10_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_190 = io_write_en ? _GEN_141 : cache_11_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_191 = io_write_en ? _GEN_142 : cache_12_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_192 = io_write_en ? _GEN_143 : cache_13_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_193 = io_write_en ? _GEN_144 : cache_14_address; // @[PredictionCache.scala 38:21 31:22]
  wire [31:0] _GEN_194 = io_write_en ? _GEN_145 : cache_15_address; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_195 = io_write_en ? _GEN_146 : cache_0_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_196 = io_write_en ? _GEN_147 : cache_1_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_197 = io_write_en ? _GEN_148 : cache_2_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_198 = io_write_en ? _GEN_149 : cache_3_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_199 = io_write_en ? _GEN_150 : cache_4_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_200 = io_write_en ? _GEN_151 : cache_5_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_201 = io_write_en ? _GEN_152 : cache_6_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_202 = io_write_en ? _GEN_153 : cache_7_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_203 = io_write_en ? _GEN_154 : cache_8_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_204 = io_write_en ? _GEN_155 : cache_9_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_205 = io_write_en ? _GEN_156 : cache_10_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_206 = io_write_en ? _GEN_157 : cache_11_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_207 = io_write_en ? _GEN_158 : cache_12_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_208 = io_write_en ? _GEN_159 : cache_13_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_209 = io_write_en ? _GEN_160 : cache_14_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_210 = io_write_en ? _GEN_161 : cache_15_data; // @[PredictionCache.scala 38:21 31:22]
  wire [1:0] _GEN_211 = cache_0_valid & cache_0_address == io_addr_read ? cache_0_data : 2'h1; // @[PredictionCache.scala 35:14 76:65 77:20]
  wire [1:0] _GEN_213 = cache_1_valid & cache_1_address == io_addr_read ? cache_1_data : _GEN_211; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_215 = cache_2_valid & cache_2_address == io_addr_read ? cache_2_data : _GEN_213; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_217 = cache_3_valid & cache_3_address == io_addr_read ? cache_3_data : _GEN_215; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_219 = cache_4_valid & cache_4_address == io_addr_read ? cache_4_data : _GEN_217; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_221 = cache_5_valid & cache_5_address == io_addr_read ? cache_5_data : _GEN_219; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_223 = cache_6_valid & cache_6_address == io_addr_read ? cache_6_data : _GEN_221; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_225 = cache_7_valid & cache_7_address == io_addr_read ? cache_7_data : _GEN_223; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_227 = cache_8_valid & cache_8_address == io_addr_read ? cache_8_data : _GEN_225; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_229 = cache_9_valid & cache_9_address == io_addr_read ? cache_9_data : _GEN_227; // @[PredictionCache.scala 76:65 77:20]
  wire  _GEN_230 = cache_9_valid & cache_9_address == io_addr_read | (cache_8_valid & cache_8_address == io_addr_read |
    (cache_7_valid & cache_7_address == io_addr_read | (cache_6_valid & cache_6_address == io_addr_read | (cache_5_valid
     & cache_5_address == io_addr_read | (cache_4_valid & cache_4_address == io_addr_read | (cache_3_valid &
    cache_3_address == io_addr_read | (cache_2_valid & cache_2_address == io_addr_read | (cache_1_valid &
    cache_1_address == io_addr_read | cache_0_valid & cache_0_address == io_addr_read)))))))); // @[PredictionCache.scala 76:65 78:24]
  wire [1:0] _GEN_231 = cache_10_valid & cache_10_address == io_addr_read ? cache_10_data : _GEN_229; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_233 = cache_11_valid & cache_11_address == io_addr_read ? cache_11_data : _GEN_231; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_235 = cache_12_valid & cache_12_address == io_addr_read ? cache_12_data : _GEN_233; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_237 = cache_13_valid & cache_13_address == io_addr_read ? cache_13_data : _GEN_235; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_239 = cache_14_valid & cache_14_address == io_addr_read ? cache_14_data : _GEN_237; // @[PredictionCache.scala 76:65 77:20]
  wire [1:0] _GEN_241 = cache_15_valid & cache_15_address == io_addr_read ? cache_15_data : _GEN_239; // @[PredictionCache.scala 76:65 77:20]
  wire  cache_hit_read = cache_15_valid & cache_15_address == io_addr_read | (cache_14_valid & cache_14_address ==
    io_addr_read | (cache_13_valid & cache_13_address == io_addr_read | (cache_12_valid & cache_12_address ==
    io_addr_read | (cache_11_valid & cache_11_address == io_addr_read | (cache_10_valid & cache_10_address ==
    io_addr_read | _GEN_230))))); // @[PredictionCache.scala 76:65 78:24]
  wire [3:0] _GEN_243 = _T_17 ? 4'hf : 4'h1; // @[PredictionCache.scala 84:22 87:31 88:26]
  wire [3:0] _GEN_244 = _T_18 ? 4'he : _GEN_243; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_245 = _T_19 ? 4'hd : _GEN_244; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_246 = _T_20 ? 4'hc : _GEN_245; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_247 = _T_21 ? 4'hb : _GEN_246; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_248 = _T_22 ? 4'ha : _GEN_247; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_249 = _T_23 ? 4'h9 : _GEN_248; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_250 = _T_24 ? 4'h8 : _GEN_249; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_251 = _T_25 ? 4'h7 : _GEN_250; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_252 = _T_26 ? 4'h6 : _GEN_251; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_253 = _T_27 ? 4'h5 : _GEN_252; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_254 = _T_28 ? 4'h4 : _GEN_253; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_255 = _T_29 ? 4'h3 : _GEN_254; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_256 = _T_30 ? 4'h2 : _GEN_255; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] _GEN_257 = _T_31 ? 4'h1 : _GEN_256; // @[PredictionCache.scala 87:31 88:26]
  wire [3:0] write_idx_read = _T_32 ? 4'h0 : _GEN_257; // @[PredictionCache.scala 87:31 88:26]
  wire  _GEN_259 = 4'h0 == write_idx_read | _GEN_163; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_260 = 4'h1 == write_idx_read | _GEN_164; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_261 = 4'h2 == write_idx_read | _GEN_165; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_262 = 4'h3 == write_idx_read | _GEN_166; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_263 = 4'h4 == write_idx_read | _GEN_167; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_264 = 4'h5 == write_idx_read | _GEN_168; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_265 = 4'h6 == write_idx_read | _GEN_169; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_266 = 4'h7 == write_idx_read | _GEN_170; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_267 = 4'h8 == write_idx_read | _GEN_171; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_268 = 4'h9 == write_idx_read | _GEN_172; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_269 = 4'ha == write_idx_read | _GEN_173; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_270 = 4'hb == write_idx_read | _GEN_174; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_271 = 4'hc == write_idx_read | _GEN_175; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_272 = 4'hd == write_idx_read | _GEN_176; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_273 = 4'he == write_idx_read | _GEN_177; // @[PredictionCache.scala 93:{37,37}]
  wire  _GEN_274 = 4'hf == write_idx_read | _GEN_178; // @[PredictionCache.scala 93:{37,37}]
  wire [31:0] _GEN_275 = 4'h0 == write_idx_read ? io_addr_read : _GEN_179; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_276 = 4'h1 == write_idx_read ? io_addr_read : _GEN_180; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_277 = 4'h2 == write_idx_read ? io_addr_read : _GEN_181; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_278 = 4'h3 == write_idx_read ? io_addr_read : _GEN_182; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_279 = 4'h4 == write_idx_read ? io_addr_read : _GEN_183; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_280 = 4'h5 == write_idx_read ? io_addr_read : _GEN_184; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_281 = 4'h6 == write_idx_read ? io_addr_read : _GEN_185; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_282 = 4'h7 == write_idx_read ? io_addr_read : _GEN_186; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_283 = 4'h8 == write_idx_read ? io_addr_read : _GEN_187; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_284 = 4'h9 == write_idx_read ? io_addr_read : _GEN_188; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_285 = 4'ha == write_idx_read ? io_addr_read : _GEN_189; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_286 = 4'hb == write_idx_read ? io_addr_read : _GEN_190; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_287 = 4'hc == write_idx_read ? io_addr_read : _GEN_191; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_288 = 4'hd == write_idx_read ? io_addr_read : _GEN_192; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_289 = 4'he == write_idx_read ? io_addr_read : _GEN_193; // @[PredictionCache.scala 94:{39,39}]
  wire [31:0] _GEN_290 = 4'hf == write_idx_read ? io_addr_read : _GEN_194; // @[PredictionCache.scala 94:{39,39}]
  wire [1:0] _GEN_291 = 4'h0 == write_idx_read ? 2'h1 : _GEN_195; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_292 = 4'h1 == write_idx_read ? 2'h1 : _GEN_196; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_293 = 4'h2 == write_idx_read ? 2'h1 : _GEN_197; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_294 = 4'h3 == write_idx_read ? 2'h1 : _GEN_198; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_295 = 4'h4 == write_idx_read ? 2'h1 : _GEN_199; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_296 = 4'h5 == write_idx_read ? 2'h1 : _GEN_200; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_297 = 4'h6 == write_idx_read ? 2'h1 : _GEN_201; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_298 = 4'h7 == write_idx_read ? 2'h1 : _GEN_202; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_299 = 4'h8 == write_idx_read ? 2'h1 : _GEN_203; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_300 = 4'h9 == write_idx_read ? 2'h1 : _GEN_204; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_301 = 4'ha == write_idx_read ? 2'h1 : _GEN_205; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_302 = 4'hb == write_idx_read ? 2'h1 : _GEN_206; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_303 = 4'hc == write_idx_read ? 2'h1 : _GEN_207; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_304 = 4'hd == write_idx_read ? 2'h1 : _GEN_208; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_305 = 4'he == write_idx_read ? 2'h1 : _GEN_209; // @[PredictionCache.scala 95:{36,36}]
  wire [1:0] _GEN_306 = 4'hf == write_idx_read ? 2'h1 : _GEN_210; // @[PredictionCache.scala 95:{36,36}]
  assign io_write_data_out = io_write_en ? _GEN_65 : 2'h1; // @[PredictionCache.scala 34:21 38:21]
  assign io_dataOut = io_read_en ? _GEN_241 : 2'h1; // @[PredictionCache.scala 35:14 72:20]
  always @(posedge clock) begin
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_0_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_0_valid <= _GEN_259;
        end else begin
          cache_0_valid <= _GEN_163;
        end
      end else begin
        cache_0_valid <= _GEN_163;
      end
    end else begin
      cache_0_valid <= _GEN_163;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_0_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_0_address <= _GEN_275;
        end else begin
          cache_0_address <= _GEN_179;
        end
      end else begin
        cache_0_address <= _GEN_179;
      end
    end else begin
      cache_0_address <= _GEN_179;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_0_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_0_data <= _GEN_291;
        end else begin
          cache_0_data <= _GEN_195;
        end
      end else begin
        cache_0_data <= _GEN_195;
      end
    end else begin
      cache_0_data <= _GEN_195;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_1_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_1_valid <= _GEN_260;
        end else begin
          cache_1_valid <= _GEN_164;
        end
      end else begin
        cache_1_valid <= _GEN_164;
      end
    end else begin
      cache_1_valid <= _GEN_164;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_1_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_1_address <= _GEN_276;
        end else begin
          cache_1_address <= _GEN_180;
        end
      end else begin
        cache_1_address <= _GEN_180;
      end
    end else begin
      cache_1_address <= _GEN_180;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_1_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_1_data <= _GEN_292;
        end else begin
          cache_1_data <= _GEN_196;
        end
      end else begin
        cache_1_data <= _GEN_196;
      end
    end else begin
      cache_1_data <= _GEN_196;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_2_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_2_valid <= _GEN_261;
        end else begin
          cache_2_valid <= _GEN_165;
        end
      end else begin
        cache_2_valid <= _GEN_165;
      end
    end else begin
      cache_2_valid <= _GEN_165;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_2_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_2_address <= _GEN_277;
        end else begin
          cache_2_address <= _GEN_181;
        end
      end else begin
        cache_2_address <= _GEN_181;
      end
    end else begin
      cache_2_address <= _GEN_181;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_2_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_2_data <= _GEN_293;
        end else begin
          cache_2_data <= _GEN_197;
        end
      end else begin
        cache_2_data <= _GEN_197;
      end
    end else begin
      cache_2_data <= _GEN_197;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_3_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_3_valid <= _GEN_262;
        end else begin
          cache_3_valid <= _GEN_166;
        end
      end else begin
        cache_3_valid <= _GEN_166;
      end
    end else begin
      cache_3_valid <= _GEN_166;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_3_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_3_address <= _GEN_278;
        end else begin
          cache_3_address <= _GEN_182;
        end
      end else begin
        cache_3_address <= _GEN_182;
      end
    end else begin
      cache_3_address <= _GEN_182;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_3_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_3_data <= _GEN_294;
        end else begin
          cache_3_data <= _GEN_198;
        end
      end else begin
        cache_3_data <= _GEN_198;
      end
    end else begin
      cache_3_data <= _GEN_198;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_4_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_4_valid <= _GEN_263;
        end else begin
          cache_4_valid <= _GEN_167;
        end
      end else begin
        cache_4_valid <= _GEN_167;
      end
    end else begin
      cache_4_valid <= _GEN_167;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_4_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_4_address <= _GEN_279;
        end else begin
          cache_4_address <= _GEN_183;
        end
      end else begin
        cache_4_address <= _GEN_183;
      end
    end else begin
      cache_4_address <= _GEN_183;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_4_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_4_data <= _GEN_295;
        end else begin
          cache_4_data <= _GEN_199;
        end
      end else begin
        cache_4_data <= _GEN_199;
      end
    end else begin
      cache_4_data <= _GEN_199;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_5_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_5_valid <= _GEN_264;
        end else begin
          cache_5_valid <= _GEN_168;
        end
      end else begin
        cache_5_valid <= _GEN_168;
      end
    end else begin
      cache_5_valid <= _GEN_168;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_5_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_5_address <= _GEN_280;
        end else begin
          cache_5_address <= _GEN_184;
        end
      end else begin
        cache_5_address <= _GEN_184;
      end
    end else begin
      cache_5_address <= _GEN_184;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_5_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_5_data <= _GEN_296;
        end else begin
          cache_5_data <= _GEN_200;
        end
      end else begin
        cache_5_data <= _GEN_200;
      end
    end else begin
      cache_5_data <= _GEN_200;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_6_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_6_valid <= _GEN_265;
        end else begin
          cache_6_valid <= _GEN_169;
        end
      end else begin
        cache_6_valid <= _GEN_169;
      end
    end else begin
      cache_6_valid <= _GEN_169;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_6_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_6_address <= _GEN_281;
        end else begin
          cache_6_address <= _GEN_185;
        end
      end else begin
        cache_6_address <= _GEN_185;
      end
    end else begin
      cache_6_address <= _GEN_185;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_6_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_6_data <= _GEN_297;
        end else begin
          cache_6_data <= _GEN_201;
        end
      end else begin
        cache_6_data <= _GEN_201;
      end
    end else begin
      cache_6_data <= _GEN_201;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_7_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_7_valid <= _GEN_266;
        end else begin
          cache_7_valid <= _GEN_170;
        end
      end else begin
        cache_7_valid <= _GEN_170;
      end
    end else begin
      cache_7_valid <= _GEN_170;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_7_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_7_address <= _GEN_282;
        end else begin
          cache_7_address <= _GEN_186;
        end
      end else begin
        cache_7_address <= _GEN_186;
      end
    end else begin
      cache_7_address <= _GEN_186;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_7_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_7_data <= _GEN_298;
        end else begin
          cache_7_data <= _GEN_202;
        end
      end else begin
        cache_7_data <= _GEN_202;
      end
    end else begin
      cache_7_data <= _GEN_202;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_8_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_8_valid <= _GEN_267;
        end else begin
          cache_8_valid <= _GEN_171;
        end
      end else begin
        cache_8_valid <= _GEN_171;
      end
    end else begin
      cache_8_valid <= _GEN_171;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_8_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_8_address <= _GEN_283;
        end else begin
          cache_8_address <= _GEN_187;
        end
      end else begin
        cache_8_address <= _GEN_187;
      end
    end else begin
      cache_8_address <= _GEN_187;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_8_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_8_data <= _GEN_299;
        end else begin
          cache_8_data <= _GEN_203;
        end
      end else begin
        cache_8_data <= _GEN_203;
      end
    end else begin
      cache_8_data <= _GEN_203;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_9_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_9_valid <= _GEN_268;
        end else begin
          cache_9_valid <= _GEN_172;
        end
      end else begin
        cache_9_valid <= _GEN_172;
      end
    end else begin
      cache_9_valid <= _GEN_172;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_9_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_9_address <= _GEN_284;
        end else begin
          cache_9_address <= _GEN_188;
        end
      end else begin
        cache_9_address <= _GEN_188;
      end
    end else begin
      cache_9_address <= _GEN_188;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_9_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_9_data <= _GEN_300;
        end else begin
          cache_9_data <= _GEN_204;
        end
      end else begin
        cache_9_data <= _GEN_204;
      end
    end else begin
      cache_9_data <= _GEN_204;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_10_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_10_valid <= _GEN_269;
        end else begin
          cache_10_valid <= _GEN_173;
        end
      end else begin
        cache_10_valid <= _GEN_173;
      end
    end else begin
      cache_10_valid <= _GEN_173;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_10_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_10_address <= _GEN_285;
        end else begin
          cache_10_address <= _GEN_189;
        end
      end else begin
        cache_10_address <= _GEN_189;
      end
    end else begin
      cache_10_address <= _GEN_189;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_10_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_10_data <= _GEN_301;
        end else begin
          cache_10_data <= _GEN_205;
        end
      end else begin
        cache_10_data <= _GEN_205;
      end
    end else begin
      cache_10_data <= _GEN_205;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_11_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_11_valid <= _GEN_270;
        end else begin
          cache_11_valid <= _GEN_174;
        end
      end else begin
        cache_11_valid <= _GEN_174;
      end
    end else begin
      cache_11_valid <= _GEN_174;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_11_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_11_address <= _GEN_286;
        end else begin
          cache_11_address <= _GEN_190;
        end
      end else begin
        cache_11_address <= _GEN_190;
      end
    end else begin
      cache_11_address <= _GEN_190;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_11_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_11_data <= _GEN_302;
        end else begin
          cache_11_data <= _GEN_206;
        end
      end else begin
        cache_11_data <= _GEN_206;
      end
    end else begin
      cache_11_data <= _GEN_206;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_12_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_12_valid <= _GEN_271;
        end else begin
          cache_12_valid <= _GEN_175;
        end
      end else begin
        cache_12_valid <= _GEN_175;
      end
    end else begin
      cache_12_valid <= _GEN_175;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_12_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_12_address <= _GEN_287;
        end else begin
          cache_12_address <= _GEN_191;
        end
      end else begin
        cache_12_address <= _GEN_191;
      end
    end else begin
      cache_12_address <= _GEN_191;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_12_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_12_data <= _GEN_303;
        end else begin
          cache_12_data <= _GEN_207;
        end
      end else begin
        cache_12_data <= _GEN_207;
      end
    end else begin
      cache_12_data <= _GEN_207;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_13_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_13_valid <= _GEN_272;
        end else begin
          cache_13_valid <= _GEN_176;
        end
      end else begin
        cache_13_valid <= _GEN_176;
      end
    end else begin
      cache_13_valid <= _GEN_176;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_13_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_13_address <= _GEN_288;
        end else begin
          cache_13_address <= _GEN_192;
        end
      end else begin
        cache_13_address <= _GEN_192;
      end
    end else begin
      cache_13_address <= _GEN_192;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_13_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_13_data <= _GEN_304;
        end else begin
          cache_13_data <= _GEN_208;
        end
      end else begin
        cache_13_data <= _GEN_208;
      end
    end else begin
      cache_13_data <= _GEN_208;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_14_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_14_valid <= _GEN_273;
        end else begin
          cache_14_valid <= _GEN_177;
        end
      end else begin
        cache_14_valid <= _GEN_177;
      end
    end else begin
      cache_14_valid <= _GEN_177;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_14_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_14_address <= _GEN_289;
        end else begin
          cache_14_address <= _GEN_193;
        end
      end else begin
        cache_14_address <= _GEN_193;
      end
    end else begin
      cache_14_address <= _GEN_193;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_14_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_14_data <= _GEN_305;
        end else begin
          cache_14_data <= _GEN_209;
        end
      end else begin
        cache_14_data <= _GEN_209;
      end
    end else begin
      cache_14_data <= _GEN_209;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_15_valid <= 1'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_15_valid <= _GEN_274;
        end else begin
          cache_15_valid <= _GEN_178;
        end
      end else begin
        cache_15_valid <= _GEN_178;
      end
    end else begin
      cache_15_valid <= _GEN_178;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_15_address <= 32'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_15_address <= _GEN_290;
        end else begin
          cache_15_address <= _GEN_194;
        end
      end else begin
        cache_15_address <= _GEN_194;
      end
    end else begin
      cache_15_address <= _GEN_194;
    end
    if (reset) begin // @[PredictionCache.scala 31:22]
      cache_15_data <= 2'h0; // @[PredictionCache.scala 31:22]
    end else if (io_read_en) begin // @[PredictionCache.scala 72:20]
      if (~cache_hit_read) begin // @[PredictionCache.scala 81:27]
        if (_T_33) begin // @[PredictionCache.scala 91:69]
          cache_15_data <= _GEN_306;
        end else begin
          cache_15_data <= _GEN_210;
        end
      end else begin
        cache_15_data <= _GEN_210;
      end
    end else begin
      cache_15_data <= _GEN_210;
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  cache_0_valid = _RAND_0[0:0];
  _RAND_1 = {1{`RANDOM}};
  cache_0_address = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  cache_0_data = _RAND_2[1:0];
  _RAND_3 = {1{`RANDOM}};
  cache_1_valid = _RAND_3[0:0];
  _RAND_4 = {1{`RANDOM}};
  cache_1_address = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  cache_1_data = _RAND_5[1:0];
  _RAND_6 = {1{`RANDOM}};
  cache_2_valid = _RAND_6[0:0];
  _RAND_7 = {1{`RANDOM}};
  cache_2_address = _RAND_7[31:0];
  _RAND_8 = {1{`RANDOM}};
  cache_2_data = _RAND_8[1:0];
  _RAND_9 = {1{`RANDOM}};
  cache_3_valid = _RAND_9[0:0];
  _RAND_10 = {1{`RANDOM}};
  cache_3_address = _RAND_10[31:0];
  _RAND_11 = {1{`RANDOM}};
  cache_3_data = _RAND_11[1:0];
  _RAND_12 = {1{`RANDOM}};
  cache_4_valid = _RAND_12[0:0];
  _RAND_13 = {1{`RANDOM}};
  cache_4_address = _RAND_13[31:0];
  _RAND_14 = {1{`RANDOM}};
  cache_4_data = _RAND_14[1:0];
  _RAND_15 = {1{`RANDOM}};
  cache_5_valid = _RAND_15[0:0];
  _RAND_16 = {1{`RANDOM}};
  cache_5_address = _RAND_16[31:0];
  _RAND_17 = {1{`RANDOM}};
  cache_5_data = _RAND_17[1:0];
  _RAND_18 = {1{`RANDOM}};
  cache_6_valid = _RAND_18[0:0];
  _RAND_19 = {1{`RANDOM}};
  cache_6_address = _RAND_19[31:0];
  _RAND_20 = {1{`RANDOM}};
  cache_6_data = _RAND_20[1:0];
  _RAND_21 = {1{`RANDOM}};
  cache_7_valid = _RAND_21[0:0];
  _RAND_22 = {1{`RANDOM}};
  cache_7_address = _RAND_22[31:0];
  _RAND_23 = {1{`RANDOM}};
  cache_7_data = _RAND_23[1:0];
  _RAND_24 = {1{`RANDOM}};
  cache_8_valid = _RAND_24[0:0];
  _RAND_25 = {1{`RANDOM}};
  cache_8_address = _RAND_25[31:0];
  _RAND_26 = {1{`RANDOM}};
  cache_8_data = _RAND_26[1:0];
  _RAND_27 = {1{`RANDOM}};
  cache_9_valid = _RAND_27[0:0];
  _RAND_28 = {1{`RANDOM}};
  cache_9_address = _RAND_28[31:0];
  _RAND_29 = {1{`RANDOM}};
  cache_9_data = _RAND_29[1:0];
  _RAND_30 = {1{`RANDOM}};
  cache_10_valid = _RAND_30[0:0];
  _RAND_31 = {1{`RANDOM}};
  cache_10_address = _RAND_31[31:0];
  _RAND_32 = {1{`RANDOM}};
  cache_10_data = _RAND_32[1:0];
  _RAND_33 = {1{`RANDOM}};
  cache_11_valid = _RAND_33[0:0];
  _RAND_34 = {1{`RANDOM}};
  cache_11_address = _RAND_34[31:0];
  _RAND_35 = {1{`RANDOM}};
  cache_11_data = _RAND_35[1:0];
  _RAND_36 = {1{`RANDOM}};
  cache_12_valid = _RAND_36[0:0];
  _RAND_37 = {1{`RANDOM}};
  cache_12_address = _RAND_37[31:0];
  _RAND_38 = {1{`RANDOM}};
  cache_12_data = _RAND_38[1:0];
  _RAND_39 = {1{`RANDOM}};
  cache_13_valid = _RAND_39[0:0];
  _RAND_40 = {1{`RANDOM}};
  cache_13_address = _RAND_40[31:0];
  _RAND_41 = {1{`RANDOM}};
  cache_13_data = _RAND_41[1:0];
  _RAND_42 = {1{`RANDOM}};
  cache_14_valid = _RAND_42[0:0];
  _RAND_43 = {1{`RANDOM}};
  cache_14_address = _RAND_43[31:0];
  _RAND_44 = {1{`RANDOM}};
  cache_14_data = _RAND_44[1:0];
  _RAND_45 = {1{`RANDOM}};
  cache_15_valid = _RAND_45[0:0];
  _RAND_46 = {1{`RANDOM}};
  cache_15_address = _RAND_46[31:0];
  _RAND_47 = {1{`RANDOM}};
  cache_15_data = _RAND_47[1:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module PCPrediction(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state_type,
  input  [31:0] io_pc,
  input  [31:0] io_instruction,
  input         io_prediction_failure_failure_occurs,
  input         io_prediction_failure_success_occurs,
  input  [31:0] io_prediction_failure_address,
  output [31:0] io_predict_new_pc
);
  wire  cache_clock; // @[PCPrediction.scala 63:23]
  wire  cache_reset; // @[PCPrediction.scala 63:23]
  wire [1:0] cache_io_cpu_state_type; // @[PCPrediction.scala 63:23]
  wire [31:0] cache_io_addr_read; // @[PCPrediction.scala 63:23]
  wire  cache_io_read_en; // @[PCPrediction.scala 63:23]
  wire [31:0] cache_io_addr_write; // @[PCPrediction.scala 63:23]
  wire [1:0] cache_io_dataIn; // @[PCPrediction.scala 63:23]
  wire  cache_io_write_en; // @[PCPrediction.scala 63:23]
  wire [1:0] cache_io_write_data_out; // @[PCPrediction.scala 63:23]
  wire [1:0] cache_io_dataOut; // @[PCPrediction.scala 63:23]
  wire [31:0] _io_predict_new_pc_T_1 = io_pc + 32'h4; // @[PCPrediction.scala 42:30]
  wire [10:0] _raw_imm_T_2 = io_instruction[31] ? 11'h7ff : 11'h0; // @[Bitwise.scala 77:12]
  wire [30:0] raw_imm = {_raw_imm_T_2,io_instruction[31],io_instruction[19:12],io_instruction[20],io_instruction[30:21]}
    ; // @[Cat.scala 33:92]
  wire [31:0] _io_predict_new_pc_T_2 = {raw_imm, 1'h0}; // @[PCPrediction.scala 48:43]
  wire [31:0] _io_predict_new_pc_T_4 = io_pc + _io_predict_new_pc_T_2; // @[PCPrediction.scala 48:32]
  wire [31:0] _GEN_0 = io_instruction[6:0] == 7'h6f ? _io_predict_new_pc_T_4 : _io_predict_new_pc_T_1; // @[PCPrediction.scala 42:21 44:48 48:23]
  wire  prediction_result_get = io_prediction_failure_failure_occurs | io_prediction_failure_success_occurs; // @[PCPrediction.scala 76:70]
  wire [19:0] _imm_T_2 = io_instruction[31] ? 20'hfffff : 20'h0; // @[Bitwise.scala 77:12]
  wire [31:0] imm = {_imm_T_2,io_instruction[31],io_instruction[7],io_instruction[30:25],io_instruction[11:8]}; // @[Cat.scala 33:92]
  wire [32:0] _io_predict_new_pc_T_5 = {imm, 1'h0}; // @[PCPrediction.scala 86:43]
  wire [32:0] _GEN_10 = {{1'd0}, io_pc}; // @[PCPrediction.scala 86:36]
  wire [32:0] _io_predict_new_pc_T_7 = _GEN_10 + _io_predict_new_pc_T_5; // @[PCPrediction.scala 86:36]
  wire [32:0] _GEN_1 = cache_io_dataOut <= 2'h1 ? _io_predict_new_pc_T_7 : {{1'd0}, _io_predict_new_pc_T_1}; // @[PCPrediction.scala 85:49 86:27 88:27]
  wire [32:0] _GEN_3 = io_instruction[6:0] == 7'h63 ? _GEN_1 : {{1'd0}, _GEN_0}; // @[PCPrediction.scala 80:49]
  wire  _T_5 = cache_io_write_data_out <= 2'h1; // @[PCPrediction.scala 94:38]
  wire [1:0] _cache_io_dataIn_T_1 = cache_io_write_data_out + 2'h1; // @[PCPrediction.scala 95:54]
  wire [1:0] _cache_io_dataIn_T_3 = cache_io_write_data_out - 2'h1; // @[PCPrediction.scala 97:54]
  wire [1:0] _GEN_4 = cache_io_write_data_out <= 2'h1 ? _cache_io_dataIn_T_1 : _cache_io_dataIn_T_3; // @[PCPrediction.scala 94:58 95:27 97:27]
  wire [1:0] _GEN_5 = cache_io_write_data_out > 2'h0 ? _cache_io_dataIn_T_3 : cache_io_write_data_out; // @[PCPrediction.scala 102:47 103:29 75:21]
  wire [1:0] _GEN_6 = cache_io_write_data_out < 2'h3 ? _cache_io_dataIn_T_1 : cache_io_write_data_out; // @[PCPrediction.scala 106:64 107:29 75:21]
  wire [1:0] _GEN_7 = _T_5 ? _GEN_5 : _GEN_6; // @[PCPrediction.scala 101:58]
  wire [1:0] _GEN_8 = io_prediction_failure_failure_occurs ? _GEN_4 : _GEN_7; // @[PCPrediction.scala 93:50]
  PredictionCache cache ( // @[PCPrediction.scala 63:23]
    .clock(cache_clock),
    .reset(cache_reset),
    .io_cpu_state_type(cache_io_cpu_state_type),
    .io_addr_read(cache_io_addr_read),
    .io_read_en(cache_io_read_en),
    .io_addr_write(cache_io_addr_write),
    .io_dataIn(cache_io_dataIn),
    .io_write_en(cache_io_write_en),
    .io_write_data_out(cache_io_write_data_out),
    .io_dataOut(cache_io_dataOut)
  );
  assign io_predict_new_pc = _GEN_3[31:0];
  assign cache_clock = clock;
  assign cache_reset = reset;
  assign cache_io_cpu_state_type = io_cpu_state_type; // @[PCPrediction.scala 70:28]
  assign cache_io_addr_read = io_pc; // @[PCPrediction.scala 71:24]
  assign cache_io_read_en = io_instruction[6:0] == 7'h63; // @[PCPrediction.scala 80:31]
  assign cache_io_addr_write = io_prediction_failure_address; // @[PCPrediction.scala 74:25]
  assign cache_io_dataIn = prediction_result_get ? _GEN_8 : cache_io_write_data_out; // @[PCPrediction.scala 75:21 92:33]
  assign cache_io_write_en = io_prediction_failure_failure_occurs | io_prediction_failure_success_occurs; // @[PCPrediction.scala 76:70]
endmodule
module NextPCGen(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state_type,
  input  [1:0]  io_nextPC_control_signal,
  input  [31:0] io_instruction,
  input         io_prediction_failure_failure_occurs,
  input         io_prediction_failure_success_occurs,
  input  [31:0] io_prediction_failure_address,
  input  [31:0] io_pc,
  input  [31:0] io_new_pc,
  output [31:0] io_nextPC
);
  wire  predictor_clock; // @[NextPCGen.scala 21:25]
  wire  predictor_reset; // @[NextPCGen.scala 21:25]
  wire [1:0] predictor_io_cpu_state_type; // @[NextPCGen.scala 21:25]
  wire [31:0] predictor_io_pc; // @[NextPCGen.scala 21:25]
  wire [31:0] predictor_io_instruction; // @[NextPCGen.scala 21:25]
  wire  predictor_io_prediction_failure_failure_occurs; // @[NextPCGen.scala 21:25]
  wire  predictor_io_prediction_failure_success_occurs; // @[NextPCGen.scala 21:25]
  wire [31:0] predictor_io_prediction_failure_address; // @[NextPCGen.scala 21:25]
  wire [31:0] predictor_io_predict_new_pc; // @[NextPCGen.scala 21:25]
  wire [31:0] _GEN_0 = 2'h2 == io_nextPC_control_signal ? io_new_pc : io_pc; // @[NextPCGen.scala 27:12 28:35 36:16]
  wire [31:0] _GEN_1 = 2'h1 == io_nextPC_control_signal ? io_pc : _GEN_0; // @[NextPCGen.scala 28:35 33:16]
  PCPrediction predictor ( // @[NextPCGen.scala 21:25]
    .clock(predictor_clock),
    .reset(predictor_reset),
    .io_cpu_state_type(predictor_io_cpu_state_type),
    .io_pc(predictor_io_pc),
    .io_instruction(predictor_io_instruction),
    .io_prediction_failure_failure_occurs(predictor_io_prediction_failure_failure_occurs),
    .io_prediction_failure_success_occurs(predictor_io_prediction_failure_success_occurs),
    .io_prediction_failure_address(predictor_io_prediction_failure_address),
    .io_predict_new_pc(predictor_io_predict_new_pc)
  );
  assign io_nextPC = 2'h0 == io_nextPC_control_signal ? predictor_io_predict_new_pc : _GEN_1; // @[NextPCGen.scala 28:35 30:16]
  assign predictor_clock = clock;
  assign predictor_reset = reset;
  assign predictor_io_cpu_state_type = io_cpu_state_type; // @[NextPCGen.scala 22:30]
  assign predictor_io_pc = io_pc; // @[NextPCGen.scala 23:18]
  assign predictor_io_instruction = io_instruction; // @[NextPCGen.scala 24:27]
  assign predictor_io_prediction_failure_failure_occurs = io_prediction_failure_failure_occurs; // @[NextPCGen.scala 25:34]
  assign predictor_io_prediction_failure_success_occurs = io_prediction_failure_success_occurs; // @[NextPCGen.scala 25:34]
  assign predictor_io_prediction_failure_address = io_prediction_failure_address; // @[NextPCGen.scala 25:34]
endmodule
module IFID(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  input  [1:0]  io_signal,
  input  [31:0] io_in_pc,
  input  [31:0] io_in_ins,
  input  [1:0]  io_in_IF_fault_IF_fault_type,
  input  [31:0] io_in_IF_fault_mtval,
  output [31:0] io_out_pc,
  output [31:0] io_out_ins,
  output [1:0]  io_out_IF_fault_IF_fault_type,
  output [31:0] io_out_IF_fault_mtval
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] regs_pc; // @[IFID.scala 28:21]
  reg [31:0] regs_ins; // @[IFID.scala 28:21]
  reg [1:0] regs_IF_fault_IF_fault_type; // @[IFID.scala 28:21]
  reg [31:0] regs_IF_fault_mtval; // @[IFID.scala 28:21]
  wire [1:0] _GEN_5 = 2'h2 == io_signal ? 2'h0 : regs_IF_fault_IF_fault_type; // @[IFID.scala 31:23 45:14 28:21]
  wire [31:0] _GEN_6 = 2'h2 == io_signal ? 32'h13 : regs_ins; // @[IFID.scala 31:23 45:14 28:21]
  wire [31:0] _GEN_7 = 2'h2 == io_signal ? 32'h0 : regs_pc; // @[IFID.scala 31:23 45:14 28:21]
  assign io_out_pc = regs_pc; // @[IFID.scala 29:10]
  assign io_out_ins = regs_ins; // @[IFID.scala 29:10]
  assign io_out_IF_fault_IF_fault_type = regs_IF_fault_IF_fault_type; // @[IFID.scala 29:10]
  assign io_out_IF_fault_mtval = regs_IF_fault_mtval; // @[IFID.scala 29:10]
  always @(posedge clock) begin
    if (reset) begin // @[IFID.scala 28:21]
      regs_pc <= 32'h0; // @[IFID.scala 28:21]
    end else if (io_cpu_state == 2'h2) begin // @[IFID.scala 30:60]
      if (2'h0 == io_signal) begin // @[IFID.scala 31:23]
        regs_pc <= io_in_pc;
      end else if (!(2'h1 == io_signal)) begin // @[IFID.scala 31:23]
        regs_pc <= _GEN_7;
      end
    end
    if (reset) begin // @[IFID.scala 28:21]
      regs_ins <= 32'h13; // @[IFID.scala 28:21]
    end else if (io_cpu_state == 2'h2) begin // @[IFID.scala 30:60]
      if (2'h0 == io_signal) begin // @[IFID.scala 31:23]
        if (io_in_IF_fault_IF_fault_type != 2'h0) begin // @[IFID.scala 33:71]
          regs_ins <= 32'h13; // @[IFID.scala 34:16]
        end else begin
          regs_ins <= io_in_ins; // @[IFID.scala 38:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IFID.scala 31:23]
        regs_ins <= _GEN_6;
      end
    end
    if (reset) begin // @[IFID.scala 28:21]
      regs_IF_fault_IF_fault_type <= 2'h0; // @[IFID.scala 28:21]
    end else if (io_cpu_state == 2'h2) begin // @[IFID.scala 30:60]
      if (2'h0 == io_signal) begin // @[IFID.scala 31:23]
        regs_IF_fault_IF_fault_type <= io_in_IF_fault_IF_fault_type;
      end else if (!(2'h1 == io_signal)) begin // @[IFID.scala 31:23]
        regs_IF_fault_IF_fault_type <= _GEN_5;
      end
    end
    if (io_cpu_state == 2'h2) begin // @[IFID.scala 30:60]
      if (2'h0 == io_signal) begin // @[IFID.scala 31:23]
        regs_IF_fault_mtval <= io_in_IF_fault_mtval;
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  regs_pc = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  regs_ins = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  regs_IF_fault_IF_fault_type = _RAND_2[1:0];
  _RAND_3 = {1{`RANDOM}};
  regs_IF_fault_mtval = _RAND_3[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module InstructionDecoder(
  input  [31:0] io_instruction,
  output [6:0]  io_opcode,
  output [4:0]  io_rs1,
  output [4:0]  io_rs2,
  output [4:0]  io_rd,
  output [2:0]  io_func3,
  output [6:0]  io_func7,
  output [19:0] io_raw_imm,
  output [11:0] io_csr
);
  wire [19:0] _io_raw_imm_T_1 = {8'h0,io_instruction[31:20]}; // @[Cat.scala 33:92]
  wire [11:0] _io_raw_imm_T_10 = {io_instruction[31:25],io_instruction[11:7]}; // @[Cat.scala 33:92]
  wire [19:0] _io_raw_imm_T_15 = {8'h0,io_instruction[31],io_instruction[7],io_instruction[30:25],io_instruction[11:8]}; // @[Cat.scala 33:92]
  wire [19:0] _io_raw_imm_T_22 = {io_instruction[31],io_instruction[19:12],io_instruction[20],io_instruction[30:21]}; // @[Cat.scala 33:92]
  wire [19:0] _GEN_0 = 7'h6f == io_opcode ? _io_raw_imm_T_22 : 20'h0; // @[InstructionDecoder.scala 25:14 27:27 57:18]
  wire [19:0] _GEN_1 = 7'h17 == io_opcode ? io_instruction[31:12] : _GEN_0; // @[InstructionDecoder.scala 27:27 54:18]
  wire [19:0] _GEN_2 = 7'h37 == io_opcode ? io_instruction[31:12] : _GEN_1; // @[InstructionDecoder.scala 27:27 51:18]
  wire [19:0] _GEN_3 = 7'h63 == io_opcode ? _io_raw_imm_T_15 : _GEN_2; // @[InstructionDecoder.scala 27:27 47:18]
  wire [19:0] _GEN_4 = 7'h23 == io_opcode ? {{8'd0}, _io_raw_imm_T_10} : _GEN_3; // @[InstructionDecoder.scala 27:27 44:18]
  wire [19:0] _GEN_5 = 7'h73 == io_opcode ? _io_raw_imm_T_1 : _GEN_4; // @[InstructionDecoder.scala 27:27 41:18]
  wire [19:0] _GEN_6 = 7'h67 == io_opcode ? _io_raw_imm_T_1 : _GEN_5; // @[InstructionDecoder.scala 27:27 38:18]
  wire [19:0] _GEN_7 = 7'h3 == io_opcode ? _io_raw_imm_T_1 : _GEN_6; // @[InstructionDecoder.scala 27:27 35:18]
  wire [19:0] _GEN_8 = 7'h13 == io_opcode ? _io_raw_imm_T_1 : _GEN_7; // @[InstructionDecoder.scala 27:27 32:18]
  assign io_opcode = io_instruction[6:0]; // @[InstructionDecoder.scala 17:30]
  assign io_rs1 = io_instruction[19:15]; // @[InstructionDecoder.scala 20:27]
  assign io_rs2 = io_instruction[24:20]; // @[InstructionDecoder.scala 21:27]
  assign io_rd = io_instruction[11:7]; // @[InstructionDecoder.scala 18:26]
  assign io_func3 = io_instruction[14:12]; // @[InstructionDecoder.scala 19:29]
  assign io_func7 = io_instruction[31:25]; // @[InstructionDecoder.scala 22:29]
  assign io_raw_imm = 7'h33 == io_opcode ? 20'h0 : _GEN_8; // @[InstructionDecoder.scala 27:27 29:18]
  assign io_csr = io_instruction[31:20]; // @[InstructionDecoder.scala 23:27]
endmodule
module ControlUnit(
  input  [31:0] io_instruction,
  input  [6:0]  io_opcode,
  input  [2:0]  io_func3,
  input  [6:0]  io_func7,
  input  [4:0]  io_rs1,
  input  [4:0]  io_rs2,
  input  [4:0]  io_rd,
  input  [19:0] io_raw_imm,
  input  [11:0] io_csr,
  input  [1:0]  io_cur_privilege,
  output [4:0]  io_rs1_out,
  output [4:0]  io_rs2_out,
  output [4:0]  io_rd_out,
  output [19:0] io_raw_imm_out,
  output [11:0] io_csr_out,
  output [3:0]  io_alu_type,
  output [1:0]  io_cmp_type,
  output        io_unsigned,
  output        io_regs_write,
  output [1:0]  io_imm_width_type,
  output        io_operand2_type,
  output        io_au_type,
  output [2:0]  io_write_back_type,
  output        io_memory_read,
  output        io_memory_write,
  output [1:0]  io_data_width,
  output        io_csr_write,
  output        io_operand1_type,
  output        io_result_stage,
  output [1:0]  io_branch_type,
  output [2:0]  io_fault_ID_fault_type,
  output [31:0] io_fault_mtval
);
  wire [9:0] _T_1 = {io_func3,io_func7}; // @[Cat.scala 33:92]
  wire  _GEN_2 = 10'h100 == _T_1 | 10'h180 == _T_1; // @[ControlUnit.scala 120:39 146:22]
  wire  _GEN_4 = 10'h100 == _T_1 ? 1'h0 : 10'h180 == _T_1; // @[ControlUnit.scala 119:19 120:39]
  wire  _GEN_6 = 10'h2a0 == _T_1 ? 1'h0 : _GEN_2; // @[ControlUnit.scala 114:18 120:39]
  wire  _GEN_8 = 10'h2a0 == _T_1 ? 1'h0 : _GEN_4; // @[ControlUnit.scala 119:19 120:39]
  wire [2:0] _GEN_9 = 10'h280 == _T_1 ? 3'h6 : 3'h7; // @[ControlUnit.scala 120:39 140:23]
  wire  _GEN_10 = 10'h280 == _T_1 ? 1'h0 : _GEN_6; // @[ControlUnit.scala 114:18 120:39]
  wire  _GEN_12 = 10'h280 == _T_1 ? 1'h0 : _GEN_8; // @[ControlUnit.scala 119:19 120:39]
  wire [2:0] _GEN_13 = 10'h80 == _T_1 ? 3'h5 : _GEN_9; // @[ControlUnit.scala 120:39 137:23]
  wire  _GEN_14 = 10'h80 == _T_1 ? 1'h0 : _GEN_10; // @[ControlUnit.scala 114:18 120:39]
  wire  _GEN_16 = 10'h80 == _T_1 ? 1'h0 : _GEN_12; // @[ControlUnit.scala 119:19 120:39]
  wire [2:0] _GEN_17 = 10'h380 == _T_1 ? 3'h4 : _GEN_13; // @[ControlUnit.scala 120:39 134:23]
  wire  _GEN_18 = 10'h380 == _T_1 ? 1'h0 : _GEN_14; // @[ControlUnit.scala 114:18 120:39]
  wire  _GEN_20 = 10'h380 == _T_1 ? 1'h0 : _GEN_16; // @[ControlUnit.scala 119:19 120:39]
  wire [2:0] _GEN_21 = 10'h300 == _T_1 ? 3'h3 : _GEN_17; // @[ControlUnit.scala 120:39 131:23]
  wire  _GEN_22 = 10'h300 == _T_1 ? 1'h0 : _GEN_18; // @[ControlUnit.scala 114:18 120:39]
  wire  _GEN_24 = 10'h300 == _T_1 ? 1'h0 : _GEN_20; // @[ControlUnit.scala 119:19 120:39]
  wire [2:0] _GEN_25 = 10'h200 == _T_1 ? 3'h2 : _GEN_21; // @[ControlUnit.scala 120:39 128:23]
  wire  _GEN_26 = 10'h200 == _T_1 ? 1'h0 : _GEN_22; // @[ControlUnit.scala 114:18 120:39]
  wire  _GEN_28 = 10'h200 == _T_1 ? 1'h0 : _GEN_24; // @[ControlUnit.scala 119:19 120:39]
  wire [2:0] _GEN_29 = 10'h20 == _T_1 ? 3'h1 : _GEN_25; // @[ControlUnit.scala 120:39 125:23]
  wire  _GEN_30 = 10'h20 == _T_1 ? 1'h0 : _GEN_26; // @[ControlUnit.scala 114:18 120:39]
  wire  _GEN_32 = 10'h20 == _T_1 ? 1'h0 : _GEN_28; // @[ControlUnit.scala 119:19 120:39]
  wire [2:0] _GEN_33 = 10'h0 == _T_1 ? 3'h0 : _GEN_29; // @[ControlUnit.scala 120:39 122:23]
  wire  _GEN_34 = 10'h0 == _T_1 ? 1'h0 : _GEN_30; // @[ControlUnit.scala 114:18 120:39]
  wire  _GEN_36 = 10'h0 == _T_1 ? 1'h0 : _GEN_32; // @[ControlUnit.scala 119:19 120:39]
  wire  _T_13 = 3'h0 == io_func3; // @[ControlUnit.scala 171:24]
  wire  _T_14 = 3'h4 == io_func3; // @[ControlUnit.scala 171:24]
  wire  _T_15 = 3'h6 == io_func3; // @[ControlUnit.scala 171:24]
  wire  _T_16 = 3'h7 == io_func3; // @[ControlUnit.scala 171:24]
  wire  _T_17 = 3'h1 == io_func3; // @[ControlUnit.scala 171:24]
  wire  _T_18 = 3'h5 == io_func3; // @[ControlUnit.scala 171:24]
  wire [2:0] _GEN_38 = 7'h0 == io_func7 ? 3'h6 : 3'h7; // @[ControlUnit.scala 188:28 190:27]
  wire  _T_21 = 3'h2 == io_func3; // @[ControlUnit.scala 171:24]
  wire  _T_22 = 3'h3 == io_func3; // @[ControlUnit.scala 171:24]
  wire  _GEN_41 = 3'h2 == io_func3 | 3'h3 == io_func3; // @[ControlUnit.scala 171:24 198:22]
  wire  _GEN_43 = 3'h2 == io_func3 ? 1'h0 : 3'h3 == io_func3; // @[ControlUnit.scala 170:19 171:24]
  wire  _GEN_45 = 3'h5 == io_func3 ? 1'h0 : _GEN_41; // @[ControlUnit.scala 164:18 171:24]
  wire  _GEN_47 = 3'h5 == io_func3 ? 1'h0 : _GEN_43; // @[ControlUnit.scala 170:19 171:24]
  wire [2:0] _GEN_48 = 3'h1 == io_func3 ? 3'h5 : _GEN_38; // @[ControlUnit.scala 171:24 185:23]
  wire  _GEN_49 = 3'h1 == io_func3 ? 1'h0 : _GEN_45; // @[ControlUnit.scala 164:18 171:24]
  wire  _GEN_51 = 3'h1 == io_func3 ? 1'h0 : _GEN_47; // @[ControlUnit.scala 170:19 171:24]
  wire [2:0] _GEN_52 = 3'h7 == io_func3 ? 3'h4 : _GEN_48; // @[ControlUnit.scala 171:24 182:23]
  wire  _GEN_53 = 3'h7 == io_func3 ? 1'h0 : _GEN_49; // @[ControlUnit.scala 164:18 171:24]
  wire  _GEN_55 = 3'h7 == io_func3 ? 1'h0 : _GEN_51; // @[ControlUnit.scala 170:19 171:24]
  wire [2:0] _GEN_56 = 3'h6 == io_func3 ? 3'h3 : _GEN_52; // @[ControlUnit.scala 171:24 179:23]
  wire  _GEN_57 = 3'h6 == io_func3 ? 1'h0 : _GEN_53; // @[ControlUnit.scala 164:18 171:24]
  wire  _GEN_59 = 3'h6 == io_func3 ? 1'h0 : _GEN_55; // @[ControlUnit.scala 170:19 171:24]
  wire [2:0] _GEN_60 = 3'h4 == io_func3 ? 3'h2 : _GEN_56; // @[ControlUnit.scala 171:24 176:23]
  wire  _GEN_61 = 3'h4 == io_func3 ? 1'h0 : _GEN_57; // @[ControlUnit.scala 164:18 171:24]
  wire  _GEN_63 = 3'h4 == io_func3 ? 1'h0 : _GEN_59; // @[ControlUnit.scala 170:19 171:24]
  wire [2:0] _GEN_64 = 3'h0 == io_func3 ? 3'h0 : _GEN_60; // @[ControlUnit.scala 171:24 173:23]
  wire  _GEN_65 = 3'h0 == io_func3 ? 1'h0 : _GEN_61; // @[ControlUnit.scala 164:18 171:24]
  wire  _GEN_67 = 3'h0 == io_func3 ? 1'h0 : _GEN_63; // @[ControlUnit.scala 170:19 171:24]
  wire  _GEN_70 = _T_14 ? 1'h0 : 1'h1; // @[ControlUnit.scala 225:24 236:25]
  wire  _GEN_71 = _T_14 | _T_18; // @[ControlUnit.scala 225:24 237:23]
  wire [1:0] _GEN_72 = _T_21 ? 2'h2 : {{1'd0}, _GEN_70}; // @[ControlUnit.scala 225:24 233:25]
  wire  _GEN_73 = _T_21 ? 1'h0 : _GEN_71; // @[ControlUnit.scala 223:19 225:24]
  wire [1:0] _GEN_74 = _T_17 ? 2'h1 : _GEN_72; // @[ControlUnit.scala 225:24 230:25]
  wire  _GEN_75 = _T_17 ? 1'h0 : _GEN_73; // @[ControlUnit.scala 223:19 225:24]
  wire [1:0] _GEN_76 = _T_13 ? 2'h0 : _GEN_74; // @[ControlUnit.scala 225:24 227:25]
  wire  _GEN_77 = _T_13 ? 1'h0 : _GEN_75; // @[ControlUnit.scala 223:19 225:24]
  wire [1:0] _GEN_79 = _T_17 ? 2'h1 : 2'h2; // @[ControlUnit.scala 262:24 267:25]
  wire [1:0] _GEN_80 = _T_13 ? 2'h0 : _GEN_79; // @[ControlUnit.scala 262:24 264:25]
  wire  _GEN_83 = _T_15 | _T_16; // @[ControlUnit.scala 291:24 305:23]
  wire  _GEN_84 = _T_15 ? 1'h0 : 1'h1; // @[ControlUnit.scala 291:24 306:23]
  wire  _GEN_85 = _T_18 | _GEN_84; // @[ControlUnit.scala 291:24 302:23]
  wire  _GEN_86 = _T_18 ? 1'h0 : _GEN_83; // @[ControlUnit.scala 288:19 291:24]
  wire  _GEN_87 = _T_14 ? 1'h0 : _GEN_85; // @[ControlUnit.scala 291:24 299:23]
  wire  _GEN_88 = _T_14 ? 1'h0 : _GEN_86; // @[ControlUnit.scala 288:19 291:24]
  wire [1:0] _GEN_89 = _T_17 ? 2'h3 : {{1'd0}, _GEN_87}; // @[ControlUnit.scala 291:24 296:23]
  wire  _GEN_90 = _T_17 ? 1'h0 : _GEN_88; // @[ControlUnit.scala 288:19 291:24]
  wire [1:0] _GEN_91 = _T_13 ? 2'h2 : _GEN_89; // @[ControlUnit.scala 291:24 293:23]
  wire  _GEN_92 = _T_13 ? 1'h0 : _GEN_90; // @[ControlUnit.scala 288:19 291:24]
  wire  _T_46 = io_cur_privilege == 2'h0; // @[ControlUnit.scala 403:33]
  wire [31:0] _GEN_94 = io_cur_privilege == 2'h0 ? io_instruction : 32'h0; // @[ControlUnit.scala 403:46 405:28 55:18]
  wire [31:0] _io_raw_imm_out_T = {27'h0,io_rs1}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_109 = _T_46 ? {{12'd0}, io_raw_imm} : _io_raw_imm_out_T; // @[ControlUnit.scala 435:46 61:18 441:27]
  wire [2:0] _GEN_121 = _T_46 ? 3'h1 : 3'h4; // @[ControlUnit.scala 478:50 479:40 482:40]
  wire [2:0] _GEN_123 = 20'h302 == io_raw_imm ? _GEN_121 : 3'h0; // @[ControlUnit.scala 470:30 54:26]
  wire [31:0] _GEN_124 = 20'h302 == io_raw_imm ? _GEN_94 : 32'h0; // @[ControlUnit.scala 470:30 55:18]
  wire [2:0] _GEN_125 = 20'h1 == io_raw_imm ? 3'h2 : _GEN_123; // @[ControlUnit.scala 470:30 475:38]
  wire [31:0] _GEN_126 = 20'h1 == io_raw_imm ? 32'h0 : _GEN_124; // @[ControlUnit.scala 470:30 55:18]
  wire [2:0] _GEN_127 = 20'h0 == io_raw_imm ? 3'h3 : _GEN_125; // @[ControlUnit.scala 470:30 472:38]
  wire [31:0] _GEN_128 = 20'h0 == io_raw_imm ? 32'h0 : _GEN_126; // @[ControlUnit.scala 470:30 55:18]
  wire [4:0] _GEN_129 = _T_13 ? 5'h0 : io_rs1; // @[ControlUnit.scala 400:24 58:14 81:17]
  wire [11:0] _GEN_130 = _T_13 ? 12'h0 : io_csr; // @[ControlUnit.scala 400:24 57:14 87:17]
  wire [4:0] _GEN_131 = _T_13 ? 5'h0 : io_rd; // @[ControlUnit.scala 400:24 60:13 90:16]
  wire [2:0] _GEN_132 = _T_13 ? _GEN_127 : 3'h0; // @[ControlUnit.scala 400:24 54:26]
  wire [31:0] _GEN_133 = _T_13 ? _GEN_128 : 32'h0; // @[ControlUnit.scala 400:24 55:18]
  wire [4:0] _GEN_134 = _T_16 ? 5'h0 : _GEN_129; // @[ControlUnit.scala 400:24 81:17]
  wire [2:0] _GEN_135 = _T_16 ? {{2'd0}, _T_46} : _GEN_132; // @[ControlUnit.scala 400:24]
  wire [31:0] _GEN_136 = _T_16 ? _GEN_94 : _GEN_133; // @[ControlUnit.scala 400:24]
  wire [31:0] _GEN_137 = _T_16 ? _GEN_109 : {{12'd0}, io_raw_imm}; // @[ControlUnit.scala 400:24 61:18]
  wire [11:0] _GEN_140 = _T_16 ? io_csr : _GEN_130; // @[ControlUnit.scala 400:24 57:14]
  wire [4:0] _GEN_141 = _T_16 ? io_rd : _GEN_131; // @[ControlUnit.scala 400:24 60:13]
  wire [4:0] _GEN_142 = _T_15 ? 5'h0 : _GEN_134; // @[ControlUnit.scala 400:24 81:17]
  wire [2:0] _GEN_143 = _T_15 ? {{2'd0}, _T_46} : _GEN_135; // @[ControlUnit.scala 400:24]
  wire [31:0] _GEN_144 = _T_15 ? _GEN_94 : _GEN_136; // @[ControlUnit.scala 400:24]
  wire [31:0] _GEN_145 = _T_15 ? _GEN_109 : _GEN_137; // @[ControlUnit.scala 400:24]
  wire [3:0] _GEN_146 = _T_15 ? 4'h3 : 4'h8; // @[ControlUnit.scala 400:24]
  wire [11:0] _GEN_148 = _T_15 ? io_csr : _GEN_140; // @[ControlUnit.scala 400:24 57:14]
  wire [4:0] _GEN_149 = _T_15 ? io_rd : _GEN_141; // @[ControlUnit.scala 400:24 60:13]
  wire [4:0] _GEN_150 = _T_18 ? 5'h0 : _GEN_142; // @[ControlUnit.scala 400:24]
  wire [2:0] _GEN_152 = _T_18 ? {{2'd0}, _T_46} : _GEN_143; // @[ControlUnit.scala 400:24]
  wire [31:0] _GEN_153 = _T_18 ? _GEN_94 : _GEN_144; // @[ControlUnit.scala 400:24]
  wire  _GEN_154 = _T_18 ? _T_46 : 1'h1; // @[ControlUnit.scala 397:24 400:24]
  wire [31:0] _GEN_155 = _T_18 ? _GEN_109 : _GEN_145; // @[ControlUnit.scala 400:24]
  wire [3:0] _GEN_156 = _T_18 ? 4'h0 : _GEN_146; // @[ControlUnit.scala 400:24]
  wire [11:0] _GEN_158 = _T_18 ? io_csr : _GEN_148; // @[ControlUnit.scala 400:24 57:14]
  wire [4:0] _GEN_159 = _T_18 ? io_rd : _GEN_149; // @[ControlUnit.scala 400:24 60:13]
  wire [4:0] _GEN_160 = _T_22 ? 5'h0 : _GEN_150; // @[ControlUnit.scala 400:24 81:17]
  wire [2:0] _GEN_161 = _T_22 ? {{2'd0}, _T_46} : _GEN_152; // @[ControlUnit.scala 400:24]
  wire [31:0] _GEN_162 = _T_22 ? _GEN_94 : _GEN_153; // @[ControlUnit.scala 400:24]
  wire [3:0] _GEN_163 = _T_22 ? 4'h8 : _GEN_156; // @[ControlUnit.scala 400:24]
  wire [4:0] _GEN_165 = _T_22 ? io_rs1 : _GEN_150; // @[ControlUnit.scala 398:18 400:24]
  wire [31:0] _GEN_167 = _T_22 ? {{12'd0}, io_raw_imm} : _GEN_155; // @[ControlUnit.scala 400:24 61:18]
  wire [11:0] _GEN_168 = _T_22 ? io_csr : _GEN_158; // @[ControlUnit.scala 400:24 57:14]
  wire [4:0] _GEN_169 = _T_22 ? io_rd : _GEN_159; // @[ControlUnit.scala 400:24 60:13]
  wire [4:0] _GEN_170 = _T_21 ? 5'h0 : _GEN_160; // @[ControlUnit.scala 400:24 81:17]
  wire [2:0] _GEN_171 = _T_21 ? {{2'd0}, _T_46} : _GEN_161; // @[ControlUnit.scala 400:24]
  wire [31:0] _GEN_172 = _T_21 ? _GEN_94 : _GEN_162; // @[ControlUnit.scala 400:24]
  wire [3:0] _GEN_173 = _T_21 ? 4'h3 : _GEN_163; // @[ControlUnit.scala 400:24]
  wire [4:0] _GEN_175 = _T_21 ? io_rs1 : _GEN_165; // @[ControlUnit.scala 398:18 400:24]
  wire  _GEN_176 = _T_21 | (_T_22 | _GEN_154); // @[ControlUnit.scala 397:24 400:24]
  wire [31:0] _GEN_177 = _T_21 ? {{12'd0}, io_raw_imm} : _GEN_167; // @[ControlUnit.scala 400:24 61:18]
  wire [11:0] _GEN_178 = _T_21 ? io_csr : _GEN_168; // @[ControlUnit.scala 400:24 57:14]
  wire [4:0] _GEN_179 = _T_21 ? io_rd : _GEN_169; // @[ControlUnit.scala 400:24 60:13]
  wire [4:0] _GEN_180 = _T_17 ? 5'h0 : _GEN_170; // @[ControlUnit.scala 400:24]
  wire [2:0] _GEN_181 = _T_17 ? {{2'd0}, _T_46} : _GEN_171; // @[ControlUnit.scala 400:24]
  wire [31:0] _GEN_182 = _T_17 ? _GEN_94 : _GEN_172; // @[ControlUnit.scala 400:24]
  wire  _GEN_183 = _T_17 ? _T_46 : _GEN_176; // @[ControlUnit.scala 400:24]
  wire [3:0] _GEN_184 = _T_17 ? 4'h0 : _GEN_173; // @[ControlUnit.scala 400:24]
  wire  _GEN_185 = _T_17 | _GEN_41; // @[ControlUnit.scala 400:24]
  wire [4:0] _GEN_186 = _T_17 ? io_rs1 : _GEN_175; // @[ControlUnit.scala 398:18 400:24]
  wire [31:0] _GEN_187 = _T_17 ? {{12'd0}, io_raw_imm} : _GEN_177; // @[ControlUnit.scala 400:24 61:18]
  wire [11:0] _GEN_188 = _T_17 ? io_csr : _GEN_178; // @[ControlUnit.scala 400:24 57:14]
  wire [4:0] _GEN_189 = _T_17 ? io_rd : _GEN_179; // @[ControlUnit.scala 400:24 60:13]
  wire [4:0] _GEN_196 = 7'h73 == io_opcode ? _GEN_186 : io_rs2; // @[ControlUnit.scala 105:21 59:14]
  wire [4:0] _GEN_197 = 7'h73 == io_opcode ? _GEN_180 : io_rs1; // @[ControlUnit.scala 105:21 58:14]
  wire [2:0] _GEN_198 = 7'h73 == io_opcode ? _GEN_181 : 3'h0; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_199 = 7'h73 == io_opcode ? _GEN_182 : 32'h0; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_202 = 7'h73 == io_opcode ? _GEN_187 : {{12'd0}, io_raw_imm}; // @[ControlUnit.scala 105:21 61:18]
  wire [11:0] _GEN_203 = 7'h73 == io_opcode ? _GEN_188 : io_csr; // @[ControlUnit.scala 105:21 57:14]
  wire [4:0] _GEN_204 = 7'h73 == io_opcode ? _GEN_189 : io_rd; // @[ControlUnit.scala 105:21 60:13]
  wire [4:0] _GEN_205 = 7'h17 == io_opcode ? 5'h0 : _GEN_197; // @[ControlUnit.scala 105:21 81:17]
  wire [4:0] _GEN_206 = 7'h17 == io_opcode ? 5'h0 : _GEN_196; // @[ControlUnit.scala 105:21 84:17]
  wire [11:0] _GEN_207 = 7'h17 == io_opcode ? 12'h0 : _GEN_203; // @[ControlUnit.scala 105:21 87:17]
  wire [1:0] _GEN_212 = 7'h17 == io_opcode ? 2'h3 : 2'h0; // @[ControlUnit.scala 105:21 378:25]
  wire [2:0] _GEN_213 = 7'h17 == io_opcode ? 3'h4 : 3'h5; // @[ControlUnit.scala 105:21 379:26]
  wire  _GEN_214 = 7'h17 == io_opcode ? 1'h0 : 1'h1; // @[ControlUnit.scala 105:21 380:19]
  wire  _GEN_217 = 7'h17 == io_opcode ? 1'h0 : 7'h73 == io_opcode; // @[ControlUnit.scala 100:16 105:21]
  wire  _GEN_218 = 7'h17 == io_opcode ? 1'h0 : 7'h73 == io_opcode & _GEN_183; // @[ControlUnit.scala 101:20 105:21]
  wire [2:0] _GEN_219 = 7'h17 == io_opcode ? 3'h0 : _GEN_198; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_220 = 7'h17 == io_opcode ? 32'h0 : _GEN_199; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_222 = 7'h17 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_202; // @[ControlUnit.scala 105:21 61:18]
  wire [4:0] _GEN_223 = 7'h17 == io_opcode ? io_rd : _GEN_204; // @[ControlUnit.scala 105:21 60:13]
  wire [4:0] _GEN_224 = 7'h37 == io_opcode ? 5'h0 : _GEN_205; // @[ControlUnit.scala 105:21 81:17]
  wire [4:0] _GEN_225 = 7'h37 == io_opcode ? 5'h0 : _GEN_206; // @[ControlUnit.scala 105:21 84:17]
  wire [11:0] _GEN_226 = 7'h37 == io_opcode ? 12'h0 : _GEN_207; // @[ControlUnit.scala 105:21 87:17]
  wire [1:0] _GEN_231 = 7'h37 == io_opcode ? 2'h3 : _GEN_212; // @[ControlUnit.scala 105:21 361:25]
  wire [2:0] _GEN_232 = 7'h37 == io_opcode ? 3'h3 : _GEN_213; // @[ControlUnit.scala 105:21 362:26]
  wire  _GEN_233 = 7'h37 == io_opcode ? 1'h0 : _GEN_214; // @[ControlUnit.scala 105:21 363:19]
  wire  _GEN_236 = 7'h37 == io_opcode ? 1'h0 : _GEN_217; // @[ControlUnit.scala 100:16 105:21]
  wire  _GEN_237 = 7'h37 == io_opcode ? 1'h0 : _GEN_218; // @[ControlUnit.scala 101:20 105:21]
  wire [2:0] _GEN_238 = 7'h37 == io_opcode ? 3'h0 : _GEN_219; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_239 = 7'h37 == io_opcode ? 32'h0 : _GEN_220; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_241 = 7'h37 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_222; // @[ControlUnit.scala 105:21 61:18]
  wire [4:0] _GEN_242 = 7'h37 == io_opcode ? io_rd : _GEN_223; // @[ControlUnit.scala 105:21 60:13]
  wire [4:0] _GEN_243 = 7'h67 == io_opcode ? 5'h0 : _GEN_225; // @[ControlUnit.scala 105:21 84:17]
  wire [11:0] _GEN_244 = 7'h67 == io_opcode ? 12'h0 : _GEN_226; // @[ControlUnit.scala 105:21 87:17]
  wire [1:0] _GEN_246 = 7'h67 == io_opcode ? 2'h2 : 2'h0; // @[ControlUnit.scala 105:21 97:20]
  wire  _GEN_250 = 7'h67 == io_opcode ? 1'h0 : _GEN_185; // @[ControlUnit.scala 105:21 343:24]
  wire [2:0] _GEN_251 = 7'h67 == io_opcode ? 3'h2 : _GEN_232; // @[ControlUnit.scala 105:21 344:26]
  wire [1:0] _GEN_252 = 7'h67 == io_opcode ? 2'h0 : _GEN_231; // @[ControlUnit.scala 105:21 345:25]
  wire  _GEN_253 = 7'h67 == io_opcode ? 1'h0 : _GEN_233; // @[ControlUnit.scala 105:21 346:19]
  wire [3:0] _GEN_255 = 7'h67 == io_opcode ? 4'h0 : _GEN_184; // @[ControlUnit.scala 105:21 348:19]
  wire [4:0] _GEN_256 = 7'h67 == io_opcode ? io_rs1 : _GEN_224; // @[ControlUnit.scala 105:21 58:14]
  wire  _GEN_257 = 7'h67 == io_opcode ? 1'h0 : _GEN_236; // @[ControlUnit.scala 100:16 105:21]
  wire  _GEN_258 = 7'h67 == io_opcode ? 1'h0 : _GEN_237; // @[ControlUnit.scala 101:20 105:21]
  wire [2:0] _GEN_259 = 7'h67 == io_opcode ? 3'h0 : _GEN_238; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_260 = 7'h67 == io_opcode ? 32'h0 : _GEN_239; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_261 = 7'h67 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_241; // @[ControlUnit.scala 105:21 61:18]
  wire [4:0] _GEN_262 = 7'h67 == io_opcode ? io_rd : _GEN_242; // @[ControlUnit.scala 105:21 60:13]
  wire [4:0] _GEN_263 = 7'h6f == io_opcode ? 5'h0 : _GEN_256; // @[ControlUnit.scala 105:21 81:17]
  wire [4:0] _GEN_264 = 7'h6f == io_opcode ? 5'h0 : _GEN_243; // @[ControlUnit.scala 105:21 84:17]
  wire [11:0] _GEN_265 = 7'h6f == io_opcode ? 12'h0 : _GEN_244; // @[ControlUnit.scala 105:21 87:17]
  wire [1:0] _GEN_267 = 7'h6f == io_opcode ? 2'h0 : _GEN_246; // @[ControlUnit.scala 105:21 97:20]
  wire  _GEN_272 = 7'h6f == io_opcode ? 1'h0 : _GEN_250; // @[ControlUnit.scala 105:21 326:24]
  wire [2:0] _GEN_273 = 7'h6f == io_opcode ? 3'h2 : _GEN_251; // @[ControlUnit.scala 105:21 327:26]
  wire [1:0] _GEN_274 = 7'h6f == io_opcode ? 2'h1 : _GEN_252; // @[ControlUnit.scala 105:21 328:25]
  wire  _GEN_275 = 7'h6f == io_opcode ? 1'h0 : _GEN_253; // @[ControlUnit.scala 105:21 329:19]
  wire [3:0] _GEN_276 = 7'h6f == io_opcode ? 4'h0 : _GEN_255; // @[ControlUnit.scala 105:21 330:19]
  wire  _GEN_277 = 7'h6f == io_opcode ? 1'h0 : _GEN_257; // @[ControlUnit.scala 100:16 105:21]
  wire  _GEN_278 = 7'h6f == io_opcode ? 1'h0 : _GEN_258; // @[ControlUnit.scala 101:20 105:21]
  wire [2:0] _GEN_279 = 7'h6f == io_opcode ? 3'h0 : _GEN_259; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_280 = 7'h6f == io_opcode ? 32'h0 : _GEN_260; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_281 = 7'h6f == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_261; // @[ControlUnit.scala 105:21 61:18]
  wire [4:0] _GEN_282 = 7'h6f == io_opcode ? io_rd : _GEN_262; // @[ControlUnit.scala 105:21 60:13]
  wire [11:0] _GEN_283 = 7'h63 == io_opcode ? 12'h0 : _GEN_265; // @[ControlUnit.scala 105:21 87:17]
  wire [4:0] _GEN_284 = 7'h63 == io_opcode ? 5'h0 : _GEN_282; // @[ControlUnit.scala 105:21 90:16]
  wire [1:0] _GEN_286 = 7'h63 == io_opcode ? 2'h1 : _GEN_267; // @[ControlUnit.scala 105:21 97:20]
  wire  _GEN_288 = 7'h63 == io_opcode ? 1'h0 : 1'h1; // @[ControlUnit.scala 105:21 282:21]
  wire  _GEN_291 = 7'h63 == io_opcode | _GEN_272; // @[ControlUnit.scala 105:21 286:24]
  wire  _GEN_293 = 7'h63 == io_opcode ? _GEN_92 : _GEN_275; // @[ControlUnit.scala 105:21]
  wire [1:0] _GEN_294 = 7'h63 == io_opcode ? 2'h1 : _GEN_274; // @[ControlUnit.scala 105:21 289:25]
  wire [4:0] _GEN_297 = 7'h63 == io_opcode ? io_rs1 : _GEN_263; // @[ControlUnit.scala 105:21 58:14]
  wire [4:0] _GEN_298 = 7'h63 == io_opcode ? io_rs2 : _GEN_264; // @[ControlUnit.scala 105:21 59:14]
  wire  _GEN_299 = 7'h63 == io_opcode ? 1'h0 : _GEN_277; // @[ControlUnit.scala 100:16 105:21]
  wire  _GEN_300 = 7'h63 == io_opcode ? 1'h0 : _GEN_278; // @[ControlUnit.scala 101:20 105:21]
  wire [2:0] _GEN_301 = 7'h63 == io_opcode ? 3'h0 : _GEN_279; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_302 = 7'h63 == io_opcode ? 32'h0 : _GEN_280; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_303 = 7'h63 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_281; // @[ControlUnit.scala 105:21 61:18]
  wire [11:0] _GEN_304 = 7'h23 == io_opcode ? 12'h0 : _GEN_283; // @[ControlUnit.scala 105:21 87:17]
  wire [4:0] _GEN_305 = 7'h23 == io_opcode ? 5'h0 : _GEN_284; // @[ControlUnit.scala 105:21 90:16]
  wire [1:0] _GEN_307 = 7'h23 == io_opcode ? 2'h0 : _GEN_286; // @[ControlUnit.scala 105:21 97:20]
  wire  _GEN_309 = 7'h23 == io_opcode ? 1'h0 : _GEN_288; // @[ControlUnit.scala 105:21 253:21]
  wire  _GEN_310 = 7'h23 == io_opcode ? 1'h0 : 7'h63 == io_opcode; // @[ControlUnit.scala 105:21 254:18]
  wire  _GEN_313 = 7'h23 == io_opcode ? 1'h0 : _GEN_291; // @[ControlUnit.scala 105:21 257:24]
  wire [1:0] _GEN_315 = 7'h23 == io_opcode ? 2'h0 : _GEN_294; // @[ControlUnit.scala 105:21 259:25]
  wire  _GEN_316 = 7'h23 == io_opcode ? 1'h0 : _GEN_293; // @[ControlUnit.scala 105:21 260:19]
  wire [3:0] _GEN_317 = 7'h23 == io_opcode ? 4'h0 : _GEN_276; // @[ControlUnit.scala 105:21 261:19]
  wire [4:0] _GEN_320 = 7'h23 == io_opcode ? io_rs1 : _GEN_297; // @[ControlUnit.scala 105:21 58:14]
  wire [4:0] _GEN_321 = 7'h23 == io_opcode ? io_rs2 : _GEN_298; // @[ControlUnit.scala 105:21 59:14]
  wire  _GEN_322 = 7'h23 == io_opcode ? 1'h0 : _GEN_299; // @[ControlUnit.scala 100:16 105:21]
  wire  _GEN_323 = 7'h23 == io_opcode ? 1'h0 : _GEN_300; // @[ControlUnit.scala 101:20 105:21]
  wire [2:0] _GEN_324 = 7'h23 == io_opcode ? 3'h0 : _GEN_301; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_325 = 7'h23 == io_opcode ? 32'h0 : _GEN_302; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_326 = 7'h23 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_303; // @[ControlUnit.scala 105:21 61:18]
  wire [4:0] _GEN_327 = 7'h3 == io_opcode ? 5'h0 : _GEN_321; // @[ControlUnit.scala 105:21 84:17]
  wire [11:0] _GEN_328 = 7'h3 == io_opcode ? 12'h0 : _GEN_304; // @[ControlUnit.scala 105:21 87:17]
  wire [1:0] _GEN_330 = 7'h3 == io_opcode ? 2'h0 : _GEN_307; // @[ControlUnit.scala 105:21 97:20]
  wire  _GEN_332 = 7'h3 == io_opcode | _GEN_309; // @[ControlUnit.scala 105:21 216:21]
  wire  _GEN_333 = 7'h3 == io_opcode ? 1'h0 : _GEN_310; // @[ControlUnit.scala 105:21 217:18]
  wire  _GEN_335 = 7'h3 == io_opcode ? 1'h0 : 7'h23 == io_opcode; // @[ControlUnit.scala 105:21 219:23]
  wire  _GEN_336 = 7'h3 == io_opcode ? 1'h0 : _GEN_313; // @[ControlUnit.scala 105:21 220:24]
  wire [2:0] _GEN_337 = 7'h3 == io_opcode ? 3'h1 : _GEN_273; // @[ControlUnit.scala 105:21 221:26]
  wire [1:0] _GEN_338 = 7'h3 == io_opcode ? 2'h0 : _GEN_315; // @[ControlUnit.scala 105:21 222:25]
  wire  _GEN_339 = 7'h3 == io_opcode ? _GEN_77 : _GEN_316; // @[ControlUnit.scala 105:21]
  wire [3:0] _GEN_340 = 7'h3 == io_opcode ? 4'h0 : _GEN_317; // @[ControlUnit.scala 105:21 224:19]
  wire [4:0] _GEN_342 = 7'h3 == io_opcode ? io_rd : _GEN_305; // @[ControlUnit.scala 105:21 60:13]
  wire [4:0] _GEN_344 = 7'h3 == io_opcode ? io_rs1 : _GEN_320; // @[ControlUnit.scala 105:21 58:14]
  wire  _GEN_345 = 7'h3 == io_opcode ? 1'h0 : _GEN_322; // @[ControlUnit.scala 100:16 105:21]
  wire  _GEN_346 = 7'h3 == io_opcode ? 1'h0 : _GEN_323; // @[ControlUnit.scala 101:20 105:21]
  wire [2:0] _GEN_347 = 7'h3 == io_opcode ? 3'h0 : _GEN_324; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_348 = 7'h3 == io_opcode ? 32'h0 : _GEN_325; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_349 = 7'h3 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_326; // @[ControlUnit.scala 105:21 61:18]
  wire [4:0] _GEN_350 = 7'h13 == io_opcode ? 5'h0 : _GEN_327; // @[ControlUnit.scala 105:21 84:17]
  wire [11:0] _GEN_351 = 7'h13 == io_opcode ? 12'h0 : _GEN_328; // @[ControlUnit.scala 105:21 87:17]
  wire  _GEN_352 = 7'h13 == io_opcode ? 1'h0 : 7'h3 == io_opcode; // @[ControlUnit.scala 105:21 96:21]
  wire [1:0] _GEN_353 = 7'h13 == io_opcode ? 2'h0 : _GEN_330; // @[ControlUnit.scala 105:21 97:20]
  wire  _GEN_355 = 7'h13 == io_opcode | _GEN_332; // @[ControlUnit.scala 105:21 163:21]
  wire  _GEN_356 = 7'h13 == io_opcode ? _GEN_65 : _GEN_333; // @[ControlUnit.scala 105:21]
  wire  _GEN_358 = 7'h13 == io_opcode ? 1'h0 : _GEN_335; // @[ControlUnit.scala 105:21 166:23]
  wire  _GEN_359 = 7'h13 == io_opcode ? 1'h0 : _GEN_336; // @[ControlUnit.scala 105:21 167:24]
  wire [2:0] _GEN_361 = 7'h13 == io_opcode ? 3'h0 : _GEN_337; // @[ControlUnit.scala 105:21 169:26]
  wire  _GEN_362 = 7'h13 == io_opcode ? _GEN_67 : _GEN_339; // @[ControlUnit.scala 105:21]
  wire [3:0] _GEN_363 = 7'h13 == io_opcode ? {{1'd0}, _GEN_64} : _GEN_340; // @[ControlUnit.scala 105:21]
  wire [1:0] _GEN_364 = 7'h13 == io_opcode ? 2'h0 : _GEN_91; // @[ControlUnit.scala 105:21]
  wire [4:0] _GEN_366 = 7'h13 == io_opcode ? io_rd : _GEN_342; // @[ControlUnit.scala 105:21 60:13]
  wire [4:0] _GEN_367 = 7'h13 == io_opcode ? io_rs1 : _GEN_344; // @[ControlUnit.scala 105:21 58:14]
  wire  _GEN_368 = 7'h13 == io_opcode ? 1'h0 : _GEN_345; // @[ControlUnit.scala 100:16 105:21]
  wire  _GEN_369 = 7'h13 == io_opcode ? 1'h0 : _GEN_346; // @[ControlUnit.scala 101:20 105:21]
  wire [2:0] _GEN_370 = 7'h13 == io_opcode ? 3'h0 : _GEN_347; // @[ControlUnit.scala 105:21 54:26]
  wire [31:0] _GEN_371 = 7'h13 == io_opcode ? 32'h0 : _GEN_348; // @[ControlUnit.scala 105:21 55:18]
  wire [31:0] _GEN_372 = 7'h13 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_349; // @[ControlUnit.scala 105:21 61:18]
  wire [31:0] _GEN_395 = 7'h33 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_372; // @[ControlUnit.scala 105:21 61:18]
  assign io_rs1_out = 7'h33 == io_opcode ? io_rs1 : _GEN_367; // @[ControlUnit.scala 105:21 58:14]
  assign io_rs2_out = 7'h33 == io_opcode ? io_rs2 : _GEN_350; // @[ControlUnit.scala 105:21 59:14]
  assign io_rd_out = 7'h33 == io_opcode ? io_rd : _GEN_366; // @[ControlUnit.scala 105:21 60:13]
  assign io_raw_imm_out = _GEN_395[19:0];
  assign io_csr_out = 7'h33 == io_opcode ? 12'h0 : _GEN_351; // @[ControlUnit.scala 105:21 87:17]
  assign io_alu_type = 7'h33 == io_opcode ? {{1'd0}, _GEN_33} : _GEN_363; // @[ControlUnit.scala 105:21]
  assign io_cmp_type = 7'h33 == io_opcode ? 2'h0 : _GEN_364; // @[ControlUnit.scala 105:21]
  assign io_unsigned = 7'h33 == io_opcode ? _GEN_36 : _GEN_362; // @[ControlUnit.scala 105:21]
  assign io_regs_write = 7'h33 == io_opcode | _GEN_355; // @[ControlUnit.scala 105:21 113:21]
  assign io_imm_width_type = 7'h13 == io_opcode ? 2'h0 : _GEN_338; // @[ControlUnit.scala 105:21 168:25]
  assign io_operand2_type = 7'h33 == io_opcode | _GEN_359; // @[ControlUnit.scala 105:21 117:24]
  assign io_au_type = 7'h33 == io_opcode ? _GEN_34 : _GEN_356; // @[ControlUnit.scala 105:21]
  assign io_write_back_type = 7'h33 == io_opcode ? 3'h0 : _GEN_361; // @[ControlUnit.scala 105:21 118:26]
  assign io_memory_read = 7'h33 == io_opcode ? 1'h0 : _GEN_352; // @[ControlUnit.scala 105:21 115:22]
  assign io_memory_write = 7'h33 == io_opcode ? 1'h0 : _GEN_358; // @[ControlUnit.scala 105:21 116:23]
  assign io_data_width = 7'h3 == io_opcode ? _GEN_76 : _GEN_80; // @[ControlUnit.scala 105:21]
  assign io_csr_write = 7'h33 == io_opcode ? 1'h0 : _GEN_368; // @[ControlUnit.scala 100:16 105:21]
  assign io_operand1_type = 7'h33 == io_opcode ? 1'h0 : _GEN_369; // @[ControlUnit.scala 101:20 105:21]
  assign io_result_stage = 7'h33 == io_opcode ? 1'h0 : _GEN_352; // @[ControlUnit.scala 105:21 96:21]
  assign io_branch_type = 7'h33 == io_opcode ? 2'h0 : _GEN_353; // @[ControlUnit.scala 105:21 97:20]
  assign io_fault_ID_fault_type = 7'h33 == io_opcode ? 3'h0 : _GEN_370; // @[ControlUnit.scala 105:21 54:26]
  assign io_fault_mtval = 7'h33 == io_opcode ? 32'h0 : _GEN_371; // @[ControlUnit.scala 105:21 55:18]
endmodule
module Registers(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  input         io_write,
  input  [4:0]  io_rs1,
  input  [4:0]  io_rs2,
  input  [4:0]  io_rd,
  input  [31:0] io_write_data,
  output [31:0] io_rs1_val,
  output [31:0] io_rs2_val
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [31:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [31:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
  reg [31:0] _RAND_20;
  reg [31:0] _RAND_21;
  reg [31:0] _RAND_22;
  reg [31:0] _RAND_23;
  reg [31:0] _RAND_24;
  reg [31:0] _RAND_25;
  reg [31:0] _RAND_26;
  reg [31:0] _RAND_27;
  reg [31:0] _RAND_28;
  reg [31:0] _RAND_29;
  reg [31:0] _RAND_30;
  reg [31:0] _RAND_31;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] regs_0; // @[Registers.scala 24:21]
  reg [31:0] regs_1; // @[Registers.scala 24:21]
  reg [31:0] regs_2; // @[Registers.scala 24:21]
  reg [31:0] regs_3; // @[Registers.scala 24:21]
  reg [31:0] regs_4; // @[Registers.scala 24:21]
  reg [31:0] regs_5; // @[Registers.scala 24:21]
  reg [31:0] regs_6; // @[Registers.scala 24:21]
  reg [31:0] regs_7; // @[Registers.scala 24:21]
  reg [31:0] regs_8; // @[Registers.scala 24:21]
  reg [31:0] regs_9; // @[Registers.scala 24:21]
  reg [31:0] regs_10; // @[Registers.scala 24:21]
  reg [31:0] regs_11; // @[Registers.scala 24:21]
  reg [31:0] regs_12; // @[Registers.scala 24:21]
  reg [31:0] regs_13; // @[Registers.scala 24:21]
  reg [31:0] regs_14; // @[Registers.scala 24:21]
  reg [31:0] regs_15; // @[Registers.scala 24:21]
  reg [31:0] regs_16; // @[Registers.scala 24:21]
  reg [31:0] regs_17; // @[Registers.scala 24:21]
  reg [31:0] regs_18; // @[Registers.scala 24:21]
  reg [31:0] regs_19; // @[Registers.scala 24:21]
  reg [31:0] regs_20; // @[Registers.scala 24:21]
  reg [31:0] regs_21; // @[Registers.scala 24:21]
  reg [31:0] regs_22; // @[Registers.scala 24:21]
  reg [31:0] regs_23; // @[Registers.scala 24:21]
  reg [31:0] regs_24; // @[Registers.scala 24:21]
  reg [31:0] regs_25; // @[Registers.scala 24:21]
  reg [31:0] regs_26; // @[Registers.scala 24:21]
  reg [31:0] regs_27; // @[Registers.scala 24:21]
  reg [31:0] regs_28; // @[Registers.scala 24:21]
  reg [31:0] regs_29; // @[Registers.scala 24:21]
  reg [31:0] regs_30; // @[Registers.scala 24:21]
  reg [31:0] regs_31; // @[Registers.scala 24:21]
  wire [31:0] _GEN_1 = 5'h1 == io_rs1 ? regs_1 : regs_0; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_2 = 5'h2 == io_rs1 ? regs_2 : _GEN_1; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_3 = 5'h3 == io_rs1 ? regs_3 : _GEN_2; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_4 = 5'h4 == io_rs1 ? regs_4 : _GEN_3; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_5 = 5'h5 == io_rs1 ? regs_5 : _GEN_4; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_6 = 5'h6 == io_rs1 ? regs_6 : _GEN_5; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_7 = 5'h7 == io_rs1 ? regs_7 : _GEN_6; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_8 = 5'h8 == io_rs1 ? regs_8 : _GEN_7; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_9 = 5'h9 == io_rs1 ? regs_9 : _GEN_8; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_10 = 5'ha == io_rs1 ? regs_10 : _GEN_9; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_11 = 5'hb == io_rs1 ? regs_11 : _GEN_10; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_12 = 5'hc == io_rs1 ? regs_12 : _GEN_11; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_13 = 5'hd == io_rs1 ? regs_13 : _GEN_12; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_14 = 5'he == io_rs1 ? regs_14 : _GEN_13; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_15 = 5'hf == io_rs1 ? regs_15 : _GEN_14; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_16 = 5'h10 == io_rs1 ? regs_16 : _GEN_15; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_17 = 5'h11 == io_rs1 ? regs_17 : _GEN_16; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_18 = 5'h12 == io_rs1 ? regs_18 : _GEN_17; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_19 = 5'h13 == io_rs1 ? regs_19 : _GEN_18; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_20 = 5'h14 == io_rs1 ? regs_20 : _GEN_19; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_21 = 5'h15 == io_rs1 ? regs_21 : _GEN_20; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_22 = 5'h16 == io_rs1 ? regs_22 : _GEN_21; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_23 = 5'h17 == io_rs1 ? regs_23 : _GEN_22; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_24 = 5'h18 == io_rs1 ? regs_24 : _GEN_23; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_25 = 5'h19 == io_rs1 ? regs_25 : _GEN_24; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_26 = 5'h1a == io_rs1 ? regs_26 : _GEN_25; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_27 = 5'h1b == io_rs1 ? regs_27 : _GEN_26; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_28 = 5'h1c == io_rs1 ? regs_28 : _GEN_27; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_29 = 5'h1d == io_rs1 ? regs_29 : _GEN_28; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_30 = 5'h1e == io_rs1 ? regs_30 : _GEN_29; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_33 = 5'h1 == io_rs2 ? regs_1 : regs_0; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_34 = 5'h2 == io_rs2 ? regs_2 : _GEN_33; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_35 = 5'h3 == io_rs2 ? regs_3 : _GEN_34; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_36 = 5'h4 == io_rs2 ? regs_4 : _GEN_35; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_37 = 5'h5 == io_rs2 ? regs_5 : _GEN_36; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_38 = 5'h6 == io_rs2 ? regs_6 : _GEN_37; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_39 = 5'h7 == io_rs2 ? regs_7 : _GEN_38; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_40 = 5'h8 == io_rs2 ? regs_8 : _GEN_39; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_41 = 5'h9 == io_rs2 ? regs_9 : _GEN_40; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_42 = 5'ha == io_rs2 ? regs_10 : _GEN_41; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_43 = 5'hb == io_rs2 ? regs_11 : _GEN_42; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_44 = 5'hc == io_rs2 ? regs_12 : _GEN_43; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_45 = 5'hd == io_rs2 ? regs_13 : _GEN_44; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_46 = 5'he == io_rs2 ? regs_14 : _GEN_45; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_47 = 5'hf == io_rs2 ? regs_15 : _GEN_46; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_48 = 5'h10 == io_rs2 ? regs_16 : _GEN_47; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_49 = 5'h11 == io_rs2 ? regs_17 : _GEN_48; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_50 = 5'h12 == io_rs2 ? regs_18 : _GEN_49; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_51 = 5'h13 == io_rs2 ? regs_19 : _GEN_50; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_52 = 5'h14 == io_rs2 ? regs_20 : _GEN_51; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_53 = 5'h15 == io_rs2 ? regs_21 : _GEN_52; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_54 = 5'h16 == io_rs2 ? regs_22 : _GEN_53; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_55 = 5'h17 == io_rs2 ? regs_23 : _GEN_54; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_56 = 5'h18 == io_rs2 ? regs_24 : _GEN_55; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_57 = 5'h19 == io_rs2 ? regs_25 : _GEN_56; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_58 = 5'h1a == io_rs2 ? regs_26 : _GEN_57; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_59 = 5'h1b == io_rs2 ? regs_27 : _GEN_58; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_60 = 5'h1c == io_rs2 ? regs_28 : _GEN_59; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_61 = 5'h1d == io_rs2 ? regs_29 : _GEN_60; // @[Registers.scala 26:{14,14}]
  wire [31:0] _GEN_62 = 5'h1e == io_rs2 ? regs_30 : _GEN_61; // @[Registers.scala 26:{14,14}]
  wire  _T = io_cpu_state == 2'h0; // @[Registers.scala 27:21]
  wire  _T_1 = _T & io_write; // @[Registers.scala 28:5]
  wire  _T_3 = _T_1 & io_rd != 5'h0; // @[Registers.scala 29:5]
  assign io_rs1_val = 5'h1f == io_rs1 ? regs_31 : _GEN_30; // @[Registers.scala 25:{14,14}]
  assign io_rs2_val = 5'h1f == io_rs2 ? regs_31 : _GEN_62; // @[Registers.scala 26:{14,14}]
  always @(posedge clock) begin
    if (reset) begin // @[Registers.scala 24:21]
      regs_0 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h0 == io_rd) begin // @[Registers.scala 34:17]
        regs_0 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_1 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h1 == io_rd) begin // @[Registers.scala 34:17]
        regs_1 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_2 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h2 == io_rd) begin // @[Registers.scala 34:17]
        regs_2 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_3 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h3 == io_rd) begin // @[Registers.scala 34:17]
        regs_3 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_4 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h4 == io_rd) begin // @[Registers.scala 34:17]
        regs_4 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_5 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h5 == io_rd) begin // @[Registers.scala 34:17]
        regs_5 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_6 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h6 == io_rd) begin // @[Registers.scala 34:17]
        regs_6 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_7 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h7 == io_rd) begin // @[Registers.scala 34:17]
        regs_7 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_8 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h8 == io_rd) begin // @[Registers.scala 34:17]
        regs_8 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_9 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h9 == io_rd) begin // @[Registers.scala 34:17]
        regs_9 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_10 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'ha == io_rd) begin // @[Registers.scala 34:17]
        regs_10 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_11 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'hb == io_rd) begin // @[Registers.scala 34:17]
        regs_11 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_12 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'hc == io_rd) begin // @[Registers.scala 34:17]
        regs_12 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_13 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'hd == io_rd) begin // @[Registers.scala 34:17]
        regs_13 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_14 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'he == io_rd) begin // @[Registers.scala 34:17]
        regs_14 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_15 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'hf == io_rd) begin // @[Registers.scala 34:17]
        regs_15 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_16 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h10 == io_rd) begin // @[Registers.scala 34:17]
        regs_16 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_17 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h11 == io_rd) begin // @[Registers.scala 34:17]
        regs_17 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_18 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h12 == io_rd) begin // @[Registers.scala 34:17]
        regs_18 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_19 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h13 == io_rd) begin // @[Registers.scala 34:17]
        regs_19 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_20 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h14 == io_rd) begin // @[Registers.scala 34:17]
        regs_20 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_21 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h15 == io_rd) begin // @[Registers.scala 34:17]
        regs_21 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_22 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h16 == io_rd) begin // @[Registers.scala 34:17]
        regs_22 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_23 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h17 == io_rd) begin // @[Registers.scala 34:17]
        regs_23 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_24 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h18 == io_rd) begin // @[Registers.scala 34:17]
        regs_24 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_25 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h19 == io_rd) begin // @[Registers.scala 34:17]
        regs_25 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_26 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h1a == io_rd) begin // @[Registers.scala 34:17]
        regs_26 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_27 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h1b == io_rd) begin // @[Registers.scala 34:17]
        regs_27 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_28 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h1c == io_rd) begin // @[Registers.scala 34:17]
        regs_28 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_29 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h1d == io_rd) begin // @[Registers.scala 34:17]
        regs_29 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_30 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h1e == io_rd) begin // @[Registers.scala 34:17]
        regs_30 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_31 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_3) begin // @[Registers.scala 30:5]
      if (5'h1f == io_rd) begin // @[Registers.scala 34:17]
        regs_31 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  regs_0 = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  regs_1 = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  regs_2 = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  regs_3 = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  regs_4 = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  regs_5 = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  regs_6 = _RAND_6[31:0];
  _RAND_7 = {1{`RANDOM}};
  regs_7 = _RAND_7[31:0];
  _RAND_8 = {1{`RANDOM}};
  regs_8 = _RAND_8[31:0];
  _RAND_9 = {1{`RANDOM}};
  regs_9 = _RAND_9[31:0];
  _RAND_10 = {1{`RANDOM}};
  regs_10 = _RAND_10[31:0];
  _RAND_11 = {1{`RANDOM}};
  regs_11 = _RAND_11[31:0];
  _RAND_12 = {1{`RANDOM}};
  regs_12 = _RAND_12[31:0];
  _RAND_13 = {1{`RANDOM}};
  regs_13 = _RAND_13[31:0];
  _RAND_14 = {1{`RANDOM}};
  regs_14 = _RAND_14[31:0];
  _RAND_15 = {1{`RANDOM}};
  regs_15 = _RAND_15[31:0];
  _RAND_16 = {1{`RANDOM}};
  regs_16 = _RAND_16[31:0];
  _RAND_17 = {1{`RANDOM}};
  regs_17 = _RAND_17[31:0];
  _RAND_18 = {1{`RANDOM}};
  regs_18 = _RAND_18[31:0];
  _RAND_19 = {1{`RANDOM}};
  regs_19 = _RAND_19[31:0];
  _RAND_20 = {1{`RANDOM}};
  regs_20 = _RAND_20[31:0];
  _RAND_21 = {1{`RANDOM}};
  regs_21 = _RAND_21[31:0];
  _RAND_22 = {1{`RANDOM}};
  regs_22 = _RAND_22[31:0];
  _RAND_23 = {1{`RANDOM}};
  regs_23 = _RAND_23[31:0];
  _RAND_24 = {1{`RANDOM}};
  regs_24 = _RAND_24[31:0];
  _RAND_25 = {1{`RANDOM}};
  regs_25 = _RAND_25[31:0];
  _RAND_26 = {1{`RANDOM}};
  regs_26 = _RAND_26[31:0];
  _RAND_27 = {1{`RANDOM}};
  regs_27 = _RAND_27[31:0];
  _RAND_28 = {1{`RANDOM}};
  regs_28 = _RAND_28[31:0];
  _RAND_29 = {1{`RANDOM}};
  regs_29 = _RAND_29[31:0];
  _RAND_30 = {1{`RANDOM}};
  regs_30 = _RAND_30[31:0];
  _RAND_31 = {1{`RANDOM}};
  regs_31 = _RAND_31[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module ImmGen(
  input  [19:0] io_raw_imm,
  input         io_unsigned,
  input  [1:0]  io_imm_width,
  output [31:0] io_real_imm
);
  wire [31:0] _io_real_imm_T_2 = {20'h0,io_raw_imm[11:0]}; // @[Cat.scala 33:92]
  wire [19:0] _io_real_imm_T_5 = io_raw_imm[11] ? 20'hfffff : 20'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_real_imm_T_7 = {_io_real_imm_T_5,io_raw_imm[11:0]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_0 = io_unsigned ? _io_real_imm_T_2 : _io_real_imm_T_7; // @[ImmGen.scala 18:24 19:21 21:21]
  wire [31:0] _io_real_imm_T_10 = {19'h0,io_raw_imm[11:0],1'h0}; // @[Cat.scala 33:92]
  wire [18:0] _io_real_imm_T_13 = io_raw_imm[11] ? 19'h7ffff : 19'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_real_imm_T_15 = {_io_real_imm_T_13,io_raw_imm[11:0],1'h0}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_1 = io_unsigned ? _io_real_imm_T_10 : _io_real_imm_T_15; // @[ImmGen.scala 25:24 26:21 28:21]
  wire [31:0] _io_real_imm_T_18 = {11'h0,io_raw_imm,1'h0}; // @[Cat.scala 33:92]
  wire [10:0] _io_real_imm_T_21 = io_raw_imm[19] ? 11'h7ff : 11'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_real_imm_T_23 = {_io_real_imm_T_21,io_raw_imm,1'h0}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_2 = io_unsigned ? _io_real_imm_T_18 : _io_real_imm_T_23; // @[ImmGen.scala 32:24 33:21 35:21]
  wire [31:0] _io_real_imm_T_26 = {io_raw_imm,12'h0}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_4 = 2'h2 == io_imm_width ? _GEN_2 : _io_real_imm_T_26; // @[ImmGen.scala 16:23]
  wire [31:0] _GEN_5 = 2'h1 == io_imm_width ? _GEN_1 : _GEN_4; // @[ImmGen.scala 16:23]
  assign io_real_imm = 2'h0 == io_imm_width ? _GEN_0 : _GEN_5; // @[ImmGen.scala 16:23]
endmodule
module ForwardSelector(
  input  [1:0]  io_forward_type,
  input  [31:0] io_origin_val,
  input  [31:0] io_ex_forward,
  input  [31:0] io_mem_forward,
  output [31:0] io_real_val
);
  wire [31:0] _GEN_1 = 2'h2 == io_forward_type ? io_mem_forward : io_origin_val; // @[ForwardSelector.scala 17:27 22:19]
  assign io_real_val = 2'h1 == io_forward_type ? io_ex_forward : _GEN_1; // @[ForwardSelector.scala 17:27 19:19]
endmodule
module OperandSelector(
  input         io_operand2Type,
  input         io_operand1Type,
  input  [31:0] io_rs1_val,
  input  [31:0] io_rs2_val,
  input  [31:0] io_real_imm,
  input  [31:0] io_csr_val,
  output [31:0] io_operand1,
  output [31:0] io_operand2
);
  assign io_operand1 = ~io_operand1Type ? io_rs1_val : io_csr_val; // @[OperandSelector.scala 21:26 23:18]
  assign io_operand2 = ~io_operand2Type ? io_real_imm : io_rs2_val; // @[OperandSelector.scala 32:27 34:19]
endmodule
module AUSelector(
  input         io_au_type,
  input  [31:0] io_alu_result,
  input         io_cmp_result,
  output [31:0] io_au_out
);
  assign io_au_out = ~io_au_type ? io_alu_result : {{31'd0}, io_cmp_result}; // @[AUSelector.scala 17:22 19:17]
endmodule
module WriteDataSelector(
  input  [2:0]  io_write_back_type,
  input  [31:0] io_imm,
  input  [31:0] io_mem_out,
  input  [31:0] io_au_out,
  input  [31:0] io_pc4,
  input  [31:0] io_pcImm,
  input  [31:0] io_csr,
  output [31:0] io_write_data
);
  wire [31:0] _GEN_1 = 3'h4 == io_write_back_type ? io_pcImm : io_csr; // @[WriteDataSelector.scala 21:30 35:21]
  wire [31:0] _GEN_2 = 3'h2 == io_write_back_type ? io_pc4 : _GEN_1; // @[WriteDataSelector.scala 21:30 32:21]
  wire [31:0] _GEN_3 = 3'h1 == io_write_back_type ? io_mem_out : _GEN_2; // @[WriteDataSelector.scala 21:30 29:21]
  wire [31:0] _GEN_4 = 3'h3 == io_write_back_type ? io_imm : _GEN_3; // @[WriteDataSelector.scala 21:30 26:21]
  assign io_write_data = 3'h0 == io_write_back_type ? io_au_out : _GEN_4; // @[WriteDataSelector.scala 21:30 23:21]
endmodule
module WBSelectorTop(
  input         io_au_type,
  input  [31:0] io_alu_result,
  input         io_cmp_result,
  input  [2:0]  io_write_back_type,
  input  [31:0] io_imm,
  input  [31:0] io_mem_out,
  input  [31:0] io_pc4,
  input  [31:0] io_pcImm,
  input  [31:0] io_csr_val,
  output [31:0] io_write_data
);
  wire  au_selector_io_au_type; // @[WBSelectorTop.scala 24:35]
  wire [31:0] au_selector_io_alu_result; // @[WBSelectorTop.scala 24:35]
  wire  au_selector_io_cmp_result; // @[WBSelectorTop.scala 24:35]
  wire [31:0] au_selector_io_au_out; // @[WBSelectorTop.scala 24:35]
  wire [2:0] wb_selector_io_write_back_type; // @[WBSelectorTop.scala 25:35]
  wire [31:0] wb_selector_io_imm; // @[WBSelectorTop.scala 25:35]
  wire [31:0] wb_selector_io_mem_out; // @[WBSelectorTop.scala 25:35]
  wire [31:0] wb_selector_io_au_out; // @[WBSelectorTop.scala 25:35]
  wire [31:0] wb_selector_io_pc4; // @[WBSelectorTop.scala 25:35]
  wire [31:0] wb_selector_io_pcImm; // @[WBSelectorTop.scala 25:35]
  wire [31:0] wb_selector_io_csr; // @[WBSelectorTop.scala 25:35]
  wire [31:0] wb_selector_io_write_data; // @[WBSelectorTop.scala 25:35]
  AUSelector au_selector ( // @[WBSelectorTop.scala 24:35]
    .io_au_type(au_selector_io_au_type),
    .io_alu_result(au_selector_io_alu_result),
    .io_cmp_result(au_selector_io_cmp_result),
    .io_au_out(au_selector_io_au_out)
  );
  WriteDataSelector wb_selector ( // @[WBSelectorTop.scala 25:35]
    .io_write_back_type(wb_selector_io_write_back_type),
    .io_imm(wb_selector_io_imm),
    .io_mem_out(wb_selector_io_mem_out),
    .io_au_out(wb_selector_io_au_out),
    .io_pc4(wb_selector_io_pc4),
    .io_pcImm(wb_selector_io_pcImm),
    .io_csr(wb_selector_io_csr),
    .io_write_data(wb_selector_io_write_data)
  );
  assign io_write_data = wb_selector_io_write_data; // @[WBSelectorTop.scala 39:17]
  assign au_selector_io_au_type = io_au_type; // @[WBSelectorTop.scala 27:26]
  assign au_selector_io_alu_result = io_alu_result; // @[WBSelectorTop.scala 28:29]
  assign au_selector_io_cmp_result = io_cmp_result; // @[WBSelectorTop.scala 29:29]
  assign wb_selector_io_write_back_type = io_write_back_type; // @[WBSelectorTop.scala 33:34]
  assign wb_selector_io_imm = io_imm; // @[WBSelectorTop.scala 34:22]
  assign wb_selector_io_mem_out = io_mem_out; // @[WBSelectorTop.scala 35:26]
  assign wb_selector_io_au_out = au_selector_io_au_out; // @[WBSelectorTop.scala 30:25]
  assign wb_selector_io_pc4 = io_pc4; // @[WBSelectorTop.scala 36:22]
  assign wb_selector_io_pcImm = io_pcImm; // @[WBSelectorTop.scala 37:24]
  assign wb_selector_io_csr = io_csr_val; // @[WBSelectorTop.scala 38:22]
endmodule
module IDEX(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  input  [1:0]  io_signal,
  input  [31:0] io_in_pc,
  input  [1:0]  io_in_IF_fault_IF_fault_type,
  input  [31:0] io_in_IF_fault_mtval,
  input  [31:0] io_in_op1,
  input  [31:0] io_in_op2,
  input  [31:0] io_in_csr_val_to_reg,
  input  [3:0]  io_in_alu_op,
  input  [1:0]  io_in_cmp_op,
  input         io_in_unsigned,
  input         io_in_mem_signal_read_data_en,
  input         io_in_mem_signal_write_data_en,
  input  [1:0]  io_in_mem_signal_data_width,
  input  [31:0] io_in_mem_signal_data_to_write,
  input         io_in_wb_signal_au_type,
  input  [2:0]  io_in_wb_signal_wb_type,
  input  [31:0] io_in_wb_signal_imm,
  input  [4:0]  io_in_wb_signal_rd,
  input         io_in_wb_signal_write_reg,
  input  [11:0] io_in_wb_signal_csr_idx,
  input         io_in_wb_signal_csr_write,
  input         io_in_result_stage,
  input  [1:0]  io_in_branch_type,
  input  [2:0]  io_in_ID_fault_ID_fault_type,
  input  [31:0] io_in_ID_fault_mtval,
  output [31:0] io_out_pc,
  output [1:0]  io_out_IF_fault_IF_fault_type,
  output [31:0] io_out_IF_fault_mtval,
  output [31:0] io_out_op1,
  output [31:0] io_out_op2,
  output [31:0] io_out_csr_val_to_reg,
  output [3:0]  io_out_alu_op,
  output [1:0]  io_out_cmp_op,
  output        io_out_unsigned,
  output        io_out_mem_signal_read_data_en,
  output        io_out_mem_signal_write_data_en,
  output [1:0]  io_out_mem_signal_data_width,
  output [31:0] io_out_mem_signal_data_to_write,
  output        io_out_wb_signal_au_type,
  output [2:0]  io_out_wb_signal_wb_type,
  output [31:0] io_out_wb_signal_imm,
  output [4:0]  io_out_wb_signal_rd,
  output        io_out_wb_signal_write_reg,
  output [11:0] io_out_wb_signal_csr_idx,
  output        io_out_wb_signal_csr_write,
  output        io_out_result_stage,
  output [1:0]  io_out_branch_type,
  output [2:0]  io_out_ID_fault_ID_fault_type,
  output [31:0] io_out_ID_fault_mtval
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [31:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [31:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
  reg [31:0] _RAND_20;
  reg [31:0] _RAND_21;
  reg [31:0] _RAND_22;
  reg [31:0] _RAND_23;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] regs_pc; // @[IDEX.scala 93:21]
  reg [1:0] regs_IF_fault_IF_fault_type; // @[IDEX.scala 93:21]
  reg [31:0] regs_IF_fault_mtval; // @[IDEX.scala 93:21]
  reg [31:0] regs_op1; // @[IDEX.scala 93:21]
  reg [31:0] regs_op2; // @[IDEX.scala 93:21]
  reg [31:0] regs_csr_val_to_reg; // @[IDEX.scala 93:21]
  reg [3:0] regs_alu_op; // @[IDEX.scala 93:21]
  reg [1:0] regs_cmp_op; // @[IDEX.scala 93:21]
  reg  regs_unsigned; // @[IDEX.scala 93:21]
  reg  regs_mem_signal_read_data_en; // @[IDEX.scala 93:21]
  reg  regs_mem_signal_write_data_en; // @[IDEX.scala 93:21]
  reg [1:0] regs_mem_signal_data_width; // @[IDEX.scala 93:21]
  reg [31:0] regs_mem_signal_data_to_write; // @[IDEX.scala 93:21]
  reg  regs_wb_signal_au_type; // @[IDEX.scala 93:21]
  reg [2:0] regs_wb_signal_wb_type; // @[IDEX.scala 93:21]
  reg [31:0] regs_wb_signal_imm; // @[IDEX.scala 93:21]
  reg [4:0] regs_wb_signal_rd; // @[IDEX.scala 93:21]
  reg  regs_wb_signal_write_reg; // @[IDEX.scala 93:21]
  reg [11:0] regs_wb_signal_csr_idx; // @[IDEX.scala 93:21]
  reg  regs_wb_signal_csr_write; // @[IDEX.scala 93:21]
  reg  regs_result_stage; // @[IDEX.scala 93:21]
  reg [1:0] regs_branch_type; // @[IDEX.scala 93:21]
  reg [2:0] regs_ID_fault_ID_fault_type; // @[IDEX.scala 93:21]
  reg [31:0] regs_ID_fault_mtval; // @[IDEX.scala 93:21]
  wire  _T_3 = io_in_ID_fault_ID_fault_type != 3'h0; // @[IDEX.scala 99:40]
  wire  _T_4 = io_in_IF_fault_IF_fault_type != 2'h0 | _T_3; // @[IDEX.scala 98:70]
  wire [2:0] _GEN_26 = 2'h2 == io_signal ? 3'h0 : regs_ID_fault_ID_fault_type; // @[IDEX.scala 112:14 93:21 96:23]
  wire [1:0] _GEN_27 = 2'h2 == io_signal ? 2'h0 : regs_branch_type; // @[IDEX.scala 112:14 93:21 96:23]
  wire  _GEN_28 = 2'h2 == io_signal ? 1'h0 : regs_result_stage; // @[IDEX.scala 112:14 93:21 96:23]
  wire  _GEN_29 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_csr_write; // @[IDEX.scala 112:14 93:21 96:23]
  wire [11:0] _GEN_30 = 2'h2 == io_signal ? 12'h0 : regs_wb_signal_csr_idx; // @[IDEX.scala 112:14 93:21 96:23]
  wire  _GEN_31 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_write_reg; // @[IDEX.scala 112:14 93:21 96:23]
  wire [4:0] _GEN_32 = 2'h2 == io_signal ? 5'h0 : regs_wb_signal_rd; // @[IDEX.scala 112:14 93:21 96:23]
  wire [31:0] _GEN_33 = 2'h2 == io_signal ? 32'h0 : regs_wb_signal_imm; // @[IDEX.scala 112:14 93:21 96:23]
  wire [2:0] _GEN_34 = 2'h2 == io_signal ? 3'h0 : regs_wb_signal_wb_type; // @[IDEX.scala 112:14 93:21 96:23]
  wire  _GEN_35 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_au_type; // @[IDEX.scala 112:14 93:21 96:23]
  wire [31:0] _GEN_36 = 2'h2 == io_signal ? 32'h0 : regs_mem_signal_data_to_write; // @[IDEX.scala 112:14 93:21 96:23]
  wire [1:0] _GEN_37 = 2'h2 == io_signal ? 2'h0 : regs_mem_signal_data_width; // @[IDEX.scala 112:14 93:21 96:23]
  wire  _GEN_38 = 2'h2 == io_signal ? 1'h0 : regs_mem_signal_write_data_en; // @[IDEX.scala 112:14 93:21 96:23]
  wire  _GEN_39 = 2'h2 == io_signal ? 1'h0 : regs_mem_signal_read_data_en; // @[IDEX.scala 112:14 93:21 96:23]
  wire  _GEN_40 = 2'h2 == io_signal ? 1'h0 : regs_unsigned; // @[IDEX.scala 112:14 93:21 96:23]
  wire [1:0] _GEN_41 = 2'h2 == io_signal ? 2'h2 : regs_cmp_op; // @[IDEX.scala 112:14 93:21 96:23]
  wire [3:0] _GEN_42 = 2'h2 == io_signal ? 4'h0 : regs_alu_op; // @[IDEX.scala 112:14 93:21 96:23]
  wire [31:0] _GEN_43 = 2'h2 == io_signal ? 32'h0 : regs_csr_val_to_reg; // @[IDEX.scala 112:14 93:21 96:23]
  wire [31:0] _GEN_44 = 2'h2 == io_signal ? 32'h0 : regs_op2; // @[IDEX.scala 112:14 93:21 96:23]
  wire [31:0] _GEN_45 = 2'h2 == io_signal ? 32'h0 : regs_op1; // @[IDEX.scala 112:14 93:21 96:23]
  wire [31:0] _GEN_46 = 2'h2 == io_signal ? 32'h0 : regs_IF_fault_mtval; // @[IDEX.scala 112:14 93:21 96:23]
  wire [1:0] _GEN_47 = 2'h2 == io_signal ? 2'h0 : regs_IF_fault_IF_fault_type; // @[IDEX.scala 112:14 93:21 96:23]
  wire [31:0] _GEN_49 = 2'h2 == io_signal ? 32'h0 : regs_pc; // @[IDEX.scala 112:14 93:21 96:23]
  assign io_out_pc = regs_pc; // @[IDEX.scala 94:10]
  assign io_out_IF_fault_IF_fault_type = regs_IF_fault_IF_fault_type; // @[IDEX.scala 94:10]
  assign io_out_IF_fault_mtval = regs_IF_fault_mtval; // @[IDEX.scala 94:10]
  assign io_out_op1 = regs_op1; // @[IDEX.scala 94:10]
  assign io_out_op2 = regs_op2; // @[IDEX.scala 94:10]
  assign io_out_csr_val_to_reg = regs_csr_val_to_reg; // @[IDEX.scala 94:10]
  assign io_out_alu_op = regs_alu_op; // @[IDEX.scala 94:10]
  assign io_out_cmp_op = regs_cmp_op; // @[IDEX.scala 94:10]
  assign io_out_unsigned = regs_unsigned; // @[IDEX.scala 94:10]
  assign io_out_mem_signal_read_data_en = regs_mem_signal_read_data_en; // @[IDEX.scala 94:10]
  assign io_out_mem_signal_write_data_en = regs_mem_signal_write_data_en; // @[IDEX.scala 94:10]
  assign io_out_mem_signal_data_width = regs_mem_signal_data_width; // @[IDEX.scala 94:10]
  assign io_out_mem_signal_data_to_write = regs_mem_signal_data_to_write; // @[IDEX.scala 94:10]
  assign io_out_wb_signal_au_type = regs_wb_signal_au_type; // @[IDEX.scala 94:10]
  assign io_out_wb_signal_wb_type = regs_wb_signal_wb_type; // @[IDEX.scala 94:10]
  assign io_out_wb_signal_imm = regs_wb_signal_imm; // @[IDEX.scala 94:10]
  assign io_out_wb_signal_rd = regs_wb_signal_rd; // @[IDEX.scala 94:10]
  assign io_out_wb_signal_write_reg = regs_wb_signal_write_reg; // @[IDEX.scala 94:10]
  assign io_out_wb_signal_csr_idx = regs_wb_signal_csr_idx; // @[IDEX.scala 94:10]
  assign io_out_wb_signal_csr_write = regs_wb_signal_csr_write; // @[IDEX.scala 94:10]
  assign io_out_result_stage = regs_result_stage; // @[IDEX.scala 94:10]
  assign io_out_branch_type = regs_branch_type; // @[IDEX.scala 94:10]
  assign io_out_ID_fault_ID_fault_type = regs_ID_fault_ID_fault_type; // @[IDEX.scala 94:10]
  assign io_out_ID_fault_mtval = regs_ID_fault_mtval; // @[IDEX.scala 94:10]
  always @(posedge clock) begin
    if (reset) begin // @[IDEX.scala 93:21]
      regs_pc <= 32'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        regs_pc <= io_in_pc;
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_pc <= _GEN_49;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_IF_fault_IF_fault_type <= 2'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        regs_IF_fault_IF_fault_type <= io_in_IF_fault_IF_fault_type;
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_IF_fault_IF_fault_type <= _GEN_47;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_IF_fault_mtval <= 32'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        regs_IF_fault_mtval <= io_in_IF_fault_mtval;
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_IF_fault_mtval <= _GEN_46;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_op1 <= 32'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_op1 <= 32'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_op1 <= io_in_op1; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_op1 <= _GEN_45;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_op2 <= 32'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_op2 <= 32'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_op2 <= io_in_op2; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_op2 <= _GEN_44;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_csr_val_to_reg <= 32'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_csr_val_to_reg <= 32'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_csr_val_to_reg <= io_in_csr_val_to_reg; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_csr_val_to_reg <= _GEN_43;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_alu_op <= 4'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_alu_op <= 4'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_alu_op <= io_in_alu_op; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_alu_op <= _GEN_42;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_cmp_op <= 2'h2; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_cmp_op <= 2'h2; // @[IDEX.scala 100:16]
        end else begin
          regs_cmp_op <= io_in_cmp_op; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_cmp_op <= _GEN_41;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_unsigned <= 1'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_unsigned <= 1'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_unsigned <= io_in_unsigned; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_unsigned <= _GEN_40;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_mem_signal_read_data_en <= 1'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_mem_signal_read_data_en <= 1'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_mem_signal_read_data_en <= io_in_mem_signal_read_data_en; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_mem_signal_read_data_en <= _GEN_39;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_mem_signal_write_data_en <= 1'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_mem_signal_write_data_en <= 1'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_mem_signal_write_data_en <= io_in_mem_signal_write_data_en; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_mem_signal_write_data_en <= _GEN_38;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_mem_signal_data_width <= 2'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_mem_signal_data_width <= 2'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_mem_signal_data_width <= io_in_mem_signal_data_width; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_mem_signal_data_width <= _GEN_37;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_mem_signal_data_to_write <= 32'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_mem_signal_data_to_write <= 32'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_mem_signal_data_to_write <= io_in_mem_signal_data_to_write; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_mem_signal_data_to_write <= _GEN_36;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_wb_signal_au_type <= 1'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_wb_signal_au_type <= 1'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_wb_signal_au_type <= io_in_wb_signal_au_type; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_wb_signal_au_type <= _GEN_35;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_wb_signal_wb_type <= 3'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_wb_signal_wb_type <= 3'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_wb_signal_wb_type <= io_in_wb_signal_wb_type; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_wb_signal_wb_type <= _GEN_34;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_wb_signal_imm <= 32'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_wb_signal_imm <= 32'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_wb_signal_imm <= io_in_wb_signal_imm; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_wb_signal_imm <= _GEN_33;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_wb_signal_rd <= 5'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_wb_signal_rd <= 5'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_wb_signal_rd <= io_in_wb_signal_rd; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_wb_signal_rd <= _GEN_32;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_wb_signal_write_reg <= 1'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_wb_signal_write_reg <= 1'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_wb_signal_write_reg <= io_in_wb_signal_write_reg; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_wb_signal_write_reg <= _GEN_31;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_wb_signal_csr_idx <= 12'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_wb_signal_csr_idx <= 12'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_wb_signal_csr_idx <= io_in_wb_signal_csr_idx; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_wb_signal_csr_idx <= _GEN_30;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_wb_signal_csr_write <= 1'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_wb_signal_csr_write <= 1'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_wb_signal_csr_write <= io_in_wb_signal_csr_write; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_wb_signal_csr_write <= _GEN_29;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_result_stage <= 1'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_result_stage <= 1'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_result_stage <= io_in_result_stage; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_result_stage <= _GEN_28;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_branch_type <= 2'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        if (_T_4) begin // @[IDEX.scala 99:68]
          regs_branch_type <= 2'h0; // @[IDEX.scala 100:16]
        end else begin
          regs_branch_type <= io_in_branch_type; // @[IDEX.scala 105:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_branch_type <= _GEN_27;
      end
    end
    if (reset) begin // @[IDEX.scala 93:21]
      regs_ID_fault_ID_fault_type <= 3'h0; // @[IDEX.scala 93:21]
    end else if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        regs_ID_fault_ID_fault_type <= io_in_ID_fault_ID_fault_type;
      end else if (!(2'h1 == io_signal)) begin // @[IDEX.scala 96:23]
        regs_ID_fault_ID_fault_type <= _GEN_26;
      end
    end
    if (io_cpu_state == 2'h2) begin // @[IDEX.scala 95:60]
      if (2'h0 == io_signal) begin // @[IDEX.scala 96:23]
        regs_ID_fault_mtval <= io_in_ID_fault_mtval;
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  regs_pc = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  regs_IF_fault_IF_fault_type = _RAND_1[1:0];
  _RAND_2 = {1{`RANDOM}};
  regs_IF_fault_mtval = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  regs_op1 = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  regs_op2 = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  regs_csr_val_to_reg = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  regs_alu_op = _RAND_6[3:0];
  _RAND_7 = {1{`RANDOM}};
  regs_cmp_op = _RAND_7[1:0];
  _RAND_8 = {1{`RANDOM}};
  regs_unsigned = _RAND_8[0:0];
  _RAND_9 = {1{`RANDOM}};
  regs_mem_signal_read_data_en = _RAND_9[0:0];
  _RAND_10 = {1{`RANDOM}};
  regs_mem_signal_write_data_en = _RAND_10[0:0];
  _RAND_11 = {1{`RANDOM}};
  regs_mem_signal_data_width = _RAND_11[1:0];
  _RAND_12 = {1{`RANDOM}};
  regs_mem_signal_data_to_write = _RAND_12[31:0];
  _RAND_13 = {1{`RANDOM}};
  regs_wb_signal_au_type = _RAND_13[0:0];
  _RAND_14 = {1{`RANDOM}};
  regs_wb_signal_wb_type = _RAND_14[2:0];
  _RAND_15 = {1{`RANDOM}};
  regs_wb_signal_imm = _RAND_15[31:0];
  _RAND_16 = {1{`RANDOM}};
  regs_wb_signal_rd = _RAND_16[4:0];
  _RAND_17 = {1{`RANDOM}};
  regs_wb_signal_write_reg = _RAND_17[0:0];
  _RAND_18 = {1{`RANDOM}};
  regs_wb_signal_csr_idx = _RAND_18[11:0];
  _RAND_19 = {1{`RANDOM}};
  regs_wb_signal_csr_write = _RAND_19[0:0];
  _RAND_20 = {1{`RANDOM}};
  regs_result_stage = _RAND_20[0:0];
  _RAND_21 = {1{`RANDOM}};
  regs_branch_type = _RAND_21[1:0];
  _RAND_22 = {1{`RANDOM}};
  regs_ID_fault_ID_fault_type = _RAND_22[2:0];
  _RAND_23 = {1{`RANDOM}};
  regs_ID_fault_mtval = _RAND_23[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module ALU(
  input  [31:0] io_operand1,
  input  [31:0] io_operand2,
  input  [3:0]  io_alu_op,
  input         io_unsigned,
  output [31:0] io_result
);
  wire [31:0] _io_result_T_1 = io_operand1 + io_operand2; // @[ALU.scala 18:32]
  wire [31:0] _io_result_T_3 = io_operand1 - io_operand2; // @[ALU.scala 21:32]
  wire [31:0] _io_result_T_4 = io_operand1 ^ io_operand2; // @[ALU.scala 24:32]
  wire [31:0] _io_result_T_5 = io_operand1 | io_operand2; // @[ALU.scala 27:32]
  wire [31:0] _io_result_T_6 = io_operand1 & io_operand2; // @[ALU.scala 30:32]
  wire [62:0] _GEN_11 = {{31'd0}, io_operand1}; // @[ALU.scala 33:32]
  wire [62:0] _io_result_T_8 = _GEN_11 << io_operand2[4:0]; // @[ALU.scala 33:32]
  wire [31:0] _io_result_T_12 = $signed(io_operand1) >>> io_operand2[4:0]; // @[ALU.scala 37:71]
  wire [31:0] _io_result_T_14 = io_operand1 >> io_operand2[4:0]; // @[ALU.scala 39:34]
  wire [31:0] _GEN_0 = ~io_unsigned ? _io_result_T_12 : _io_result_T_14; // @[ALU.scala 36:26 37:19 39:19]
  wire [31:0] _GEN_10 = {{31'd0}, io_operand2 == 32'h0}; // @[ALU.scala 46:35]
  wire [31:0] _io_result_T_18 = _GEN_10 & io_operand1; // @[ALU.scala 46:35]
  wire [31:0] _GEN_2 = 4'h6 == io_alu_op ? _io_result_T_14 : _io_result_T_18; // @[ALU.scala 16:21 43:17]
  wire [31:0] _GEN_3 = 4'h7 == io_alu_op ? _GEN_0 : _GEN_2; // @[ALU.scala 16:21]
  wire [62:0] _GEN_4 = 4'h5 == io_alu_op ? _io_result_T_8 : {{31'd0}, _GEN_3}; // @[ALU.scala 16:21 33:17]
  wire [62:0] _GEN_5 = 4'h4 == io_alu_op ? {{31'd0}, _io_result_T_6} : _GEN_4; // @[ALU.scala 16:21 30:17]
  wire [62:0] _GEN_6 = 4'h3 == io_alu_op ? {{31'd0}, _io_result_T_5} : _GEN_5; // @[ALU.scala 16:21 27:17]
  wire [62:0] _GEN_7 = 4'h2 == io_alu_op ? {{31'd0}, _io_result_T_4} : _GEN_6; // @[ALU.scala 16:21 24:17]
  wire [62:0] _GEN_8 = 4'h1 == io_alu_op ? {{31'd0}, _io_result_T_3} : _GEN_7; // @[ALU.scala 16:21 21:17]
  wire [62:0] _GEN_9 = 4'h0 == io_alu_op ? {{31'd0}, _io_result_T_1} : _GEN_8; // @[ALU.scala 16:21 18:17]
  assign io_result = _GEN_9[31:0];
endmodule
module CMP(
  input  [31:0] io_operand1,
  input  [31:0] io_operand2,
  input  [1:0]  io_cmp_op,
  input         io_unsigned,
  output        io_result
);
  wire  _GEN_0 = io_unsigned ? io_operand1 < io_operand2 : $signed(io_operand1) < $signed(io_operand2); // @[CMP.scala 18:24 19:19 21:19]
  wire  _GEN_1 = io_unsigned ? io_operand1 >= io_operand2 : $signed(io_operand1) >= $signed(io_operand2); // @[CMP.scala 25:24 26:19 28:19]
  wire  _io_result_T_9 = io_operand1 != io_operand2; // @[CMP.scala 35:32]
  wire  _GEN_3 = 2'h2 == io_cmp_op ? io_operand1 == io_operand2 : _io_result_T_9; // @[CMP.scala 16:20 32:17]
  wire  _GEN_4 = 2'h1 == io_cmp_op ? _GEN_1 : _GEN_3; // @[CMP.scala 16:20]
  assign io_result = 2'h0 == io_cmp_op ? _GEN_0 : _GEN_4; // @[CMP.scala 16:20]
endmodule
module EXMEM(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  input  [1:0]  io_signal,
  input  [31:0] io_in_pc,
  input  [1:0]  io_in_IF_fault_IF_fault_type,
  input  [31:0] io_in_IF_fault_mtval,
  input  [2:0]  io_in_ID_fault_ID_fault_type,
  input  [31:0] io_in_ID_fault_mtval,
  input  [31:0] io_in_csr_val_to_reg,
  input         io_in_unsigned,
  input         io_in_mem_signal_read_data_en,
  input         io_in_mem_signal_write_data_en,
  input  [1:0]  io_in_mem_signal_data_width,
  input  [31:0] io_in_mem_signal_data_to_write,
  input         io_in_wb_signal_au_type,
  input  [2:0]  io_in_wb_signal_wb_type,
  input  [31:0] io_in_wb_signal_imm,
  input  [4:0]  io_in_wb_signal_rd,
  input         io_in_wb_signal_write_reg,
  input  [11:0] io_in_wb_signal_csr_idx,
  input         io_in_wb_signal_csr_write,
  input  [31:0] io_in_alu_out,
  input         io_in_cmp_out,
  output [31:0] io_out_pc,
  output [1:0]  io_out_IF_fault_IF_fault_type,
  output [31:0] io_out_IF_fault_mtval,
  output [2:0]  io_out_ID_fault_ID_fault_type,
  output [31:0] io_out_ID_fault_mtval,
  output [31:0] io_out_csr_val_to_reg,
  output        io_out_unsigned,
  output        io_out_mem_signal_read_data_en,
  output        io_out_mem_signal_write_data_en,
  output [1:0]  io_out_mem_signal_data_width,
  output [31:0] io_out_mem_signal_data_to_write,
  output        io_out_wb_signal_au_type,
  output [2:0]  io_out_wb_signal_wb_type,
  output [31:0] io_out_wb_signal_imm,
  output [4:0]  io_out_wb_signal_rd,
  output        io_out_wb_signal_write_reg,
  output [11:0] io_out_wb_signal_csr_idx,
  output        io_out_wb_signal_csr_write,
  output [31:0] io_out_alu_out,
  output        io_out_cmp_out
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
  reg [31:0] _RAND_12;
  reg [31:0] _RAND_13;
  reg [31:0] _RAND_14;
  reg [31:0] _RAND_15;
  reg [31:0] _RAND_16;
  reg [31:0] _RAND_17;
  reg [31:0] _RAND_18;
  reg [31:0] _RAND_19;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] regs_pc; // @[EXMEM.scala 63:21]
  reg [1:0] regs_IF_fault_IF_fault_type; // @[EXMEM.scala 63:21]
  reg [31:0] regs_IF_fault_mtval; // @[EXMEM.scala 63:21]
  reg [2:0] regs_ID_fault_ID_fault_type; // @[EXMEM.scala 63:21]
  reg [31:0] regs_ID_fault_mtval; // @[EXMEM.scala 63:21]
  reg [31:0] regs_csr_val_to_reg; // @[EXMEM.scala 63:21]
  reg  regs_unsigned; // @[EXMEM.scala 63:21]
  reg  regs_mem_signal_read_data_en; // @[EXMEM.scala 63:21]
  reg  regs_mem_signal_write_data_en; // @[EXMEM.scala 63:21]
  reg [1:0] regs_mem_signal_data_width; // @[EXMEM.scala 63:21]
  reg [31:0] regs_mem_signal_data_to_write; // @[EXMEM.scala 63:21]
  reg  regs_wb_signal_au_type; // @[EXMEM.scala 63:21]
  reg [2:0] regs_wb_signal_wb_type; // @[EXMEM.scala 63:21]
  reg [31:0] regs_wb_signal_imm; // @[EXMEM.scala 63:21]
  reg [4:0] regs_wb_signal_rd; // @[EXMEM.scala 63:21]
  reg  regs_wb_signal_write_reg; // @[EXMEM.scala 63:21]
  reg [11:0] regs_wb_signal_csr_idx; // @[EXMEM.scala 63:21]
  reg  regs_wb_signal_csr_write; // @[EXMEM.scala 63:21]
  reg [31:0] regs_alu_out; // @[EXMEM.scala 63:21]
  reg  regs_cmp_out; // @[EXMEM.scala 63:21]
  wire  _T_3 = io_in_ID_fault_ID_fault_type != 3'h0; // @[EXMEM.scala 69:40]
  wire  _T_4 = io_in_IF_fault_IF_fault_type != 2'h0 | _T_3; // @[EXMEM.scala 68:70]
  wire  _GEN_22 = 2'h2 == io_signal ? 1'h0 : regs_cmp_out; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [31:0] _GEN_23 = 2'h2 == io_signal ? 32'h0 : regs_alu_out; // @[EXMEM.scala 66:23 82:14 63:21]
  wire  _GEN_24 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_csr_write; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [11:0] _GEN_25 = 2'h2 == io_signal ? 12'h0 : regs_wb_signal_csr_idx; // @[EXMEM.scala 66:23 82:14 63:21]
  wire  _GEN_26 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_write_reg; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [4:0] _GEN_27 = 2'h2 == io_signal ? 5'h0 : regs_wb_signal_rd; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [31:0] _GEN_28 = 2'h2 == io_signal ? 32'h0 : regs_wb_signal_imm; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [2:0] _GEN_29 = 2'h2 == io_signal ? 3'h0 : regs_wb_signal_wb_type; // @[EXMEM.scala 66:23 82:14 63:21]
  wire  _GEN_30 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_au_type; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [31:0] _GEN_31 = 2'h2 == io_signal ? 32'h0 : regs_mem_signal_data_to_write; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [1:0] _GEN_32 = 2'h2 == io_signal ? 2'h0 : regs_mem_signal_data_width; // @[EXMEM.scala 66:23 82:14 63:21]
  wire  _GEN_33 = 2'h2 == io_signal ? 1'h0 : regs_mem_signal_write_data_en; // @[EXMEM.scala 66:23 82:14 63:21]
  wire  _GEN_34 = 2'h2 == io_signal ? 1'h0 : regs_mem_signal_read_data_en; // @[EXMEM.scala 66:23 82:14 63:21]
  wire  _GEN_35 = 2'h2 == io_signal ? 1'h0 : regs_unsigned; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [31:0] _GEN_36 = 2'h2 == io_signal ? 32'h0 : regs_csr_val_to_reg; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [31:0] _GEN_37 = 2'h2 == io_signal ? 32'h0 : regs_ID_fault_mtval; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [2:0] _GEN_38 = 2'h2 == io_signal ? 3'h0 : regs_ID_fault_ID_fault_type; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [31:0] _GEN_39 = 2'h2 == io_signal ? 32'h0 : regs_IF_fault_mtval; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [1:0] _GEN_40 = 2'h2 == io_signal ? 2'h0 : regs_IF_fault_IF_fault_type; // @[EXMEM.scala 66:23 82:14 63:21]
  wire [31:0] _GEN_43 = 2'h2 == io_signal ? 32'h0 : regs_pc; // @[EXMEM.scala 66:23 82:14 63:21]
  assign io_out_pc = regs_pc; // @[EXMEM.scala 64:10]
  assign io_out_IF_fault_IF_fault_type = regs_IF_fault_IF_fault_type; // @[EXMEM.scala 64:10]
  assign io_out_IF_fault_mtval = regs_IF_fault_mtval; // @[EXMEM.scala 64:10]
  assign io_out_ID_fault_ID_fault_type = regs_ID_fault_ID_fault_type; // @[EXMEM.scala 64:10]
  assign io_out_ID_fault_mtval = regs_ID_fault_mtval; // @[EXMEM.scala 64:10]
  assign io_out_csr_val_to_reg = regs_csr_val_to_reg; // @[EXMEM.scala 64:10]
  assign io_out_unsigned = regs_unsigned; // @[EXMEM.scala 64:10]
  assign io_out_mem_signal_read_data_en = regs_mem_signal_read_data_en; // @[EXMEM.scala 64:10]
  assign io_out_mem_signal_write_data_en = regs_mem_signal_write_data_en; // @[EXMEM.scala 64:10]
  assign io_out_mem_signal_data_width = regs_mem_signal_data_width; // @[EXMEM.scala 64:10]
  assign io_out_mem_signal_data_to_write = regs_mem_signal_data_to_write; // @[EXMEM.scala 64:10]
  assign io_out_wb_signal_au_type = regs_wb_signal_au_type; // @[EXMEM.scala 64:10]
  assign io_out_wb_signal_wb_type = regs_wb_signal_wb_type; // @[EXMEM.scala 64:10]
  assign io_out_wb_signal_imm = regs_wb_signal_imm; // @[EXMEM.scala 64:10]
  assign io_out_wb_signal_rd = regs_wb_signal_rd; // @[EXMEM.scala 64:10]
  assign io_out_wb_signal_write_reg = regs_wb_signal_write_reg; // @[EXMEM.scala 64:10]
  assign io_out_wb_signal_csr_idx = regs_wb_signal_csr_idx; // @[EXMEM.scala 64:10]
  assign io_out_wb_signal_csr_write = regs_wb_signal_csr_write; // @[EXMEM.scala 64:10]
  assign io_out_alu_out = regs_alu_out; // @[EXMEM.scala 64:10]
  assign io_out_cmp_out = regs_cmp_out; // @[EXMEM.scala 64:10]
  always @(posedge clock) begin
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_pc <= 32'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        regs_pc <= io_in_pc;
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_pc <= _GEN_43;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_IF_fault_IF_fault_type <= 2'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        regs_IF_fault_IF_fault_type <= io_in_IF_fault_IF_fault_type;
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_IF_fault_IF_fault_type <= _GEN_40;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_IF_fault_mtval <= 32'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        regs_IF_fault_mtval <= io_in_IF_fault_mtval;
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_IF_fault_mtval <= _GEN_39;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_ID_fault_ID_fault_type <= 3'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        regs_ID_fault_ID_fault_type <= io_in_ID_fault_ID_fault_type;
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_ID_fault_ID_fault_type <= _GEN_38;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_ID_fault_mtval <= 32'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        regs_ID_fault_mtval <= io_in_ID_fault_mtval;
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_ID_fault_mtval <= _GEN_37;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_csr_val_to_reg <= 32'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_csr_val_to_reg <= 32'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_csr_val_to_reg <= io_in_csr_val_to_reg; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_csr_val_to_reg <= _GEN_36;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_unsigned <= 1'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_unsigned <= 1'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_unsigned <= io_in_unsigned; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_unsigned <= _GEN_35;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_mem_signal_read_data_en <= 1'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_mem_signal_read_data_en <= 1'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_mem_signal_read_data_en <= io_in_mem_signal_read_data_en; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_mem_signal_read_data_en <= _GEN_34;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_mem_signal_write_data_en <= 1'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_mem_signal_write_data_en <= 1'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_mem_signal_write_data_en <= io_in_mem_signal_write_data_en; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_mem_signal_write_data_en <= _GEN_33;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_mem_signal_data_width <= 2'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_mem_signal_data_width <= 2'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_mem_signal_data_width <= io_in_mem_signal_data_width; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_mem_signal_data_width <= _GEN_32;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_mem_signal_data_to_write <= 32'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_mem_signal_data_to_write <= 32'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_mem_signal_data_to_write <= io_in_mem_signal_data_to_write; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_mem_signal_data_to_write <= _GEN_31;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_wb_signal_au_type <= 1'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_wb_signal_au_type <= 1'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_wb_signal_au_type <= io_in_wb_signal_au_type; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_wb_signal_au_type <= _GEN_30;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_wb_signal_wb_type <= 3'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_wb_signal_wb_type <= 3'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_wb_signal_wb_type <= io_in_wb_signal_wb_type; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_wb_signal_wb_type <= _GEN_29;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_wb_signal_imm <= 32'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_wb_signal_imm <= 32'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_wb_signal_imm <= io_in_wb_signal_imm; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_wb_signal_imm <= _GEN_28;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_wb_signal_rd <= 5'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_wb_signal_rd <= 5'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_wb_signal_rd <= io_in_wb_signal_rd; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_wb_signal_rd <= _GEN_27;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_wb_signal_write_reg <= 1'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_wb_signal_write_reg <= 1'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_wb_signal_write_reg <= io_in_wb_signal_write_reg; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_wb_signal_write_reg <= _GEN_26;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_wb_signal_csr_idx <= 12'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_wb_signal_csr_idx <= 12'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_wb_signal_csr_idx <= io_in_wb_signal_csr_idx; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_wb_signal_csr_idx <= _GEN_25;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_wb_signal_csr_write <= 1'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_wb_signal_csr_write <= 1'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_wb_signal_csr_write <= io_in_wb_signal_csr_write; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_wb_signal_csr_write <= _GEN_24;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_alu_out <= 32'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_alu_out <= 32'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_alu_out <= io_in_alu_out; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_alu_out <= _GEN_23;
      end
    end
    if (reset) begin // @[EXMEM.scala 63:21]
      regs_cmp_out <= 1'h0; // @[EXMEM.scala 63:21]
    end else if (io_cpu_state == 2'h2) begin // @[EXMEM.scala 65:60]
      if (2'h0 == io_signal) begin // @[EXMEM.scala 66:23]
        if (_T_4) begin // @[EXMEM.scala 69:68]
          regs_cmp_out <= 1'h0; // @[EXMEM.scala 70:16]
        end else begin
          regs_cmp_out <= io_in_cmp_out; // @[EXMEM.scala 75:16]
        end
      end else if (!(2'h1 == io_signal)) begin // @[EXMEM.scala 66:23]
        regs_cmp_out <= _GEN_22;
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  regs_pc = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  regs_IF_fault_IF_fault_type = _RAND_1[1:0];
  _RAND_2 = {1{`RANDOM}};
  regs_IF_fault_mtval = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  regs_ID_fault_ID_fault_type = _RAND_3[2:0];
  _RAND_4 = {1{`RANDOM}};
  regs_ID_fault_mtval = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  regs_csr_val_to_reg = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  regs_unsigned = _RAND_6[0:0];
  _RAND_7 = {1{`RANDOM}};
  regs_mem_signal_read_data_en = _RAND_7[0:0];
  _RAND_8 = {1{`RANDOM}};
  regs_mem_signal_write_data_en = _RAND_8[0:0];
  _RAND_9 = {1{`RANDOM}};
  regs_mem_signal_data_width = _RAND_9[1:0];
  _RAND_10 = {1{`RANDOM}};
  regs_mem_signal_data_to_write = _RAND_10[31:0];
  _RAND_11 = {1{`RANDOM}};
  regs_wb_signal_au_type = _RAND_11[0:0];
  _RAND_12 = {1{`RANDOM}};
  regs_wb_signal_wb_type = _RAND_12[2:0];
  _RAND_13 = {1{`RANDOM}};
  regs_wb_signal_imm = _RAND_13[31:0];
  _RAND_14 = {1{`RANDOM}};
  regs_wb_signal_rd = _RAND_14[4:0];
  _RAND_15 = {1{`RANDOM}};
  regs_wb_signal_write_reg = _RAND_15[0:0];
  _RAND_16 = {1{`RANDOM}};
  regs_wb_signal_csr_idx = _RAND_16[11:0];
  _RAND_17 = {1{`RANDOM}};
  regs_wb_signal_csr_write = _RAND_17[0:0];
  _RAND_18 = {1{`RANDOM}};
  regs_alu_out = _RAND_18[31:0];
  _RAND_19 = {1{`RANDOM}};
  regs_cmp_out = _RAND_19[0:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module DualPortRAM(
  input         clock,
  input         io_write,
  input  [13:0] io_write_addr,
  input  [31:0] io_write_data,
  output [31:0] io_read_data,
  input  [13:0] io2_read_addr,
  output [31:0] io2_read_data
);
  wire  ip_mem_clk; // @[MemoryPacket.scala 63:24]
  wire [13:0] ip_mem_a; // @[MemoryPacket.scala 63:24]
  wire [31:0] ip_mem_d; // @[MemoryPacket.scala 63:24]
  wire [13:0] ip_mem_dpra; // @[MemoryPacket.scala 63:24]
  wire  ip_mem_we; // @[MemoryPacket.scala 63:24]
  wire [31:0] ip_mem_spo; // @[MemoryPacket.scala 63:24]
  wire [31:0] ip_mem_dpo; // @[MemoryPacket.scala 63:24]
  ins_mem ip_mem ( // @[MemoryPacket.scala 63:24]
    .clk(ip_mem_clk),
    .a(ip_mem_a),
    .d(ip_mem_d),
    .dpra(ip_mem_dpra),
    .we(ip_mem_we),
    .spo(ip_mem_spo),
    .dpo(ip_mem_dpo)
  );
  assign io_read_data = ip_mem_spo; // @[MemoryPacket.scala 69:18]
  assign io2_read_data = ip_mem_dpo; // @[MemoryPacket.scala 70:19]
  assign ip_mem_clk = clock; // @[MemoryPacket.scala 64:19]
  assign ip_mem_a = io_write ? io_write_addr : io2_read_addr; // @[MemoryPacket.scala 65:23]
  assign ip_mem_d = io_write_data; // @[MemoryPacket.scala 66:17]
  assign ip_mem_dpra = io2_read_addr; // @[MemoryPacket.scala 67:20]
  assign ip_mem_we = io_write; // @[MemoryPacket.scala 68:18]
endmodule
module InsRAM(
  input         clock,
  input         io_write,
  input  [31:0] io_write_addr,
  input  [31:0] io_write_data,
  output [31:0] io_read_data,
  input  [31:0] io2_read_addr,
  output [31:0] io2_read_data
);
  wire  insRAM_clock; // @[InsRAM.scala 17:22]
  wire  insRAM_io_write; // @[InsRAM.scala 17:22]
  wire [13:0] insRAM_io_write_addr; // @[InsRAM.scala 17:22]
  wire [31:0] insRAM_io_write_data; // @[InsRAM.scala 17:22]
  wire [31:0] insRAM_io_read_data; // @[InsRAM.scala 17:22]
  wire [13:0] insRAM_io2_read_addr; // @[InsRAM.scala 17:22]
  wire [31:0] insRAM_io2_read_data; // @[InsRAM.scala 17:22]
  DualPortRAM insRAM ( // @[InsRAM.scala 17:22]
    .clock(insRAM_clock),
    .io_write(insRAM_io_write),
    .io_write_addr(insRAM_io_write_addr),
    .io_write_data(insRAM_io_write_data),
    .io_read_data(insRAM_io_read_data),
    .io2_read_addr(insRAM_io2_read_addr),
    .io2_read_data(insRAM_io2_read_data)
  );
  assign io_read_data = insRAM_io_read_data; // @[InsRAM.scala 24:6]
  assign io2_read_data = insRAM_io2_read_data; // @[InsRAM.scala 25:7]
  assign insRAM_clock = clock;
  assign insRAM_io_write = io_write; // @[InsRAM.scala 24:6]
  assign insRAM_io_write_addr = io_write_addr[13:0]; // @[InsRAM.scala 24:6]
  assign insRAM_io_write_data = io_write_data; // @[InsRAM.scala 24:6]
  assign insRAM_io2_read_addr = io2_read_addr[13:0]; // @[InsRAM.scala 25:7]
endmodule
module RAM(
  input         clock,
  input         io_write,
  input  [13:0] io_read_addr,
  input  [13:0] io_write_addr,
  input  [31:0] io_write_data,
  output [31:0] io_read_data
);
  wire  ip_ram_clk; // @[MemoryPacket.scala 31:24]
  wire [31:0] ip_ram_a; // @[MemoryPacket.scala 31:24]
  wire [31:0] ip_ram_d; // @[MemoryPacket.scala 31:24]
  wire  ip_ram_we; // @[MemoryPacket.scala 31:24]
  wire [31:0] ip_ram_spo; // @[MemoryPacket.scala 31:24]
  wire [13:0] _ip_ram_io_a_T = io_write ? io_write_addr : io_read_addr; // @[MemoryPacket.scala 33:23]
  data_mem ip_ram ( // @[MemoryPacket.scala 31:24]
    .clk(ip_ram_clk),
    .a(ip_ram_a),
    .d(ip_ram_d),
    .we(ip_ram_we),
    .spo(ip_ram_spo)
  );
  assign io_read_data = ip_ram_spo; // @[MemoryPacket.scala 36:18]
  assign ip_ram_clk = clock; // @[MemoryPacket.scala 32:19]
  assign ip_ram_a = {{18'd0}, _ip_ram_io_a_T}; // @[MemoryPacket.scala 33:17]
  assign ip_ram_d = io_write_data; // @[MemoryPacket.scala 34:17]
  assign ip_ram_we = io_write; // @[MemoryPacket.scala 35:18]
endmodule
module DataRAM(
  input         clock,
  input         io_write,
  input  [31:0] io_read_addr,
  input  [31:0] io_write_addr,
  input  [31:0] io_write_data,
  output [31:0] io_read_data
);
  wire  dataRAM_clock; // @[DataRAM.scala 12:23]
  wire  dataRAM_io_write; // @[DataRAM.scala 12:23]
  wire [13:0] dataRAM_io_read_addr; // @[DataRAM.scala 12:23]
  wire [13:0] dataRAM_io_write_addr; // @[DataRAM.scala 12:23]
  wire [31:0] dataRAM_io_write_data; // @[DataRAM.scala 12:23]
  wire [31:0] dataRAM_io_read_data; // @[DataRAM.scala 12:23]
  RAM dataRAM ( // @[DataRAM.scala 12:23]
    .clock(dataRAM_clock),
    .io_write(dataRAM_io_write),
    .io_read_addr(dataRAM_io_read_addr),
    .io_write_addr(dataRAM_io_write_addr),
    .io_write_data(dataRAM_io_write_data),
    .io_read_data(dataRAM_io_read_data)
  );
  assign io_read_data = dataRAM_io_read_data; // @[DataRAM.scala 18:7]
  assign dataRAM_clock = clock;
  assign dataRAM_io_write = io_write; // @[DataRAM.scala 18:7]
  assign dataRAM_io_read_addr = io_read_addr[13:0]; // @[DataRAM.scala 18:7]
  assign dataRAM_io_write_addr = io_write_addr[13:0]; // @[DataRAM.scala 18:7]
  assign dataRAM_io_write_data = io_write_data; // @[DataRAM.scala 18:7]
endmodule
module OutRegisters(
  input         clock,
  input         reset,
  input         io_mem_write,
  input  [31:0] io_mem_read_addr,
  input  [31:0] io_mem_write_addr,
  input  [31:0] io_mem_write_data,
  output [31:0] io_mem_read_data,
  output [31:0] io_external_led_led,
  output [31:0] io_external_seg7_seg7,
  input  [31:0] io_external_btn_button,
  input  [31:0] io_external_switches_switches,
  input  [7:0]  io_external_uart_rxData,
  input         io_external_uart_rxValid,
  output        io_external_uart_rxReady,
  output [7:0]  io_external_uart_txData,
  output        io_external_uart_txStart,
  input         io_external_uart_txReady
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] btn; // @[OutRegisters.scala 25:20]
  reg [31:0] switches; // @[OutRegisters.scala 28:25]
  reg [31:0] led; // @[OutRegisters.scala 32:20]
  reg [31:0] seg7; // @[OutRegisters.scala 35:21]
  reg [31:0] rxData; // @[OutRegisters.scala 40:23]
  reg [31:0] rxVlaid; // @[OutRegisters.scala 43:24]
  reg [31:0] rxReady; // @[OutRegisters.scala 46:24]
  reg [31:0] txData; // @[OutRegisters.scala 49:23]
  reg [31:0] txStart; // @[OutRegisters.scala 52:24]
  reg [31:0] txReady; // @[OutRegisters.scala 55:24]
  reg [31:0] vga; // @[OutRegisters.scala 59:20]
  wire [31:0] addr = io_mem_write ? io_mem_write_addr : io_mem_read_addr; // @[OutRegisters.scala 62:23]
  wire  _T_1 = addr == 32'h3fffffc1; // @[OutRegisters.scala 65:13]
  wire  _T_3 = ~reset; // @[OutRegisters.scala 67:13]
  wire  _T_5 = addr == 32'h3fffffc2; // @[OutRegisters.scala 71:19]
  wire  _T_9 = addr == 32'h3fffffc0; // @[OutRegisters.scala 77:21]
  wire [31:0] _GEN_2 = io_mem_write ? io_mem_write_data : led; // @[OutRegisters.scala 78:26 79:13 32:20]
  wire  _T_11 = addr == 32'h3fffffc3; // @[OutRegisters.scala 83:21]
  wire [31:0] _GEN_4 = io_mem_write ? io_mem_write_data : seg7; // @[OutRegisters.scala 84:26 85:14 35:21]
  wire  _T_13 = addr == 32'h3fffffc5; // @[OutRegisters.scala 89:21]
  wire  _T_17 = addr == 32'h3fffffc6; // @[OutRegisters.scala 95:21]
  wire  _T_21 = addr == 32'h3fffffc7; // @[OutRegisters.scala 101:21]
  wire [31:0] _GEN_8 = io_mem_write ? io_mem_write_data : rxReady; // @[OutRegisters.scala 102:26 103:17 46:24]
  wire  _T_23 = addr == 32'h3fffffc8; // @[OutRegisters.scala 107:21]
  wire [31:0] _GEN_10 = io_mem_write ? io_mem_write_data : txData; // @[OutRegisters.scala 108:26 109:16 49:23]
  wire  _T_25 = addr == 32'h3fffffca; // @[OutRegisters.scala 113:21]
  wire [31:0] _GEN_12 = io_mem_write ? io_mem_write_data : txStart; // @[OutRegisters.scala 114:26 115:17 52:24]
  wire  _T_27 = addr == 32'h3fffffc9; // @[OutRegisters.scala 119:21]
  wire [31:0] _GEN_14 = io_mem_write ? io_mem_write_data : vga; // @[OutRegisters.scala 127:26 128:13 59:20]
  wire [31:0] _GEN_16 = addr == 32'h3fffffc4 ? _GEN_14 : vga; // @[OutRegisters.scala 126:68 59:20]
  wire [31:0] _GEN_18 = addr == 32'h3fffffc9 ? txReady : vga; // @[OutRegisters.scala 119:70]
  wire [31:0] _GEN_19 = addr == 32'h3fffffc9 ? vga : _GEN_16; // @[OutRegisters.scala 119:70 59:20]
  wire [31:0] _GEN_20 = addr == 32'h3fffffca ? _GEN_12 : txStart; // @[OutRegisters.scala 113:70 52:24]
  wire [31:0] _GEN_21 = addr == 32'h3fffffca ? txReady : _GEN_18; // @[OutRegisters.scala 113:70]
  wire [31:0] _GEN_22 = addr == 32'h3fffffca ? vga : _GEN_19; // @[OutRegisters.scala 113:70 59:20]
  wire [31:0] _GEN_23 = addr == 32'h3fffffc8 ? _GEN_10 : txData; // @[OutRegisters.scala 107:69 49:23]
  wire [31:0] _GEN_24 = addr == 32'h3fffffc8 ? txData : _GEN_21; // @[OutRegisters.scala 107:69]
  wire [31:0] _GEN_25 = addr == 32'h3fffffc8 ? txStart : _GEN_20; // @[OutRegisters.scala 107:69 52:24]
  wire [31:0] _GEN_26 = addr == 32'h3fffffc8 ? vga : _GEN_22; // @[OutRegisters.scala 107:69 59:20]
  wire [31:0] _GEN_27 = addr == 32'h3fffffc7 ? _GEN_8 : rxReady; // @[OutRegisters.scala 101:74 46:24]
  wire [31:0] _GEN_28 = addr == 32'h3fffffc7 ? rxReady : _GEN_24; // @[OutRegisters.scala 101:74]
  wire [31:0] _GEN_29 = addr == 32'h3fffffc7 ? txData : _GEN_23; // @[OutRegisters.scala 101:74 49:23]
  wire [31:0] _GEN_30 = addr == 32'h3fffffc7 ? txStart : _GEN_25; // @[OutRegisters.scala 101:74 52:24]
  wire [31:0] _GEN_31 = addr == 32'h3fffffc7 ? vga : _GEN_26; // @[OutRegisters.scala 101:74 59:20]
  wire [31:0] _GEN_32 = addr == 32'h3fffffc6 ? rxVlaid : _GEN_28; // @[OutRegisters.scala 95:74]
  wire [31:0] _GEN_33 = addr == 32'h3fffffc6 ? rxReady : _GEN_27; // @[OutRegisters.scala 46:24 95:74]
  wire [31:0] _GEN_34 = addr == 32'h3fffffc6 ? txData : _GEN_29; // @[OutRegisters.scala 49:23 95:74]
  wire [31:0] _GEN_35 = addr == 32'h3fffffc6 ? txStart : _GEN_30; // @[OutRegisters.scala 52:24 95:74]
  wire [31:0] _GEN_36 = addr == 32'h3fffffc6 ? vga : _GEN_31; // @[OutRegisters.scala 59:20 95:74]
  wire [31:0] _GEN_37 = addr == 32'h3fffffc5 ? rxData : _GEN_32; // @[OutRegisters.scala 89:69]
  wire [31:0] _GEN_38 = addr == 32'h3fffffc5 ? rxReady : _GEN_33; // @[OutRegisters.scala 46:24 89:69]
  wire [31:0] _GEN_39 = addr == 32'h3fffffc5 ? txData : _GEN_34; // @[OutRegisters.scala 49:23 89:69]
  wire [31:0] _GEN_40 = addr == 32'h3fffffc5 ? txStart : _GEN_35; // @[OutRegisters.scala 52:24 89:69]
  wire [31:0] _GEN_41 = addr == 32'h3fffffc5 ? vga : _GEN_36; // @[OutRegisters.scala 59:20 89:69]
  wire [31:0] _GEN_42 = addr == 32'h3fffffc3 ? _GEN_4 : seg7; // @[OutRegisters.scala 35:21 83:67]
  wire [31:0] _GEN_43 = addr == 32'h3fffffc3 ? seg7 : _GEN_37; // @[OutRegisters.scala 83:67]
  wire [31:0] _GEN_44 = addr == 32'h3fffffc3 ? rxReady : _GEN_38; // @[OutRegisters.scala 46:24 83:67]
  wire [31:0] _GEN_45 = addr == 32'h3fffffc3 ? txData : _GEN_39; // @[OutRegisters.scala 49:23 83:67]
  wire [31:0] _GEN_46 = addr == 32'h3fffffc3 ? txStart : _GEN_40; // @[OutRegisters.scala 52:24 83:67]
  wire [31:0] _GEN_47 = addr == 32'h3fffffc3 ? vga : _GEN_41; // @[OutRegisters.scala 59:20 83:67]
  wire [31:0] _GEN_49 = addr == 32'h3fffffc0 ? led : _GEN_43; // @[OutRegisters.scala 77:66]
  wire [31:0] _GEN_55 = addr == 32'h3fffffc2 ? switches : _GEN_49; // @[OutRegisters.scala 71:67]
  wire  _GEN_70 = ~_T_1; // @[OutRegisters.scala 73:15]
  wire  _GEN_79 = _GEN_70 & ~_T_5 & ~_T_9 & ~_T_11; // @[OutRegisters.scala 91:15]
  wire  _GEN_90 = _GEN_79 & ~_T_13; // @[OutRegisters.scala 97:15]
  assign io_mem_read_data = addr == 32'h3fffffc1 ? btn : _GEN_55; // @[OutRegisters.scala 65:58]
  assign io_external_led_led = led; // @[OutRegisters.scala 33:23]
  assign io_external_seg7_seg7 = seg7; // @[OutRegisters.scala 36:25]
  assign io_external_uart_rxReady = rxReady[0]; // @[OutRegisters.scala 47:28]
  assign io_external_uart_txData = txData[7:0]; // @[OutRegisters.scala 50:27]
  assign io_external_uart_txStart = txStart[0]; // @[OutRegisters.scala 53:28]
  always @(posedge clock) begin
    if (reset) begin // @[OutRegisters.scala 25:20]
      btn <= 32'h0; // @[OutRegisters.scala 25:20]
    end else begin
      btn <= io_external_btn_button; // @[OutRegisters.scala 26:7]
    end
    if (reset) begin // @[OutRegisters.scala 28:25]
      switches <= 32'h0; // @[OutRegisters.scala 28:25]
    end else begin
      switches <= io_external_switches_switches; // @[OutRegisters.scala 29:12]
    end
    if (reset) begin // @[OutRegisters.scala 32:20]
      led <= 32'h0; // @[OutRegisters.scala 32:20]
    end else if (!(addr == 32'h3fffffc1)) begin // @[OutRegisters.scala 65:58]
      if (!(addr == 32'h3fffffc2)) begin // @[OutRegisters.scala 71:67]
        if (addr == 32'h3fffffc0) begin // @[OutRegisters.scala 77:66]
          led <= _GEN_2;
        end
      end
    end
    if (reset) begin // @[OutRegisters.scala 35:21]
      seg7 <= 32'h0; // @[OutRegisters.scala 35:21]
    end else if (!(addr == 32'h3fffffc1)) begin // @[OutRegisters.scala 65:58]
      if (!(addr == 32'h3fffffc2)) begin // @[OutRegisters.scala 71:67]
        if (!(addr == 32'h3fffffc0)) begin // @[OutRegisters.scala 77:66]
          seg7 <= _GEN_42;
        end
      end
    end
    if (reset) begin // @[OutRegisters.scala 40:23]
      rxData <= 32'h0; // @[OutRegisters.scala 40:23]
    end else begin
      rxData <= {{24'd0}, io_external_uart_rxData}; // @[OutRegisters.scala 41:10]
    end
    if (reset) begin // @[OutRegisters.scala 43:24]
      rxVlaid <= 32'h0; // @[OutRegisters.scala 43:24]
    end else begin
      rxVlaid <= {{31'd0}, io_external_uart_rxValid}; // @[OutRegisters.scala 44:11]
    end
    if (reset) begin // @[OutRegisters.scala 46:24]
      rxReady <= 32'h0; // @[OutRegisters.scala 46:24]
    end else if (!(addr == 32'h3fffffc1)) begin // @[OutRegisters.scala 65:58]
      if (!(addr == 32'h3fffffc2)) begin // @[OutRegisters.scala 71:67]
        if (!(addr == 32'h3fffffc0)) begin // @[OutRegisters.scala 77:66]
          rxReady <= _GEN_44;
        end
      end
    end
    if (reset) begin // @[OutRegisters.scala 49:23]
      txData <= 32'h0; // @[OutRegisters.scala 49:23]
    end else if (!(addr == 32'h3fffffc1)) begin // @[OutRegisters.scala 65:58]
      if (!(addr == 32'h3fffffc2)) begin // @[OutRegisters.scala 71:67]
        if (!(addr == 32'h3fffffc0)) begin // @[OutRegisters.scala 77:66]
          txData <= _GEN_45;
        end
      end
    end
    if (reset) begin // @[OutRegisters.scala 52:24]
      txStart <= 32'h0; // @[OutRegisters.scala 52:24]
    end else if (!(addr == 32'h3fffffc1)) begin // @[OutRegisters.scala 65:58]
      if (!(addr == 32'h3fffffc2)) begin // @[OutRegisters.scala 71:67]
        if (!(addr == 32'h3fffffc0)) begin // @[OutRegisters.scala 77:66]
          txStart <= _GEN_46;
        end
      end
    end
    if (reset) begin // @[OutRegisters.scala 55:24]
      txReady <= 32'h0; // @[OutRegisters.scala 55:24]
    end else begin
      txReady <= {{31'd0}, io_external_uart_txReady}; // @[OutRegisters.scala 56:11]
    end
    if (reset) begin // @[OutRegisters.scala 59:20]
      vga <= 32'h0; // @[OutRegisters.scala 59:20]
    end else if (!(addr == 32'h3fffffc1)) begin // @[OutRegisters.scala 65:58]
      if (!(addr == 32'h3fffffc2)) begin // @[OutRegisters.scala 71:67]
        if (!(addr == 32'h3fffffc0)) begin // @[OutRegisters.scala 77:66]
          vga <= _GEN_47;
        end
      end
    end
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_1 & io_mem_write & ~reset) begin
          $fwrite(32'h80000002,"btn cannot write"); // @[OutRegisters.scala 67:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~_T_1 & _T_5 & io_mem_write & _T_3) begin
          $fwrite(32'h80000002,"switches cannot write"); // @[OutRegisters.scala 73:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_70 & ~_T_5 & ~_T_9 & ~_T_11 & _T_13 & io_mem_write & _T_3) begin
          $fwrite(32'h80000002,"UART cannot write"); // @[OutRegisters.scala 91:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_79 & ~_T_13 & _T_17 & io_mem_write & _T_3) begin
          $fwrite(32'h80000002,"UART cannot write"); // @[OutRegisters.scala 97:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_90 & ~_T_17 & ~_T_21 & ~_T_23 & ~_T_25 & _T_27 & io_mem_write & _T_3) begin
          $fwrite(32'h80000002,"UART cannot write"); // @[OutRegisters.scala 121:15]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  btn = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  switches = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  led = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  seg7 = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  rxData = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  rxVlaid = _RAND_5[31:0];
  _RAND_6 = {1{`RANDOM}};
  rxReady = _RAND_6[31:0];
  _RAND_7 = {1{`RANDOM}};
  txData = _RAND_7[31:0];
  _RAND_8 = {1{`RANDOM}};
  txStart = _RAND_8[31:0];
  _RAND_9 = {1{`RANDOM}};
  txReady = _RAND_9[31:0];
  _RAND_10 = {1{`RANDOM}};
  vga = _RAND_10[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module MemoryDispatch(
  input         clock,
  input         reset,
  input         io_uart_load,
  input  [1:0]  io_cpu_state,
  input  [31:0] io_ins_addr,
  output [31:0] io_ins_out,
  input         io_read_data,
  input         io_write_data,
  input         io_unsigned,
  input  [1:0]  io_data_width,
  input  [31:0] io_data_addr,
  input  [31:0] io_data_write,
  output [31:0] io_data_out,
  output [31:0] io_external_led_led,
  output [31:0] io_external_seg7_seg7,
  input  [31:0] io_external_btn_button,
  input  [31:0] io_external_switches_switches,
  input  [7:0]  io_external_uart_rxData,
  input         io_external_uart_rxValid,
  output        io_external_uart_rxReady,
  output [7:0]  io_external_uart_txData,
  output        io_external_uart_txStart,
  input         io_external_uart_txReady,
  output [1:0]  io_IF_fault_IF_fault_type,
  output [31:0] io_IF_fault_mtval,
  output [2:0]  io_MEM_fault_mem_fault_type,
  output [31:0] io_MEM_fault_mtval
);
  wire  insRAM_clock; // @[MemoryDispatch.scala 50:22]
  wire  insRAM_io_write; // @[MemoryDispatch.scala 50:22]
  wire [31:0] insRAM_io_write_addr; // @[MemoryDispatch.scala 50:22]
  wire [31:0] insRAM_io_write_data; // @[MemoryDispatch.scala 50:22]
  wire [31:0] insRAM_io_read_data; // @[MemoryDispatch.scala 50:22]
  wire [31:0] insRAM_io2_read_addr; // @[MemoryDispatch.scala 50:22]
  wire [31:0] insRAM_io2_read_data; // @[MemoryDispatch.scala 50:22]
  wire  dataRAM_clock; // @[MemoryDispatch.scala 51:23]
  wire  dataRAM_io_write; // @[MemoryDispatch.scala 51:23]
  wire [31:0] dataRAM_io_read_addr; // @[MemoryDispatch.scala 51:23]
  wire [31:0] dataRAM_io_write_addr; // @[MemoryDispatch.scala 51:23]
  wire [31:0] dataRAM_io_write_data; // @[MemoryDispatch.scala 51:23]
  wire [31:0] dataRAM_io_read_data; // @[MemoryDispatch.scala 51:23]
  wire  outRegisters_clock; // @[MemoryDispatch.scala 52:28]
  wire  outRegisters_reset; // @[MemoryDispatch.scala 52:28]
  wire  outRegisters_io_mem_write; // @[MemoryDispatch.scala 52:28]
  wire [31:0] outRegisters_io_mem_read_addr; // @[MemoryDispatch.scala 52:28]
  wire [31:0] outRegisters_io_mem_write_addr; // @[MemoryDispatch.scala 52:28]
  wire [31:0] outRegisters_io_mem_write_data; // @[MemoryDispatch.scala 52:28]
  wire [31:0] outRegisters_io_mem_read_data; // @[MemoryDispatch.scala 52:28]
  wire [31:0] outRegisters_io_external_led_led; // @[MemoryDispatch.scala 52:28]
  wire [31:0] outRegisters_io_external_seg7_seg7; // @[MemoryDispatch.scala 52:28]
  wire [31:0] outRegisters_io_external_btn_button; // @[MemoryDispatch.scala 52:28]
  wire [31:0] outRegisters_io_external_switches_switches; // @[MemoryDispatch.scala 52:28]
  wire [7:0] outRegisters_io_external_uart_rxData; // @[MemoryDispatch.scala 52:28]
  wire  outRegisters_io_external_uart_rxValid; // @[MemoryDispatch.scala 52:28]
  wire  outRegisters_io_external_uart_rxReady; // @[MemoryDispatch.scala 52:28]
  wire [7:0] outRegisters_io_external_uart_txData; // @[MemoryDispatch.scala 52:28]
  wire  outRegisters_io_external_uart_txStart; // @[MemoryDispatch.scala 52:28]
  wire  outRegisters_io_external_uart_txReady; // @[MemoryDispatch.scala 52:28]
  wire [29:0] rw_mem_addr = io_data_addr[31:2]; // @[MemoryDispatch.scala 46:34]
  wire [29:0] read_ins_addr = io_ins_addr[31:2]; // @[MemoryDispatch.scala 47:35]
  wire  is_write_clk = io_cpu_state == 2'h1 | io_uart_load; // @[MemoryDispatch.scala 59:74]
  wire [1:0] _GEN_0 = io_ins_addr > 32'hffff ? 2'h2 : 2'h0; // @[MemoryDispatch.scala 40:29 93:48 94:31]
  wire  _T_4 = io_data_addr <= 32'hffff; // @[MemoryDispatch.scala 114:21]
  wire  _GEN_4 = io_write_data & (is_write_clk & io_write_data); // @[MemoryDispatch.scala 116:25 117:23 MemoryPacket.scala 17:16]
  wire  _T_6 = 32'h10000 <= io_data_addr; // @[MemoryDispatch.scala 122:36]
  wire  _T_8 = _T_6 & io_data_addr <= 32'h1ffff; // @[MemoryDispatch.scala 123:5]
  wire [31:0] _havard_mem_T_1 = io_data_addr - 32'h10000; // @[MemoryDispatch.scala 125:36]
  wire [29:0] havard_mem = _havard_mem_T_1[31:2]; // @[MemoryDispatch.scala 125:61]
  wire  _T_9 = 32'hffffff00 <= io_data_addr; // @[MemoryDispatch.scala 135:38]
  wire [31:0] _GEN_6 = outRegisters_io_mem_read_data; // @[MemoryDispatch.scala 137:14 140:30 141:16]
  wire [31:0] _GEN_8 = io_write_data ? outRegisters_io_mem_read_data : _GEN_6; // @[MemoryDispatch.scala 137:14 138:25]
  wire  _T_13 = ~reset; // @[MemoryDispatch.scala 150:13]
  wire [1:0] _GEN_9 = ~io_uart_load ? 2'h2 : 2'h0; // @[MemoryDispatch.scala 103:25 104:35 42:31]
  wire [2:0] _GEN_11 = ~io_uart_load ? 3'h4 : 3'h0; // @[MemoryDispatch.scala 103:25 104:35 42:31]
  wire [2:0] _GEN_13 = io_write_data ? _GEN_11 : 3'h0; // @[MemoryDispatch.scala 152:31 42:31]
  wire [2:0] _GEN_15 = io_read_data ? {{1'd0}, _GEN_9} : _GEN_13; // @[MemoryDispatch.scala 149:24]
  wire  _GEN_18 = _T_9 & _GEN_4; // @[MemoryDispatch.scala 136:47 MemoryPacket.scala 17:16]
  wire [2:0] _GEN_19 = _T_9 ? 3'h0 : _GEN_15; // @[MemoryDispatch.scala 136:47 42:31]
  wire [29:0] _GEN_21 = _T_6 & io_data_addr <= 32'h1ffff ? havard_mem : rw_mem_addr; // @[MemoryDispatch.scala 123:45 126:26 MemoryPacket.scala 18:20]
  wire [31:0] _GEN_22 = _T_6 & io_data_addr <= 32'h1ffff ? dataRAM_io_read_data : _GEN_8; // @[MemoryDispatch.scala 123:45 128:14]
  wire  _GEN_23 = _T_6 & io_data_addr <= 32'h1ffff & _GEN_4; // @[MemoryDispatch.scala 123:45 MemoryPacket.scala 17:16]
  wire  _GEN_24 = _T_6 & io_data_addr <= 32'h1ffff ? 1'h0 : _GEN_18; // @[MemoryDispatch.scala 123:45 MemoryPacket.scala 17:16]
  wire [2:0] _GEN_25 = _T_6 & io_data_addr <= 32'h1ffff ? 3'h0 : _GEN_19; // @[MemoryDispatch.scala 123:45 42:31]
  wire [31:0] data_out = io_data_addr <= 32'hffff ? insRAM_io_read_data : _GEN_22; // @[MemoryDispatch.scala 114:44 115:14]
  wire [29:0] _GEN_29 = io_data_addr <= 32'hffff ? rw_mem_addr : _GEN_21; // @[MemoryDispatch.scala 114:44 MemoryPacket.scala 18:20]
  wire [2:0] _GEN_32 = io_data_addr <= 32'hffff ? 3'h0 : _GEN_25; // @[MemoryDispatch.scala 114:44 42:31]
  wire  _T_18 = 2'h0 == io_data_width; // @[MemoryDispatch.scala 161:25]
  wire [7:0] value = io_data_write[7:0]; // @[MemoryDispatch.scala 163:32]
  wire  _T_20 = 2'h0 == io_data_addr[1:0]; // @[MemoryDispatch.scala 164:34]
  wire [31:0] _data_in_T_1 = {data_out[31:8],value}; // @[Cat.scala 33:92]
  wire  _T_21 = 2'h1 == io_data_addr[1:0]; // @[MemoryDispatch.scala 164:34]
  wire [31:0] _data_in_T_4 = {data_out[31:16],value,data_out[7:0]}; // @[Cat.scala 33:92]
  wire  _T_22 = 2'h2 == io_data_addr[1:0]; // @[MemoryDispatch.scala 164:34]
  wire [31:0] _data_in_T_7 = {data_out[31:24],value,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_9 = {value,data_out[23:0]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_35 = 2'h2 == io_data_addr[1:0] ? _data_in_T_7 : _data_in_T_9; // @[MemoryDispatch.scala 164:34 172:19]
  wire [31:0] _GEN_36 = 2'h1 == io_data_addr[1:0] ? _data_in_T_4 : _GEN_35; // @[MemoryDispatch.scala 164:34 169:19]
  wire [31:0] _GEN_37 = 2'h0 == io_data_addr[1:0] ? _data_in_T_1 : _GEN_36; // @[MemoryDispatch.scala 164:34 166:19]
  wire  _T_24 = 2'h1 == io_data_width; // @[MemoryDispatch.scala 161:25]
  wire [15:0] value_1 = io_data_write[15:0]; // @[MemoryDispatch.scala 180:32]
  wire [31:0] _data_in_T_11 = {data_out[31:16],value_1}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_13 = {value_1,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_38 = ~io_data_addr[1] ? _data_in_T_11 : _data_in_T_13; // @[MemoryDispatch.scala 181:40 182:17 184:17]
  wire  _T_27 = 2'h2 == io_data_width; // @[MemoryDispatch.scala 161:25]
  wire [31:0] _GEN_40 = 2'h1 == io_data_width ? _GEN_38 : io_data_write; // @[MemoryDispatch.scala 161:25]
  wire  _high_bit_T_1 = io_unsigned ? 1'h0 : data_out[7]; // @[MemoryDispatch.scala 199:38]
  wire [23:0] high_bit = _high_bit_T_1 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_1 = {high_bit,data_out[7:0]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_4 = io_unsigned ? 1'h0 : data_out[15]; // @[MemoryDispatch.scala 203:38]
  wire [23:0] high_bit_1 = _high_bit_T_4 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_3 = {high_bit_1,data_out[15:8]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_7 = io_unsigned ? 1'h0 : data_out[23]; // @[MemoryDispatch.scala 207:38]
  wire [23:0] high_bit_2 = _high_bit_T_7 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_5 = {high_bit_2,data_out[23:16]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_10 = io_unsigned ? 1'h0 : data_out[31]; // @[MemoryDispatch.scala 211:38]
  wire [23:0] high_bit_3 = _high_bit_T_10 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_7 = {high_bit_3,data_out[31:24]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_43 = _T_22 ? _io_data_out_T_5 : _io_data_out_T_7; // @[MemoryDispatch.scala 197:34 208:23]
  wire [31:0] _GEN_44 = _T_21 ? _io_data_out_T_3 : _GEN_43; // @[MemoryDispatch.scala 197:34 204:23]
  wire [31:0] _GEN_45 = _T_20 ? _io_data_out_T_1 : _GEN_44; // @[MemoryDispatch.scala 197:34 200:23]
  wire [15:0] high_bit_4 = _high_bit_T_4 ? 16'hffff : 16'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_9 = {high_bit_4,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [15:0] high_bit_5 = _high_bit_T_10 ? 16'hffff : 16'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_11 = {high_bit_5,data_out[31:16]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_47 = _T_20 ? _io_data_out_T_9 : _io_data_out_T_11; // @[MemoryDispatch.scala 219:36 222:25]
  wire [2:0] _GEN_48 = ~io_uart_load ? 3'h1 : _GEN_32; // @[MemoryDispatch.scala 103:25 104:35]
  wire [2:0] _GEN_50 = io_read_data ? _GEN_48 : _GEN_32; // @[MemoryDispatch.scala 230:28]
  wire [2:0] _GEN_52 = ~io_uart_load ? 3'h3 : _GEN_50; // @[MemoryDispatch.scala 103:25 104:35]
  wire [2:0] _GEN_54 = io_write_data ? _GEN_52 : _GEN_50; // @[MemoryDispatch.scala 233:29]
  wire [2:0] _GEN_57 = ~io_data_addr[0] ? _GEN_32 : _GEN_54; // @[MemoryDispatch.scala 218:37]
  wire [2:0] _GEN_68 = io_data_addr[1:0] == 2'h0 ? _GEN_32 : _GEN_54; // @[MemoryDispatch.scala 239:40]
  wire [2:0] _GEN_71 = _T_27 ? _GEN_68 : _GEN_32; // @[MemoryDispatch.scala 195:25]
  wire [31:0] _GEN_73 = _T_24 ? _GEN_47 : data_out; // @[MemoryDispatch.scala 195:25]
  wire [2:0] _GEN_74 = _T_24 ? _GEN_57 : _GEN_71; // @[MemoryDispatch.scala 195:25]
  wire  _GEN_83 = ~_T_4 & ~_T_8 & ~_T_9; // @[MemoryDispatch.scala 150:13]
  InsRAM insRAM ( // @[MemoryDispatch.scala 50:22]
    .clock(insRAM_clock),
    .io_write(insRAM_io_write),
    .io_write_addr(insRAM_io_write_addr),
    .io_write_data(insRAM_io_write_data),
    .io_read_data(insRAM_io_read_data),
    .io2_read_addr(insRAM_io2_read_addr),
    .io2_read_data(insRAM_io2_read_data)
  );
  DataRAM dataRAM ( // @[MemoryDispatch.scala 51:23]
    .clock(dataRAM_clock),
    .io_write(dataRAM_io_write),
    .io_read_addr(dataRAM_io_read_addr),
    .io_write_addr(dataRAM_io_write_addr),
    .io_write_data(dataRAM_io_write_data),
    .io_read_data(dataRAM_io_read_data)
  );
  OutRegisters outRegisters ( // @[MemoryDispatch.scala 52:28]
    .clock(outRegisters_clock),
    .reset(outRegisters_reset),
    .io_mem_write(outRegisters_io_mem_write),
    .io_mem_read_addr(outRegisters_io_mem_read_addr),
    .io_mem_write_addr(outRegisters_io_mem_write_addr),
    .io_mem_write_data(outRegisters_io_mem_write_data),
    .io_mem_read_data(outRegisters_io_mem_read_data),
    .io_external_led_led(outRegisters_io_external_led_led),
    .io_external_seg7_seg7(outRegisters_io_external_seg7_seg7),
    .io_external_btn_button(outRegisters_io_external_btn_button),
    .io_external_switches_switches(outRegisters_io_external_switches_switches),
    .io_external_uart_rxData(outRegisters_io_external_uart_rxData),
    .io_external_uart_rxValid(outRegisters_io_external_uart_rxValid),
    .io_external_uart_rxReady(outRegisters_io_external_uart_rxReady),
    .io_external_uart_txData(outRegisters_io_external_uart_txData),
    .io_external_uart_txStart(outRegisters_io_external_uart_txStart),
    .io_external_uart_txReady(outRegisters_io_external_uart_txReady)
  );
  assign io_ins_out = insRAM_io2_read_data; // @[MemoryDispatch.scala 63:14]
  assign io_data_out = _T_18 ? _GEN_45 : _GEN_73; // @[MemoryDispatch.scala 195:25]
  assign io_external_led_led = outRegisters_io_external_led_led; // @[MemoryDispatch.scala 87:28]
  assign io_external_seg7_seg7 = outRegisters_io_external_seg7_seg7; // @[MemoryDispatch.scala 87:28]
  assign io_external_uart_rxReady = outRegisters_io_external_uart_rxReady; // @[MemoryDispatch.scala 87:28]
  assign io_external_uart_txData = outRegisters_io_external_uart_txData; // @[MemoryDispatch.scala 87:28]
  assign io_external_uart_txStart = outRegisters_io_external_uart_txStart; // @[MemoryDispatch.scala 87:28]
  assign io_IF_fault_IF_fault_type = io_ins_addr[1:0] != 2'h0 ? 2'h1 : _GEN_0; // @[MemoryDispatch.scala 90:35 91:31]
  assign io_IF_fault_mtval = io_ins_addr; // @[MemoryDispatch.scala 90:35 92:23]
  assign io_MEM_fault_mem_fault_type = _T_18 ? _GEN_32 : _GEN_74; // @[MemoryDispatch.scala 195:25]
  assign io_MEM_fault_mtval = io_data_addr; // @[MemoryDispatch.scala 195:25]
  assign insRAM_clock = clock;
  assign insRAM_io_write = io_data_addr <= 32'hffff & _GEN_4; // @[MemoryDispatch.scala 114:44 MemoryPacket.scala 17:16]
  assign insRAM_io_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 19:21]
  assign insRAM_io_write_data = 2'h0 == io_data_width ? _GEN_37 : _GEN_40; // @[MemoryDispatch.scala 161:25]
  assign insRAM_io2_read_addr = {{2'd0}, read_ins_addr}; // @[MemoryDispatch.scala 62:24]
  assign dataRAM_clock = clock;
  assign dataRAM_io_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_23; // @[MemoryDispatch.scala 114:44 MemoryPacket.scala 17:16]
  assign dataRAM_io_read_addr = {{2'd0}, _GEN_29};
  assign dataRAM_io_write_addr = {{2'd0}, _GEN_29};
  assign dataRAM_io_write_data = 2'h0 == io_data_width ? _GEN_37 : _GEN_40; // @[MemoryDispatch.scala 161:25]
  assign outRegisters_clock = clock;
  assign outRegisters_reset = reset;
  assign outRegisters_io_mem_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_24; // @[MemoryDispatch.scala 114:44 MemoryPacket.scala 17:16]
  assign outRegisters_io_mem_read_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 18:20]
  assign outRegisters_io_mem_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 19:21]
  assign outRegisters_io_mem_write_data = 2'h0 == io_data_width ? _GEN_37 : _GEN_40; // @[MemoryDispatch.scala 161:25]
  assign outRegisters_io_external_btn_button = io_external_btn_button; // @[MemoryDispatch.scala 87:28]
  assign outRegisters_io_external_switches_switches = io_external_switches_switches; // @[MemoryDispatch.scala 87:28]
  assign outRegisters_io_external_uart_rxData = io_external_uart_rxData; // @[MemoryDispatch.scala 87:28]
  assign outRegisters_io_external_uart_rxValid = io_external_uart_rxValid; // @[MemoryDispatch.scala 87:28]
  assign outRegisters_io_external_uart_txReady = io_external_uart_txReady; // @[MemoryDispatch.scala 87:28]
  always @(posedge clock) begin
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~_T_4 & ~_T_8 & ~_T_9 & io_read_data & ~reset) begin
          $fwrite(32'h80000002,"Unexpected Mem address %x",io_data_addr); // @[MemoryDispatch.scala 150:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_83 & ~io_read_data & io_write_data & _T_13) begin
          $fwrite(32'h80000002,"Unexpected Mem address %x",io_data_addr); // @[MemoryDispatch.scala 153:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
endmodule
module UARTLoader(
  input         clock,
  input         reset,
  input         io_uart_load,
  input         io_rxValid,
  input  [7:0]  io_rxData,
  output        io_mem_mem_write,
  output [31:0] io_mem_data_to_write,
  output [31:0] io_mem_data_addr
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
`endif // RANDOMIZE_REG_INIT
  reg [1:0] cur_state; // @[UARTLoader.scala 34:26]
  reg [31:0] data_addr; // @[UARTLoader.scala 35:26]
  wire  _T_2 = ~io_uart_load; // @[UARTLoader.scala 57:12]
  wire [1:0] _GEN_2 = ~io_rxValid ? 2'h3 : cur_state; // @[UARTLoader.scala 60:38 61:21 34:26]
  wire [31:0] _data_addr_T_1 = data_addr + 32'h1; // @[UARTLoader.scala 71:34]
  wire [31:0] _GEN_4 = io_rxValid ? _data_addr_T_1 : data_addr; // @[UARTLoader.scala 70:37 71:21 35:26]
  wire [1:0] _GEN_5 = io_rxValid ? 2'h2 : cur_state; // @[UARTLoader.scala 70:37 72:21 34:26]
  wire [1:0] _GEN_6 = _T_2 ? 2'h0 : _GEN_5; // @[UARTLoader.scala 67:27 68:19]
  wire [31:0] _GEN_7 = _T_2 ? data_addr : _GEN_4; // @[UARTLoader.scala 35:26 67:27]
  wire [1:0] _GEN_9 = io_rxValid ? cur_state : 2'h3; // @[UARTLoader.scala 34:26 81:37 85:21]
  wire [1:0] _GEN_10 = _T_2 ? 2'h0 : _GEN_9; // @[UARTLoader.scala 78:27 79:19]
  wire  _GEN_11 = _T_2 ? 1'h0 : io_rxValid; // @[UARTLoader.scala 42:20 78:27]
  wire [1:0] _GEN_13 = 2'h2 == cur_state ? _GEN_10 : cur_state; // @[UARTLoader.scala 47:21 34:26]
  wire  _GEN_18 = 2'h3 == cur_state ? 1'h0 : 2'h2 == cur_state & _GEN_11; // @[UARTLoader.scala 42:20 47:21]
  wire  _GEN_22 = 2'h1 == cur_state ? 1'h0 : _GEN_18; // @[UARTLoader.scala 42:20 47:21]
  assign io_mem_mem_write = 2'h0 == cur_state ? 1'h0 : _GEN_22; // @[UARTLoader.scala 42:20 47:21]
  assign io_mem_data_to_write = {{24'd0}, io_rxData}; // @[UARTLoader.scala 40:24]
  assign io_mem_data_addr = data_addr; // @[UARTLoader.scala 38:20]
  always @(posedge clock) begin
    if (reset) begin // @[UARTLoader.scala 34:26]
      cur_state <= 2'h0; // @[UARTLoader.scala 34:26]
    end else if (2'h0 == cur_state) begin // @[UARTLoader.scala 47:21]
      if (io_uart_load) begin // @[UARTLoader.scala 50:26]
        cur_state <= 2'h1; // @[UARTLoader.scala 52:19]
      end
    end else if (2'h1 == cur_state) begin // @[UARTLoader.scala 47:21]
      if (~io_uart_load) begin // @[UARTLoader.scala 57:27]
        cur_state <= 2'h0; // @[UARTLoader.scala 58:19]
      end else begin
        cur_state <= _GEN_2;
      end
    end else if (2'h3 == cur_state) begin // @[UARTLoader.scala 47:21]
      cur_state <= _GEN_6;
    end else begin
      cur_state <= _GEN_13;
    end
    if (reset) begin // @[UARTLoader.scala 35:26]
      data_addr <= 32'hffffffff; // @[UARTLoader.scala 35:26]
    end else if (2'h0 == cur_state) begin // @[UARTLoader.scala 47:21]
      if (io_uart_load) begin // @[UARTLoader.scala 50:26]
        data_addr <= 32'hffffffff; // @[UARTLoader.scala 51:19]
      end
    end else if (!(2'h1 == cur_state)) begin // @[UARTLoader.scala 47:21]
      if (2'h3 == cur_state) begin // @[UARTLoader.scala 47:21]
        data_addr <= _GEN_7;
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  cur_state = _RAND_0[1:0];
  _RAND_1 = {1{`RANDOM}};
  data_addr = _RAND_1[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module MemWriteSelector(
  input         io_uart_load,
  input         io_uart_in_mem_write,
  input  [31:0] io_uart_in_data_to_write,
  input  [31:0] io_uart_in_data_addr,
  input         io_cpu_read_data,
  input         io_cpu_write_data,
  input  [1:0]  io_cpu_data_width,
  input  [31:0] io_cpu_data_addr,
  input  [31:0] io_cpu_data_write,
  output        io_read_data,
  output        io_write_data,
  output [1:0]  io_data_width,
  output [31:0] io_data_addr,
  output [31:0] io_data_write
);
  assign io_read_data = io_uart_load ? 1'h0 : io_cpu_read_data; // @[MemWriteSelector.scala 29:21 30:17 36:17]
  assign io_write_data = io_uart_load ? io_uart_in_mem_write : io_cpu_write_data; // @[MemWriteSelector.scala 29:21 31:18 37:18]
  assign io_data_width = io_uart_load ? 2'h0 : io_cpu_data_width; // @[MemWriteSelector.scala 29:21 32:18 38:18]
  assign io_data_addr = io_uart_load ? io_uart_in_data_addr : io_cpu_data_addr; // @[MemWriteSelector.scala 29:21 33:17 39:17]
  assign io_data_write = io_uart_load ? io_uart_in_data_to_write : io_cpu_data_write; // @[MemWriteSelector.scala 29:21 34:18 40:18]
endmodule
module MEMWB(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  input  [1:0]  io_signal,
  input  [31:0] io_in_pc,
  input  [31:0] io_in_csr_val_to_reg,
  input         io_in_wb_signal_au_type,
  input  [2:0]  io_in_wb_signal_wb_type,
  input  [31:0] io_in_wb_signal_imm,
  input  [4:0]  io_in_wb_signal_rd,
  input         io_in_wb_signal_write_reg,
  input  [11:0] io_in_wb_signal_csr_idx,
  input         io_in_wb_signal_csr_write,
  input  [31:0] io_in_alu_out,
  input         io_in_cmp_out,
  input  [31:0] io_in_data_out,
  output [31:0] io_out_pc,
  output [31:0] io_out_csr_val_to_reg,
  output        io_out_wb_signal_au_type,
  output [2:0]  io_out_wb_signal_wb_type,
  output [31:0] io_out_wb_signal_imm,
  output [4:0]  io_out_wb_signal_rd,
  output        io_out_wb_signal_write_reg,
  output [11:0] io_out_wb_signal_csr_idx,
  output        io_out_wb_signal_csr_write,
  output [31:0] io_out_alu_out,
  output        io_out_cmp_out,
  output [31:0] io_out_data_out
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
  reg [31:0] _RAND_6;
  reg [31:0] _RAND_7;
  reg [31:0] _RAND_8;
  reg [31:0] _RAND_9;
  reg [31:0] _RAND_10;
  reg [31:0] _RAND_11;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] regs_pc; // @[MEMWB.scala 45:20]
  reg [31:0] regs_csr_val_to_reg; // @[MEMWB.scala 45:20]
  reg  regs_wb_signal_au_type; // @[MEMWB.scala 45:20]
  reg [2:0] regs_wb_signal_wb_type; // @[MEMWB.scala 45:20]
  reg [31:0] regs_wb_signal_imm; // @[MEMWB.scala 45:20]
  reg [4:0] regs_wb_signal_rd; // @[MEMWB.scala 45:20]
  reg  regs_wb_signal_write_reg; // @[MEMWB.scala 45:20]
  reg [11:0] regs_wb_signal_csr_idx; // @[MEMWB.scala 45:20]
  reg  regs_wb_signal_csr_write; // @[MEMWB.scala 45:20]
  reg [31:0] regs_alu_out; // @[MEMWB.scala 45:20]
  reg  regs_cmp_out; // @[MEMWB.scala 45:20]
  reg [31:0] regs_data_out; // @[MEMWB.scala 45:20]
  wire [31:0] _GEN_0 = 2'h2 == io_signal ? 32'h0 : regs_data_out; // @[MEMWB.scala 48:23 56:14 45:20]
  wire  _GEN_1 = 2'h2 == io_signal ? 1'h0 : regs_cmp_out; // @[MEMWB.scala 48:23 56:14 45:20]
  wire [31:0] _GEN_2 = 2'h2 == io_signal ? 32'h0 : regs_alu_out; // @[MEMWB.scala 48:23 56:14 45:20]
  wire  _GEN_3 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_csr_write; // @[MEMWB.scala 48:23 56:14 45:20]
  wire [11:0] _GEN_4 = 2'h2 == io_signal ? 12'h0 : regs_wb_signal_csr_idx; // @[MEMWB.scala 48:23 56:14 45:20]
  wire  _GEN_5 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_write_reg; // @[MEMWB.scala 48:23 56:14 45:20]
  wire [4:0] _GEN_6 = 2'h2 == io_signal ? 5'h0 : regs_wb_signal_rd; // @[MEMWB.scala 48:23 56:14 45:20]
  wire [31:0] _GEN_7 = 2'h2 == io_signal ? 32'h0 : regs_wb_signal_imm; // @[MEMWB.scala 48:23 56:14 45:20]
  wire [2:0] _GEN_8 = 2'h2 == io_signal ? 3'h0 : regs_wb_signal_wb_type; // @[MEMWB.scala 48:23 56:14 45:20]
  wire  _GEN_9 = 2'h2 == io_signal ? 1'h0 : regs_wb_signal_au_type; // @[MEMWB.scala 48:23 56:14 45:20]
  wire [31:0] _GEN_10 = 2'h2 == io_signal ? 32'h0 : regs_csr_val_to_reg; // @[MEMWB.scala 48:23 56:14 45:20]
  wire [31:0] _GEN_12 = 2'h2 == io_signal ? 32'h0 : regs_pc; // @[MEMWB.scala 48:23 56:14 45:20]
  assign io_out_pc = regs_pc; // @[MEMWB.scala 46:9]
  assign io_out_csr_val_to_reg = regs_csr_val_to_reg; // @[MEMWB.scala 46:9]
  assign io_out_wb_signal_au_type = regs_wb_signal_au_type; // @[MEMWB.scala 46:9]
  assign io_out_wb_signal_wb_type = regs_wb_signal_wb_type; // @[MEMWB.scala 46:9]
  assign io_out_wb_signal_imm = regs_wb_signal_imm; // @[MEMWB.scala 46:9]
  assign io_out_wb_signal_rd = regs_wb_signal_rd; // @[MEMWB.scala 46:9]
  assign io_out_wb_signal_write_reg = regs_wb_signal_write_reg; // @[MEMWB.scala 46:9]
  assign io_out_wb_signal_csr_idx = regs_wb_signal_csr_idx; // @[MEMWB.scala 46:9]
  assign io_out_wb_signal_csr_write = regs_wb_signal_csr_write; // @[MEMWB.scala 46:9]
  assign io_out_alu_out = regs_alu_out; // @[MEMWB.scala 46:9]
  assign io_out_cmp_out = regs_cmp_out; // @[MEMWB.scala 46:9]
  assign io_out_data_out = regs_data_out; // @[MEMWB.scala 46:9]
  always @(posedge clock) begin
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_pc <= 32'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_pc <= io_in_pc; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_pc <= _GEN_12;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_csr_val_to_reg <= 32'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_csr_val_to_reg <= io_in_csr_val_to_reg; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_csr_val_to_reg <= _GEN_10;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_wb_signal_au_type <= 1'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_au_type <= io_in_wb_signal_au_type; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_au_type <= _GEN_9;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_wb_signal_wb_type <= 3'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_wb_type <= io_in_wb_signal_wb_type; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_wb_type <= _GEN_8;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_wb_signal_imm <= 32'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_imm <= io_in_wb_signal_imm; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_imm <= _GEN_7;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_wb_signal_rd <= 5'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_rd <= io_in_wb_signal_rd; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_rd <= _GEN_6;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_wb_signal_write_reg <= 1'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_write_reg <= io_in_wb_signal_write_reg; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_write_reg <= _GEN_5;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_wb_signal_csr_idx <= 12'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_csr_idx <= io_in_wb_signal_csr_idx; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_csr_idx <= _GEN_4;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_wb_signal_csr_write <= 1'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_csr_write <= io_in_wb_signal_csr_write; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_wb_signal_csr_write <= _GEN_3;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_alu_out <= 32'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_alu_out <= io_in_alu_out; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_alu_out <= _GEN_2;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_cmp_out <= 1'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_cmp_out <= io_in_cmp_out; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_cmp_out <= _GEN_1;
      end
    end
    if (reset) begin // @[MEMWB.scala 45:20]
      regs_data_out <= 32'h0; // @[MEMWB.scala 45:20]
    end else if (io_cpu_state == 2'h2) begin // @[MEMWB.scala 47:60]
      if (2'h0 == io_signal) begin // @[MEMWB.scala 48:23]
        regs_data_out <= io_in_data_out; // @[MEMWB.scala 50:14]
      end else if (!(2'h1 == io_signal)) begin // @[MEMWB.scala 48:23]
        regs_data_out <= _GEN_0;
      end
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  regs_pc = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  regs_csr_val_to_reg = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  regs_wb_signal_au_type = _RAND_2[0:0];
  _RAND_3 = {1{`RANDOM}};
  regs_wb_signal_wb_type = _RAND_3[2:0];
  _RAND_4 = {1{`RANDOM}};
  regs_wb_signal_imm = _RAND_4[31:0];
  _RAND_5 = {1{`RANDOM}};
  regs_wb_signal_rd = _RAND_5[4:0];
  _RAND_6 = {1{`RANDOM}};
  regs_wb_signal_write_reg = _RAND_6[0:0];
  _RAND_7 = {1{`RANDOM}};
  regs_wb_signal_csr_idx = _RAND_7[11:0];
  _RAND_8 = {1{`RANDOM}};
  regs_wb_signal_csr_write = _RAND_8[0:0];
  _RAND_9 = {1{`RANDOM}};
  regs_alu_out = _RAND_9[31:0];
  _RAND_10 = {1{`RANDOM}};
  regs_cmp_out = _RAND_10[0:0];
  _RAND_11 = {1{`RANDOM}};
  regs_data_out = _RAND_11[31:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module CoreTop(
  input         clock,
  input         reset,
  output [31:0] io_external_led_led,
  output [31:0] io_external_seg7_seg7,
  input  [31:0] io_external_btn_button,
  input  [31:0] io_external_switches_switches,
  input  [7:0]  io_external_uart_rxData,
  input         io_external_uart_rxValid,
  output        io_external_uart_rxReady,
  output [7:0]  io_external_uart_txData,
  output        io_external_uart_txStart,
  input         io_external_uart_txReady,
  input         io_external_signal_load_data_mode
);
  wire  conflictController_io_uart_loading; // @[CoreTop.scala 22:34]
  wire [4:0] conflictController_io_rs1_use; // @[CoreTop.scala 22:34]
  wire [4:0] conflictController_io_rs2_use; // @[CoreTop.scala 22:34]
  wire [11:0] conflictController_io_csr_use; // @[CoreTop.scala 22:34]
  wire [4:0] conflictController_io_IDEX_reg_to_write; // @[CoreTop.scala 22:34]
  wire  conflictController_io_IDEX_write_reg; // @[CoreTop.scala 22:34]
  wire [11:0] conflictController_io_IDEX_csr_to_write; // @[CoreTop.scala 22:34]
  wire  conflictController_io_IDEX_write_csr; // @[CoreTop.scala 22:34]
  wire  conflictController_io_IDEX_reg_result_stage; // @[CoreTop.scala 22:34]
  wire [4:0] conflictController_io_EXMEM_reg_to_write; // @[CoreTop.scala 22:34]
  wire  conflictController_io_EXMEM_write_reg; // @[CoreTop.scala 22:34]
  wire [11:0] conflictController_io_EXMEM_csr_to_write; // @[CoreTop.scala 22:34]
  wire  conflictController_io_EXMEM_write_csr; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_rs1_forward_type; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_rs2_forward_type; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_csr_forward_type; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_branch_type; // @[CoreTop.scala 22:34]
  wire [31:0] conflictController_io_pc; // @[CoreTop.scala 22:34]
  wire [31:0] conflictController_io_imm; // @[CoreTop.scala 22:34]
  wire [31:0] conflictController_io_alu_result; // @[CoreTop.scala 22:34]
  wire  conflictController_io_cmp_result; // @[CoreTop.scala 22:34]
  wire [31:0] conflictController_io_predict_next_pc; // @[CoreTop.scala 22:34]
  wire  conflictController_io_exception_occurs; // @[CoreTop.scala 22:34]
  wire [31:0] conflictController_io_exception_new_pc; // @[CoreTop.scala 22:34]
  wire [31:0] conflictController_io_new_pc; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_next_control_signal; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_IFID_control_signal; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_IDEX_control_signal; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_EXMEM_control_signal; // @[CoreTop.scala 22:34]
  wire [1:0] conflictController_io_MEMWB_control_signal; // @[CoreTop.scala 22:34]
  wire  conflictController_io_prediction_failure_failure_occurs; // @[CoreTop.scala 22:34]
  wire  conflictController_io_prediction_failure_success_occurs; // @[CoreTop.scala 22:34]
  wire [31:0] conflictController_io_prediction_failure_address; // @[CoreTop.scala 22:34]
  wire  state_clock; // @[CoreTop.scala 23:21]
  wire  state_reset; // @[CoreTop.scala 23:21]
  wire  state_io_stall; // @[CoreTop.scala 23:21]
  wire [1:0] state_io_cpu_state; // @[CoreTop.scala 23:21]
  wire  CSR_clock; // @[CoreTop.scala 26:19]
  wire  CSR_reset; // @[CoreTop.scala 26:19]
  wire [1:0] CSR_io_cpu_state; // @[CoreTop.scala 26:19]
  wire [1:0] CSR_io_cur_privilege; // @[CoreTop.scala 26:19]
  wire  CSR_io_write; // @[CoreTop.scala 26:19]
  wire [11:0] CSR_io_csr_idx_write; // @[CoreTop.scala 26:19]
  wire [31:0] CSR_io_write_data; // @[CoreTop.scala 26:19]
  wire [11:0] CSR_io_csr_idx_read; // @[CoreTop.scala 26:19]
  wire [31:0] CSR_io_csr_val; // @[CoreTop.scala 26:19]
  wire [1:0] CSR_io_IF_fault_IF_fault_type; // @[CoreTop.scala 26:19]
  wire [31:0] CSR_io_IF_fault_mtval; // @[CoreTop.scala 26:19]
  wire [2:0] CSR_io_ID_fault_ID_fault_type; // @[CoreTop.scala 26:19]
  wire [31:0] CSR_io_ID_fault_mtval; // @[CoreTop.scala 26:19]
  wire [2:0] CSR_io_MEM_fault_mem_fault_type; // @[CoreTop.scala 26:19]
  wire [31:0] CSR_io_MEM_fault_mtval; // @[CoreTop.scala 26:19]
  wire [31:0] CSR_io_pc; // @[CoreTop.scala 26:19]
  wire  CSR_io_fault_occurs; // @[CoreTop.scala 26:19]
  wire [31:0] CSR_io_fault_new_PC; // @[CoreTop.scala 26:19]
  wire  pc_clock; // @[CoreTop.scala 31:18]
  wire  pc_reset; // @[CoreTop.scala 31:18]
  wire [1:0] pc_io_cpu_state; // @[CoreTop.scala 31:18]
  wire [31:0] pc_io_next_addr; // @[CoreTop.scala 31:18]
  wire [31:0] pc_io_addr; // @[CoreTop.scala 31:18]
  wire  nextPCGen_clock; // @[CoreTop.scala 32:25]
  wire  nextPCGen_reset; // @[CoreTop.scala 32:25]
  wire [1:0] nextPCGen_io_cpu_state_type; // @[CoreTop.scala 32:25]
  wire [1:0] nextPCGen_io_nextPC_control_signal; // @[CoreTop.scala 32:25]
  wire [31:0] nextPCGen_io_instruction; // @[CoreTop.scala 32:25]
  wire  nextPCGen_io_prediction_failure_failure_occurs; // @[CoreTop.scala 32:25]
  wire  nextPCGen_io_prediction_failure_success_occurs; // @[CoreTop.scala 32:25]
  wire [31:0] nextPCGen_io_prediction_failure_address; // @[CoreTop.scala 32:25]
  wire [31:0] nextPCGen_io_pc; // @[CoreTop.scala 32:25]
  wire [31:0] nextPCGen_io_new_pc; // @[CoreTop.scala 32:25]
  wire [31:0] nextPCGen_io_nextPC; // @[CoreTop.scala 32:25]
  wire  IF_ID_clock; // @[CoreTop.scala 34:21]
  wire  IF_ID_reset; // @[CoreTop.scala 34:21]
  wire [1:0] IF_ID_io_cpu_state; // @[CoreTop.scala 34:21]
  wire [1:0] IF_ID_io_signal; // @[CoreTop.scala 34:21]
  wire [31:0] IF_ID_io_in_pc; // @[CoreTop.scala 34:21]
  wire [31:0] IF_ID_io_in_ins; // @[CoreTop.scala 34:21]
  wire [1:0] IF_ID_io_in_IF_fault_IF_fault_type; // @[CoreTop.scala 34:21]
  wire [31:0] IF_ID_io_in_IF_fault_mtval; // @[CoreTop.scala 34:21]
  wire [31:0] IF_ID_io_out_pc; // @[CoreTop.scala 34:21]
  wire [31:0] IF_ID_io_out_ins; // @[CoreTop.scala 34:21]
  wire [1:0] IF_ID_io_out_IF_fault_IF_fault_type; // @[CoreTop.scala 34:21]
  wire [31:0] IF_ID_io_out_IF_fault_mtval; // @[CoreTop.scala 34:21]
  wire [31:0] insDecode_io_instruction; // @[CoreTop.scala 35:25]
  wire [6:0] insDecode_io_opcode; // @[CoreTop.scala 35:25]
  wire [4:0] insDecode_io_rs1; // @[CoreTop.scala 35:25]
  wire [4:0] insDecode_io_rs2; // @[CoreTop.scala 35:25]
  wire [4:0] insDecode_io_rd; // @[CoreTop.scala 35:25]
  wire [2:0] insDecode_io_func3; // @[CoreTop.scala 35:25]
  wire [6:0] insDecode_io_func7; // @[CoreTop.scala 35:25]
  wire [19:0] insDecode_io_raw_imm; // @[CoreTop.scala 35:25]
  wire [11:0] insDecode_io_csr; // @[CoreTop.scala 35:25]
  wire [31:0] CU_io_instruction; // @[CoreTop.scala 36:18]
  wire [6:0] CU_io_opcode; // @[CoreTop.scala 36:18]
  wire [2:0] CU_io_func3; // @[CoreTop.scala 36:18]
  wire [6:0] CU_io_func7; // @[CoreTop.scala 36:18]
  wire [4:0] CU_io_rs1; // @[CoreTop.scala 36:18]
  wire [4:0] CU_io_rs2; // @[CoreTop.scala 36:18]
  wire [4:0] CU_io_rd; // @[CoreTop.scala 36:18]
  wire [19:0] CU_io_raw_imm; // @[CoreTop.scala 36:18]
  wire [11:0] CU_io_csr; // @[CoreTop.scala 36:18]
  wire [1:0] CU_io_cur_privilege; // @[CoreTop.scala 36:18]
  wire [4:0] CU_io_rs1_out; // @[CoreTop.scala 36:18]
  wire [4:0] CU_io_rs2_out; // @[CoreTop.scala 36:18]
  wire [4:0] CU_io_rd_out; // @[CoreTop.scala 36:18]
  wire [19:0] CU_io_raw_imm_out; // @[CoreTop.scala 36:18]
  wire [11:0] CU_io_csr_out; // @[CoreTop.scala 36:18]
  wire [3:0] CU_io_alu_type; // @[CoreTop.scala 36:18]
  wire [1:0] CU_io_cmp_type; // @[CoreTop.scala 36:18]
  wire  CU_io_unsigned; // @[CoreTop.scala 36:18]
  wire  CU_io_regs_write; // @[CoreTop.scala 36:18]
  wire [1:0] CU_io_imm_width_type; // @[CoreTop.scala 36:18]
  wire  CU_io_operand2_type; // @[CoreTop.scala 36:18]
  wire  CU_io_au_type; // @[CoreTop.scala 36:18]
  wire [2:0] CU_io_write_back_type; // @[CoreTop.scala 36:18]
  wire  CU_io_memory_read; // @[CoreTop.scala 36:18]
  wire  CU_io_memory_write; // @[CoreTop.scala 36:18]
  wire [1:0] CU_io_data_width; // @[CoreTop.scala 36:18]
  wire  CU_io_csr_write; // @[CoreTop.scala 36:18]
  wire  CU_io_operand1_type; // @[CoreTop.scala 36:18]
  wire  CU_io_result_stage; // @[CoreTop.scala 36:18]
  wire [1:0] CU_io_branch_type; // @[CoreTop.scala 36:18]
  wire [2:0] CU_io_fault_ID_fault_type; // @[CoreTop.scala 36:18]
  wire [31:0] CU_io_fault_mtval; // @[CoreTop.scala 36:18]
  wire  regs_clock; // @[CoreTop.scala 38:20]
  wire  regs_reset; // @[CoreTop.scala 38:20]
  wire [1:0] regs_io_cpu_state; // @[CoreTop.scala 38:20]
  wire  regs_io_write; // @[CoreTop.scala 38:20]
  wire [4:0] regs_io_rs1; // @[CoreTop.scala 38:20]
  wire [4:0] regs_io_rs2; // @[CoreTop.scala 38:20]
  wire [4:0] regs_io_rd; // @[CoreTop.scala 38:20]
  wire [31:0] regs_io_write_data; // @[CoreTop.scala 38:20]
  wire [31:0] regs_io_rs1_val; // @[CoreTop.scala 38:20]
  wire [31:0] regs_io_rs2_val; // @[CoreTop.scala 38:20]
  wire [19:0] immGen_io_raw_imm; // @[CoreTop.scala 39:22]
  wire  immGen_io_unsigned; // @[CoreTop.scala 39:22]
  wire [1:0] immGen_io_imm_width; // @[CoreTop.scala 39:22]
  wire [31:0] immGen_io_real_imm; // @[CoreTop.scala 39:22]
  wire [1:0] rs1_forward_selector_io_forward_type; // @[CoreTop.scala 40:36]
  wire [31:0] rs1_forward_selector_io_origin_val; // @[CoreTop.scala 40:36]
  wire [31:0] rs1_forward_selector_io_ex_forward; // @[CoreTop.scala 40:36]
  wire [31:0] rs1_forward_selector_io_mem_forward; // @[CoreTop.scala 40:36]
  wire [31:0] rs1_forward_selector_io_real_val; // @[CoreTop.scala 40:36]
  wire [1:0] rs2_forward_selector_io_forward_type; // @[CoreTop.scala 41:36]
  wire [31:0] rs2_forward_selector_io_origin_val; // @[CoreTop.scala 41:36]
  wire [31:0] rs2_forward_selector_io_ex_forward; // @[CoreTop.scala 41:36]
  wire [31:0] rs2_forward_selector_io_mem_forward; // @[CoreTop.scala 41:36]
  wire [31:0] rs2_forward_selector_io_real_val; // @[CoreTop.scala 41:36]
  wire [1:0] csr_forward_selector_io_forward_type; // @[CoreTop.scala 42:36]
  wire [31:0] csr_forward_selector_io_origin_val; // @[CoreTop.scala 42:36]
  wire [31:0] csr_forward_selector_io_ex_forward; // @[CoreTop.scala 42:36]
  wire [31:0] csr_forward_selector_io_mem_forward; // @[CoreTop.scala 42:36]
  wire [31:0] csr_forward_selector_io_real_val; // @[CoreTop.scala 42:36]
  wire  operandSelector_io_operand2Type; // @[CoreTop.scala 43:31]
  wire  operandSelector_io_operand1Type; // @[CoreTop.scala 43:31]
  wire [31:0] operandSelector_io_rs1_val; // @[CoreTop.scala 43:31]
  wire [31:0] operandSelector_io_rs2_val; // @[CoreTop.scala 43:31]
  wire [31:0] operandSelector_io_real_imm; // @[CoreTop.scala 43:31]
  wire [31:0] operandSelector_io_csr_val; // @[CoreTop.scala 43:31]
  wire [31:0] operandSelector_io_operand1; // @[CoreTop.scala 43:31]
  wire [31:0] operandSelector_io_operand2; // @[CoreTop.scala 43:31]
  wire  EXForwarding_io_au_type; // @[CoreTop.scala 44:28]
  wire [31:0] EXForwarding_io_alu_result; // @[CoreTop.scala 44:28]
  wire  EXForwarding_io_cmp_result; // @[CoreTop.scala 44:28]
  wire [2:0] EXForwarding_io_write_back_type; // @[CoreTop.scala 44:28]
  wire [31:0] EXForwarding_io_imm; // @[CoreTop.scala 44:28]
  wire [31:0] EXForwarding_io_mem_out; // @[CoreTop.scala 44:28]
  wire [31:0] EXForwarding_io_pc4; // @[CoreTop.scala 44:28]
  wire [31:0] EXForwarding_io_pcImm; // @[CoreTop.scala 44:28]
  wire [31:0] EXForwarding_io_csr_val; // @[CoreTop.scala 44:28]
  wire [31:0] EXForwarding_io_write_data; // @[CoreTop.scala 44:28]
  wire  MEMForwarding_io_au_type; // @[CoreTop.scala 45:29]
  wire [31:0] MEMForwarding_io_alu_result; // @[CoreTop.scala 45:29]
  wire  MEMForwarding_io_cmp_result; // @[CoreTop.scala 45:29]
  wire [2:0] MEMForwarding_io_write_back_type; // @[CoreTop.scala 45:29]
  wire [31:0] MEMForwarding_io_imm; // @[CoreTop.scala 45:29]
  wire [31:0] MEMForwarding_io_mem_out; // @[CoreTop.scala 45:29]
  wire [31:0] MEMForwarding_io_pc4; // @[CoreTop.scala 45:29]
  wire [31:0] MEMForwarding_io_pcImm; // @[CoreTop.scala 45:29]
  wire [31:0] MEMForwarding_io_csr_val; // @[CoreTop.scala 45:29]
  wire [31:0] MEMForwarding_io_write_data; // @[CoreTop.scala 45:29]
  wire  ID_EX_clock; // @[CoreTop.scala 47:21]
  wire  ID_EX_reset; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_cpu_state; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_signal; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_in_pc; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_in_IF_fault_IF_fault_type; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_in_IF_fault_mtval; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_in_op1; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_in_op2; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_in_csr_val_to_reg; // @[CoreTop.scala 47:21]
  wire [3:0] ID_EX_io_in_alu_op; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_in_cmp_op; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_in_unsigned; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_in_mem_signal_read_data_en; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_in_mem_signal_write_data_en; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_in_mem_signal_data_width; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_in_mem_signal_data_to_write; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_in_wb_signal_au_type; // @[CoreTop.scala 47:21]
  wire [2:0] ID_EX_io_in_wb_signal_wb_type; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_in_wb_signal_imm; // @[CoreTop.scala 47:21]
  wire [4:0] ID_EX_io_in_wb_signal_rd; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_in_wb_signal_write_reg; // @[CoreTop.scala 47:21]
  wire [11:0] ID_EX_io_in_wb_signal_csr_idx; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_in_wb_signal_csr_write; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_in_result_stage; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_in_branch_type; // @[CoreTop.scala 47:21]
  wire [2:0] ID_EX_io_in_ID_fault_ID_fault_type; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_in_ID_fault_mtval; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_out_pc; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_out_IF_fault_IF_fault_type; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_out_IF_fault_mtval; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_out_op1; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_out_op2; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_out_csr_val_to_reg; // @[CoreTop.scala 47:21]
  wire [3:0] ID_EX_io_out_alu_op; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_out_cmp_op; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_out_unsigned; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_out_mem_signal_read_data_en; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_out_mem_signal_write_data_en; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_out_mem_signal_data_width; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_out_mem_signal_data_to_write; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_out_wb_signal_au_type; // @[CoreTop.scala 47:21]
  wire [2:0] ID_EX_io_out_wb_signal_wb_type; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_out_wb_signal_imm; // @[CoreTop.scala 47:21]
  wire [4:0] ID_EX_io_out_wb_signal_rd; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_out_wb_signal_write_reg; // @[CoreTop.scala 47:21]
  wire [11:0] ID_EX_io_out_wb_signal_csr_idx; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_out_wb_signal_csr_write; // @[CoreTop.scala 47:21]
  wire  ID_EX_io_out_result_stage; // @[CoreTop.scala 47:21]
  wire [1:0] ID_EX_io_out_branch_type; // @[CoreTop.scala 47:21]
  wire [2:0] ID_EX_io_out_ID_fault_ID_fault_type; // @[CoreTop.scala 47:21]
  wire [31:0] ID_EX_io_out_ID_fault_mtval; // @[CoreTop.scala 47:21]
  wire [31:0] ALU_io_operand1; // @[CoreTop.scala 48:19]
  wire [31:0] ALU_io_operand2; // @[CoreTop.scala 48:19]
  wire [3:0] ALU_io_alu_op; // @[CoreTop.scala 48:19]
  wire  ALU_io_unsigned; // @[CoreTop.scala 48:19]
  wire [31:0] ALU_io_result; // @[CoreTop.scala 48:19]
  wire [31:0] CMP_io_operand1; // @[CoreTop.scala 49:19]
  wire [31:0] CMP_io_operand2; // @[CoreTop.scala 49:19]
  wire [1:0] CMP_io_cmp_op; // @[CoreTop.scala 49:19]
  wire  CMP_io_unsigned; // @[CoreTop.scala 49:19]
  wire  CMP_io_result; // @[CoreTop.scala 49:19]
  wire  EX_MEM_clock; // @[CoreTop.scala 52:22]
  wire  EX_MEM_reset; // @[CoreTop.scala 52:22]
  wire [1:0] EX_MEM_io_cpu_state; // @[CoreTop.scala 52:22]
  wire [1:0] EX_MEM_io_signal; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_in_pc; // @[CoreTop.scala 52:22]
  wire [1:0] EX_MEM_io_in_IF_fault_IF_fault_type; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_in_IF_fault_mtval; // @[CoreTop.scala 52:22]
  wire [2:0] EX_MEM_io_in_ID_fault_ID_fault_type; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_in_ID_fault_mtval; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_in_csr_val_to_reg; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_in_unsigned; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_in_mem_signal_read_data_en; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_in_mem_signal_write_data_en; // @[CoreTop.scala 52:22]
  wire [1:0] EX_MEM_io_in_mem_signal_data_width; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_in_mem_signal_data_to_write; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_in_wb_signal_au_type; // @[CoreTop.scala 52:22]
  wire [2:0] EX_MEM_io_in_wb_signal_wb_type; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_in_wb_signal_imm; // @[CoreTop.scala 52:22]
  wire [4:0] EX_MEM_io_in_wb_signal_rd; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_in_wb_signal_write_reg; // @[CoreTop.scala 52:22]
  wire [11:0] EX_MEM_io_in_wb_signal_csr_idx; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_in_wb_signal_csr_write; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_in_alu_out; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_in_cmp_out; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_out_pc; // @[CoreTop.scala 52:22]
  wire [1:0] EX_MEM_io_out_IF_fault_IF_fault_type; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_out_IF_fault_mtval; // @[CoreTop.scala 52:22]
  wire [2:0] EX_MEM_io_out_ID_fault_ID_fault_type; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_out_ID_fault_mtval; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_out_csr_val_to_reg; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_out_unsigned; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_out_mem_signal_read_data_en; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_out_mem_signal_write_data_en; // @[CoreTop.scala 52:22]
  wire [1:0] EX_MEM_io_out_mem_signal_data_width; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_out_mem_signal_data_to_write; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_out_wb_signal_au_type; // @[CoreTop.scala 52:22]
  wire [2:0] EX_MEM_io_out_wb_signal_wb_type; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_out_wb_signal_imm; // @[CoreTop.scala 52:22]
  wire [4:0] EX_MEM_io_out_wb_signal_rd; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_out_wb_signal_write_reg; // @[CoreTop.scala 52:22]
  wire [11:0] EX_MEM_io_out_wb_signal_csr_idx; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_out_wb_signal_csr_write; // @[CoreTop.scala 52:22]
  wire [31:0] EX_MEM_io_out_alu_out; // @[CoreTop.scala 52:22]
  wire  EX_MEM_io_out_cmp_out; // @[CoreTop.scala 52:22]
  wire  memory_clock; // @[CoreTop.scala 53:22]
  wire  memory_reset; // @[CoreTop.scala 53:22]
  wire  memory_io_uart_load; // @[CoreTop.scala 53:22]
  wire [1:0] memory_io_cpu_state; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_ins_addr; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_ins_out; // @[CoreTop.scala 53:22]
  wire  memory_io_read_data; // @[CoreTop.scala 53:22]
  wire  memory_io_write_data; // @[CoreTop.scala 53:22]
  wire  memory_io_unsigned; // @[CoreTop.scala 53:22]
  wire [1:0] memory_io_data_width; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_data_addr; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_data_write; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_data_out; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_external_led_led; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_external_seg7_seg7; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_external_btn_button; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_external_switches_switches; // @[CoreTop.scala 53:22]
  wire [7:0] memory_io_external_uart_rxData; // @[CoreTop.scala 53:22]
  wire  memory_io_external_uart_rxValid; // @[CoreTop.scala 53:22]
  wire  memory_io_external_uart_rxReady; // @[CoreTop.scala 53:22]
  wire [7:0] memory_io_external_uart_txData; // @[CoreTop.scala 53:22]
  wire  memory_io_external_uart_txStart; // @[CoreTop.scala 53:22]
  wire  memory_io_external_uart_txReady; // @[CoreTop.scala 53:22]
  wire [1:0] memory_io_IF_fault_IF_fault_type; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_IF_fault_mtval; // @[CoreTop.scala 53:22]
  wire [2:0] memory_io_MEM_fault_mem_fault_type; // @[CoreTop.scala 53:22]
  wire [31:0] memory_io_MEM_fault_mtval; // @[CoreTop.scala 53:22]
  wire  uartLoader_clock; // @[CoreTop.scala 54:26]
  wire  uartLoader_reset; // @[CoreTop.scala 54:26]
  wire  uartLoader_io_uart_load; // @[CoreTop.scala 54:26]
  wire  uartLoader_io_rxValid; // @[CoreTop.scala 54:26]
  wire [7:0] uartLoader_io_rxData; // @[CoreTop.scala 54:26]
  wire  uartLoader_io_mem_mem_write; // @[CoreTop.scala 54:26]
  wire [31:0] uartLoader_io_mem_data_to_write; // @[CoreTop.scala 54:26]
  wire [31:0] uartLoader_io_mem_data_addr; // @[CoreTop.scala 54:26]
  wire  memInSelector_io_uart_load; // @[CoreTop.scala 55:29]
  wire  memInSelector_io_uart_in_mem_write; // @[CoreTop.scala 55:29]
  wire [31:0] memInSelector_io_uart_in_data_to_write; // @[CoreTop.scala 55:29]
  wire [31:0] memInSelector_io_uart_in_data_addr; // @[CoreTop.scala 55:29]
  wire  memInSelector_io_cpu_read_data; // @[CoreTop.scala 55:29]
  wire  memInSelector_io_cpu_write_data; // @[CoreTop.scala 55:29]
  wire [1:0] memInSelector_io_cpu_data_width; // @[CoreTop.scala 55:29]
  wire [31:0] memInSelector_io_cpu_data_addr; // @[CoreTop.scala 55:29]
  wire [31:0] memInSelector_io_cpu_data_write; // @[CoreTop.scala 55:29]
  wire  memInSelector_io_read_data; // @[CoreTop.scala 55:29]
  wire  memInSelector_io_write_data; // @[CoreTop.scala 55:29]
  wire [1:0] memInSelector_io_data_width; // @[CoreTop.scala 55:29]
  wire [31:0] memInSelector_io_data_addr; // @[CoreTop.scala 55:29]
  wire [31:0] memInSelector_io_data_write; // @[CoreTop.scala 55:29]
  wire  MEM_WB_clock; // @[CoreTop.scala 58:22]
  wire  MEM_WB_reset; // @[CoreTop.scala 58:22]
  wire [1:0] MEM_WB_io_cpu_state; // @[CoreTop.scala 58:22]
  wire [1:0] MEM_WB_io_signal; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_in_pc; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_in_csr_val_to_reg; // @[CoreTop.scala 58:22]
  wire  MEM_WB_io_in_wb_signal_au_type; // @[CoreTop.scala 58:22]
  wire [2:0] MEM_WB_io_in_wb_signal_wb_type; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_in_wb_signal_imm; // @[CoreTop.scala 58:22]
  wire [4:0] MEM_WB_io_in_wb_signal_rd; // @[CoreTop.scala 58:22]
  wire  MEM_WB_io_in_wb_signal_write_reg; // @[CoreTop.scala 58:22]
  wire [11:0] MEM_WB_io_in_wb_signal_csr_idx; // @[CoreTop.scala 58:22]
  wire  MEM_WB_io_in_wb_signal_csr_write; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_in_alu_out; // @[CoreTop.scala 58:22]
  wire  MEM_WB_io_in_cmp_out; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_in_data_out; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_out_pc; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_out_csr_val_to_reg; // @[CoreTop.scala 58:22]
  wire  MEM_WB_io_out_wb_signal_au_type; // @[CoreTop.scala 58:22]
  wire [2:0] MEM_WB_io_out_wb_signal_wb_type; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_out_wb_signal_imm; // @[CoreTop.scala 58:22]
  wire [4:0] MEM_WB_io_out_wb_signal_rd; // @[CoreTop.scala 58:22]
  wire  MEM_WB_io_out_wb_signal_write_reg; // @[CoreTop.scala 58:22]
  wire [11:0] MEM_WB_io_out_wb_signal_csr_idx; // @[CoreTop.scala 58:22]
  wire  MEM_WB_io_out_wb_signal_csr_write; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_out_alu_out; // @[CoreTop.scala 58:22]
  wire  MEM_WB_io_out_cmp_out; // @[CoreTop.scala 58:22]
  wire [31:0] MEM_WB_io_out_data_out; // @[CoreTop.scala 58:22]
  wire  wb_selector_io_au_type; // @[CoreTop.scala 59:27]
  wire [31:0] wb_selector_io_alu_result; // @[CoreTop.scala 59:27]
  wire  wb_selector_io_cmp_result; // @[CoreTop.scala 59:27]
  wire [2:0] wb_selector_io_write_back_type; // @[CoreTop.scala 59:27]
  wire [31:0] wb_selector_io_imm; // @[CoreTop.scala 59:27]
  wire [31:0] wb_selector_io_mem_out; // @[CoreTop.scala 59:27]
  wire [31:0] wb_selector_io_pc4; // @[CoreTop.scala 59:27]
  wire [31:0] wb_selector_io_pcImm; // @[CoreTop.scala 59:27]
  wire [31:0] wb_selector_io_csr_val; // @[CoreTop.scala 59:27]
  wire [31:0] wb_selector_io_write_data; // @[CoreTop.scala 59:27]
  ConflictController conflictController ( // @[CoreTop.scala 22:34]
    .io_uart_loading(conflictController_io_uart_loading),
    .io_rs1_use(conflictController_io_rs1_use),
    .io_rs2_use(conflictController_io_rs2_use),
    .io_csr_use(conflictController_io_csr_use),
    .io_IDEX_reg_to_write(conflictController_io_IDEX_reg_to_write),
    .io_IDEX_write_reg(conflictController_io_IDEX_write_reg),
    .io_IDEX_csr_to_write(conflictController_io_IDEX_csr_to_write),
    .io_IDEX_write_csr(conflictController_io_IDEX_write_csr),
    .io_IDEX_reg_result_stage(conflictController_io_IDEX_reg_result_stage),
    .io_EXMEM_reg_to_write(conflictController_io_EXMEM_reg_to_write),
    .io_EXMEM_write_reg(conflictController_io_EXMEM_write_reg),
    .io_EXMEM_csr_to_write(conflictController_io_EXMEM_csr_to_write),
    .io_EXMEM_write_csr(conflictController_io_EXMEM_write_csr),
    .io_rs1_forward_type(conflictController_io_rs1_forward_type),
    .io_rs2_forward_type(conflictController_io_rs2_forward_type),
    .io_csr_forward_type(conflictController_io_csr_forward_type),
    .io_branch_type(conflictController_io_branch_type),
    .io_pc(conflictController_io_pc),
    .io_imm(conflictController_io_imm),
    .io_alu_result(conflictController_io_alu_result),
    .io_cmp_result(conflictController_io_cmp_result),
    .io_predict_next_pc(conflictController_io_predict_next_pc),
    .io_exception_occurs(conflictController_io_exception_occurs),
    .io_exception_new_pc(conflictController_io_exception_new_pc),
    .io_new_pc(conflictController_io_new_pc),
    .io_next_control_signal(conflictController_io_next_control_signal),
    .io_IFID_control_signal(conflictController_io_IFID_control_signal),
    .io_IDEX_control_signal(conflictController_io_IDEX_control_signal),
    .io_EXMEM_control_signal(conflictController_io_EXMEM_control_signal),
    .io_MEMWB_control_signal(conflictController_io_MEMWB_control_signal),
    .io_prediction_failure_failure_occurs(conflictController_io_prediction_failure_failure_occurs),
    .io_prediction_failure_success_occurs(conflictController_io_prediction_failure_success_occurs),
    .io_prediction_failure_address(conflictController_io_prediction_failure_address)
  );
  CPUState state ( // @[CoreTop.scala 23:21]
    .clock(state_clock),
    .reset(state_reset),
    .io_stall(state_io_stall),
    .io_cpu_state(state_io_cpu_state)
  );
  CSR CSR ( // @[CoreTop.scala 26:19]
    .clock(CSR_clock),
    .reset(CSR_reset),
    .io_cpu_state(CSR_io_cpu_state),
    .io_cur_privilege(CSR_io_cur_privilege),
    .io_write(CSR_io_write),
    .io_csr_idx_write(CSR_io_csr_idx_write),
    .io_write_data(CSR_io_write_data),
    .io_csr_idx_read(CSR_io_csr_idx_read),
    .io_csr_val(CSR_io_csr_val),
    .io_IF_fault_IF_fault_type(CSR_io_IF_fault_IF_fault_type),
    .io_IF_fault_mtval(CSR_io_IF_fault_mtval),
    .io_ID_fault_ID_fault_type(CSR_io_ID_fault_ID_fault_type),
    .io_ID_fault_mtval(CSR_io_ID_fault_mtval),
    .io_MEM_fault_mem_fault_type(CSR_io_MEM_fault_mem_fault_type),
    .io_MEM_fault_mtval(CSR_io_MEM_fault_mtval),
    .io_pc(CSR_io_pc),
    .io_fault_occurs(CSR_io_fault_occurs),
    .io_fault_new_PC(CSR_io_fault_new_PC)
  );
  PC pc ( // @[CoreTop.scala 31:18]
    .clock(pc_clock),
    .reset(pc_reset),
    .io_cpu_state(pc_io_cpu_state),
    .io_next_addr(pc_io_next_addr),
    .io_addr(pc_io_addr)
  );
  NextPCGen nextPCGen ( // @[CoreTop.scala 32:25]
    .clock(nextPCGen_clock),
    .reset(nextPCGen_reset),
    .io_cpu_state_type(nextPCGen_io_cpu_state_type),
    .io_nextPC_control_signal(nextPCGen_io_nextPC_control_signal),
    .io_instruction(nextPCGen_io_instruction),
    .io_prediction_failure_failure_occurs(nextPCGen_io_prediction_failure_failure_occurs),
    .io_prediction_failure_success_occurs(nextPCGen_io_prediction_failure_success_occurs),
    .io_prediction_failure_address(nextPCGen_io_prediction_failure_address),
    .io_pc(nextPCGen_io_pc),
    .io_new_pc(nextPCGen_io_new_pc),
    .io_nextPC(nextPCGen_io_nextPC)
  );
  IFID IF_ID ( // @[CoreTop.scala 34:21]
    .clock(IF_ID_clock),
    .reset(IF_ID_reset),
    .io_cpu_state(IF_ID_io_cpu_state),
    .io_signal(IF_ID_io_signal),
    .io_in_pc(IF_ID_io_in_pc),
    .io_in_ins(IF_ID_io_in_ins),
    .io_in_IF_fault_IF_fault_type(IF_ID_io_in_IF_fault_IF_fault_type),
    .io_in_IF_fault_mtval(IF_ID_io_in_IF_fault_mtval),
    .io_out_pc(IF_ID_io_out_pc),
    .io_out_ins(IF_ID_io_out_ins),
    .io_out_IF_fault_IF_fault_type(IF_ID_io_out_IF_fault_IF_fault_type),
    .io_out_IF_fault_mtval(IF_ID_io_out_IF_fault_mtval)
  );
  InstructionDecoder insDecode ( // @[CoreTop.scala 35:25]
    .io_instruction(insDecode_io_instruction),
    .io_opcode(insDecode_io_opcode),
    .io_rs1(insDecode_io_rs1),
    .io_rs2(insDecode_io_rs2),
    .io_rd(insDecode_io_rd),
    .io_func3(insDecode_io_func3),
    .io_func7(insDecode_io_func7),
    .io_raw_imm(insDecode_io_raw_imm),
    .io_csr(insDecode_io_csr)
  );
  ControlUnit CU ( // @[CoreTop.scala 36:18]
    .io_instruction(CU_io_instruction),
    .io_opcode(CU_io_opcode),
    .io_func3(CU_io_func3),
    .io_func7(CU_io_func7),
    .io_rs1(CU_io_rs1),
    .io_rs2(CU_io_rs2),
    .io_rd(CU_io_rd),
    .io_raw_imm(CU_io_raw_imm),
    .io_csr(CU_io_csr),
    .io_cur_privilege(CU_io_cur_privilege),
    .io_rs1_out(CU_io_rs1_out),
    .io_rs2_out(CU_io_rs2_out),
    .io_rd_out(CU_io_rd_out),
    .io_raw_imm_out(CU_io_raw_imm_out),
    .io_csr_out(CU_io_csr_out),
    .io_alu_type(CU_io_alu_type),
    .io_cmp_type(CU_io_cmp_type),
    .io_unsigned(CU_io_unsigned),
    .io_regs_write(CU_io_regs_write),
    .io_imm_width_type(CU_io_imm_width_type),
    .io_operand2_type(CU_io_operand2_type),
    .io_au_type(CU_io_au_type),
    .io_write_back_type(CU_io_write_back_type),
    .io_memory_read(CU_io_memory_read),
    .io_memory_write(CU_io_memory_write),
    .io_data_width(CU_io_data_width),
    .io_csr_write(CU_io_csr_write),
    .io_operand1_type(CU_io_operand1_type),
    .io_result_stage(CU_io_result_stage),
    .io_branch_type(CU_io_branch_type),
    .io_fault_ID_fault_type(CU_io_fault_ID_fault_type),
    .io_fault_mtval(CU_io_fault_mtval)
  );
  Registers regs ( // @[CoreTop.scala 38:20]
    .clock(regs_clock),
    .reset(regs_reset),
    .io_cpu_state(regs_io_cpu_state),
    .io_write(regs_io_write),
    .io_rs1(regs_io_rs1),
    .io_rs2(regs_io_rs2),
    .io_rd(regs_io_rd),
    .io_write_data(regs_io_write_data),
    .io_rs1_val(regs_io_rs1_val),
    .io_rs2_val(regs_io_rs2_val)
  );
  ImmGen immGen ( // @[CoreTop.scala 39:22]
    .io_raw_imm(immGen_io_raw_imm),
    .io_unsigned(immGen_io_unsigned),
    .io_imm_width(immGen_io_imm_width),
    .io_real_imm(immGen_io_real_imm)
  );
  ForwardSelector rs1_forward_selector ( // @[CoreTop.scala 40:36]
    .io_forward_type(rs1_forward_selector_io_forward_type),
    .io_origin_val(rs1_forward_selector_io_origin_val),
    .io_ex_forward(rs1_forward_selector_io_ex_forward),
    .io_mem_forward(rs1_forward_selector_io_mem_forward),
    .io_real_val(rs1_forward_selector_io_real_val)
  );
  ForwardSelector rs2_forward_selector ( // @[CoreTop.scala 41:36]
    .io_forward_type(rs2_forward_selector_io_forward_type),
    .io_origin_val(rs2_forward_selector_io_origin_val),
    .io_ex_forward(rs2_forward_selector_io_ex_forward),
    .io_mem_forward(rs2_forward_selector_io_mem_forward),
    .io_real_val(rs2_forward_selector_io_real_val)
  );
  ForwardSelector csr_forward_selector ( // @[CoreTop.scala 42:36]
    .io_forward_type(csr_forward_selector_io_forward_type),
    .io_origin_val(csr_forward_selector_io_origin_val),
    .io_ex_forward(csr_forward_selector_io_ex_forward),
    .io_mem_forward(csr_forward_selector_io_mem_forward),
    .io_real_val(csr_forward_selector_io_real_val)
  );
  OperandSelector operandSelector ( // @[CoreTop.scala 43:31]
    .io_operand2Type(operandSelector_io_operand2Type),
    .io_operand1Type(operandSelector_io_operand1Type),
    .io_rs1_val(operandSelector_io_rs1_val),
    .io_rs2_val(operandSelector_io_rs2_val),
    .io_real_imm(operandSelector_io_real_imm),
    .io_csr_val(operandSelector_io_csr_val),
    .io_operand1(operandSelector_io_operand1),
    .io_operand2(operandSelector_io_operand2)
  );
  WBSelectorTop EXForwarding ( // @[CoreTop.scala 44:28]
    .io_au_type(EXForwarding_io_au_type),
    .io_alu_result(EXForwarding_io_alu_result),
    .io_cmp_result(EXForwarding_io_cmp_result),
    .io_write_back_type(EXForwarding_io_write_back_type),
    .io_imm(EXForwarding_io_imm),
    .io_mem_out(EXForwarding_io_mem_out),
    .io_pc4(EXForwarding_io_pc4),
    .io_pcImm(EXForwarding_io_pcImm),
    .io_csr_val(EXForwarding_io_csr_val),
    .io_write_data(EXForwarding_io_write_data)
  );
  WBSelectorTop MEMForwarding ( // @[CoreTop.scala 45:29]
    .io_au_type(MEMForwarding_io_au_type),
    .io_alu_result(MEMForwarding_io_alu_result),
    .io_cmp_result(MEMForwarding_io_cmp_result),
    .io_write_back_type(MEMForwarding_io_write_back_type),
    .io_imm(MEMForwarding_io_imm),
    .io_mem_out(MEMForwarding_io_mem_out),
    .io_pc4(MEMForwarding_io_pc4),
    .io_pcImm(MEMForwarding_io_pcImm),
    .io_csr_val(MEMForwarding_io_csr_val),
    .io_write_data(MEMForwarding_io_write_data)
  );
  IDEX ID_EX ( // @[CoreTop.scala 47:21]
    .clock(ID_EX_clock),
    .reset(ID_EX_reset),
    .io_cpu_state(ID_EX_io_cpu_state),
    .io_signal(ID_EX_io_signal),
    .io_in_pc(ID_EX_io_in_pc),
    .io_in_IF_fault_IF_fault_type(ID_EX_io_in_IF_fault_IF_fault_type),
    .io_in_IF_fault_mtval(ID_EX_io_in_IF_fault_mtval),
    .io_in_op1(ID_EX_io_in_op1),
    .io_in_op2(ID_EX_io_in_op2),
    .io_in_csr_val_to_reg(ID_EX_io_in_csr_val_to_reg),
    .io_in_alu_op(ID_EX_io_in_alu_op),
    .io_in_cmp_op(ID_EX_io_in_cmp_op),
    .io_in_unsigned(ID_EX_io_in_unsigned),
    .io_in_mem_signal_read_data_en(ID_EX_io_in_mem_signal_read_data_en),
    .io_in_mem_signal_write_data_en(ID_EX_io_in_mem_signal_write_data_en),
    .io_in_mem_signal_data_width(ID_EX_io_in_mem_signal_data_width),
    .io_in_mem_signal_data_to_write(ID_EX_io_in_mem_signal_data_to_write),
    .io_in_wb_signal_au_type(ID_EX_io_in_wb_signal_au_type),
    .io_in_wb_signal_wb_type(ID_EX_io_in_wb_signal_wb_type),
    .io_in_wb_signal_imm(ID_EX_io_in_wb_signal_imm),
    .io_in_wb_signal_rd(ID_EX_io_in_wb_signal_rd),
    .io_in_wb_signal_write_reg(ID_EX_io_in_wb_signal_write_reg),
    .io_in_wb_signal_csr_idx(ID_EX_io_in_wb_signal_csr_idx),
    .io_in_wb_signal_csr_write(ID_EX_io_in_wb_signal_csr_write),
    .io_in_result_stage(ID_EX_io_in_result_stage),
    .io_in_branch_type(ID_EX_io_in_branch_type),
    .io_in_ID_fault_ID_fault_type(ID_EX_io_in_ID_fault_ID_fault_type),
    .io_in_ID_fault_mtval(ID_EX_io_in_ID_fault_mtval),
    .io_out_pc(ID_EX_io_out_pc),
    .io_out_IF_fault_IF_fault_type(ID_EX_io_out_IF_fault_IF_fault_type),
    .io_out_IF_fault_mtval(ID_EX_io_out_IF_fault_mtval),
    .io_out_op1(ID_EX_io_out_op1),
    .io_out_op2(ID_EX_io_out_op2),
    .io_out_csr_val_to_reg(ID_EX_io_out_csr_val_to_reg),
    .io_out_alu_op(ID_EX_io_out_alu_op),
    .io_out_cmp_op(ID_EX_io_out_cmp_op),
    .io_out_unsigned(ID_EX_io_out_unsigned),
    .io_out_mem_signal_read_data_en(ID_EX_io_out_mem_signal_read_data_en),
    .io_out_mem_signal_write_data_en(ID_EX_io_out_mem_signal_write_data_en),
    .io_out_mem_signal_data_width(ID_EX_io_out_mem_signal_data_width),
    .io_out_mem_signal_data_to_write(ID_EX_io_out_mem_signal_data_to_write),
    .io_out_wb_signal_au_type(ID_EX_io_out_wb_signal_au_type),
    .io_out_wb_signal_wb_type(ID_EX_io_out_wb_signal_wb_type),
    .io_out_wb_signal_imm(ID_EX_io_out_wb_signal_imm),
    .io_out_wb_signal_rd(ID_EX_io_out_wb_signal_rd),
    .io_out_wb_signal_write_reg(ID_EX_io_out_wb_signal_write_reg),
    .io_out_wb_signal_csr_idx(ID_EX_io_out_wb_signal_csr_idx),
    .io_out_wb_signal_csr_write(ID_EX_io_out_wb_signal_csr_write),
    .io_out_result_stage(ID_EX_io_out_result_stage),
    .io_out_branch_type(ID_EX_io_out_branch_type),
    .io_out_ID_fault_ID_fault_type(ID_EX_io_out_ID_fault_ID_fault_type),
    .io_out_ID_fault_mtval(ID_EX_io_out_ID_fault_mtval)
  );
  ALU ALU ( // @[CoreTop.scala 48:19]
    .io_operand1(ALU_io_operand1),
    .io_operand2(ALU_io_operand2),
    .io_alu_op(ALU_io_alu_op),
    .io_unsigned(ALU_io_unsigned),
    .io_result(ALU_io_result)
  );
  CMP CMP ( // @[CoreTop.scala 49:19]
    .io_operand1(CMP_io_operand1),
    .io_operand2(CMP_io_operand2),
    .io_cmp_op(CMP_io_cmp_op),
    .io_unsigned(CMP_io_unsigned),
    .io_result(CMP_io_result)
  );
  EXMEM EX_MEM ( // @[CoreTop.scala 52:22]
    .clock(EX_MEM_clock),
    .reset(EX_MEM_reset),
    .io_cpu_state(EX_MEM_io_cpu_state),
    .io_signal(EX_MEM_io_signal),
    .io_in_pc(EX_MEM_io_in_pc),
    .io_in_IF_fault_IF_fault_type(EX_MEM_io_in_IF_fault_IF_fault_type),
    .io_in_IF_fault_mtval(EX_MEM_io_in_IF_fault_mtval),
    .io_in_ID_fault_ID_fault_type(EX_MEM_io_in_ID_fault_ID_fault_type),
    .io_in_ID_fault_mtval(EX_MEM_io_in_ID_fault_mtval),
    .io_in_csr_val_to_reg(EX_MEM_io_in_csr_val_to_reg),
    .io_in_unsigned(EX_MEM_io_in_unsigned),
    .io_in_mem_signal_read_data_en(EX_MEM_io_in_mem_signal_read_data_en),
    .io_in_mem_signal_write_data_en(EX_MEM_io_in_mem_signal_write_data_en),
    .io_in_mem_signal_data_width(EX_MEM_io_in_mem_signal_data_width),
    .io_in_mem_signal_data_to_write(EX_MEM_io_in_mem_signal_data_to_write),
    .io_in_wb_signal_au_type(EX_MEM_io_in_wb_signal_au_type),
    .io_in_wb_signal_wb_type(EX_MEM_io_in_wb_signal_wb_type),
    .io_in_wb_signal_imm(EX_MEM_io_in_wb_signal_imm),
    .io_in_wb_signal_rd(EX_MEM_io_in_wb_signal_rd),
    .io_in_wb_signal_write_reg(EX_MEM_io_in_wb_signal_write_reg),
    .io_in_wb_signal_csr_idx(EX_MEM_io_in_wb_signal_csr_idx),
    .io_in_wb_signal_csr_write(EX_MEM_io_in_wb_signal_csr_write),
    .io_in_alu_out(EX_MEM_io_in_alu_out),
    .io_in_cmp_out(EX_MEM_io_in_cmp_out),
    .io_out_pc(EX_MEM_io_out_pc),
    .io_out_IF_fault_IF_fault_type(EX_MEM_io_out_IF_fault_IF_fault_type),
    .io_out_IF_fault_mtval(EX_MEM_io_out_IF_fault_mtval),
    .io_out_ID_fault_ID_fault_type(EX_MEM_io_out_ID_fault_ID_fault_type),
    .io_out_ID_fault_mtval(EX_MEM_io_out_ID_fault_mtval),
    .io_out_csr_val_to_reg(EX_MEM_io_out_csr_val_to_reg),
    .io_out_unsigned(EX_MEM_io_out_unsigned),
    .io_out_mem_signal_read_data_en(EX_MEM_io_out_mem_signal_read_data_en),
    .io_out_mem_signal_write_data_en(EX_MEM_io_out_mem_signal_write_data_en),
    .io_out_mem_signal_data_width(EX_MEM_io_out_mem_signal_data_width),
    .io_out_mem_signal_data_to_write(EX_MEM_io_out_mem_signal_data_to_write),
    .io_out_wb_signal_au_type(EX_MEM_io_out_wb_signal_au_type),
    .io_out_wb_signal_wb_type(EX_MEM_io_out_wb_signal_wb_type),
    .io_out_wb_signal_imm(EX_MEM_io_out_wb_signal_imm),
    .io_out_wb_signal_rd(EX_MEM_io_out_wb_signal_rd),
    .io_out_wb_signal_write_reg(EX_MEM_io_out_wb_signal_write_reg),
    .io_out_wb_signal_csr_idx(EX_MEM_io_out_wb_signal_csr_idx),
    .io_out_wb_signal_csr_write(EX_MEM_io_out_wb_signal_csr_write),
    .io_out_alu_out(EX_MEM_io_out_alu_out),
    .io_out_cmp_out(EX_MEM_io_out_cmp_out)
  );
  MemoryDispatch memory ( // @[CoreTop.scala 53:22]
    .clock(memory_clock),
    .reset(memory_reset),
    .io_uart_load(memory_io_uart_load),
    .io_cpu_state(memory_io_cpu_state),
    .io_ins_addr(memory_io_ins_addr),
    .io_ins_out(memory_io_ins_out),
    .io_read_data(memory_io_read_data),
    .io_write_data(memory_io_write_data),
    .io_unsigned(memory_io_unsigned),
    .io_data_width(memory_io_data_width),
    .io_data_addr(memory_io_data_addr),
    .io_data_write(memory_io_data_write),
    .io_data_out(memory_io_data_out),
    .io_external_led_led(memory_io_external_led_led),
    .io_external_seg7_seg7(memory_io_external_seg7_seg7),
    .io_external_btn_button(memory_io_external_btn_button),
    .io_external_switches_switches(memory_io_external_switches_switches),
    .io_external_uart_rxData(memory_io_external_uart_rxData),
    .io_external_uart_rxValid(memory_io_external_uart_rxValid),
    .io_external_uart_rxReady(memory_io_external_uart_rxReady),
    .io_external_uart_txData(memory_io_external_uart_txData),
    .io_external_uart_txStart(memory_io_external_uart_txStart),
    .io_external_uart_txReady(memory_io_external_uart_txReady),
    .io_IF_fault_IF_fault_type(memory_io_IF_fault_IF_fault_type),
    .io_IF_fault_mtval(memory_io_IF_fault_mtval),
    .io_MEM_fault_mem_fault_type(memory_io_MEM_fault_mem_fault_type),
    .io_MEM_fault_mtval(memory_io_MEM_fault_mtval)
  );
  UARTLoader uartLoader ( // @[CoreTop.scala 54:26]
    .clock(uartLoader_clock),
    .reset(uartLoader_reset),
    .io_uart_load(uartLoader_io_uart_load),
    .io_rxValid(uartLoader_io_rxValid),
    .io_rxData(uartLoader_io_rxData),
    .io_mem_mem_write(uartLoader_io_mem_mem_write),
    .io_mem_data_to_write(uartLoader_io_mem_data_to_write),
    .io_mem_data_addr(uartLoader_io_mem_data_addr)
  );
  MemWriteSelector memInSelector ( // @[CoreTop.scala 55:29]
    .io_uart_load(memInSelector_io_uart_load),
    .io_uart_in_mem_write(memInSelector_io_uart_in_mem_write),
    .io_uart_in_data_to_write(memInSelector_io_uart_in_data_to_write),
    .io_uart_in_data_addr(memInSelector_io_uart_in_data_addr),
    .io_cpu_read_data(memInSelector_io_cpu_read_data),
    .io_cpu_write_data(memInSelector_io_cpu_write_data),
    .io_cpu_data_width(memInSelector_io_cpu_data_width),
    .io_cpu_data_addr(memInSelector_io_cpu_data_addr),
    .io_cpu_data_write(memInSelector_io_cpu_data_write),
    .io_read_data(memInSelector_io_read_data),
    .io_write_data(memInSelector_io_write_data),
    .io_data_width(memInSelector_io_data_width),
    .io_data_addr(memInSelector_io_data_addr),
    .io_data_write(memInSelector_io_data_write)
  );
  MEMWB MEM_WB ( // @[CoreTop.scala 58:22]
    .clock(MEM_WB_clock),
    .reset(MEM_WB_reset),
    .io_cpu_state(MEM_WB_io_cpu_state),
    .io_signal(MEM_WB_io_signal),
    .io_in_pc(MEM_WB_io_in_pc),
    .io_in_csr_val_to_reg(MEM_WB_io_in_csr_val_to_reg),
    .io_in_wb_signal_au_type(MEM_WB_io_in_wb_signal_au_type),
    .io_in_wb_signal_wb_type(MEM_WB_io_in_wb_signal_wb_type),
    .io_in_wb_signal_imm(MEM_WB_io_in_wb_signal_imm),
    .io_in_wb_signal_rd(MEM_WB_io_in_wb_signal_rd),
    .io_in_wb_signal_write_reg(MEM_WB_io_in_wb_signal_write_reg),
    .io_in_wb_signal_csr_idx(MEM_WB_io_in_wb_signal_csr_idx),
    .io_in_wb_signal_csr_write(MEM_WB_io_in_wb_signal_csr_write),
    .io_in_alu_out(MEM_WB_io_in_alu_out),
    .io_in_cmp_out(MEM_WB_io_in_cmp_out),
    .io_in_data_out(MEM_WB_io_in_data_out),
    .io_out_pc(MEM_WB_io_out_pc),
    .io_out_csr_val_to_reg(MEM_WB_io_out_csr_val_to_reg),
    .io_out_wb_signal_au_type(MEM_WB_io_out_wb_signal_au_type),
    .io_out_wb_signal_wb_type(MEM_WB_io_out_wb_signal_wb_type),
    .io_out_wb_signal_imm(MEM_WB_io_out_wb_signal_imm),
    .io_out_wb_signal_rd(MEM_WB_io_out_wb_signal_rd),
    .io_out_wb_signal_write_reg(MEM_WB_io_out_wb_signal_write_reg),
    .io_out_wb_signal_csr_idx(MEM_WB_io_out_wb_signal_csr_idx),
    .io_out_wb_signal_csr_write(MEM_WB_io_out_wb_signal_csr_write),
    .io_out_alu_out(MEM_WB_io_out_alu_out),
    .io_out_cmp_out(MEM_WB_io_out_cmp_out),
    .io_out_data_out(MEM_WB_io_out_data_out)
  );
  WBSelectorTop wb_selector ( // @[CoreTop.scala 59:27]
    .io_au_type(wb_selector_io_au_type),
    .io_alu_result(wb_selector_io_alu_result),
    .io_cmp_result(wb_selector_io_cmp_result),
    .io_write_back_type(wb_selector_io_write_back_type),
    .io_imm(wb_selector_io_imm),
    .io_mem_out(wb_selector_io_mem_out),
    .io_pc4(wb_selector_io_pc4),
    .io_pcImm(wb_selector_io_pcImm),
    .io_csr_val(wb_selector_io_csr_val),
    .io_write_data(wb_selector_io_write_data)
  );
  assign io_external_led_led = memory_io_external_led_led; // @[CoreTop.scala 235:22]
  assign io_external_seg7_seg7 = memory_io_external_seg7_seg7; // @[CoreTop.scala 235:22]
  assign io_external_uart_rxReady = memory_io_external_uart_rxReady; // @[CoreTop.scala 235:22]
  assign io_external_uart_txData = memory_io_external_uart_txData; // @[CoreTop.scala 235:22]
  assign io_external_uart_txStart = memory_io_external_uart_txStart; // @[CoreTop.scala 235:22]
  assign conflictController_io_uart_loading = io_external_signal_load_data_mode; // @[CoreTop.scala 292:38]
  assign conflictController_io_rs1_use = CU_io_rs1_out; // @[CoreTop.scala 294:33]
  assign conflictController_io_rs2_use = CU_io_rs2_out; // @[CoreTop.scala 295:33]
  assign conflictController_io_csr_use = CU_io_csr_out; // @[CoreTop.scala 296:33]
  assign conflictController_io_IDEX_reg_to_write = ID_EX_io_out_wb_signal_rd; // @[CoreTop.scala 298:43]
  assign conflictController_io_IDEX_write_reg = ID_EX_io_out_wb_signal_write_reg; // @[CoreTop.scala 299:40]
  assign conflictController_io_IDEX_csr_to_write = ID_EX_io_out_wb_signal_csr_idx; // @[CoreTop.scala 300:43]
  assign conflictController_io_IDEX_write_csr = ID_EX_io_out_wb_signal_csr_write; // @[CoreTop.scala 301:40]
  assign conflictController_io_IDEX_reg_result_stage = ID_EX_io_out_result_stage; // @[CoreTop.scala 302:47]
  assign conflictController_io_EXMEM_reg_to_write = EX_MEM_io_out_wb_signal_rd; // @[CoreTop.scala 304:44]
  assign conflictController_io_EXMEM_write_reg = EX_MEM_io_out_wb_signal_write_reg; // @[CoreTop.scala 305:41]
  assign conflictController_io_EXMEM_csr_to_write = EX_MEM_io_out_wb_signal_csr_idx; // @[CoreTop.scala 306:44]
  assign conflictController_io_EXMEM_write_csr = EX_MEM_io_out_wb_signal_csr_write; // @[CoreTop.scala 307:41]
  assign conflictController_io_branch_type = ID_EX_io_out_branch_type; // @[CoreTop.scala 311:37]
  assign conflictController_io_pc = ID_EX_io_out_pc; // @[CoreTop.scala 312:28]
  assign conflictController_io_imm = ID_EX_io_out_wb_signal_imm; // @[CoreTop.scala 313:29]
  assign conflictController_io_alu_result = ALU_io_result; // @[CoreTop.scala 314:36]
  assign conflictController_io_cmp_result = CMP_io_result; // @[CoreTop.scala 315:36]
  assign conflictController_io_predict_next_pc = IF_ID_io_out_pc; // @[CoreTop.scala 316:41]
  assign conflictController_io_exception_occurs = CSR_io_fault_occurs; // @[CoreTop.scala 319:42]
  assign conflictController_io_exception_new_pc = CSR_io_fault_new_PC; // @[CoreTop.scala 320:42]
  assign state_clock = clock;
  assign state_reset = reset;
  assign state_io_stall = io_external_signal_load_data_mode; // @[CoreTop.scala 62:18]
  assign CSR_clock = clock;
  assign CSR_reset = reset;
  assign CSR_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 95:20]
  assign CSR_io_write = MEM_WB_io_out_wb_signal_csr_write; // @[CoreTop.scala 269:16]
  assign CSR_io_csr_idx_write = MEM_WB_io_out_wb_signal_csr_idx; // @[CoreTop.scala 270:24]
  assign CSR_io_write_data = MEM_WB_io_out_alu_out; // @[CoreTop.scala 271:21]
  assign CSR_io_csr_idx_read = CU_io_csr_out; // @[CoreTop.scala 96:23]
  assign CSR_io_IF_fault_IF_fault_type = EX_MEM_io_out_IF_fault_IF_fault_type; // @[CoreTop.scala 239:19]
  assign CSR_io_IF_fault_mtval = EX_MEM_io_out_IF_fault_mtval; // @[CoreTop.scala 239:19]
  assign CSR_io_ID_fault_ID_fault_type = EX_MEM_io_out_ID_fault_ID_fault_type; // @[CoreTop.scala 240:19]
  assign CSR_io_ID_fault_mtval = EX_MEM_io_out_ID_fault_mtval; // @[CoreTop.scala 240:19]
  assign CSR_io_MEM_fault_mem_fault_type = memory_io_MEM_fault_mem_fault_type; // @[CoreTop.scala 241:20]
  assign CSR_io_MEM_fault_mtval = memory_io_MEM_fault_mtval; // @[CoreTop.scala 241:20]
  assign CSR_io_pc = EX_MEM_io_out_pc; // @[CoreTop.scala 242:13]
  assign pc_clock = clock;
  assign pc_reset = reset;
  assign pc_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 72:19]
  assign pc_io_next_addr = nextPCGen_io_nextPC; // @[CoreTop.scala 73:19]
  assign nextPCGen_clock = clock;
  assign nextPCGen_reset = reset;
  assign nextPCGen_io_cpu_state_type = io_external_signal_load_data_mode ? 2'h0 : state_io_cpu_state; // @[CoreTop.scala 65:36]
  assign nextPCGen_io_nextPC_control_signal = conflictController_io_next_control_signal; // @[CoreTop.scala 67:38]
  assign nextPCGen_io_instruction = memory_io_ins_out; // @[CoreTop.scala 70:28]
  assign nextPCGen_io_prediction_failure_failure_occurs = conflictController_io_prediction_failure_failure_occurs; // @[CoreTop.scala 66:34]
  assign nextPCGen_io_prediction_failure_success_occurs = conflictController_io_prediction_failure_success_occurs; // @[CoreTop.scala 66:34]
  assign nextPCGen_io_prediction_failure_address = conflictController_io_prediction_failure_address; // @[CoreTop.scala 66:34]
  assign nextPCGen_io_pc = pc_io_addr; // @[CoreTop.scala 68:19]
  assign nextPCGen_io_new_pc = conflictController_io_new_pc; // @[CoreTop.scala 69:23]
  assign IF_ID_clock = clock;
  assign IF_ID_reset = reset;
  assign IF_ID_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 76:21]
  assign IF_ID_io_signal = conflictController_io_IFID_control_signal; // @[CoreTop.scala 77:19]
  assign IF_ID_io_in_pc = pc_io_addr; // @[CoreTop.scala 78:18]
  assign IF_ID_io_in_ins = memory_io_ins_out; // @[CoreTop.scala 79:19]
  assign IF_ID_io_in_IF_fault_IF_fault_type = memory_io_IF_fault_IF_fault_type; // @[CoreTop.scala 80:24]
  assign IF_ID_io_in_IF_fault_mtval = memory_io_IF_fault_mtval; // @[CoreTop.scala 80:24]
  assign insDecode_io_instruction = IF_ID_io_out_ins; // @[CoreTop.scala 83:28]
  assign CU_io_instruction = IF_ID_io_out_ins; // @[CoreTop.scala 84:21]
  assign CU_io_opcode = insDecode_io_opcode; // @[CoreTop.scala 86:16]
  assign CU_io_func3 = insDecode_io_func3; // @[CoreTop.scala 87:15]
  assign CU_io_func7 = insDecode_io_func7; // @[CoreTop.scala 88:15]
  assign CU_io_rs1 = insDecode_io_rs1; // @[CoreTop.scala 89:13]
  assign CU_io_rs2 = insDecode_io_rs2; // @[CoreTop.scala 90:13]
  assign CU_io_rd = insDecode_io_rd; // @[CoreTop.scala 91:12]
  assign CU_io_raw_imm = insDecode_io_raw_imm; // @[CoreTop.scala 92:17]
  assign CU_io_csr = insDecode_io_csr; // @[CoreTop.scala 85:13]
  assign CU_io_cur_privilege = CSR_io_cur_privilege; // @[CoreTop.scala 93:23]
  assign regs_clock = clock;
  assign regs_reset = reset;
  assign regs_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 99:21]
  assign regs_io_write = MEM_WB_io_out_wb_signal_write_reg; // @[CoreTop.scala 287:17]
  assign regs_io_rs1 = CU_io_rs1_out; // @[CoreTop.scala 100:15]
  assign regs_io_rs2 = CU_io_rs2_out; // @[CoreTop.scala 101:15]
  assign regs_io_rd = MEM_WB_io_out_wb_signal_rd; // @[CoreTop.scala 288:14]
  assign regs_io_write_data = wb_selector_io_write_data; // @[CoreTop.scala 289:22]
  assign immGen_io_raw_imm = CU_io_raw_imm_out; // @[CoreTop.scala 104:21]
  assign immGen_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 105:22]
  assign immGen_io_imm_width = CU_io_imm_width_type; // @[CoreTop.scala 106:23]
  assign rs1_forward_selector_io_forward_type = conflictController_io_rs1_forward_type; // @[CoreTop.scala 109:40]
  assign rs1_forward_selector_io_origin_val = regs_io_rs1_val; // @[CoreTop.scala 110:38]
  assign rs1_forward_selector_io_ex_forward = EXForwarding_io_write_data; // @[CoreTop.scala 111:38]
  assign rs1_forward_selector_io_mem_forward = MEMForwarding_io_write_data; // @[CoreTop.scala 112:39]
  assign rs2_forward_selector_io_forward_type = conflictController_io_rs2_forward_type; // @[CoreTop.scala 113:40]
  assign rs2_forward_selector_io_origin_val = regs_io_rs2_val; // @[CoreTop.scala 114:38]
  assign rs2_forward_selector_io_ex_forward = EXForwarding_io_write_data; // @[CoreTop.scala 115:38]
  assign rs2_forward_selector_io_mem_forward = MEMForwarding_io_write_data; // @[CoreTop.scala 116:39]
  assign csr_forward_selector_io_forward_type = conflictController_io_csr_forward_type; // @[CoreTop.scala 118:40]
  assign csr_forward_selector_io_origin_val = CSR_io_csr_val; // @[CoreTop.scala 119:38]
  assign csr_forward_selector_io_ex_forward = ALU_io_result; // @[CoreTop.scala 120:38]
  assign csr_forward_selector_io_mem_forward = EX_MEM_io_out_alu_out; // @[CoreTop.scala 121:39]
  assign operandSelector_io_operand2Type = CU_io_operand2_type; // @[CoreTop.scala 131:35]
  assign operandSelector_io_operand1Type = CU_io_operand1_type; // @[CoreTop.scala 132:35]
  assign operandSelector_io_rs1_val = rs1_forward_selector_io_real_val; // @[CoreTop.scala 128:30]
  assign operandSelector_io_rs2_val = rs2_forward_selector_io_real_val; // @[CoreTop.scala 129:30]
  assign operandSelector_io_real_imm = immGen_io_real_imm; // @[CoreTop.scala 130:31]
  assign operandSelector_io_csr_val = csr_forward_selector_io_real_val; // @[CoreTop.scala 127:30]
  assign EXForwarding_io_au_type = ID_EX_io_out_wb_signal_au_type; // @[CoreTop.scala 183:27]
  assign EXForwarding_io_alu_result = ALU_io_result; // @[CoreTop.scala 184:30]
  assign EXForwarding_io_cmp_result = CMP_io_result; // @[CoreTop.scala 186:30]
  assign EXForwarding_io_write_back_type = ID_EX_io_out_wb_signal_wb_type; // @[CoreTop.scala 187:35]
  assign EXForwarding_io_imm = ID_EX_io_out_wb_signal_imm; // @[CoreTop.scala 188:23]
  assign EXForwarding_io_mem_out = 32'h0;
  assign EXForwarding_io_pc4 = ID_EX_io_out_pc + 32'h4; // @[CoreTop.scala 190:42]
  assign EXForwarding_io_pcImm = ID_EX_io_out_pc + ID_EX_io_out_wb_signal_imm; // @[CoreTop.scala 191:44]
  assign EXForwarding_io_csr_val = ID_EX_io_out_csr_val_to_reg; // @[CoreTop.scala 192:27]
  assign MEMForwarding_io_au_type = EX_MEM_io_out_wb_signal_au_type; // @[CoreTop.scala 245:28]
  assign MEMForwarding_io_alu_result = EX_MEM_io_out_alu_out; // @[CoreTop.scala 246:31]
  assign MEMForwarding_io_cmp_result = EX_MEM_io_out_cmp_out; // @[CoreTop.scala 247:31]
  assign MEMForwarding_io_write_back_type = EX_MEM_io_out_wb_signal_wb_type; // @[CoreTop.scala 248:36]
  assign MEMForwarding_io_imm = EX_MEM_io_out_wb_signal_imm; // @[CoreTop.scala 249:24]
  assign MEMForwarding_io_mem_out = memory_io_data_out; // @[CoreTop.scala 250:28]
  assign MEMForwarding_io_pc4 = EX_MEM_io_out_pc + 32'h4; // @[CoreTop.scala 251:44]
  assign MEMForwarding_io_pcImm = EX_MEM_io_out_pc + EX_MEM_io_out_wb_signal_imm; // @[CoreTop.scala 252:46]
  assign MEMForwarding_io_csr_val = EX_MEM_io_out_csr_val_to_reg; // @[CoreTop.scala 253:28]
  assign ID_EX_clock = clock;
  assign ID_EX_reset = reset;
  assign ID_EX_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 135:21]
  assign ID_EX_io_signal = conflictController_io_IDEX_control_signal; // @[CoreTop.scala 136:19]
  assign ID_EX_io_in_pc = IF_ID_io_out_pc; // @[CoreTop.scala 137:18]
  assign ID_EX_io_in_IF_fault_IF_fault_type = IF_ID_io_out_IF_fault_IF_fault_type; // @[CoreTop.scala 139:24]
  assign ID_EX_io_in_IF_fault_mtval = IF_ID_io_out_IF_fault_mtval; // @[CoreTop.scala 139:24]
  assign ID_EX_io_in_op1 = operandSelector_io_operand1; // @[CoreTop.scala 141:19]
  assign ID_EX_io_in_op2 = operandSelector_io_operand2; // @[CoreTop.scala 142:19]
  assign ID_EX_io_in_csr_val_to_reg = csr_forward_selector_io_real_val; // @[CoreTop.scala 143:30]
  assign ID_EX_io_in_alu_op = CU_io_alu_type; // @[CoreTop.scala 145:22]
  assign ID_EX_io_in_cmp_op = CU_io_cmp_type; // @[CoreTop.scala 146:22]
  assign ID_EX_io_in_unsigned = CU_io_unsigned; // @[CoreTop.scala 147:24]
  assign ID_EX_io_in_mem_signal_read_data_en = CU_io_memory_read; // @[CoreTop.scala 149:39]
  assign ID_EX_io_in_mem_signal_write_data_en = CU_io_memory_write; // @[CoreTop.scala 150:40]
  assign ID_EX_io_in_mem_signal_data_width = CU_io_data_width; // @[CoreTop.scala 151:37]
  assign ID_EX_io_in_mem_signal_data_to_write = rs2_forward_selector_io_real_val; // @[CoreTop.scala 152:40]
  assign ID_EX_io_in_wb_signal_au_type = CU_io_au_type; // @[CoreTop.scala 154:33]
  assign ID_EX_io_in_wb_signal_wb_type = CU_io_write_back_type; // @[CoreTop.scala 155:33]
  assign ID_EX_io_in_wb_signal_imm = immGen_io_real_imm; // @[CoreTop.scala 156:29]
  assign ID_EX_io_in_wb_signal_rd = CU_io_rd_out; // @[CoreTop.scala 157:28]
  assign ID_EX_io_in_wb_signal_write_reg = CU_io_regs_write; // @[CoreTop.scala 158:35]
  assign ID_EX_io_in_wb_signal_csr_idx = CU_io_csr_out; // @[CoreTop.scala 159:33]
  assign ID_EX_io_in_wb_signal_csr_write = CU_io_csr_write; // @[CoreTop.scala 160:35]
  assign ID_EX_io_in_result_stage = CU_io_result_stage; // @[CoreTop.scala 162:28]
  assign ID_EX_io_in_branch_type = CU_io_branch_type; // @[CoreTop.scala 163:27]
  assign ID_EX_io_in_ID_fault_ID_fault_type = CU_io_fault_ID_fault_type; // @[CoreTop.scala 164:24]
  assign ID_EX_io_in_ID_fault_mtval = CU_io_fault_mtval; // @[CoreTop.scala 164:24]
  assign ALU_io_operand1 = ID_EX_io_out_op1; // @[CoreTop.scala 168:19]
  assign ALU_io_operand2 = ID_EX_io_out_op2; // @[CoreTop.scala 169:19]
  assign ALU_io_alu_op = ID_EX_io_out_alu_op; // @[CoreTop.scala 170:17]
  assign ALU_io_unsigned = ID_EX_io_out_cmp_op[0]; // @[CoreTop.scala 171:19]
  assign CMP_io_operand1 = ID_EX_io_out_op1; // @[CoreTop.scala 174:19]
  assign CMP_io_operand2 = ID_EX_io_out_op2; // @[CoreTop.scala 175:19]
  assign CMP_io_cmp_op = ID_EX_io_out_cmp_op; // @[CoreTop.scala 176:17]
  assign CMP_io_unsigned = ID_EX_io_out_unsigned; // @[CoreTop.scala 177:19]
  assign EX_MEM_clock = clock;
  assign EX_MEM_reset = reset;
  assign EX_MEM_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 195:22]
  assign EX_MEM_io_signal = conflictController_io_EXMEM_control_signal; // @[CoreTop.scala 196:20]
  assign EX_MEM_io_in_pc = ID_EX_io_out_pc; // @[CoreTop.scala 197:19]
  assign EX_MEM_io_in_IF_fault_IF_fault_type = ID_EX_io_out_IF_fault_IF_fault_type; // @[CoreTop.scala 200:25]
  assign EX_MEM_io_in_IF_fault_mtval = ID_EX_io_out_IF_fault_mtval; // @[CoreTop.scala 200:25]
  assign EX_MEM_io_in_ID_fault_ID_fault_type = ID_EX_io_out_ID_fault_ID_fault_type; // @[CoreTop.scala 201:25]
  assign EX_MEM_io_in_ID_fault_mtval = ID_EX_io_out_ID_fault_mtval; // @[CoreTop.scala 201:25]
  assign EX_MEM_io_in_csr_val_to_reg = ID_EX_io_out_csr_val_to_reg; // @[CoreTop.scala 202:31]
  assign EX_MEM_io_in_unsigned = ID_EX_io_out_unsigned; // @[CoreTop.scala 203:25]
  assign EX_MEM_io_in_mem_signal_read_data_en = ID_EX_io_out_mem_signal_read_data_en; // @[CoreTop.scala 204:27]
  assign EX_MEM_io_in_mem_signal_write_data_en = ID_EX_io_out_mem_signal_write_data_en; // @[CoreTop.scala 204:27]
  assign EX_MEM_io_in_mem_signal_data_width = ID_EX_io_out_mem_signal_data_width; // @[CoreTop.scala 204:27]
  assign EX_MEM_io_in_mem_signal_data_to_write = ID_EX_io_out_mem_signal_data_to_write; // @[CoreTop.scala 204:27]
  assign EX_MEM_io_in_wb_signal_au_type = ID_EX_io_out_wb_signal_au_type; // @[CoreTop.scala 205:26]
  assign EX_MEM_io_in_wb_signal_wb_type = ID_EX_io_out_wb_signal_wb_type; // @[CoreTop.scala 205:26]
  assign EX_MEM_io_in_wb_signal_imm = ID_EX_io_out_wb_signal_imm; // @[CoreTop.scala 205:26]
  assign EX_MEM_io_in_wb_signal_rd = ID_EX_io_out_wb_signal_rd; // @[CoreTop.scala 205:26]
  assign EX_MEM_io_in_wb_signal_write_reg = ID_EX_io_out_wb_signal_write_reg; // @[CoreTop.scala 205:26]
  assign EX_MEM_io_in_wb_signal_csr_idx = ID_EX_io_out_wb_signal_csr_idx; // @[CoreTop.scala 205:26]
  assign EX_MEM_io_in_wb_signal_csr_write = ID_EX_io_out_wb_signal_csr_write; // @[CoreTop.scala 205:26]
  assign EX_MEM_io_in_alu_out = ALU_io_result; // @[CoreTop.scala 206:24]
  assign EX_MEM_io_in_cmp_out = CMP_io_result; // @[CoreTop.scala 207:24]
  assign memory_clock = clock;
  assign memory_reset = reset;
  assign memory_io_uart_load = io_external_signal_load_data_mode; // @[CoreTop.scala 227:23]
  assign memory_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 228:23]
  assign memory_io_ins_addr = pc_io_addr; // @[CoreTop.scala 74:22]
  assign memory_io_read_data = memInSelector_io_read_data; // @[CoreTop.scala 233:23]
  assign memory_io_write_data = memInSelector_io_write_data; // @[CoreTop.scala 232:24]
  assign memory_io_unsigned = EX_MEM_io_out_unsigned; // @[CoreTop.scala 234:22]
  assign memory_io_data_width = memInSelector_io_data_width; // @[CoreTop.scala 231:24]
  assign memory_io_data_addr = memInSelector_io_data_addr; // @[CoreTop.scala 229:23]
  assign memory_io_data_write = memInSelector_io_data_write; // @[CoreTop.scala 230:24]
  assign memory_io_external_btn_button = io_external_btn_button; // @[CoreTop.scala 235:22]
  assign memory_io_external_switches_switches = io_external_switches_switches; // @[CoreTop.scala 235:22]
  assign memory_io_external_uart_rxData = io_external_uart_rxData; // @[CoreTop.scala 235:22]
  assign memory_io_external_uart_rxValid = io_external_uart_rxValid; // @[CoreTop.scala 235:22]
  assign memory_io_external_uart_txReady = io_external_uart_txReady; // @[CoreTop.scala 235:22]
  assign uartLoader_clock = clock;
  assign uartLoader_reset = reset;
  assign uartLoader_io_uart_load = io_external_signal_load_data_mode; // @[CoreTop.scala 212:27]
  assign uartLoader_io_rxValid = io_external_uart_rxValid; // @[CoreTop.scala 213:25]
  assign uartLoader_io_rxData = io_external_uart_rxData; // @[CoreTop.scala 214:24]
  assign memInSelector_io_uart_load = io_external_signal_load_data_mode; // @[CoreTop.scala 218:30]
  assign memInSelector_io_uart_in_mem_write = uartLoader_io_mem_mem_write; // @[CoreTop.scala 219:28]
  assign memInSelector_io_uart_in_data_to_write = uartLoader_io_mem_data_to_write; // @[CoreTop.scala 219:28]
  assign memInSelector_io_uart_in_data_addr = uartLoader_io_mem_data_addr; // @[CoreTop.scala 219:28]
  assign memInSelector_io_cpu_read_data = EX_MEM_io_out_mem_signal_read_data_en; // @[CoreTop.scala 220:34]
  assign memInSelector_io_cpu_write_data = EX_MEM_io_out_mem_signal_write_data_en; // @[CoreTop.scala 221:35]
  assign memInSelector_io_cpu_data_width = EX_MEM_io_out_mem_signal_data_width; // @[CoreTop.scala 222:35]
  assign memInSelector_io_cpu_data_addr = EX_MEM_io_out_alu_out; // @[CoreTop.scala 223:34]
  assign memInSelector_io_cpu_data_write = EX_MEM_io_out_mem_signal_data_to_write; // @[CoreTop.scala 224:35]
  assign MEM_WB_clock = clock;
  assign MEM_WB_reset = reset;
  assign MEM_WB_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 257:22]
  assign MEM_WB_io_signal = conflictController_io_MEMWB_control_signal; // @[CoreTop.scala 258:20]
  assign MEM_WB_io_in_pc = EX_MEM_io_out_pc; // @[CoreTop.scala 259:19]
  assign MEM_WB_io_in_csr_val_to_reg = EX_MEM_io_out_csr_val_to_reg; // @[CoreTop.scala 261:31]
  assign MEM_WB_io_in_wb_signal_au_type = EX_MEM_io_out_wb_signal_au_type; // @[CoreTop.scala 262:26]
  assign MEM_WB_io_in_wb_signal_wb_type = EX_MEM_io_out_wb_signal_wb_type; // @[CoreTop.scala 262:26]
  assign MEM_WB_io_in_wb_signal_imm = EX_MEM_io_out_wb_signal_imm; // @[CoreTop.scala 262:26]
  assign MEM_WB_io_in_wb_signal_rd = EX_MEM_io_out_wb_signal_rd; // @[CoreTop.scala 262:26]
  assign MEM_WB_io_in_wb_signal_write_reg = EX_MEM_io_out_wb_signal_write_reg; // @[CoreTop.scala 262:26]
  assign MEM_WB_io_in_wb_signal_csr_idx = EX_MEM_io_out_wb_signal_csr_idx; // @[CoreTop.scala 262:26]
  assign MEM_WB_io_in_wb_signal_csr_write = EX_MEM_io_out_wb_signal_csr_write; // @[CoreTop.scala 262:26]
  assign MEM_WB_io_in_alu_out = EX_MEM_io_out_alu_out; // @[CoreTop.scala 263:24]
  assign MEM_WB_io_in_cmp_out = EX_MEM_io_out_cmp_out; // @[CoreTop.scala 264:24]
  assign MEM_WB_io_in_data_out = memory_io_data_out; // @[CoreTop.scala 265:25]
  assign wb_selector_io_au_type = MEM_WB_io_out_wb_signal_au_type; // @[CoreTop.scala 276:26]
  assign wb_selector_io_alu_result = MEM_WB_io_out_alu_out; // @[CoreTop.scala 277:29]
  assign wb_selector_io_cmp_result = MEM_WB_io_out_cmp_out; // @[CoreTop.scala 278:29]
  assign wb_selector_io_write_back_type = MEM_WB_io_out_wb_signal_wb_type; // @[CoreTop.scala 279:34]
  assign wb_selector_io_imm = MEM_WB_io_out_wb_signal_imm; // @[CoreTop.scala 280:22]
  assign wb_selector_io_mem_out = MEM_WB_io_out_data_out; // @[CoreTop.scala 281:26]
  assign wb_selector_io_pc4 = MEM_WB_io_out_pc + 32'h4; // @[CoreTop.scala 282:42]
  assign wb_selector_io_pcImm = MEM_WB_io_out_pc + MEM_WB_io_out_wb_signal_imm; // @[CoreTop.scala 283:44]
  assign wb_selector_io_csr_val = MEM_WB_io_out_csr_val_to_reg; // @[CoreTop.scala 284:26]
endmodule
module Led(
  input  [31:0] io_mmio_led,
  output [23:0] io_board_led
);
  assign io_board_led = io_mmio_led[23:0]; // @[Led.scala 20:16]
endmodule
module HexToSeg7(
  input  [3:0] io_hexDigit,
  input  [1:0] io_special,
  input  [2:0] io_counter,
  output [7:0] io_seg7
);
  wire [7:0] _io_seg7_seg7_T_1 = 4'h1 == io_hexDigit ? 8'h9f : 8'h3; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_3 = 4'h2 == io_hexDigit ? 8'h25 : _io_seg7_seg7_T_1; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_5 = 4'h3 == io_hexDigit ? 8'hd : _io_seg7_seg7_T_3; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_7 = 4'h4 == io_hexDigit ? 8'h99 : _io_seg7_seg7_T_5; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_9 = 4'h5 == io_hexDigit ? 8'h49 : _io_seg7_seg7_T_7; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_11 = 4'h6 == io_hexDigit ? 8'h41 : _io_seg7_seg7_T_9; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_13 = 4'h7 == io_hexDigit ? 8'h1f : _io_seg7_seg7_T_11; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_15 = 4'h8 == io_hexDigit ? 8'h1 : _io_seg7_seg7_T_13; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_17 = 4'h9 == io_hexDigit ? 8'h9 : _io_seg7_seg7_T_15; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_19 = 4'ha == io_hexDigit ? 8'h11 : _io_seg7_seg7_T_17; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_21 = 4'hb == io_hexDigit ? 8'hc1 : _io_seg7_seg7_T_19; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_23 = 4'hc == io_hexDigit ? 8'h63 : _io_seg7_seg7_T_21; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_25 = 4'hd == io_hexDigit ? 8'h85 : _io_seg7_seg7_T_23; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_27 = 4'he == io_hexDigit ? 8'h61 : _io_seg7_seg7_T_25; // @[Mux.scala 81:58]
  wire [7:0] io_seg7_seg7 = 4'hf == io_hexDigit ? 8'h71 : _io_seg7_seg7_T_27; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_31 = 3'h0 == io_counter ? 8'h71 : 8'hff; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_33 = 3'h1 == io_counter ? 8'hd5 : _io_seg7_seg7_T_31; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_35 = 3'h2 == io_counter ? 8'hf : _io_seg7_seg7_T_33; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_37 = 3'h0 == io_counter ? 8'hd5 : 8'hff; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_39 = 3'h1 == io_counter ? 8'h11 : _io_seg7_seg7_T_37; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_41 = 3'h2 == io_counter ? 8'hd5 : _io_seg7_seg7_T_39; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_43 = 3'h0 == io_counter ? 8'h73 : 8'hff; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_45 = 3'h1 == io_counter ? 8'hc5 : _io_seg7_seg7_T_43; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_47 = 3'h2 == io_counter ? 8'h73 : _io_seg7_seg7_T_45; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_49 = 3'h3 == io_counter ? 8'h73 : _io_seg7_seg7_T_47; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_51 = 3'h4 == io_counter ? 8'h61 : _io_seg7_seg7_T_49; // @[Mux.scala 81:58]
  wire [7:0] _GEN_0 = io_special == 2'h3 ? _io_seg7_seg7_T_41 : _io_seg7_seg7_T_51; // @[Seg7.scala 111:37 112:12 120:12]
  wire [7:0] io_seg7_seg7_1 = io_special == 2'h2 ? _io_seg7_seg7_T_35 : _GEN_0; // @[Seg7.scala 103:31 104:12]
  assign io_seg7 = io_special == 2'h0 ? io_seg7_seg7 : io_seg7_seg7_1; // @[Seg7.scala 140:29 141:13 143:13]
endmodule
module Seg7(
  input         clock,
  input         reset,
  input  [31:0] io_mmio_seg7,
  output [7:0]  io_board_seg7,
  output [7:0]  io_board_an
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
`endif // RANDOMIZE_REG_INIT
  wire [3:0] seg7Module_io_hexDigit; // @[Seg7.scala 56:26]
  wire [1:0] seg7Module_io_special; // @[Seg7.scala 56:26]
  wire [2:0] seg7Module_io_counter; // @[Seg7.scala 56:26]
  wire [7:0] seg7Module_io_seg7; // @[Seg7.scala 56:26]
  reg [31:0] clock_counter; // @[Seg7.scala 25:30]
  wire [31:0] _clock_counter_T_1 = clock_counter - 32'h1; // @[Seg7.scala 27:34]
  reg [2:0] counter; // @[Seg7.scala 29:24]
  wire [2:0] _counter_T_1 = counter + 3'h1; // @[Seg7.scala 33:24]
  reg [1:0] special; // @[Seg7.scala 38:24]
  wire  _T_3 = io_mmio_seg7 == 32'h400000; // @[Seg7.scala 44:27]
  wire [3:0] digits_0 = io_mmio_seg7[3:0]; // @[Seg7.scala 50:57]
  wire [3:0] digits_1 = io_mmio_seg7[7:4]; // @[Seg7.scala 50:57]
  wire [3:0] digits_2 = io_mmio_seg7[11:8]; // @[Seg7.scala 50:57]
  wire [3:0] digits_3 = io_mmio_seg7[15:12]; // @[Seg7.scala 50:57]
  wire [3:0] digits_4 = io_mmio_seg7[19:16]; // @[Seg7.scala 50:57]
  wire [3:0] digits_5 = io_mmio_seg7[23:20]; // @[Seg7.scala 50:57]
  wire [3:0] digits_6 = io_mmio_seg7[27:24]; // @[Seg7.scala 50:57]
  wire [3:0] digits_7 = io_mmio_seg7[31:28]; // @[Seg7.scala 50:57]
  wire [3:0] _GEN_6 = 3'h1 == counter ? digits_1 : digits_0; // @[Seg7.scala 57:{26,26}]
  wire [3:0] _GEN_7 = 3'h2 == counter ? digits_2 : _GEN_6; // @[Seg7.scala 57:{26,26}]
  wire [3:0] _GEN_8 = 3'h3 == counter ? digits_3 : _GEN_7; // @[Seg7.scala 57:{26,26}]
  wire [3:0] _GEN_9 = 3'h4 == counter ? digits_4 : _GEN_8; // @[Seg7.scala 57:{26,26}]
  wire [3:0] _GEN_10 = 3'h5 == counter ? digits_5 : _GEN_9; // @[Seg7.scala 57:{26,26}]
  wire [3:0] _GEN_11 = 3'h6 == counter ? digits_6 : _GEN_10; // @[Seg7.scala 57:{26,26}]
  wire [7:0] _io_board_an_T_1 = 3'h1 == counter ? 8'hfd : 8'hfe; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_3 = 3'h2 == counter ? 8'hfb : _io_board_an_T_1; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_5 = 3'h3 == counter ? 8'hf7 : _io_board_an_T_3; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_7 = 3'h4 == counter ? 8'hef : _io_board_an_T_5; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_9 = 3'h5 == counter ? 8'hdf : _io_board_an_T_7; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_11 = 3'h6 == counter ? 8'hbf : _io_board_an_T_9; // @[Mux.scala 81:58]
  HexToSeg7 seg7Module ( // @[Seg7.scala 56:26]
    .io_hexDigit(seg7Module_io_hexDigit),
    .io_special(seg7Module_io_special),
    .io_counter(seg7Module_io_counter),
    .io_seg7(seg7Module_io_seg7)
  );
  assign io_board_seg7 = seg7Module_io_seg7; // @[Seg7.scala 60:17]
  assign io_board_an = 3'h7 == counter ? 8'h7f : _io_board_an_T_11; // @[Mux.scala 81:58]
  assign seg7Module_io_hexDigit = 3'h7 == counter ? digits_7 : _GEN_11; // @[Seg7.scala 57:{26,26}]
  assign seg7Module_io_special = special; // @[Seg7.scala 58:25]
  assign seg7Module_io_counter = counter; // @[Seg7.scala 59:25]
  always @(posedge clock) begin
    if (reset) begin // @[Seg7.scala 25:30]
      clock_counter <= 32'h186a0; // @[Seg7.scala 25:30]
    end else if (clock_counter == 32'h0) begin // @[Seg7.scala 31:31]
      clock_counter <= 32'h186a0; // @[Seg7.scala 32:19]
    end else begin
      clock_counter <= _clock_counter_T_1; // @[Seg7.scala 27:17]
    end
    if (reset) begin // @[Seg7.scala 29:24]
      counter <= 3'h0; // @[Seg7.scala 29:24]
    end else if (clock_counter == 32'h0) begin // @[Seg7.scala 31:31]
      counter <= _counter_T_1; // @[Seg7.scala 33:13]
    end
    if (reset) begin // @[Seg7.scala 38:24]
      special <= 2'h0; // @[Seg7.scala 38:24]
    end else if (io_mmio_seg7 == 32'h800000) begin // @[Seg7.scala 40:44]
      special <= 2'h2; // @[Seg7.scala 41:13]
    end else if (io_mmio_seg7 == 32'hc00000) begin // @[Seg7.scala 42:50]
      special <= 2'h3; // @[Seg7.scala 43:13]
    end else begin
      special <= {{1'd0}, _T_3};
    end
  end
// Register and memory initialization
`ifdef RANDOMIZE_GARBAGE_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_INVALID_ASSIGN
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_REG_INIT
`define RANDOMIZE
`endif
`ifdef RANDOMIZE_MEM_INIT
`define RANDOMIZE
`endif
`ifndef RANDOM
`define RANDOM $random
`endif
`ifdef RANDOMIZE_MEM_INIT
  integer initvar;
`endif
`ifndef SYNTHESIS
`ifdef FIRRTL_BEFORE_INITIAL
`FIRRTL_BEFORE_INITIAL
`endif
initial begin
  `ifdef RANDOMIZE
    `ifdef INIT_RANDOM
      `INIT_RANDOM
    `endif
    `ifndef VERILATOR
      `ifdef RANDOMIZE_DELAY
        #`RANDOMIZE_DELAY begin end
      `else
        #0.002 begin end
      `endif
    `endif
`ifdef RANDOMIZE_REG_INIT
  _RAND_0 = {1{`RANDOM}};
  clock_counter = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  counter = _RAND_1[2:0];
  _RAND_2 = {1{`RANDOM}};
  special = _RAND_2[1:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module Button(
  output [31:0] io_mmio_button,
  input  [4:0]  io_board_button
);
  assign io_mmio_button = {{27'd0}, io_board_button}; // @[Button.scala 24:18]
endmodule
module Switches(
  output [31:0] io_mmio_switches,
  input  [23:0] io_board_switches
);
  assign io_mmio_switches = {{8'd0}, io_board_switches}; // @[Switches.scala 23:20]
endmodule
module UARTWrapper(
  input        clock,
  input        reset,
  input        io_board_rx,
  output       io_board_tx,
  output [7:0] io_mmio_rxData,
  output       io_mmio_rxValid,
  input        io_mmio_rxReady,
  input  [7:0] io_mmio_txData,
  input        io_mmio_txStart,
  output       io_mmio_txReady
);
  wire  uart_clk; // @[UARTWrapper.scala 32:22]
  wire  uart_rst; // @[UARTWrapper.scala 32:22]
  wire [7:0] uart_s_axis_tdata; // @[UARTWrapper.scala 32:22]
  wire  uart_s_axis_tvalid; // @[UARTWrapper.scala 32:22]
  wire  uart_s_axis_tready; // @[UARTWrapper.scala 32:22]
  wire [7:0] uart_m_axis_tdata; // @[UARTWrapper.scala 32:22]
  wire  uart_m_axis_tvalid; // @[UARTWrapper.scala 32:22]
  wire  uart_m_axis_tready; // @[UARTWrapper.scala 32:22]
  wire  uart_rxd; // @[UARTWrapper.scala 32:22]
  wire  uart_txd; // @[UARTWrapper.scala 32:22]
  wire  uart_tx_busy; // @[UARTWrapper.scala 32:22]
  wire  uart_rx_busy; // @[UARTWrapper.scala 32:22]
  wire  uart_rx_overrun_error; // @[UARTWrapper.scala 32:22]
  wire  uart_rx_frame_error; // @[UARTWrapper.scala 32:22]
  wire [15:0] uart_prescale; // @[UARTWrapper.scala 32:22]
  uart uart ( // @[UARTWrapper.scala 32:22]
    .clk(uart_clk),
    .rst(uart_rst),
    .s_axis_tdata(uart_s_axis_tdata),
    .s_axis_tvalid(uart_s_axis_tvalid),
    .s_axis_tready(uart_s_axis_tready),
    .m_axis_tdata(uart_m_axis_tdata),
    .m_axis_tvalid(uart_m_axis_tvalid),
    .m_axis_tready(uart_m_axis_tready),
    .rxd(uart_rxd),
    .txd(uart_txd),
    .tx_busy(uart_tx_busy),
    .rx_busy(uart_rx_busy),
    .rx_overrun_error(uart_rx_overrun_error),
    .rx_frame_error(uart_rx_frame_error),
    .prescale(uart_prescale)
  );
  assign io_board_tx = uart_txd; // @[UARTWrapper.scala 41:17]
  assign io_mmio_rxData = uart_m_axis_tdata; // @[UARTWrapper.scala 43:20]
  assign io_mmio_rxValid = uart_m_axis_tvalid; // @[UARTWrapper.scala 44:21]
  assign io_mmio_txReady = uart_s_axis_tready; // @[UARTWrapper.scala 38:21]
  assign uart_clk = clock; // @[UARTWrapper.scala 33:26]
  assign uart_rst = reset; // @[UARTWrapper.scala 34:26]
  assign uart_s_axis_tdata = io_mmio_txData; // @[UARTWrapper.scala 36:26]
  assign uart_s_axis_tvalid = io_mmio_txStart; // @[UARTWrapper.scala 37:27]
  assign uart_m_axis_tready = io_mmio_rxReady; // @[UARTWrapper.scala 45:27]
  assign uart_rxd = io_board_rx; // @[UARTWrapper.scala 40:17]
  assign uart_prescale = 16'h516; // @[UARTWrapper.scala 47:22]
endmodule
module VGA(
  output        io_board_hsync,
  output        io_board_vsync,
  output [11:0] io_board_vga_rgb,
  input  [31:0] io_mmio_value
);
  wire [31:0] vga_digit; // @[VGA.scala 21:19]
  wire  vga_hsync; // @[VGA.scala 21:19]
  wire  vga_vsync; // @[VGA.scala 21:19]
  wire [11:0] vga_vga_rgb; // @[VGA.scala 21:19]
  vga vga ( // @[VGA.scala 21:19]
    .digit(vga_digit),
    .hsync(vga_hsync),
    .vsync(vga_vsync),
    .vga_rgb(vga_vga_rgb)
  );
  assign io_board_hsync = vga_hsync; // @[VGA.scala 23:18]
  assign io_board_vsync = vga_vsync; // @[VGA.scala 24:18]
  assign io_board_vga_rgb = vga_vga_rgb; // @[VGA.scala 25:20]
  assign vga_digit = io_mmio_value; // @[VGA.scala 22:16]
endmodule
module DeviceTop(
  input         clock,
  input         reset,
  input  [31:0] io_mmio_led_led,
  input  [31:0] io_mmio_seg7_seg7,
  output [31:0] io_mmio_btn_button,
  output [31:0] io_mmio_switches_switches,
  output [7:0]  io_mmio_uart_rxData,
  output        io_mmio_uart_rxValid,
  input         io_mmio_uart_rxReady,
  input  [7:0]  io_mmio_uart_txData,
  input         io_mmio_uart_txStart,
  output        io_mmio_uart_txReady,
  output [23:0] io_board_led_led,
  output [7:0]  io_board_seg7_seg7,
  output [7:0]  io_board_seg7_an,
  input  [4:0]  io_board_btn_button,
  input  [23:0] io_board_switch_switches,
  input         io_board_uart_rx,
  output        io_board_uart_tx,
  output        io_board_vga_hsync,
  output        io_board_vga_vsync,
  output [11:0] io_board_vga_vga_rgb,
  output        io_external_signal_load_data_mode
);
  wire [31:0] led_io_mmio_led; // @[DeviceTop.scala 46:19]
  wire [23:0] led_io_board_led; // @[DeviceTop.scala 46:19]
  wire  seg7_clock; // @[DeviceTop.scala 47:20]
  wire  seg7_reset; // @[DeviceTop.scala 47:20]
  wire [31:0] seg7_io_mmio_seg7; // @[DeviceTop.scala 47:20]
  wire [7:0] seg7_io_board_seg7; // @[DeviceTop.scala 47:20]
  wire [7:0] seg7_io_board_an; // @[DeviceTop.scala 47:20]
  wire [31:0] btn_io_mmio_button; // @[DeviceTop.scala 48:19]
  wire [4:0] btn_io_board_button; // @[DeviceTop.scala 48:19]
  wire [31:0] switches_io_mmio_switches; // @[DeviceTop.scala 49:24]
  wire [23:0] switches_io_board_switches; // @[DeviceTop.scala 49:24]
  wire  uart_clock; // @[DeviceTop.scala 50:20]
  wire  uart_reset; // @[DeviceTop.scala 50:20]
  wire  uart_io_board_rx; // @[DeviceTop.scala 50:20]
  wire  uart_io_board_tx; // @[DeviceTop.scala 50:20]
  wire [7:0] uart_io_mmio_rxData; // @[DeviceTop.scala 50:20]
  wire  uart_io_mmio_rxValid; // @[DeviceTop.scala 50:20]
  wire  uart_io_mmio_rxReady; // @[DeviceTop.scala 50:20]
  wire [7:0] uart_io_mmio_txData; // @[DeviceTop.scala 50:20]
  wire  uart_io_mmio_txStart; // @[DeviceTop.scala 50:20]
  wire  uart_io_mmio_txReady; // @[DeviceTop.scala 50:20]
  wire  vga_io_board_hsync; // @[DeviceTop.scala 51:19]
  wire  vga_io_board_vsync; // @[DeviceTop.scala 51:19]
  wire [11:0] vga_io_board_vga_rgb; // @[DeviceTop.scala 51:19]
  wire [31:0] vga_io_mmio_value; // @[DeviceTop.scala 51:19]
  Led led ( // @[DeviceTop.scala 46:19]
    .io_mmio_led(led_io_mmio_led),
    .io_board_led(led_io_board_led)
  );
  Seg7 seg7 ( // @[DeviceTop.scala 47:20]
    .clock(seg7_clock),
    .reset(seg7_reset),
    .io_mmio_seg7(seg7_io_mmio_seg7),
    .io_board_seg7(seg7_io_board_seg7),
    .io_board_an(seg7_io_board_an)
  );
  Button btn ( // @[DeviceTop.scala 48:19]
    .io_mmio_button(btn_io_mmio_button),
    .io_board_button(btn_io_board_button)
  );
  Switches switches ( // @[DeviceTop.scala 49:24]
    .io_mmio_switches(switches_io_mmio_switches),
    .io_board_switches(switches_io_board_switches)
  );
  UARTWrapper uart ( // @[DeviceTop.scala 50:20]
    .clock(uart_clock),
    .reset(uart_reset),
    .io_board_rx(uart_io_board_rx),
    .io_board_tx(uart_io_board_tx),
    .io_mmio_rxData(uart_io_mmio_rxData),
    .io_mmio_rxValid(uart_io_mmio_rxValid),
    .io_mmio_rxReady(uart_io_mmio_rxReady),
    .io_mmio_txData(uart_io_mmio_txData),
    .io_mmio_txStart(uart_io_mmio_txStart),
    .io_mmio_txReady(uart_io_mmio_txReady)
  );
  VGA vga ( // @[DeviceTop.scala 51:19]
    .io_board_hsync(vga_io_board_hsync),
    .io_board_vsync(vga_io_board_vsync),
    .io_board_vga_rgb(vga_io_board_vga_rgb),
    .io_mmio_value(vga_io_mmio_value)
  );
  assign io_mmio_btn_button = btn_io_mmio_button; // @[DeviceTop.scala 55:15]
  assign io_mmio_switches_switches = switches_io_mmio_switches; // @[DeviceTop.scala 56:20]
  assign io_mmio_uart_rxData = uart_io_mmio_rxData; // @[DeviceTop.scala 57:16]
  assign io_mmio_uart_rxValid = uart_io_mmio_rxValid; // @[DeviceTop.scala 57:16]
  assign io_mmio_uart_txReady = uart_io_mmio_txReady; // @[DeviceTop.scala 57:16]
  assign io_board_led_led = led_io_board_led; // @[DeviceTop.scala 61:16]
  assign io_board_seg7_seg7 = seg7_io_board_seg7; // @[DeviceTop.scala 62:17]
  assign io_board_seg7_an = seg7_io_board_an; // @[DeviceTop.scala 62:17]
  assign io_board_uart_tx = uart_io_board_tx; // @[DeviceTop.scala 65:17]
  assign io_board_vga_hsync = vga_io_board_hsync; // @[DeviceTop.scala 66:16]
  assign io_board_vga_vsync = vga_io_board_vsync; // @[DeviceTop.scala 66:16]
  assign io_board_vga_vga_rgb = vga_io_board_vga_rgb; // @[DeviceTop.scala 66:16]
  assign io_external_signal_load_data_mode = io_board_switch_switches[23]; // @[DeviceTop.scala 69:64]
  assign led_io_mmio_led = io_mmio_led_led; // @[DeviceTop.scala 53:15]
  assign seg7_clock = clock;
  assign seg7_reset = reset;
  assign seg7_io_mmio_seg7 = io_mmio_seg7_seg7; // @[DeviceTop.scala 54:16]
  assign btn_io_board_button = io_board_btn_button; // @[DeviceTop.scala 63:16]
  assign switches_io_board_switches = io_board_switch_switches; // @[DeviceTop.scala 64:21]
  assign uart_clock = clock;
  assign uart_reset = reset;
  assign uart_io_board_rx = io_board_uart_rx; // @[DeviceTop.scala 65:17]
  assign uart_io_mmio_rxReady = io_mmio_uart_rxReady; // @[DeviceTop.scala 57:16]
  assign uart_io_mmio_txData = io_mmio_uart_txData; // @[DeviceTop.scala 57:16]
  assign uart_io_mmio_txStart = io_mmio_uart_txStart; // @[DeviceTop.scala 57:16]
  assign vga_io_mmio_value = io_mmio_seg7_seg7; // @[DeviceTop.scala 59:20]
endmodule
module Top(
  input         clock,
  input         reset,
  output [23:0] io_led_led,
  output [7:0]  io_seg7_seg7,
  output [7:0]  io_seg7_an,
  input  [4:0]  io_btn_button,
  input  [23:0] io_switch_switches,
  input         io_uart_rx,
  output        io_uart_tx,
  output        io_vga_hsync,
  output        io_vga_vsync,
  output [11:0] io_vga_vga_rgb
);
  wire  clockSeparator_clock; // @[Top.scala 15:30]
  wire  clockSeparator_io_cpuClock; // @[Top.scala 15:30]
  wire  cpu_clock; // @[Top.scala 17:11]
  wire  cpu_reset; // @[Top.scala 17:11]
  wire [31:0] cpu_io_external_led_led; // @[Top.scala 17:11]
  wire [31:0] cpu_io_external_seg7_seg7; // @[Top.scala 17:11]
  wire [31:0] cpu_io_external_btn_button; // @[Top.scala 17:11]
  wire [31:0] cpu_io_external_switches_switches; // @[Top.scala 17:11]
  wire [7:0] cpu_io_external_uart_rxData; // @[Top.scala 17:11]
  wire  cpu_io_external_uart_rxValid; // @[Top.scala 17:11]
  wire  cpu_io_external_uart_rxReady; // @[Top.scala 17:11]
  wire [7:0] cpu_io_external_uart_txData; // @[Top.scala 17:11]
  wire  cpu_io_external_uart_txStart; // @[Top.scala 17:11]
  wire  cpu_io_external_uart_txReady; // @[Top.scala 17:11]
  wire  cpu_io_external_signal_load_data_mode; // @[Top.scala 17:11]
  wire  device_clock; // @[Top.scala 19:22]
  wire  device_reset; // @[Top.scala 19:22]
  wire [31:0] device_io_mmio_led_led; // @[Top.scala 19:22]
  wire [31:0] device_io_mmio_seg7_seg7; // @[Top.scala 19:22]
  wire [31:0] device_io_mmio_btn_button; // @[Top.scala 19:22]
  wire [31:0] device_io_mmio_switches_switches; // @[Top.scala 19:22]
  wire [7:0] device_io_mmio_uart_rxData; // @[Top.scala 19:22]
  wire  device_io_mmio_uart_rxValid; // @[Top.scala 19:22]
  wire  device_io_mmio_uart_rxReady; // @[Top.scala 19:22]
  wire [7:0] device_io_mmio_uart_txData; // @[Top.scala 19:22]
  wire  device_io_mmio_uart_txStart; // @[Top.scala 19:22]
  wire  device_io_mmio_uart_txReady; // @[Top.scala 19:22]
  wire [23:0] device_io_board_led_led; // @[Top.scala 19:22]
  wire [7:0] device_io_board_seg7_seg7; // @[Top.scala 19:22]
  wire [7:0] device_io_board_seg7_an; // @[Top.scala 19:22]
  wire [4:0] device_io_board_btn_button; // @[Top.scala 19:22]
  wire [23:0] device_io_board_switch_switches; // @[Top.scala 19:22]
  wire  device_io_board_uart_rx; // @[Top.scala 19:22]
  wire  device_io_board_uart_tx; // @[Top.scala 19:22]
  wire  device_io_board_vga_hsync; // @[Top.scala 19:22]
  wire  device_io_board_vga_vsync; // @[Top.scala 19:22]
  wire [11:0] device_io_board_vga_vga_rgb; // @[Top.scala 19:22]
  wire  device_io_external_signal_load_data_mode; // @[Top.scala 19:22]
  ClockSeparator clockSeparator ( // @[Top.scala 15:30]
    .clock(clockSeparator_clock),
    .io_cpuClock(clockSeparator_io_cpuClock)
  );
  CoreTop cpu ( // @[Top.scala 17:11]
    .clock(cpu_clock),
    .reset(cpu_reset),
    .io_external_led_led(cpu_io_external_led_led),
    .io_external_seg7_seg7(cpu_io_external_seg7_seg7),
    .io_external_btn_button(cpu_io_external_btn_button),
    .io_external_switches_switches(cpu_io_external_switches_switches),
    .io_external_uart_rxData(cpu_io_external_uart_rxData),
    .io_external_uart_rxValid(cpu_io_external_uart_rxValid),
    .io_external_uart_rxReady(cpu_io_external_uart_rxReady),
    .io_external_uart_txData(cpu_io_external_uart_txData),
    .io_external_uart_txStart(cpu_io_external_uart_txStart),
    .io_external_uart_txReady(cpu_io_external_uart_txReady),
    .io_external_signal_load_data_mode(cpu_io_external_signal_load_data_mode)
  );
  DeviceTop device ( // @[Top.scala 19:22]
    .clock(device_clock),
    .reset(device_reset),
    .io_mmio_led_led(device_io_mmio_led_led),
    .io_mmio_seg7_seg7(device_io_mmio_seg7_seg7),
    .io_mmio_btn_button(device_io_mmio_btn_button),
    .io_mmio_switches_switches(device_io_mmio_switches_switches),
    .io_mmio_uart_rxData(device_io_mmio_uart_rxData),
    .io_mmio_uart_rxValid(device_io_mmio_uart_rxValid),
    .io_mmio_uart_rxReady(device_io_mmio_uart_rxReady),
    .io_mmio_uart_txData(device_io_mmio_uart_txData),
    .io_mmio_uart_txStart(device_io_mmio_uart_txStart),
    .io_mmio_uart_txReady(device_io_mmio_uart_txReady),
    .io_board_led_led(device_io_board_led_led),
    .io_board_seg7_seg7(device_io_board_seg7_seg7),
    .io_board_seg7_an(device_io_board_seg7_an),
    .io_board_btn_button(device_io_board_btn_button),
    .io_board_switch_switches(device_io_board_switch_switches),
    .io_board_uart_rx(device_io_board_uart_rx),
    .io_board_uart_tx(device_io_board_uart_tx),
    .io_board_vga_hsync(device_io_board_vga_hsync),
    .io_board_vga_vsync(device_io_board_vga_vsync),
    .io_board_vga_vga_rgb(device_io_board_vga_vga_rgb),
    .io_external_signal_load_data_mode(device_io_external_signal_load_data_mode)
  );
  assign io_led_led = device_io_board_led_led; // @[Top.scala 22:18]
  assign io_seg7_seg7 = device_io_board_seg7_seg7; // @[Top.scala 22:18]
  assign io_seg7_an = device_io_board_seg7_an; // @[Top.scala 22:18]
  assign io_uart_tx = device_io_board_uart_tx; // @[Top.scala 22:18]
  assign io_vga_hsync = device_io_board_vga_hsync; // @[Top.scala 22:18]
  assign io_vga_vsync = device_io_board_vga_vsync; // @[Top.scala 22:18]
  assign io_vga_vga_rgb = device_io_board_vga_vga_rgb; // @[Top.scala 22:18]
  assign clockSeparator_clock = clock;
  assign cpu_clock = clockSeparator_io_cpuClock;
  assign cpu_reset = reset;
  assign cpu_io_external_btn_button = device_io_mmio_btn_button; // @[Top.scala 25:18]
  assign cpu_io_external_switches_switches = device_io_mmio_switches_switches; // @[Top.scala 25:18]
  assign cpu_io_external_uart_rxData = device_io_mmio_uart_rxData; // @[Top.scala 25:18]
  assign cpu_io_external_uart_rxValid = device_io_mmio_uart_rxValid; // @[Top.scala 25:18]
  assign cpu_io_external_uart_txReady = device_io_mmio_uart_txReady; // @[Top.scala 25:18]
  assign cpu_io_external_signal_load_data_mode = device_io_external_signal_load_data_mode; // @[Top.scala 26:25]
  assign device_clock = clock;
  assign device_reset = reset;
  assign device_io_mmio_led_led = cpu_io_external_led_led; // @[Top.scala 25:18]
  assign device_io_mmio_seg7_seg7 = cpu_io_external_seg7_seg7; // @[Top.scala 25:18]
  assign device_io_mmio_uart_rxReady = cpu_io_external_uart_rxReady; // @[Top.scala 25:18]
  assign device_io_mmio_uart_txData = cpu_io_external_uart_txData; // @[Top.scala 25:18]
  assign device_io_mmio_uart_txStart = cpu_io_external_uart_txStart; // @[Top.scala 25:18]
  assign device_io_board_btn_button = io_btn_button; // @[Top.scala 22:18]
  assign device_io_board_switch_switches = io_switch_switches; // @[Top.scala 22:18]
  assign device_io_board_uart_rx = io_uart_rx; // @[Top.scala 22:18]
endmodule
