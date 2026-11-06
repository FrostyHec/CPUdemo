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
module CSR(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  output [1:0]  io_cur_privilege,
  input         io_write,
  input  [11:0] io_csr,
  input  [31:0] io_write_data,
  output [31:0] io_csr_val,
  input  [2:0]  io_mem_fault_mem_fault_type,
  input  [31:0] io_mem_fault_mtval,
  input  [2:0]  io_ins_fault_ins_fault_type,
  input  [31:0] io_ins_fault_mtval,
  input  [31:0] io_pc,
  output        io_fault_state,
  output [31:0] io_fault_write_PC
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
  reg [1:0] cur_privilege; // @[CSR.scala 35:30]
  reg [31:0] mstatus; // @[CSR.scala 38:24]
  reg [31:0] mie; // @[CSR.scala 39:20]
  reg [31:0] mtvec; // @[CSR.scala 40:22]
  reg [31:0] mscratch; // @[CSR.scala 42:25]
  reg [31:0] mepc; // @[CSR.scala 43:21]
  reg [31:0] mcause; // @[CSR.scala 44:23]
  reg [31:0] mtval; // @[CSR.scala 45:22]
  reg [31:0] mip; // @[CSR.scala 46:20]
  wire  read_MPIE = mstatus[7]; // @[CSR.scala 50:26]
  wire [1:0] read_MPP = mstatus[12:11]; // @[CSR.scala 51:25]
  wire [31:0] _mip_T_4 = ~mip; // @[CSR.scala 53:20]
  wire [31:0] _mip_T_5 = _mip_T_4 | 32'h800; // @[CSR.scala 53:20]
  wire [31:0] _mip_T_6 = ~_mip_T_5; // @[CSR.scala 53:20]
  wire  _no_fault_T_1 = io_ins_fault_ins_fault_type == 3'h0; // @[CSR.scala 56:34]
  wire  no_fault = io_mem_fault_mem_fault_type == 3'h0 & _no_fault_T_1; // @[CSR.scala 55:76]
  wire [31:0] _GEN_0 = io_cpu_state == 2'h1 & io_write & no_fault ? io_write_data : mstatus; // @[CSR.scala 62:84 63:13 38:24]
  wire [31:0] _GEN_2 = io_csr == 12'h300 ? _GEN_0 : mstatus; // @[CSR.scala 38:24 60:26]
  wire [31:0] _GEN_4 = io_csr == 12'h304 ? mie : mstatus; // @[CSR.scala 60:26 61:18]
  wire [31:0] _GEN_7 = io_csr == 12'h305 ? mtvec : _GEN_4; // @[CSR.scala 60:26 61:18]
  wire [31:0] _GEN_10 = io_csr == 12'h340 ? mscratch : _GEN_7; // @[CSR.scala 60:26 61:18]
  wire [31:0] _GEN_12 = io_cpu_state == 2'h1 & io_write & no_fault ? io_write_data : mepc; // @[CSR.scala 62:84 63:13 43:21]
  wire [31:0] _GEN_13 = io_csr == 12'h341 ? mepc : _GEN_10; // @[CSR.scala 60:26 61:18]
  wire [31:0] _GEN_14 = io_csr == 12'h341 ? _GEN_12 : mepc; // @[CSR.scala 43:21 60:26]
  wire [31:0] _GEN_15 = io_cpu_state == 2'h1 & io_write & no_fault ? io_write_data : mcause; // @[CSR.scala 62:84 63:13 44:23]
  wire [31:0] _GEN_16 = io_csr == 12'h342 ? mcause : _GEN_13; // @[CSR.scala 60:26 61:18]
  wire [31:0] _GEN_17 = io_csr == 12'h342 ? _GEN_15 : mcause; // @[CSR.scala 44:23 60:26]
  wire [31:0] _GEN_18 = io_cpu_state == 2'h1 & io_write & no_fault ? io_write_data : mtval; // @[CSR.scala 62:84 63:13 45:22]
  wire [31:0] _GEN_19 = io_csr == 12'h343 ? mtval : _GEN_16; // @[CSR.scala 60:26 61:18]
  wire [31:0] _GEN_20 = io_csr == 12'h343 ? _GEN_18 : mtval; // @[CSR.scala 45:22 60:26]
  wire  fault_type = io_ins_fault_ins_fault_type == 3'h4; // @[CSR.scala 108:38]
  wire [31:0] _mstatus_T_14 = {mstatus[31:13],cur_privilege,mstatus[10:0]}; // @[Cat.scala 33:92]
  wire  _T_43 = io_mem_fault_mem_fault_type == 3'h2; // @[CSR.scala 135:47]
  wire [31:0] _GEN_33 = io_mem_fault_mem_fault_type == 3'h2 ? io_pc : _GEN_14; // @[CSR.scala 135:82 77:10]
  wire [31:0] _GEN_35 = io_mem_fault_mem_fault_type == 3'h2 ? 32'h1 : _GEN_17; // @[CSR.scala 135:82 79:12]
  wire [31:0] _GEN_36 = io_mem_fault_mem_fault_type == 3'h2 ? io_mem_fault_mtval : _GEN_20; // @[CSR.scala 135:82 80:11]
  wire [31:0] _GEN_37 = io_mem_fault_mem_fault_type == 3'h2 ? _mstatus_T_14 : _GEN_2; // @[CSR.scala 135:82 85:13]
  wire [1:0] _GEN_38 = io_mem_fault_mem_fault_type == 3'h2 ? 2'h3 : cur_privilege; // @[CSR.scala 135:82 86:19]
  wire  _GEN_39 = io_mem_fault_mem_fault_type == 3'h1 | _T_43; // @[CSR.scala 132:87 76:20]
  wire [31:0] _GEN_40 = io_mem_fault_mem_fault_type == 3'h1 ? io_pc : _GEN_33; // @[CSR.scala 132:87 77:10]
  wire [31:0] _GEN_42 = io_mem_fault_mem_fault_type == 3'h1 ? 32'h0 : _GEN_35; // @[CSR.scala 132:87 79:12]
  wire [31:0] _GEN_43 = io_mem_fault_mem_fault_type == 3'h1 ? io_mem_fault_mtval : _GEN_36; // @[CSR.scala 132:87 80:11]
  wire [31:0] _GEN_44 = io_mem_fault_mem_fault_type == 3'h1 ? _mstatus_T_14 : _GEN_37; // @[CSR.scala 132:87 85:13]
  wire [1:0] _GEN_45 = io_mem_fault_mem_fault_type == 3'h1 ? 2'h3 : _GEN_38; // @[CSR.scala 132:87 86:19]
  wire  _GEN_46 = io_ins_fault_ins_fault_type == 3'h3 | _GEN_39; // @[CSR.scala 128:81 76:20]
  wire [31:0] _GEN_47 = io_ins_fault_ins_fault_type == 3'h3 ? io_pc : _GEN_40; // @[CSR.scala 128:81 77:10]
  wire [31:0] _GEN_49 = io_ins_fault_ins_fault_type == 3'h3 ? 32'hb : _GEN_42; // @[CSR.scala 128:81 79:12]
  wire [31:0] _GEN_50 = io_ins_fault_ins_fault_type == 3'h3 ? io_ins_fault_mtval : _GEN_43; // @[CSR.scala 128:81 80:11]
  wire [31:0] _GEN_51 = io_ins_fault_ins_fault_type == 3'h3 ? _mstatus_T_14 : _GEN_44; // @[CSR.scala 128:81 85:13]
  wire [1:0] _GEN_52 = io_ins_fault_ins_fault_type == 3'h3 ? 2'h3 : _GEN_45; // @[CSR.scala 128:81 86:19]
  wire  _GEN_53 = io_ins_fault_ins_fault_type == 3'h2 | _GEN_46; // @[CSR.scala 126:85 76:20]
  wire [31:0] _GEN_54 = io_ins_fault_ins_fault_type == 3'h2 ? io_pc : _GEN_47; // @[CSR.scala 126:85 77:10]
  wire [31:0] _GEN_56 = io_ins_fault_ins_fault_type == 3'h2 ? 32'h3 : _GEN_49; // @[CSR.scala 126:85 79:12]
  wire [31:0] _GEN_57 = io_ins_fault_ins_fault_type == 3'h2 ? io_ins_fault_mtval : _GEN_50; // @[CSR.scala 126:85 80:11]
  wire [31:0] _GEN_58 = io_ins_fault_ins_fault_type == 3'h2 ? _mstatus_T_14 : _GEN_51; // @[CSR.scala 126:85 85:13]
  wire [1:0] _GEN_59 = io_ins_fault_ins_fault_type == 3'h2 ? 2'h3 : _GEN_52; // @[CSR.scala 126:85 86:19]
  wire  _GEN_60 = io_ins_fault_ins_fault_type == 3'h1 | _GEN_53; // @[CSR.scala 124:84 76:20]
  wire [31:0] _GEN_61 = io_ins_fault_ins_fault_type == 3'h1 ? io_pc : _GEN_54; // @[CSR.scala 124:84 77:10]
  wire [31:0] _GEN_63 = io_ins_fault_ins_fault_type == 3'h1 ? 32'h2 : _GEN_56; // @[CSR.scala 124:84 79:12]
  wire [31:0] _GEN_64 = io_ins_fault_ins_fault_type == 3'h1 ? io_ins_fault_mtval : _GEN_57; // @[CSR.scala 124:84 80:11]
  wire [31:0] _GEN_65 = io_ins_fault_ins_fault_type == 3'h1 ? _mstatus_T_14 : _GEN_58; // @[CSR.scala 124:84 85:13]
  wire [1:0] _GEN_66 = io_ins_fault_ins_fault_type == 3'h1 ? 2'h3 : _GEN_59; // @[CSR.scala 124:84 86:19]
  wire  _GEN_67 = io_mem_fault_mem_fault_type == 3'h6 | _GEN_60; // @[CSR.scala 120:85 76:20]
  wire [31:0] _GEN_68 = io_mem_fault_mem_fault_type == 3'h6 ? io_pc : _GEN_61; // @[CSR.scala 120:85 77:10]
  wire [31:0] _GEN_70 = io_mem_fault_mem_fault_type == 3'h6 ? 32'h7 : _GEN_63; // @[CSR.scala 120:85 79:12]
  wire [31:0] _GEN_71 = io_mem_fault_mem_fault_type == 3'h6 ? io_mem_fault_mtval : _GEN_64; // @[CSR.scala 120:85 80:11]
  wire [31:0] _GEN_72 = io_mem_fault_mem_fault_type == 3'h6 ? _mstatus_T_14 : _GEN_65; // @[CSR.scala 120:85 85:13]
  wire [1:0] _GEN_73 = io_mem_fault_mem_fault_type == 3'h6 ? 2'h3 : _GEN_66; // @[CSR.scala 120:85 86:19]
  wire  _GEN_74 = io_mem_fault_mem_fault_type == 3'h5 | _GEN_67; // @[CSR.scala 118:90 76:20]
  wire [31:0] _GEN_75 = io_mem_fault_mem_fault_type == 3'h5 ? io_pc : _GEN_68; // @[CSR.scala 118:90 77:10]
  wire [31:0] _GEN_77 = io_mem_fault_mem_fault_type == 3'h5 ? 32'h6 : _GEN_70; // @[CSR.scala 118:90 79:12]
  wire [31:0] _GEN_78 = io_mem_fault_mem_fault_type == 3'h5 ? io_mem_fault_mtval : _GEN_71; // @[CSR.scala 118:90 80:11]
  wire [31:0] _GEN_79 = io_mem_fault_mem_fault_type == 3'h5 ? _mstatus_T_14 : _GEN_72; // @[CSR.scala 118:90 85:13]
  wire [1:0] _GEN_80 = io_mem_fault_mem_fault_type == 3'h5 ? 2'h3 : _GEN_73; // @[CSR.scala 118:90 86:19]
  wire  _GEN_81 = io_mem_fault_mem_fault_type == 3'h4 | _GEN_74; // @[CSR.scala 116:82 76:20]
  wire [31:0] _GEN_82 = io_mem_fault_mem_fault_type == 3'h4 ? io_pc : _GEN_75; // @[CSR.scala 116:82 77:10]
  wire [31:0] _GEN_84 = io_mem_fault_mem_fault_type == 3'h4 ? 32'h5 : _GEN_77; // @[CSR.scala 116:82 79:12]
  wire [31:0] _GEN_85 = io_mem_fault_mem_fault_type == 3'h4 ? io_mem_fault_mtval : _GEN_78; // @[CSR.scala 116:82 80:11]
  wire [31:0] _GEN_86 = io_mem_fault_mem_fault_type == 3'h4 ? _mstatus_T_14 : _GEN_79; // @[CSR.scala 116:82 85:13]
  wire [1:0] _GEN_87 = io_mem_fault_mem_fault_type == 3'h4 ? 2'h3 : _GEN_80; // @[CSR.scala 116:82 86:19]
  wire  _GEN_88 = io_mem_fault_mem_fault_type == 3'h3 | _GEN_81; // @[CSR.scala 114:81 76:20]
  wire [31:0] _mstatus_T_151 = mstatus | 32'h8; // @[CSR.scala 146:32]
  wire [31:0] _mstatus_T_152 = ~mstatus; // @[CSR.scala 146:32]
  wire [31:0] _mstatus_T_153 = _mstatus_T_152 | 32'h8; // @[CSR.scala 146:32]
  wire [31:0] _mstatus_T_154 = ~_mstatus_T_153; // @[CSR.scala 146:32]
  wire  _GEN_95 = ~fault_type ? _GEN_88 : 1'h1; // @[CSR.scala 113:62 143:22]
  assign io_cur_privilege = cur_privilege; // @[CSR.scala 36:20]
  assign io_csr_val = io_csr == 12'h344 ? mip : _GEN_19; // @[CSR.scala 60:26 61:18]
  assign io_fault_state = ~no_fault & _GEN_95; // @[CSR.scala 105:19 93:18]
  assign io_fault_write_PC = ~fault_type ? mtvec : mepc; // @[CSR.scala 113:62 144:25]
  always @(posedge clock) begin
    if (reset) begin // @[CSR.scala 35:30]
      cur_privilege <= 2'h3; // @[CSR.scala 35:30]
    end else if (~no_fault) begin // @[CSR.scala 105:19]
      if (~fault_type) begin // @[CSR.scala 113:62]
        if (io_mem_fault_mem_fault_type == 3'h3) begin // @[CSR.scala 114:81]
          cur_privilege <= 2'h3; // @[CSR.scala 86:19]
        end else begin
          cur_privilege <= _GEN_87;
        end
      end else begin
        cur_privilege <= read_MPP; // @[CSR.scala 147:21]
      end
    end
    if (reset) begin // @[CSR.scala 38:24]
      mstatus <= 32'h0; // @[CSR.scala 38:24]
    end else if (~no_fault) begin // @[CSR.scala 105:19]
      if (~fault_type) begin // @[CSR.scala 113:62]
        if (io_mem_fault_mem_fault_type == 3'h3) begin // @[CSR.scala 114:81]
          mstatus <= _mstatus_T_14; // @[CSR.scala 85:13]
        end else begin
          mstatus <= _GEN_86;
        end
      end else if (read_MPIE) begin // @[CSR.scala 146:32]
        mstatus <= _mstatus_T_151;
      end else begin
        mstatus <= _mstatus_T_154;
      end
    end else if (io_csr == 12'h300) begin // @[CSR.scala 60:26]
      if (io_cpu_state == 2'h1 & io_write & no_fault) begin // @[CSR.scala 62:84]
        mstatus <= io_write_data; // @[CSR.scala 63:13]
      end
    end
    if (reset) begin // @[CSR.scala 39:20]
      mie <= 32'h0; // @[CSR.scala 39:20]
    end else if (io_csr == 12'h304) begin // @[CSR.scala 60:26]
      if (io_cpu_state == 2'h1 & io_write & no_fault) begin // @[CSR.scala 62:84]
        mie <= io_write_data; // @[CSR.scala 63:13]
      end
    end
    if (reset) begin // @[CSR.scala 40:22]
      mtvec <= 32'h0; // @[CSR.scala 40:22]
    end else if (io_csr == 12'h305) begin // @[CSR.scala 60:26]
      if (io_cpu_state == 2'h1 & io_write & no_fault) begin // @[CSR.scala 62:84]
        mtvec <= io_write_data; // @[CSR.scala 63:13]
      end
    end
    if (reset) begin // @[CSR.scala 42:25]
      mscratch <= 32'h0; // @[CSR.scala 42:25]
    end else if (io_csr == 12'h340) begin // @[CSR.scala 60:26]
      if (io_cpu_state == 2'h1 & io_write & no_fault) begin // @[CSR.scala 62:84]
        mscratch <= io_write_data; // @[CSR.scala 63:13]
      end
    end
    if (reset) begin // @[CSR.scala 43:21]
      mepc <= 32'h0; // @[CSR.scala 43:21]
    end else if (~no_fault) begin // @[CSR.scala 105:19]
      if (~fault_type) begin // @[CSR.scala 113:62]
        if (io_mem_fault_mem_fault_type == 3'h3) begin // @[CSR.scala 114:81]
          mepc <= io_pc; // @[CSR.scala 77:10]
        end else begin
          mepc <= _GEN_82;
        end
      end else begin
        mepc <= _GEN_14;
      end
    end else begin
      mepc <= _GEN_14;
    end
    if (reset) begin // @[CSR.scala 44:23]
      mcause <= 32'h0; // @[CSR.scala 44:23]
    end else if (~no_fault) begin // @[CSR.scala 105:19]
      if (~fault_type) begin // @[CSR.scala 113:62]
        if (io_mem_fault_mem_fault_type == 3'h3) begin // @[CSR.scala 114:81]
          mcause <= 32'h4; // @[CSR.scala 79:12]
        end else begin
          mcause <= _GEN_84;
        end
      end else begin
        mcause <= _GEN_17;
      end
    end else begin
      mcause <= _GEN_17;
    end
    if (reset) begin // @[CSR.scala 45:22]
      mtval <= 32'h0; // @[CSR.scala 45:22]
    end else if (~no_fault) begin // @[CSR.scala 105:19]
      if (~fault_type) begin // @[CSR.scala 113:62]
        if (io_mem_fault_mem_fault_type == 3'h3) begin // @[CSR.scala 114:81]
          mtval <= io_mem_fault_mtval; // @[CSR.scala 80:11]
        end else begin
          mtval <= _GEN_85;
        end
      end else begin
        mtval <= _GEN_20;
      end
    end else begin
      mtval <= _GEN_20;
    end
    if (reset) begin // @[CSR.scala 46:20]
      mip <= 32'h0; // @[CSR.scala 46:20]
    end else if (io_csr == 12'h344) begin // @[CSR.scala 60:26]
      if (io_cpu_state == 2'h1 & io_write & no_fault) begin // @[CSR.scala 62:84]
        mip <= io_write_data; // @[CSR.scala 63:13]
      end else begin
        mip <= _mip_T_6; // @[CSR.scala 53:7]
      end
    end else begin
      mip <= _mip_T_6; // @[CSR.scala 53:7]
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
  input  [1:0]  io_cpu_state,
  input         io_fault_occurs,
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
  output [2:0]  io_fault_mem_fault_type,
  output [31:0] io_fault_mtval,
  input  [31:0] io_pc
);
  wire  insRAM_clock; // @[MemoryDispatch.scala 46:22]
  wire  insRAM_io_write; // @[MemoryDispatch.scala 46:22]
  wire [31:0] insRAM_io_write_addr; // @[MemoryDispatch.scala 46:22]
  wire [31:0] insRAM_io_write_data; // @[MemoryDispatch.scala 46:22]
  wire [31:0] insRAM_io_read_data; // @[MemoryDispatch.scala 46:22]
  wire [31:0] insRAM_io2_read_addr; // @[MemoryDispatch.scala 46:22]
  wire [31:0] insRAM_io2_read_data; // @[MemoryDispatch.scala 46:22]
  wire  dataRAM_clock; // @[MemoryDispatch.scala 47:23]
  wire  dataRAM_io_write; // @[MemoryDispatch.scala 47:23]
  wire [31:0] dataRAM_io_read_addr; // @[MemoryDispatch.scala 47:23]
  wire [31:0] dataRAM_io_write_addr; // @[MemoryDispatch.scala 47:23]
  wire [31:0] dataRAM_io_write_data; // @[MemoryDispatch.scala 47:23]
  wire [31:0] dataRAM_io_read_data; // @[MemoryDispatch.scala 47:23]
  wire  outRegisters_clock; // @[MemoryDispatch.scala 48:28]
  wire  outRegisters_reset; // @[MemoryDispatch.scala 48:28]
  wire  outRegisters_io_mem_write; // @[MemoryDispatch.scala 48:28]
  wire [31:0] outRegisters_io_mem_read_addr; // @[MemoryDispatch.scala 48:28]
  wire [31:0] outRegisters_io_mem_write_addr; // @[MemoryDispatch.scala 48:28]
  wire [31:0] outRegisters_io_mem_write_data; // @[MemoryDispatch.scala 48:28]
  wire [31:0] outRegisters_io_mem_read_data; // @[MemoryDispatch.scala 48:28]
  wire [31:0] outRegisters_io_external_led_led; // @[MemoryDispatch.scala 48:28]
  wire [31:0] outRegisters_io_external_seg7_seg7; // @[MemoryDispatch.scala 48:28]
  wire [31:0] outRegisters_io_external_btn_button; // @[MemoryDispatch.scala 48:28]
  wire [31:0] outRegisters_io_external_switches_switches; // @[MemoryDispatch.scala 48:28]
  wire [7:0] outRegisters_io_external_uart_rxData; // @[MemoryDispatch.scala 48:28]
  wire  outRegisters_io_external_uart_rxValid; // @[MemoryDispatch.scala 48:28]
  wire  outRegisters_io_external_uart_rxReady; // @[MemoryDispatch.scala 48:28]
  wire [7:0] outRegisters_io_external_uart_txData; // @[MemoryDispatch.scala 48:28]
  wire  outRegisters_io_external_uart_txStart; // @[MemoryDispatch.scala 48:28]
  wire  outRegisters_io_external_uart_txReady; // @[MemoryDispatch.scala 48:28]
  wire [29:0] rw_mem_addr = io_data_addr[31:2]; // @[MemoryDispatch.scala 42:34]
  wire [29:0] read_ins_addr = io_ins_addr[31:2]; // @[MemoryDispatch.scala 43:35]
  wire  is_write_clk = (io_cpu_state == 2'h1 | io_cpu_state == 2'h3) & ~io_fault_occurs; // @[MemoryDispatch.scala 55:123]
  wire  _T_1 = io_data_addr <= 32'hffff; // @[MemoryDispatch.scala 101:21]
  wire  _GEN_0 = io_write_data & (is_write_clk & io_write_data); // @[MemoryDispatch.scala 103:25 104:23 MemoryPacket.scala 17:16]
  wire  _T_3 = 32'h10000 <= io_data_addr; // @[MemoryDispatch.scala 109:36]
  wire  _T_5 = _T_3 & io_data_addr <= 32'h1ffff; // @[MemoryDispatch.scala 110:5]
  wire [31:0] _havard_mem_T_1 = io_data_addr - 32'h10000; // @[MemoryDispatch.scala 112:36]
  wire [29:0] havard_mem = _havard_mem_T_1[31:2]; // @[MemoryDispatch.scala 112:61]
  wire  _T_6 = 32'hffffff00 <= io_data_addr; // @[MemoryDispatch.scala 122:38]
  wire [31:0] _GEN_2 = outRegisters_io_mem_read_data; // @[MemoryDispatch.scala 124:14 127:30 128:16]
  wire [31:0] _GEN_4 = io_write_data ? outRegisters_io_mem_read_data : _GEN_2; // @[MemoryDispatch.scala 124:14 125:25]
  wire  _T_10 = ~reset; // @[MemoryDispatch.scala 137:13]
  wire [2:0] _GEN_5 = io_cpu_state != 2'h3 ? 3'h4 : 3'h0; // @[MemoryDispatch.scala 38:27 90:57 91:31]
  wire [2:0] _GEN_7 = io_cpu_state != 2'h3 ? 3'h6 : 3'h0; // @[MemoryDispatch.scala 38:27 90:57 91:31]
  wire [2:0] _GEN_9 = io_write_data ? _GEN_7 : 3'h0; // @[MemoryDispatch.scala 139:31 38:27]
  wire [2:0] _GEN_11 = io_read_data ? _GEN_5 : _GEN_9; // @[MemoryDispatch.scala 136:24]
  wire  _GEN_14 = _T_6 & _GEN_0; // @[MemoryDispatch.scala 123:47 MemoryPacket.scala 17:16]
  wire [2:0] _GEN_15 = _T_6 ? 3'h0 : _GEN_11; // @[MemoryDispatch.scala 123:47 38:27]
  wire [29:0] _GEN_17 = _T_3 & io_data_addr <= 32'h1ffff ? havard_mem : rw_mem_addr; // @[MemoryDispatch.scala 110:45 113:26 MemoryPacket.scala 18:20]
  wire [31:0] _GEN_18 = _T_3 & io_data_addr <= 32'h1ffff ? dataRAM_io_read_data : _GEN_4; // @[MemoryDispatch.scala 110:45 115:14]
  wire  _GEN_19 = _T_3 & io_data_addr <= 32'h1ffff & _GEN_0; // @[MemoryDispatch.scala 110:45 MemoryPacket.scala 17:16]
  wire  _GEN_20 = _T_3 & io_data_addr <= 32'h1ffff ? 1'h0 : _GEN_14; // @[MemoryDispatch.scala 110:45 MemoryPacket.scala 17:16]
  wire [2:0] _GEN_21 = _T_3 & io_data_addr <= 32'h1ffff ? 3'h0 : _GEN_15; // @[MemoryDispatch.scala 110:45 38:27]
  wire [31:0] data_out = io_data_addr <= 32'hffff ? insRAM_io_read_data : _GEN_18; // @[MemoryDispatch.scala 101:44 102:14]
  wire [29:0] _GEN_25 = io_data_addr <= 32'hffff ? rw_mem_addr : _GEN_17; // @[MemoryDispatch.scala 101:44 MemoryPacket.scala 18:20]
  wire [2:0] _GEN_28 = io_data_addr <= 32'hffff ? 3'h0 : _GEN_21; // @[MemoryDispatch.scala 101:44 38:27]
  wire  _T_15 = 2'h0 == io_data_width; // @[MemoryDispatch.scala 148:25]
  wire [7:0] value = io_data_write[7:0]; // @[MemoryDispatch.scala 150:32]
  wire  _T_17 = 2'h0 == io_data_addr[1:0]; // @[MemoryDispatch.scala 151:34]
  wire [31:0] _data_in_T_1 = {data_out[31:8],value}; // @[Cat.scala 33:92]
  wire  _T_18 = 2'h1 == io_data_addr[1:0]; // @[MemoryDispatch.scala 151:34]
  wire [31:0] _data_in_T_4 = {data_out[31:16],value,data_out[7:0]}; // @[Cat.scala 33:92]
  wire  _T_19 = 2'h2 == io_data_addr[1:0]; // @[MemoryDispatch.scala 151:34]
  wire [31:0] _data_in_T_7 = {data_out[31:24],value,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_9 = {value,data_out[23:0]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_31 = 2'h2 == io_data_addr[1:0] ? _data_in_T_7 : _data_in_T_9; // @[MemoryDispatch.scala 151:34 159:19]
  wire [31:0] _GEN_32 = 2'h1 == io_data_addr[1:0] ? _data_in_T_4 : _GEN_31; // @[MemoryDispatch.scala 151:34 156:19]
  wire [31:0] _GEN_33 = 2'h0 == io_data_addr[1:0] ? _data_in_T_1 : _GEN_32; // @[MemoryDispatch.scala 151:34 153:19]
  wire  _T_21 = 2'h1 == io_data_width; // @[MemoryDispatch.scala 148:25]
  wire [15:0] value_1 = io_data_write[15:0]; // @[MemoryDispatch.scala 167:32]
  wire [31:0] _data_in_T_11 = {data_out[31:16],value_1}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_13 = {value_1,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_34 = ~io_data_addr[1] ? _data_in_T_11 : _data_in_T_13; // @[MemoryDispatch.scala 168:40 169:17 171:17]
  wire  _T_24 = 2'h2 == io_data_width; // @[MemoryDispatch.scala 148:25]
  wire [31:0] _GEN_36 = 2'h1 == io_data_width ? _GEN_34 : io_data_write; // @[MemoryDispatch.scala 148:25]
  wire  _high_bit_T_1 = io_unsigned ? 1'h0 : data_out[7]; // @[MemoryDispatch.scala 186:38]
  wire [23:0] high_bit = _high_bit_T_1 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_1 = {high_bit,data_out[7:0]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_4 = io_unsigned ? 1'h0 : data_out[15]; // @[MemoryDispatch.scala 190:38]
  wire [23:0] high_bit_1 = _high_bit_T_4 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_3 = {high_bit_1,data_out[15:8]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_7 = io_unsigned ? 1'h0 : data_out[23]; // @[MemoryDispatch.scala 194:38]
  wire [23:0] high_bit_2 = _high_bit_T_7 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_5 = {high_bit_2,data_out[23:16]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_10 = io_unsigned ? 1'h0 : data_out[31]; // @[MemoryDispatch.scala 198:38]
  wire [23:0] high_bit_3 = _high_bit_T_10 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_7 = {high_bit_3,data_out[31:24]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_39 = _T_19 ? _io_data_out_T_5 : _io_data_out_T_7; // @[MemoryDispatch.scala 184:34 195:23]
  wire [31:0] _GEN_40 = _T_18 ? _io_data_out_T_3 : _GEN_39; // @[MemoryDispatch.scala 184:34 191:23]
  wire [31:0] _GEN_41 = _T_17 ? _io_data_out_T_1 : _GEN_40; // @[MemoryDispatch.scala 184:34 187:23]
  wire [15:0] high_bit_4 = _high_bit_T_4 ? 16'hffff : 16'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_9 = {high_bit_4,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [15:0] high_bit_5 = _high_bit_T_10 ? 16'hffff : 16'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_11 = {high_bit_5,data_out[31:16]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_43 = _T_17 ? _io_data_out_T_9 : _io_data_out_T_11; // @[MemoryDispatch.scala 206:36 209:25]
  wire [2:0] _GEN_44 = io_cpu_state != 2'h3 ? 3'h3 : _GEN_28; // @[MemoryDispatch.scala 90:57 91:31]
  wire [2:0] _GEN_46 = io_read_data ? _GEN_44 : _GEN_28; // @[MemoryDispatch.scala 217:28]
  wire [2:0] _GEN_48 = io_cpu_state != 2'h3 ? 3'h5 : _GEN_46; // @[MemoryDispatch.scala 90:57 91:31]
  wire [2:0] _GEN_50 = io_write_data ? _GEN_48 : _GEN_46; // @[MemoryDispatch.scala 220:29]
  wire [2:0] _GEN_53 = ~io_data_addr[0] ? _GEN_28 : _GEN_50; // @[MemoryDispatch.scala 205:37]
  wire [2:0] _GEN_64 = io_data_addr[1:0] == 2'h0 ? _GEN_28 : _GEN_50; // @[MemoryDispatch.scala 226:40]
  wire [2:0] _GEN_67 = _T_24 ? _GEN_64 : _GEN_28; // @[MemoryDispatch.scala 182:25]
  wire [31:0] _GEN_69 = _T_21 ? _GEN_43 : data_out; // @[MemoryDispatch.scala 182:25]
  wire [2:0] _GEN_70 = _T_21 ? _GEN_53 : _GEN_67; // @[MemoryDispatch.scala 182:25]
  wire [2:0] _GEN_73 = _T_15 ? _GEN_28 : _GEN_70; // @[MemoryDispatch.scala 182:25]
  wire [2:0] _GEN_75 = io_ins_addr > 32'hffff ? 3'h2 : _GEN_73; // @[MemoryDispatch.scala 245:48 246:29]
  wire [31:0] _GEN_76 = io_ins_addr > 32'hffff ? io_pc : io_data_addr; // @[MemoryDispatch.scala 245:48 247:20]
  wire  _GEN_83 = ~_T_1 & ~_T_5 & ~_T_6; // @[MemoryDispatch.scala 137:13]
  InsRAM insRAM ( // @[MemoryDispatch.scala 46:22]
    .clock(insRAM_clock),
    .io_write(insRAM_io_write),
    .io_write_addr(insRAM_io_write_addr),
    .io_write_data(insRAM_io_write_data),
    .io_read_data(insRAM_io_read_data),
    .io2_read_addr(insRAM_io2_read_addr),
    .io2_read_data(insRAM_io2_read_data)
  );
  DataRAM dataRAM ( // @[MemoryDispatch.scala 47:23]
    .clock(dataRAM_clock),
    .io_write(dataRAM_io_write),
    .io_read_addr(dataRAM_io_read_addr),
    .io_write_addr(dataRAM_io_write_addr),
    .io_write_data(dataRAM_io_write_data),
    .io_read_data(dataRAM_io_read_data)
  );
  OutRegisters outRegisters ( // @[MemoryDispatch.scala 48:28]
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
  assign io_ins_out = insRAM_io2_read_data; // @[MemoryDispatch.scala 59:14]
  assign io_data_out = _T_15 ? _GEN_41 : _GEN_69; // @[MemoryDispatch.scala 182:25]
  assign io_external_led_led = outRegisters_io_external_led_led; // @[MemoryDispatch.scala 83:28]
  assign io_external_seg7_seg7 = outRegisters_io_external_seg7_seg7; // @[MemoryDispatch.scala 83:28]
  assign io_external_uart_rxReady = outRegisters_io_external_uart_rxReady; // @[MemoryDispatch.scala 83:28]
  assign io_external_uart_txData = outRegisters_io_external_uart_txData; // @[MemoryDispatch.scala 83:28]
  assign io_external_uart_txStart = outRegisters_io_external_uart_txStart; // @[MemoryDispatch.scala 83:28]
  assign io_fault_mem_fault_type = io_ins_addr[1:0] != 2'h0 ? 3'h1 : _GEN_75; // @[MemoryDispatch.scala 242:35 243:29]
  assign io_fault_mtval = io_ins_addr[1:0] != 2'h0 ? io_pc : _GEN_76; // @[MemoryDispatch.scala 242:35 244:20]
  assign insRAM_clock = clock;
  assign insRAM_io_write = io_data_addr <= 32'hffff & _GEN_0; // @[MemoryDispatch.scala 101:44 MemoryPacket.scala 17:16]
  assign insRAM_io_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 19:21]
  assign insRAM_io_write_data = 2'h0 == io_data_width ? _GEN_33 : _GEN_36; // @[MemoryDispatch.scala 148:25]
  assign insRAM_io2_read_addr = {{2'd0}, read_ins_addr}; // @[MemoryDispatch.scala 58:24]
  assign dataRAM_clock = clock;
  assign dataRAM_io_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_19; // @[MemoryDispatch.scala 101:44 MemoryPacket.scala 17:16]
  assign dataRAM_io_read_addr = {{2'd0}, _GEN_25};
  assign dataRAM_io_write_addr = {{2'd0}, _GEN_25};
  assign dataRAM_io_write_data = 2'h0 == io_data_width ? _GEN_33 : _GEN_36; // @[MemoryDispatch.scala 148:25]
  assign outRegisters_clock = clock;
  assign outRegisters_reset = reset;
  assign outRegisters_io_mem_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_20; // @[MemoryDispatch.scala 101:44 MemoryPacket.scala 17:16]
  assign outRegisters_io_mem_read_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 18:20]
  assign outRegisters_io_mem_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 19:21]
  assign outRegisters_io_mem_write_data = 2'h0 == io_data_width ? _GEN_33 : _GEN_36; // @[MemoryDispatch.scala 148:25]
  assign outRegisters_io_external_btn_button = io_external_btn_button; // @[MemoryDispatch.scala 83:28]
  assign outRegisters_io_external_switches_switches = io_external_switches_switches; // @[MemoryDispatch.scala 83:28]
  assign outRegisters_io_external_uart_rxData = io_external_uart_rxData; // @[MemoryDispatch.scala 83:28]
  assign outRegisters_io_external_uart_rxValid = io_external_uart_rxValid; // @[MemoryDispatch.scala 83:28]
  assign outRegisters_io_external_uart_txReady = io_external_uart_txReady; // @[MemoryDispatch.scala 83:28]
  always @(posedge clock) begin
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~_T_1 & ~_T_5 & ~_T_6 & io_read_data & ~reset) begin
          $fwrite(32'h80000002,"Unexpected Mem address %x",io_data_addr); // @[MemoryDispatch.scala 137:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_83 & ~io_read_data & io_write_data & _T_10) begin
          $fwrite(32'h80000002,"Unexpected Mem address %x",io_data_addr); // @[MemoryDispatch.scala 140:13]
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
  input  [1:0]  io_cpu_state,
  input         io_rxValid,
  input  [7:0]  io_rxData,
  output        io_rxReady,
  output        io_mem_mem_write,
  output [31:0] io_mem_data_to_write,
  output [31:0] io_mem_data_addr
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
`endif // RANDOMIZE_REG_INIT
  reg [1:0] cur_state; // @[UARTLoader.scala 34:26]
  reg [31:0] data_addr; // @[UARTLoader.scala 35:26]
  reg  rxReadyReg; // @[UARTLoader.scala 44:27]
  wire  _T_3 = io_cpu_state != 2'h3; // @[UARTLoader.scala 57:25]
  wire [1:0] _GEN_2 = ~io_rxValid ? 2'h3 : cur_state; // @[UARTLoader.scala 60:38 61:21 34:26]
  wire [31:0] _data_addr_T_1 = data_addr + 32'h1; // @[UARTLoader.scala 71:34]
  wire [31:0] _GEN_4 = io_rxValid ? _data_addr_T_1 : data_addr; // @[UARTLoader.scala 70:37 71:21 35:26]
  wire [1:0] _GEN_5 = io_rxValid ? 2'h2 : cur_state; // @[UARTLoader.scala 70:37 72:21 34:26]
  wire [1:0] _GEN_6 = _T_3 ? 2'h0 : _GEN_5; // @[UARTLoader.scala 67:61 68:19]
  wire [31:0] _GEN_7 = _T_3 ? data_addr : _GEN_4; // @[UARTLoader.scala 35:26 67:61]
  wire [1:0] _GEN_9 = io_rxValid ? cur_state : 2'h3; // @[UARTLoader.scala 34:26 81:37 85:21]
  wire [1:0] _GEN_10 = _T_3 ? 2'h0 : _GEN_9; // @[UARTLoader.scala 78:61 79:19]
  wire  _GEN_11 = _T_3 ? 1'h0 : io_rxValid; // @[UARTLoader.scala 42:20 78:61]
  wire  _GEN_12 = 2'h2 == cur_state ? _GEN_11 : rxReadyReg; // @[UARTLoader.scala 47:21 44:27]
  wire [1:0] _GEN_13 = 2'h2 == cur_state ? _GEN_10 : cur_state; // @[UARTLoader.scala 47:21 34:26]
  wire  _GEN_18 = 2'h3 == cur_state ? 1'h0 : 2'h2 == cur_state & _GEN_11; // @[UARTLoader.scala 42:20 47:21]
  wire  _GEN_22 = 2'h1 == cur_state ? 1'h0 : _GEN_18; // @[UARTLoader.scala 42:20 47:21]
  assign io_rxReady = rxReadyReg; // @[UARTLoader.scala 45:13]
  assign io_mem_mem_write = 2'h0 == cur_state ? 1'h0 : _GEN_22; // @[UARTLoader.scala 42:20 47:21]
  assign io_mem_data_to_write = {{24'd0}, io_rxData}; // @[UARTLoader.scala 40:24]
  assign io_mem_data_addr = data_addr; // @[UARTLoader.scala 38:20]
  always @(posedge clock) begin
    if (reset) begin // @[UARTLoader.scala 34:26]
      cur_state <= 2'h0; // @[UARTLoader.scala 34:26]
    end else if (2'h0 == cur_state) begin // @[UARTLoader.scala 47:21]
      if (io_cpu_state == 2'h3) begin // @[UARTLoader.scala 50:61]
        cur_state <= 2'h1; // @[UARTLoader.scala 52:19]
      end
    end else if (2'h1 == cur_state) begin // @[UARTLoader.scala 47:21]
      if (io_cpu_state != 2'h3) begin // @[UARTLoader.scala 57:61]
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
      if (io_cpu_state == 2'h3) begin // @[UARTLoader.scala 50:61]
        data_addr <= 32'hffffffff; // @[UARTLoader.scala 51:19]
      end
    end else if (!(2'h1 == cur_state)) begin // @[UARTLoader.scala 47:21]
      if (2'h3 == cur_state) begin // @[UARTLoader.scala 47:21]
        data_addr <= _GEN_7;
      end
    end
    if (reset) begin // @[UARTLoader.scala 44:27]
      rxReadyReg <= 1'h0; // @[UARTLoader.scala 44:27]
    end else if (2'h0 == cur_state) begin // @[UARTLoader.scala 47:21]
      rxReadyReg <= 1'h0; // @[UARTLoader.scala 49:17]
    end else if (2'h1 == cur_state) begin // @[UARTLoader.scala 47:21]
      rxReadyReg <= 1'h0; // @[UARTLoader.scala 56:17]
    end else if (2'h3 == cur_state) begin // @[UARTLoader.scala 47:21]
      rxReadyReg <= 1'h0; // @[UARTLoader.scala 66:17]
    end else begin
      rxReadyReg <= _GEN_12;
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
  _RAND_2 = {1{`RANDOM}};
  rxReadyReg = _RAND_2[0:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module MemWriteSelector(
  input  [1:0]  io_cpu_state,
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
  assign io_read_data = io_cpu_state == 2'h3 ? 1'h0 : io_cpu_read_data; // @[MemWriteSelector.scala 30:54 31:17 37:17]
  assign io_write_data = io_cpu_state == 2'h3 ? io_uart_in_mem_write : io_cpu_write_data; // @[MemWriteSelector.scala 30:54 32:18 38:18]
  assign io_data_width = io_cpu_state == 2'h3 ? 2'h0 : io_cpu_data_width; // @[MemWriteSelector.scala 30:54 33:18 39:18]
  assign io_data_addr = io_cpu_state == 2'h3 ? io_uart_in_data_addr : io_cpu_data_addr; // @[MemWriteSelector.scala 30:54 34:17 40:17]
  assign io_data_write = io_cpu_state == 2'h3 ? io_uart_in_data_to_write : io_cpu_data_write; // @[MemWriteSelector.scala 30:54 35:18 41:18]
endmodule
module CPUState(
  input        clock,
  input        reset,
  input        io_fault_state,
  output [1:0] io_cpu_state,
  input        io_load_mode
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  reg [1:0] state; // @[CPUState.scala 16:22]
  wire  _GEN_0 = state == 2'h1 ? 1'h0 : 1'h1; // @[CPUState.scala 29:59 30:13 32:13]
  assign io_cpu_state = ~io_load_mode ? state : 2'h3; // @[CPUState.scala 20:23 21:18 36:18]
  always @(posedge clock) begin
    if (reset) begin // @[CPUState.scala 16:22]
      state <= 2'h1; // @[CPUState.scala 16:22]
    end else if (~io_load_mode) begin // @[CPUState.scala 20:23]
      if (state == 2'h3) begin // @[CPUState.scala 22:52]
        state <= 2'h1; // @[CPUState.scala 23:13]
      end else if (!(io_fault_state)) begin // @[CPUState.scala 24:32]
        state <= {{1'd0}, _GEN_0};
      end
    end else begin
      state <= 2'h3; // @[CPUState.scala 37:11]
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
module PC(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  input         io_fault_occurs,
  input  [31:0] io_next_addr,
  input  [31:0] io_fault_write_PC,
  output [31:0] io_addr
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] pc; // @[PC.scala 20:17]
  assign io_addr = pc; // @[PC.scala 21:10]
  always @(posedge clock) begin
    if (reset) begin // @[PC.scala 20:17]
      pc <= 32'h0; // @[PC.scala 20:17]
    end else if (io_fault_occurs) begin // @[PC.scala 22:24]
      pc <= io_fault_write_PC; // @[PC.scala 26:7]
    end else if (io_cpu_state == 2'h0) begin // @[PC.scala 27:59]
      pc <= io_next_addr; // @[PC.scala 28:7]
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
  output [1:0]  io_nextPC_type,
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
  output [2:0]  io_fault_ins_fault_type,
  output [31:0] io_fault_mtval
);
  wire [9:0] _T_1 = {io_func3,io_func7}; // @[Cat.scala 33:92]
  wire  _GEN_2 = 10'h100 == _T_1 | 10'h180 == _T_1; // @[ControlUnit.scala 114:22 88:39]
  wire  _GEN_4 = 10'h100 == _T_1 ? 1'h0 : 10'h180 == _T_1; // @[ControlUnit.scala 87:19 88:39]
  wire  _GEN_6 = 10'h2a0 == _T_1 ? 1'h0 : _GEN_2; // @[ControlUnit.scala 82:18 88:39]
  wire  _GEN_8 = 10'h2a0 == _T_1 ? 1'h0 : _GEN_4; // @[ControlUnit.scala 87:19 88:39]
  wire [2:0] _GEN_9 = 10'h280 == _T_1 ? 3'h6 : 3'h7; // @[ControlUnit.scala 108:23 88:39]
  wire  _GEN_10 = 10'h280 == _T_1 ? 1'h0 : _GEN_6; // @[ControlUnit.scala 82:18 88:39]
  wire  _GEN_12 = 10'h280 == _T_1 ? 1'h0 : _GEN_8; // @[ControlUnit.scala 87:19 88:39]
  wire [2:0] _GEN_13 = 10'h80 == _T_1 ? 3'h5 : _GEN_9; // @[ControlUnit.scala 105:23 88:39]
  wire  _GEN_14 = 10'h80 == _T_1 ? 1'h0 : _GEN_10; // @[ControlUnit.scala 82:18 88:39]
  wire  _GEN_16 = 10'h80 == _T_1 ? 1'h0 : _GEN_12; // @[ControlUnit.scala 87:19 88:39]
  wire [2:0] _GEN_17 = 10'h380 == _T_1 ? 3'h4 : _GEN_13; // @[ControlUnit.scala 102:23 88:39]
  wire  _GEN_18 = 10'h380 == _T_1 ? 1'h0 : _GEN_14; // @[ControlUnit.scala 82:18 88:39]
  wire  _GEN_20 = 10'h380 == _T_1 ? 1'h0 : _GEN_16; // @[ControlUnit.scala 87:19 88:39]
  wire [2:0] _GEN_21 = 10'h300 == _T_1 ? 3'h3 : _GEN_17; // @[ControlUnit.scala 88:39 99:23]
  wire  _GEN_22 = 10'h300 == _T_1 ? 1'h0 : _GEN_18; // @[ControlUnit.scala 82:18 88:39]
  wire  _GEN_24 = 10'h300 == _T_1 ? 1'h0 : _GEN_20; // @[ControlUnit.scala 87:19 88:39]
  wire [2:0] _GEN_25 = 10'h200 == _T_1 ? 3'h2 : _GEN_21; // @[ControlUnit.scala 88:39 96:23]
  wire  _GEN_26 = 10'h200 == _T_1 ? 1'h0 : _GEN_22; // @[ControlUnit.scala 82:18 88:39]
  wire  _GEN_28 = 10'h200 == _T_1 ? 1'h0 : _GEN_24; // @[ControlUnit.scala 87:19 88:39]
  wire [2:0] _GEN_29 = 10'h20 == _T_1 ? 3'h1 : _GEN_25; // @[ControlUnit.scala 88:39 93:23]
  wire  _GEN_30 = 10'h20 == _T_1 ? 1'h0 : _GEN_26; // @[ControlUnit.scala 82:18 88:39]
  wire  _GEN_32 = 10'h20 == _T_1 ? 1'h0 : _GEN_28; // @[ControlUnit.scala 87:19 88:39]
  wire [2:0] _GEN_33 = 10'h0 == _T_1 ? 3'h0 : _GEN_29; // @[ControlUnit.scala 88:39 90:23]
  wire  _GEN_34 = 10'h0 == _T_1 ? 1'h0 : _GEN_30; // @[ControlUnit.scala 82:18 88:39]
  wire  _GEN_36 = 10'h0 == _T_1 ? 1'h0 : _GEN_32; // @[ControlUnit.scala 87:19 88:39]
  wire  _T_13 = 3'h0 == io_func3; // @[ControlUnit.scala 134:24]
  wire  _T_14 = 3'h4 == io_func3; // @[ControlUnit.scala 134:24]
  wire  _T_15 = 3'h6 == io_func3; // @[ControlUnit.scala 134:24]
  wire  _T_16 = 3'h7 == io_func3; // @[ControlUnit.scala 134:24]
  wire  _T_17 = 3'h1 == io_func3; // @[ControlUnit.scala 134:24]
  wire  _T_18 = 3'h5 == io_func3; // @[ControlUnit.scala 134:24]
  wire [2:0] _GEN_38 = 7'h0 == io_func7 ? 3'h6 : 3'h7; // @[ControlUnit.scala 151:28 153:27]
  wire  _T_21 = 3'h2 == io_func3; // @[ControlUnit.scala 134:24]
  wire  _T_22 = 3'h3 == io_func3; // @[ControlUnit.scala 134:24]
  wire  _GEN_41 = 3'h2 == io_func3 | 3'h3 == io_func3; // @[ControlUnit.scala 134:24 161:22]
  wire  _GEN_43 = 3'h2 == io_func3 ? 1'h0 : 3'h3 == io_func3; // @[ControlUnit.scala 133:19 134:24]
  wire  _GEN_45 = 3'h5 == io_func3 ? 1'h0 : _GEN_41; // @[ControlUnit.scala 127:18 134:24]
  wire  _GEN_47 = 3'h5 == io_func3 ? 1'h0 : _GEN_43; // @[ControlUnit.scala 133:19 134:24]
  wire [2:0] _GEN_48 = 3'h1 == io_func3 ? 3'h5 : _GEN_38; // @[ControlUnit.scala 134:24 148:23]
  wire  _GEN_49 = 3'h1 == io_func3 ? 1'h0 : _GEN_45; // @[ControlUnit.scala 127:18 134:24]
  wire  _GEN_51 = 3'h1 == io_func3 ? 1'h0 : _GEN_47; // @[ControlUnit.scala 133:19 134:24]
  wire [2:0] _GEN_52 = 3'h7 == io_func3 ? 3'h4 : _GEN_48; // @[ControlUnit.scala 134:24 145:23]
  wire  _GEN_53 = 3'h7 == io_func3 ? 1'h0 : _GEN_49; // @[ControlUnit.scala 127:18 134:24]
  wire  _GEN_55 = 3'h7 == io_func3 ? 1'h0 : _GEN_51; // @[ControlUnit.scala 133:19 134:24]
  wire [2:0] _GEN_56 = 3'h6 == io_func3 ? 3'h3 : _GEN_52; // @[ControlUnit.scala 134:24 142:23]
  wire  _GEN_57 = 3'h6 == io_func3 ? 1'h0 : _GEN_53; // @[ControlUnit.scala 127:18 134:24]
  wire  _GEN_59 = 3'h6 == io_func3 ? 1'h0 : _GEN_55; // @[ControlUnit.scala 133:19 134:24]
  wire [2:0] _GEN_60 = 3'h4 == io_func3 ? 3'h2 : _GEN_56; // @[ControlUnit.scala 134:24 139:23]
  wire  _GEN_61 = 3'h4 == io_func3 ? 1'h0 : _GEN_57; // @[ControlUnit.scala 127:18 134:24]
  wire  _GEN_63 = 3'h4 == io_func3 ? 1'h0 : _GEN_59; // @[ControlUnit.scala 133:19 134:24]
  wire [2:0] _GEN_64 = 3'h0 == io_func3 ? 3'h0 : _GEN_60; // @[ControlUnit.scala 134:24 136:23]
  wire  _GEN_65 = 3'h0 == io_func3 ? 1'h0 : _GEN_61; // @[ControlUnit.scala 127:18 134:24]
  wire  _GEN_67 = 3'h0 == io_func3 ? 1'h0 : _GEN_63; // @[ControlUnit.scala 133:19 134:24]
  wire  _GEN_70 = _T_14 ? 1'h0 : 1'h1; // @[ControlUnit.scala 183:24 194:25]
  wire  _GEN_71 = _T_14 | _T_18; // @[ControlUnit.scala 183:24 195:23]
  wire [1:0] _GEN_72 = _T_21 ? 2'h2 : {{1'd0}, _GEN_70}; // @[ControlUnit.scala 183:24 191:25]
  wire  _GEN_73 = _T_21 ? 1'h0 : _GEN_71; // @[ControlUnit.scala 181:19 183:24]
  wire [1:0] _GEN_74 = _T_17 ? 2'h1 : _GEN_72; // @[ControlUnit.scala 183:24 188:25]
  wire  _GEN_75 = _T_17 ? 1'h0 : _GEN_73; // @[ControlUnit.scala 181:19 183:24]
  wire [1:0] _GEN_76 = _T_13 ? 2'h0 : _GEN_74; // @[ControlUnit.scala 183:24 185:25]
  wire  _GEN_77 = _T_13 ? 1'h0 : _GEN_75; // @[ControlUnit.scala 181:19 183:24]
  wire [1:0] _GEN_79 = _T_17 ? 2'h1 : 2'h2; // @[ControlUnit.scala 215:24 220:25]
  wire [1:0] _GEN_80 = _T_13 ? 2'h0 : _GEN_79; // @[ControlUnit.scala 215:24 217:25]
  wire  _GEN_83 = _T_15 | _T_16; // @[ControlUnit.scala 239:24 253:23]
  wire  _GEN_84 = _T_15 ? 1'h0 : 1'h1; // @[ControlUnit.scala 239:24 254:23]
  wire  _GEN_85 = _T_18 | _GEN_84; // @[ControlUnit.scala 239:24 250:23]
  wire  _GEN_86 = _T_18 ? 1'h0 : _GEN_83; // @[ControlUnit.scala 236:19 239:24]
  wire  _GEN_87 = _T_14 ? 1'h0 : _GEN_85; // @[ControlUnit.scala 239:24 247:23]
  wire  _GEN_88 = _T_14 ? 1'h0 : _GEN_86; // @[ControlUnit.scala 236:19 239:24]
  wire [1:0] _GEN_89 = _T_17 ? 2'h3 : {{1'd0}, _GEN_87}; // @[ControlUnit.scala 239:24 244:23]
  wire  _GEN_90 = _T_17 ? 1'h0 : _GEN_88; // @[ControlUnit.scala 236:19 239:24]
  wire [1:0] _GEN_91 = _T_13 ? 2'h2 : _GEN_89; // @[ControlUnit.scala 239:24 241:23]
  wire  _GEN_92 = _T_13 ? 1'h0 : _GEN_90; // @[ControlUnit.scala 236:19 239:24]
  wire  _T_46 = io_cur_privilege == 2'h0; // @[ControlUnit.scala 326:33]
  wire [31:0] _GEN_94 = io_cur_privilege == 2'h0 ? io_instruction : 32'h0; // @[ControlUnit.scala 326:46 328:28 53:18]
  wire [4:0] _GEN_95 = io_cur_privilege == 2'h0 ? io_rs1 : 5'h0; // @[ControlUnit.scala 326:46 56:14 331:24]
  wire [31:0] _io_raw_imm_out_T = {27'h0,io_rs1}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_109 = _T_46 ? {{12'd0}, io_raw_imm} : _io_raw_imm_out_T; // @[ControlUnit.scala 355:46 59:18 361:27]
  wire [2:0] _GEN_121 = _T_46 ? 3'h1 : 3'h4; // @[ControlUnit.scala 395:50 396:41 399:41]
  wire [2:0] _GEN_123 = 20'h302 == io_raw_imm ? _GEN_121 : 3'h0; // @[ControlUnit.scala 387:30 52:27]
  wire [31:0] _GEN_124 = 20'h302 == io_raw_imm ? _GEN_94 : 32'h0; // @[ControlUnit.scala 387:30 53:18]
  wire [2:0] _GEN_125 = 20'h1 == io_raw_imm ? 3'h2 : _GEN_123; // @[ControlUnit.scala 387:30 392:39]
  wire [31:0] _GEN_126 = 20'h1 == io_raw_imm ? 32'h0 : _GEN_124; // @[ControlUnit.scala 387:30 53:18]
  wire [2:0] _GEN_127 = 20'h0 == io_raw_imm ? 3'h3 : _GEN_125; // @[ControlUnit.scala 387:30 389:39]
  wire [31:0] _GEN_128 = 20'h0 == io_raw_imm ? 32'h0 : _GEN_126; // @[ControlUnit.scala 387:30 53:18]
  wire [2:0] _GEN_129 = _T_13 ? _GEN_127 : 3'h0; // @[ControlUnit.scala 324:24 52:27]
  wire [31:0] _GEN_130 = _T_13 ? _GEN_128 : 32'h0; // @[ControlUnit.scala 324:24 53:18]
  wire [2:0] _GEN_131 = _T_16 ? {{2'd0}, _T_46} : _GEN_129; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_132 = _T_16 ? _GEN_94 : _GEN_130; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_133 = _T_16 ? _GEN_109 : {{12'd0}, io_raw_imm}; // @[ControlUnit.scala 324:24 59:18]
  wire [2:0] _GEN_136 = _T_15 ? {{2'd0}, _T_46} : _GEN_131; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_137 = _T_15 ? _GEN_94 : _GEN_132; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_138 = _T_15 ? _GEN_109 : _GEN_133; // @[ControlUnit.scala 324:24]
  wire [3:0] _GEN_139 = _T_15 ? 4'h3 : 4'h8; // @[ControlUnit.scala 324:24]
  wire [2:0] _GEN_141 = _T_18 ? {{2'd0}, _T_46} : _GEN_136; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_142 = _T_18 ? _GEN_94 : _GEN_137; // @[ControlUnit.scala 324:24]
  wire  _GEN_143 = _T_18 ? _T_46 : 1'h1; // @[ControlUnit.scala 321:24 324:24]
  wire [4:0] _GEN_144 = _T_18 ? _GEN_95 : io_rs1; // @[ControlUnit.scala 324:24 56:14]
  wire [31:0] _GEN_145 = _T_18 ? _GEN_109 : _GEN_138; // @[ControlUnit.scala 324:24]
  wire [3:0] _GEN_146 = _T_18 ? 4'h0 : _GEN_139; // @[ControlUnit.scala 324:24]
  wire [2:0] _GEN_148 = _T_22 ? {{2'd0}, _T_46} : _GEN_141; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_149 = _T_22 ? _GEN_94 : _GEN_142; // @[ControlUnit.scala 324:24]
  wire [3:0] _GEN_150 = _T_22 ? 4'h8 : _GEN_146; // @[ControlUnit.scala 324:24]
  wire [4:0] _GEN_153 = _T_22 ? io_rs1 : _GEN_144; // @[ControlUnit.scala 324:24 56:14]
  wire [31:0] _GEN_154 = _T_22 ? {{12'd0}, io_raw_imm} : _GEN_145; // @[ControlUnit.scala 324:24 59:18]
  wire [2:0] _GEN_155 = _T_21 ? {{2'd0}, _T_46} : _GEN_148; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_156 = _T_21 ? _GEN_94 : _GEN_149; // @[ControlUnit.scala 324:24]
  wire [3:0] _GEN_157 = _T_21 ? 4'h3 : _GEN_150; // @[ControlUnit.scala 324:24]
  wire  _GEN_159 = _T_21 | (_T_22 | _GEN_143); // @[ControlUnit.scala 321:24 324:24]
  wire [4:0] _GEN_160 = _T_21 ? io_rs1 : _GEN_153; // @[ControlUnit.scala 324:24 56:14]
  wire [31:0] _GEN_161 = _T_21 ? {{12'd0}, io_raw_imm} : _GEN_154; // @[ControlUnit.scala 324:24 59:18]
  wire [2:0] _GEN_162 = _T_17 ? {{2'd0}, _T_46} : _GEN_155; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_163 = _T_17 ? _GEN_94 : _GEN_156; // @[ControlUnit.scala 324:24]
  wire  _GEN_164 = _T_17 ? _T_46 : _GEN_159; // @[ControlUnit.scala 324:24]
  wire [4:0] _GEN_165 = _T_17 ? _GEN_95 : _GEN_160; // @[ControlUnit.scala 324:24]
  wire [3:0] _GEN_166 = _T_17 ? 4'h0 : _GEN_157; // @[ControlUnit.scala 324:24]
  wire  _GEN_167 = _T_17 | _GEN_41; // @[ControlUnit.scala 324:24]
  wire [31:0] _GEN_168 = _T_17 ? {{12'd0}, io_raw_imm} : _GEN_161; // @[ControlUnit.scala 324:24 59:18]
  wire [4:0] _GEN_175 = 7'h73 == io_opcode ? io_rs1 : io_rs2; // @[ControlUnit.scala 322:18 57:14 78:21]
  wire [2:0] _GEN_176 = 7'h73 == io_opcode ? _GEN_162 : 3'h0; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_177 = 7'h73 == io_opcode ? _GEN_163 : 32'h0; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_178 = 7'h73 == io_opcode ? _GEN_165 : io_rs1; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_181 = 7'h73 == io_opcode ? _GEN_168 : {{12'd0}, io_raw_imm}; // @[ControlUnit.scala 59:18 78:21]
  wire [1:0] _GEN_186 = 7'h17 == io_opcode ? 2'h3 : 2'h0; // @[ControlUnit.scala 78:21 306:25]
  wire [2:0] _GEN_187 = 7'h17 == io_opcode ? 3'h4 : 3'h5; // @[ControlUnit.scala 78:21 307:26]
  wire  _GEN_188 = 7'h17 == io_opcode ? 1'h0 : 1'h1; // @[ControlUnit.scala 308:19 78:21]
  wire  _GEN_191 = 7'h17 == io_opcode ? 1'h0 : 7'h73 == io_opcode; // @[ControlUnit.scala 75:16 78:21]
  wire  _GEN_192 = 7'h17 == io_opcode ? 1'h0 : 7'h73 == io_opcode & _GEN_164; // @[ControlUnit.scala 76:20 78:21]
  wire [4:0] _GEN_193 = 7'h17 == io_opcode ? io_rs2 : _GEN_175; // @[ControlUnit.scala 57:14 78:21]
  wire [2:0] _GEN_194 = 7'h17 == io_opcode ? 3'h0 : _GEN_176; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_195 = 7'h17 == io_opcode ? 32'h0 : _GEN_177; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_196 = 7'h17 == io_opcode ? io_rs1 : _GEN_178; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_198 = 7'h17 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_181; // @[ControlUnit.scala 59:18 78:21]
  wire [1:0] _GEN_203 = 7'h37 == io_opcode ? 2'h3 : _GEN_186; // @[ControlUnit.scala 78:21 294:25]
  wire [2:0] _GEN_204 = 7'h37 == io_opcode ? 3'h3 : _GEN_187; // @[ControlUnit.scala 78:21 295:26]
  wire  _GEN_205 = 7'h37 == io_opcode ? 1'h0 : _GEN_188; // @[ControlUnit.scala 296:19 78:21]
  wire  _GEN_208 = 7'h37 == io_opcode ? 1'h0 : _GEN_191; // @[ControlUnit.scala 75:16 78:21]
  wire  _GEN_209 = 7'h37 == io_opcode ? 1'h0 : _GEN_192; // @[ControlUnit.scala 76:20 78:21]
  wire [4:0] _GEN_210 = 7'h37 == io_opcode ? io_rs2 : _GEN_193; // @[ControlUnit.scala 57:14 78:21]
  wire [2:0] _GEN_211 = 7'h37 == io_opcode ? 3'h0 : _GEN_194; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_212 = 7'h37 == io_opcode ? 32'h0 : _GEN_195; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_213 = 7'h37 == io_opcode ? io_rs1 : _GEN_196; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_215 = 7'h37 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_198; // @[ControlUnit.scala 59:18 78:21]
  wire [1:0] _GEN_216 = 7'h67 == io_opcode ? 2'h2 : 2'h0; // @[ControlUnit.scala 78:21 277:22]
  wire  _GEN_220 = 7'h67 == io_opcode ? 1'h0 : _GEN_167; // @[ControlUnit.scala 78:21 281:24]
  wire [2:0] _GEN_221 = 7'h67 == io_opcode ? 3'h2 : _GEN_204; // @[ControlUnit.scala 78:21 282:26]
  wire [1:0] _GEN_222 = 7'h67 == io_opcode ? 2'h0 : _GEN_203; // @[ControlUnit.scala 78:21 283:25]
  wire  _GEN_223 = 7'h67 == io_opcode ? 1'h0 : _GEN_205; // @[ControlUnit.scala 284:19 78:21]
  wire [3:0] _GEN_225 = 7'h67 == io_opcode ? 4'h0 : _GEN_166; // @[ControlUnit.scala 286:19 78:21]
  wire  _GEN_226 = 7'h67 == io_opcode ? 1'h0 : _GEN_208; // @[ControlUnit.scala 75:16 78:21]
  wire  _GEN_227 = 7'h67 == io_opcode ? 1'h0 : _GEN_209; // @[ControlUnit.scala 76:20 78:21]
  wire [4:0] _GEN_228 = 7'h67 == io_opcode ? io_rs2 : _GEN_210; // @[ControlUnit.scala 57:14 78:21]
  wire [2:0] _GEN_229 = 7'h67 == io_opcode ? 3'h0 : _GEN_211; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_230 = 7'h67 == io_opcode ? 32'h0 : _GEN_212; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_231 = 7'h67 == io_opcode ? io_rs1 : _GEN_213; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_232 = 7'h67 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_215; // @[ControlUnit.scala 59:18 78:21]
  wire [1:0] _GEN_233 = 7'h6f == io_opcode ? 2'h3 : _GEN_216; // @[ControlUnit.scala 78:21 264:22]
  wire  _GEN_238 = 7'h6f == io_opcode ? 1'h0 : _GEN_220; // @[ControlUnit.scala 78:21 269:24]
  wire [2:0] _GEN_239 = 7'h6f == io_opcode ? 3'h2 : _GEN_221; // @[ControlUnit.scala 78:21 270:26]
  wire [1:0] _GEN_240 = 7'h6f == io_opcode ? 2'h1 : _GEN_222; // @[ControlUnit.scala 78:21 271:25]
  wire  _GEN_241 = 7'h6f == io_opcode ? 1'h0 : _GEN_223; // @[ControlUnit.scala 272:19 78:21]
  wire [3:0] _GEN_242 = 7'h6f == io_opcode ? 4'h0 : _GEN_225; // @[ControlUnit.scala 273:19 78:21]
  wire  _GEN_243 = 7'h6f == io_opcode ? 1'h0 : _GEN_226; // @[ControlUnit.scala 75:16 78:21]
  wire  _GEN_244 = 7'h6f == io_opcode ? 1'h0 : _GEN_227; // @[ControlUnit.scala 76:20 78:21]
  wire [4:0] _GEN_245 = 7'h6f == io_opcode ? io_rs2 : _GEN_228; // @[ControlUnit.scala 57:14 78:21]
  wire [2:0] _GEN_246 = 7'h6f == io_opcode ? 3'h0 : _GEN_229; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_247 = 7'h6f == io_opcode ? 32'h0 : _GEN_230; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_248 = 7'h6f == io_opcode ? io_rs1 : _GEN_231; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_249 = 7'h6f == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_232; // @[ControlUnit.scala 59:18 78:21]
  wire [1:0] _GEN_250 = 7'h63 == io_opcode ? 2'h1 : _GEN_233; // @[ControlUnit.scala 78:21 229:22]
  wire  _GEN_251 = 7'h63 == io_opcode ? 1'h0 : 1'h1; // @[ControlUnit.scala 230:21 78:21]
  wire  _GEN_255 = 7'h63 == io_opcode | _GEN_238; // @[ControlUnit.scala 78:21 234:24]
  wire  _GEN_257 = 7'h63 == io_opcode ? _GEN_92 : _GEN_241; // @[ControlUnit.scala 78:21]
  wire [1:0] _GEN_258 = 7'h63 == io_opcode ? 2'h1 : _GEN_240; // @[ControlUnit.scala 78:21 237:25]
  wire  _GEN_261 = 7'h63 == io_opcode ? 1'h0 : _GEN_243; // @[ControlUnit.scala 75:16 78:21]
  wire  _GEN_262 = 7'h63 == io_opcode ? 1'h0 : _GEN_244; // @[ControlUnit.scala 76:20 78:21]
  wire [4:0] _GEN_263 = 7'h63 == io_opcode ? io_rs2 : _GEN_245; // @[ControlUnit.scala 57:14 78:21]
  wire [2:0] _GEN_264 = 7'h63 == io_opcode ? 3'h0 : _GEN_246; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_265 = 7'h63 == io_opcode ? 32'h0 : _GEN_247; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_266 = 7'h63 == io_opcode ? io_rs1 : _GEN_248; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_267 = 7'h63 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_249; // @[ControlUnit.scala 59:18 78:21]
  wire [1:0] _GEN_268 = 7'h23 == io_opcode ? 2'h0 : _GEN_250; // @[ControlUnit.scala 78:21 205:22]
  wire  _GEN_269 = 7'h23 == io_opcode ? 1'h0 : _GEN_251; // @[ControlUnit.scala 206:21 78:21]
  wire  _GEN_270 = 7'h23 == io_opcode ? 1'h0 : 7'h63 == io_opcode; // @[ControlUnit.scala 207:18 78:21]
  wire  _GEN_273 = 7'h23 == io_opcode ? 1'h0 : _GEN_255; // @[ControlUnit.scala 78:21 210:24]
  wire [1:0] _GEN_275 = 7'h23 == io_opcode ? 2'h0 : _GEN_258; // @[ControlUnit.scala 78:21 212:25]
  wire  _GEN_276 = 7'h23 == io_opcode ? 1'h0 : _GEN_257; // @[ControlUnit.scala 213:19 78:21]
  wire [3:0] _GEN_277 = 7'h23 == io_opcode ? 4'h0 : _GEN_242; // @[ControlUnit.scala 214:19 78:21]
  wire  _GEN_280 = 7'h23 == io_opcode ? 1'h0 : _GEN_261; // @[ControlUnit.scala 75:16 78:21]
  wire  _GEN_281 = 7'h23 == io_opcode ? 1'h0 : _GEN_262; // @[ControlUnit.scala 76:20 78:21]
  wire [4:0] _GEN_282 = 7'h23 == io_opcode ? io_rs2 : _GEN_263; // @[ControlUnit.scala 57:14 78:21]
  wire [2:0] _GEN_283 = 7'h23 == io_opcode ? 3'h0 : _GEN_264; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_284 = 7'h23 == io_opcode ? 32'h0 : _GEN_265; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_285 = 7'h23 == io_opcode ? io_rs1 : _GEN_266; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_286 = 7'h23 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_267; // @[ControlUnit.scala 59:18 78:21]
  wire [1:0] _GEN_287 = 7'h3 == io_opcode ? 2'h0 : _GEN_268; // @[ControlUnit.scala 78:21 173:22]
  wire  _GEN_288 = 7'h3 == io_opcode | _GEN_269; // @[ControlUnit.scala 174:21 78:21]
  wire  _GEN_289 = 7'h3 == io_opcode ? 1'h0 : _GEN_270; // @[ControlUnit.scala 175:18 78:21]
  wire  _GEN_291 = 7'h3 == io_opcode ? 1'h0 : 7'h23 == io_opcode; // @[ControlUnit.scala 78:21 177:23]
  wire  _GEN_292 = 7'h3 == io_opcode ? 1'h0 : _GEN_273; // @[ControlUnit.scala 78:21 178:24]
  wire [2:0] _GEN_293 = 7'h3 == io_opcode ? 3'h1 : _GEN_239; // @[ControlUnit.scala 78:21 179:26]
  wire [1:0] _GEN_294 = 7'h3 == io_opcode ? 2'h0 : _GEN_275; // @[ControlUnit.scala 78:21 180:25]
  wire  _GEN_295 = 7'h3 == io_opcode ? _GEN_77 : _GEN_276; // @[ControlUnit.scala 78:21]
  wire [3:0] _GEN_296 = 7'h3 == io_opcode ? 4'h0 : _GEN_277; // @[ControlUnit.scala 182:19 78:21]
  wire  _GEN_299 = 7'h3 == io_opcode ? 1'h0 : _GEN_280; // @[ControlUnit.scala 75:16 78:21]
  wire  _GEN_300 = 7'h3 == io_opcode ? 1'h0 : _GEN_281; // @[ControlUnit.scala 76:20 78:21]
  wire [4:0] _GEN_301 = 7'h3 == io_opcode ? io_rs2 : _GEN_282; // @[ControlUnit.scala 57:14 78:21]
  wire [2:0] _GEN_302 = 7'h3 == io_opcode ? 3'h0 : _GEN_283; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_303 = 7'h3 == io_opcode ? 32'h0 : _GEN_284; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_304 = 7'h3 == io_opcode ? io_rs1 : _GEN_285; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_305 = 7'h3 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_286; // @[ControlUnit.scala 59:18 78:21]
  wire [1:0] _GEN_306 = 7'h13 == io_opcode ? 2'h0 : _GEN_287; // @[ControlUnit.scala 78:21 125:22]
  wire  _GEN_307 = 7'h13 == io_opcode | _GEN_288; // @[ControlUnit.scala 126:21 78:21]
  wire  _GEN_308 = 7'h13 == io_opcode ? _GEN_65 : _GEN_289; // @[ControlUnit.scala 78:21]
  wire  _GEN_309 = 7'h13 == io_opcode ? 1'h0 : 7'h3 == io_opcode; // @[ControlUnit.scala 78:21 128:22]
  wire  _GEN_310 = 7'h13 == io_opcode ? 1'h0 : _GEN_291; // @[ControlUnit.scala 78:21 129:23]
  wire  _GEN_311 = 7'h13 == io_opcode ? 1'h0 : _GEN_292; // @[ControlUnit.scala 78:21 130:24]
  wire [2:0] _GEN_313 = 7'h13 == io_opcode ? 3'h0 : _GEN_293; // @[ControlUnit.scala 78:21 132:26]
  wire  _GEN_314 = 7'h13 == io_opcode ? _GEN_67 : _GEN_295; // @[ControlUnit.scala 78:21]
  wire [3:0] _GEN_315 = 7'h13 == io_opcode ? {{1'd0}, _GEN_64} : _GEN_296; // @[ControlUnit.scala 78:21]
  wire [1:0] _GEN_316 = 7'h13 == io_opcode ? 2'h0 : _GEN_91; // @[ControlUnit.scala 78:21]
  wire  _GEN_318 = 7'h13 == io_opcode ? 1'h0 : _GEN_299; // @[ControlUnit.scala 75:16 78:21]
  wire  _GEN_319 = 7'h13 == io_opcode ? 1'h0 : _GEN_300; // @[ControlUnit.scala 76:20 78:21]
  wire [4:0] _GEN_320 = 7'h13 == io_opcode ? io_rs2 : _GEN_301; // @[ControlUnit.scala 57:14 78:21]
  wire [2:0] _GEN_321 = 7'h13 == io_opcode ? 3'h0 : _GEN_302; // @[ControlUnit.scala 78:21 52:27]
  wire [31:0] _GEN_322 = 7'h13 == io_opcode ? 32'h0 : _GEN_303; // @[ControlUnit.scala 53:18 78:21]
  wire [4:0] _GEN_323 = 7'h13 == io_opcode ? io_rs1 : _GEN_304; // @[ControlUnit.scala 56:14 78:21]
  wire [31:0] _GEN_324 = 7'h13 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_305; // @[ControlUnit.scala 59:18 78:21]
  wire [31:0] _GEN_343 = 7'h33 == io_opcode ? {{12'd0}, io_raw_imm} : _GEN_324; // @[ControlUnit.scala 59:18 78:21]
  assign io_rs1_out = 7'h33 == io_opcode ? io_rs1 : _GEN_323; // @[ControlUnit.scala 56:14 78:21]
  assign io_rs2_out = 7'h33 == io_opcode ? io_rs2 : _GEN_320; // @[ControlUnit.scala 57:14 78:21]
  assign io_rd_out = io_rd; // @[ControlUnit.scala 58:13]
  assign io_raw_imm_out = _GEN_343[19:0];
  assign io_csr_out = io_csr; // @[ControlUnit.scala 55:14]
  assign io_alu_type = 7'h33 == io_opcode ? {{1'd0}, _GEN_33} : _GEN_315; // @[ControlUnit.scala 78:21]
  assign io_cmp_type = 7'h33 == io_opcode ? 2'h0 : _GEN_316; // @[ControlUnit.scala 78:21]
  assign io_unsigned = 7'h33 == io_opcode ? _GEN_36 : _GEN_314; // @[ControlUnit.scala 78:21]
  assign io_nextPC_type = 7'h33 == io_opcode ? 2'h0 : _GEN_306; // @[ControlUnit.scala 78:21 80:22]
  assign io_regs_write = 7'h33 == io_opcode | _GEN_307; // @[ControlUnit.scala 78:21 81:21]
  assign io_imm_width_type = 7'h13 == io_opcode ? 2'h0 : _GEN_294; // @[ControlUnit.scala 78:21 131:25]
  assign io_operand2_type = 7'h33 == io_opcode | _GEN_311; // @[ControlUnit.scala 78:21 85:24]
  assign io_au_type = 7'h33 == io_opcode ? _GEN_34 : _GEN_308; // @[ControlUnit.scala 78:21]
  assign io_write_back_type = 7'h33 == io_opcode ? 3'h0 : _GEN_313; // @[ControlUnit.scala 78:21 86:26]
  assign io_memory_read = 7'h33 == io_opcode ? 1'h0 : _GEN_309; // @[ControlUnit.scala 78:21 83:22]
  assign io_memory_write = 7'h33 == io_opcode ? 1'h0 : _GEN_310; // @[ControlUnit.scala 78:21 84:23]
  assign io_data_width = 7'h3 == io_opcode ? _GEN_76 : _GEN_80; // @[ControlUnit.scala 78:21]
  assign io_csr_write = 7'h33 == io_opcode ? 1'h0 : _GEN_318; // @[ControlUnit.scala 75:16 78:21]
  assign io_operand1_type = 7'h33 == io_opcode ? 1'h0 : _GEN_319; // @[ControlUnit.scala 76:20 78:21]
  assign io_fault_ins_fault_type = 7'h33 == io_opcode ? 3'h0 : _GEN_321; // @[ControlUnit.scala 78:21 52:27]
  assign io_fault_mtval = 7'h33 == io_opcode ? 32'h0 : _GEN_322; // @[ControlUnit.scala 53:18 78:21]
endmodule
module Registers(
  input         clock,
  input         reset,
  input  [1:0]  io_cpu_state,
  input         io_fault_occurs,
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
  wire  _T = io_cpu_state == 2'h1; // @[Registers.scala 27:21]
  wire  _T_1 = _T & io_write; // @[Registers.scala 28:5]
  wire  _T_3 = _T_1 & io_rd != 5'h0; // @[Registers.scala 29:5]
  wire  _T_5 = _T_3 & ~io_fault_occurs; // @[Registers.scala 30:5]
  assign io_rs1_val = 5'h1f == io_rs1 ? regs_31 : _GEN_30; // @[Registers.scala 25:{14,14}]
  assign io_rs2_val = 5'h1f == io_rs2 ? regs_31 : _GEN_62; // @[Registers.scala 26:{14,14}]
  always @(posedge clock) begin
    if (reset) begin // @[Registers.scala 24:21]
      regs_0 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h0 == io_rd) begin // @[Registers.scala 37:17]
        regs_0 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_1 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h1 == io_rd) begin // @[Registers.scala 37:17]
        regs_1 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_2 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h2 == io_rd) begin // @[Registers.scala 37:17]
        regs_2 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_3 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h3 == io_rd) begin // @[Registers.scala 37:17]
        regs_3 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_4 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h4 == io_rd) begin // @[Registers.scala 37:17]
        regs_4 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_5 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h5 == io_rd) begin // @[Registers.scala 37:17]
        regs_5 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_6 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h6 == io_rd) begin // @[Registers.scala 37:17]
        regs_6 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_7 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h7 == io_rd) begin // @[Registers.scala 37:17]
        regs_7 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_8 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h8 == io_rd) begin // @[Registers.scala 37:17]
        regs_8 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_9 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h9 == io_rd) begin // @[Registers.scala 37:17]
        regs_9 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_10 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'ha == io_rd) begin // @[Registers.scala 37:17]
        regs_10 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_11 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'hb == io_rd) begin // @[Registers.scala 37:17]
        regs_11 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_12 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'hc == io_rd) begin // @[Registers.scala 37:17]
        regs_12 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_13 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'hd == io_rd) begin // @[Registers.scala 37:17]
        regs_13 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_14 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'he == io_rd) begin // @[Registers.scala 37:17]
        regs_14 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_15 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'hf == io_rd) begin // @[Registers.scala 37:17]
        regs_15 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_16 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h10 == io_rd) begin // @[Registers.scala 37:17]
        regs_16 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_17 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h11 == io_rd) begin // @[Registers.scala 37:17]
        regs_17 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_18 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h12 == io_rd) begin // @[Registers.scala 37:17]
        regs_18 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_19 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h13 == io_rd) begin // @[Registers.scala 37:17]
        regs_19 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_20 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h14 == io_rd) begin // @[Registers.scala 37:17]
        regs_20 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_21 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h15 == io_rd) begin // @[Registers.scala 37:17]
        regs_21 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_22 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h16 == io_rd) begin // @[Registers.scala 37:17]
        regs_22 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_23 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h17 == io_rd) begin // @[Registers.scala 37:17]
        regs_23 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_24 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h18 == io_rd) begin // @[Registers.scala 37:17]
        regs_24 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_25 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h19 == io_rd) begin // @[Registers.scala 37:17]
        regs_25 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_26 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h1a == io_rd) begin // @[Registers.scala 37:17]
        regs_26 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_27 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h1b == io_rd) begin // @[Registers.scala 37:17]
        regs_27 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_28 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h1c == io_rd) begin // @[Registers.scala 37:17]
        regs_28 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_29 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h1d == io_rd) begin // @[Registers.scala 37:17]
        regs_29 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_30 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h1e == io_rd) begin // @[Registers.scala 37:17]
        regs_30 <= io_write_data; // @[Registers.scala 37:17]
      end
    end
    if (reset) begin // @[Registers.scala 24:21]
      regs_31 <= 32'h0; // @[Registers.scala 24:21]
    end else if (_T_5) begin // @[Registers.scala 31:5]
      if (5'h1f == io_rd) begin // @[Registers.scala 37:17]
        regs_31 <= io_write_data; // @[Registers.scala 37:17]
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
module NextPCGen(
  input  [1:0]  io_nextPC_type,
  input         io_cmp_result,
  input  [31:0] io_alu_result,
  input  [31:0] io_imm,
  input  [31:0] io_pc,
  output [31:0] io_nextPC,
  output [31:0] io_pc4,
  output [31:0] io_pcImm
);
  wire [31:0] _io_pc4_T_1 = io_pc + 32'h4; // @[NextPCGen.scala 20:19]
  wire [31:0] _io_pcImm_T_1 = io_pc + io_alu_result; // @[NextPCGen.scala 26:25]
  wire [31:0] _io_nextPC_T_3 = io_pc + io_imm; // @[NextPCGen.scala 30:45]
  wire [31:0] _io_nextPC_T_6 = io_cmp_result ? _io_nextPC_T_3 : _io_pc4_T_1; // @[NextPCGen.scala 30:23]
  wire [31:0] _GEN_1 = 2'h3 == io_nextPC_type ? _io_nextPC_T_3 : 32'h0; // @[NextPCGen.scala 21:12 23:25 40:16]
  wire [31:0] _GEN_2 = 2'h2 == io_nextPC_type ? io_alu_result : _io_nextPC_T_3; // @[NextPCGen.scala 23:25 35:17]
  wire [31:0] _GEN_3 = 2'h2 == io_nextPC_type ? _io_pcImm_T_1 : _GEN_1; // @[NextPCGen.scala 23:25 36:16]
  wire [31:0] _GEN_4 = 2'h1 == io_nextPC_type ? _io_nextPC_T_6 : _GEN_2; // @[NextPCGen.scala 23:25 30:17]
  wire [31:0] _GEN_5 = 2'h1 == io_nextPC_type ? _io_pc4_T_1 : _GEN_3; // @[NextPCGen.scala 23:25 31:16]
  assign io_nextPC = 2'h0 == io_nextPC_type ? _io_pc4_T_1 : _GEN_4; // @[NextPCGen.scala 23:25 25:17]
  assign io_pc4 = io_pc + 32'h4; // @[NextPCGen.scala 20:19]
  assign io_pcImm = 2'h0 == io_nextPC_type ? _io_pcImm_T_1 : _GEN_5; // @[NextPCGen.scala 23:25 26:16]
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
module AUSelector(
  input         io_au_type,
  input  [31:0] io_alu_result,
  input         io_cmp_result,
  output [31:0] io_au_out
);
  assign io_au_out = ~io_au_type ? io_alu_result : {{31'd0}, io_cmp_result}; // @[AUSelector.scala 17:22 19:17]
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
  wire  CSR_clock; // @[CoreTop.scala 21:19]
  wire  CSR_reset; // @[CoreTop.scala 21:19]
  wire [1:0] CSR_io_cpu_state; // @[CoreTop.scala 21:19]
  wire [1:0] CSR_io_cur_privilege; // @[CoreTop.scala 21:19]
  wire  CSR_io_write; // @[CoreTop.scala 21:19]
  wire [11:0] CSR_io_csr; // @[CoreTop.scala 21:19]
  wire [31:0] CSR_io_write_data; // @[CoreTop.scala 21:19]
  wire [31:0] CSR_io_csr_val; // @[CoreTop.scala 21:19]
  wire [2:0] CSR_io_mem_fault_mem_fault_type; // @[CoreTop.scala 21:19]
  wire [31:0] CSR_io_mem_fault_mtval; // @[CoreTop.scala 21:19]
  wire [2:0] CSR_io_ins_fault_ins_fault_type; // @[CoreTop.scala 21:19]
  wire [31:0] CSR_io_ins_fault_mtval; // @[CoreTop.scala 21:19]
  wire [31:0] CSR_io_pc; // @[CoreTop.scala 21:19]
  wire  CSR_io_fault_state; // @[CoreTop.scala 21:19]
  wire [31:0] CSR_io_fault_write_PC; // @[CoreTop.scala 21:19]
  wire  memory_clock; // @[CoreTop.scala 27:22]
  wire  memory_reset; // @[CoreTop.scala 27:22]
  wire [1:0] memory_io_cpu_state; // @[CoreTop.scala 27:22]
  wire  memory_io_fault_occurs; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_ins_addr; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_ins_out; // @[CoreTop.scala 27:22]
  wire  memory_io_read_data; // @[CoreTop.scala 27:22]
  wire  memory_io_write_data; // @[CoreTop.scala 27:22]
  wire  memory_io_unsigned; // @[CoreTop.scala 27:22]
  wire [1:0] memory_io_data_width; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_data_addr; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_data_write; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_data_out; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_external_led_led; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_external_seg7_seg7; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_external_btn_button; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_external_switches_switches; // @[CoreTop.scala 27:22]
  wire [7:0] memory_io_external_uart_rxData; // @[CoreTop.scala 27:22]
  wire  memory_io_external_uart_rxValid; // @[CoreTop.scala 27:22]
  wire  memory_io_external_uart_rxReady; // @[CoreTop.scala 27:22]
  wire [7:0] memory_io_external_uart_txData; // @[CoreTop.scala 27:22]
  wire  memory_io_external_uart_txStart; // @[CoreTop.scala 27:22]
  wire  memory_io_external_uart_txReady; // @[CoreTop.scala 27:22]
  wire [2:0] memory_io_fault_mem_fault_type; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_fault_mtval; // @[CoreTop.scala 27:22]
  wire [31:0] memory_io_pc; // @[CoreTop.scala 27:22]
  wire  uartLoader_clock; // @[CoreTop.scala 28:26]
  wire  uartLoader_reset; // @[CoreTop.scala 28:26]
  wire [1:0] uartLoader_io_cpu_state; // @[CoreTop.scala 28:26]
  wire  uartLoader_io_rxValid; // @[CoreTop.scala 28:26]
  wire [7:0] uartLoader_io_rxData; // @[CoreTop.scala 28:26]
  wire  uartLoader_io_rxReady; // @[CoreTop.scala 28:26]
  wire  uartLoader_io_mem_mem_write; // @[CoreTop.scala 28:26]
  wire [31:0] uartLoader_io_mem_data_to_write; // @[CoreTop.scala 28:26]
  wire [31:0] uartLoader_io_mem_data_addr; // @[CoreTop.scala 28:26]
  wire [1:0] memInSelector_io_cpu_state; // @[CoreTop.scala 29:29]
  wire  memInSelector_io_uart_in_mem_write; // @[CoreTop.scala 29:29]
  wire [31:0] memInSelector_io_uart_in_data_to_write; // @[CoreTop.scala 29:29]
  wire [31:0] memInSelector_io_uart_in_data_addr; // @[CoreTop.scala 29:29]
  wire  memInSelector_io_cpu_read_data; // @[CoreTop.scala 29:29]
  wire  memInSelector_io_cpu_write_data; // @[CoreTop.scala 29:29]
  wire [1:0] memInSelector_io_cpu_data_width; // @[CoreTop.scala 29:29]
  wire [31:0] memInSelector_io_cpu_data_addr; // @[CoreTop.scala 29:29]
  wire [31:0] memInSelector_io_cpu_data_write; // @[CoreTop.scala 29:29]
  wire  memInSelector_io_read_data; // @[CoreTop.scala 29:29]
  wire  memInSelector_io_write_data; // @[CoreTop.scala 29:29]
  wire [1:0] memInSelector_io_data_width; // @[CoreTop.scala 29:29]
  wire [31:0] memInSelector_io_data_addr; // @[CoreTop.scala 29:29]
  wire [31:0] memInSelector_io_data_write; // @[CoreTop.scala 29:29]
  wire  state_clock; // @[CoreTop.scala 34:21]
  wire  state_reset; // @[CoreTop.scala 34:21]
  wire  state_io_fault_state; // @[CoreTop.scala 34:21]
  wire [1:0] state_io_cpu_state; // @[CoreTop.scala 34:21]
  wire  state_io_load_mode; // @[CoreTop.scala 34:21]
  wire  pc_clock; // @[CoreTop.scala 38:18]
  wire  pc_reset; // @[CoreTop.scala 38:18]
  wire [1:0] pc_io_cpu_state; // @[CoreTop.scala 38:18]
  wire  pc_io_fault_occurs; // @[CoreTop.scala 38:18]
  wire [31:0] pc_io_next_addr; // @[CoreTop.scala 38:18]
  wire [31:0] pc_io_fault_write_PC; // @[CoreTop.scala 38:18]
  wire [31:0] pc_io_addr; // @[CoreTop.scala 38:18]
  wire [31:0] insDecode_io_instruction; // @[CoreTop.scala 41:25]
  wire [6:0] insDecode_io_opcode; // @[CoreTop.scala 41:25]
  wire [4:0] insDecode_io_rs1; // @[CoreTop.scala 41:25]
  wire [4:0] insDecode_io_rs2; // @[CoreTop.scala 41:25]
  wire [4:0] insDecode_io_rd; // @[CoreTop.scala 41:25]
  wire [2:0] insDecode_io_func3; // @[CoreTop.scala 41:25]
  wire [6:0] insDecode_io_func7; // @[CoreTop.scala 41:25]
  wire [19:0] insDecode_io_raw_imm; // @[CoreTop.scala 41:25]
  wire [11:0] insDecode_io_csr; // @[CoreTop.scala 41:25]
  wire [31:0] CU_io_instruction; // @[CoreTop.scala 42:18]
  wire [6:0] CU_io_opcode; // @[CoreTop.scala 42:18]
  wire [2:0] CU_io_func3; // @[CoreTop.scala 42:18]
  wire [6:0] CU_io_func7; // @[CoreTop.scala 42:18]
  wire [4:0] CU_io_rs1; // @[CoreTop.scala 42:18]
  wire [4:0] CU_io_rs2; // @[CoreTop.scala 42:18]
  wire [4:0] CU_io_rd; // @[CoreTop.scala 42:18]
  wire [19:0] CU_io_raw_imm; // @[CoreTop.scala 42:18]
  wire [11:0] CU_io_csr; // @[CoreTop.scala 42:18]
  wire [1:0] CU_io_cur_privilege; // @[CoreTop.scala 42:18]
  wire [4:0] CU_io_rs1_out; // @[CoreTop.scala 42:18]
  wire [4:0] CU_io_rs2_out; // @[CoreTop.scala 42:18]
  wire [4:0] CU_io_rd_out; // @[CoreTop.scala 42:18]
  wire [19:0] CU_io_raw_imm_out; // @[CoreTop.scala 42:18]
  wire [11:0] CU_io_csr_out; // @[CoreTop.scala 42:18]
  wire [3:0] CU_io_alu_type; // @[CoreTop.scala 42:18]
  wire [1:0] CU_io_cmp_type; // @[CoreTop.scala 42:18]
  wire  CU_io_unsigned; // @[CoreTop.scala 42:18]
  wire [1:0] CU_io_nextPC_type; // @[CoreTop.scala 42:18]
  wire  CU_io_regs_write; // @[CoreTop.scala 42:18]
  wire [1:0] CU_io_imm_width_type; // @[CoreTop.scala 42:18]
  wire  CU_io_operand2_type; // @[CoreTop.scala 42:18]
  wire  CU_io_au_type; // @[CoreTop.scala 42:18]
  wire [2:0] CU_io_write_back_type; // @[CoreTop.scala 42:18]
  wire  CU_io_memory_read; // @[CoreTop.scala 42:18]
  wire  CU_io_memory_write; // @[CoreTop.scala 42:18]
  wire [1:0] CU_io_data_width; // @[CoreTop.scala 42:18]
  wire  CU_io_csr_write; // @[CoreTop.scala 42:18]
  wire  CU_io_operand1_type; // @[CoreTop.scala 42:18]
  wire [2:0] CU_io_fault_ins_fault_type; // @[CoreTop.scala 42:18]
  wire [31:0] CU_io_fault_mtval; // @[CoreTop.scala 42:18]
  wire  regs_clock; // @[CoreTop.scala 45:20]
  wire  regs_reset; // @[CoreTop.scala 45:20]
  wire [1:0] regs_io_cpu_state; // @[CoreTop.scala 45:20]
  wire  regs_io_fault_occurs; // @[CoreTop.scala 45:20]
  wire  regs_io_write; // @[CoreTop.scala 45:20]
  wire [4:0] regs_io_rs1; // @[CoreTop.scala 45:20]
  wire [4:0] regs_io_rs2; // @[CoreTop.scala 45:20]
  wire [4:0] regs_io_rd; // @[CoreTop.scala 45:20]
  wire [31:0] regs_io_write_data; // @[CoreTop.scala 45:20]
  wire [31:0] regs_io_rs1_val; // @[CoreTop.scala 45:20]
  wire [31:0] regs_io_rs2_val; // @[CoreTop.scala 45:20]
  wire [19:0] immGen_io_raw_imm; // @[CoreTop.scala 46:22]
  wire  immGen_io_unsigned; // @[CoreTop.scala 46:22]
  wire [1:0] immGen_io_imm_width; // @[CoreTop.scala 46:22]
  wire [31:0] immGen_io_real_imm; // @[CoreTop.scala 46:22]
  wire  operandSelector_io_operand2Type; // @[CoreTop.scala 47:31]
  wire  operandSelector_io_operand1Type; // @[CoreTop.scala 47:31]
  wire [31:0] operandSelector_io_rs1_val; // @[CoreTop.scala 47:31]
  wire [31:0] operandSelector_io_rs2_val; // @[CoreTop.scala 47:31]
  wire [31:0] operandSelector_io_real_imm; // @[CoreTop.scala 47:31]
  wire [31:0] operandSelector_io_csr_val; // @[CoreTop.scala 47:31]
  wire [31:0] operandSelector_io_operand1; // @[CoreTop.scala 47:31]
  wire [31:0] operandSelector_io_operand2; // @[CoreTop.scala 47:31]
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
  wire [1:0] nextPCGen_io_nextPC_type; // @[CoreTop.scala 50:25]
  wire  nextPCGen_io_cmp_result; // @[CoreTop.scala 50:25]
  wire [31:0] nextPCGen_io_alu_result; // @[CoreTop.scala 50:25]
  wire [31:0] nextPCGen_io_imm; // @[CoreTop.scala 50:25]
  wire [31:0] nextPCGen_io_pc; // @[CoreTop.scala 50:25]
  wire [31:0] nextPCGen_io_nextPC; // @[CoreTop.scala 50:25]
  wire [31:0] nextPCGen_io_pc4; // @[CoreTop.scala 50:25]
  wire [31:0] nextPCGen_io_pcImm; // @[CoreTop.scala 50:25]
  wire [2:0] writeDataSelector_io_write_back_type; // @[CoreTop.scala 71:33]
  wire [31:0] writeDataSelector_io_imm; // @[CoreTop.scala 71:33]
  wire [31:0] writeDataSelector_io_mem_out; // @[CoreTop.scala 71:33]
  wire [31:0] writeDataSelector_io_au_out; // @[CoreTop.scala 71:33]
  wire [31:0] writeDataSelector_io_pc4; // @[CoreTop.scala 71:33]
  wire [31:0] writeDataSelector_io_pcImm; // @[CoreTop.scala 71:33]
  wire [31:0] writeDataSelector_io_csr; // @[CoreTop.scala 71:33]
  wire [31:0] writeDataSelector_io_write_data; // @[CoreTop.scala 71:33]
  wire  auSelector_io_au_type; // @[CoreTop.scala 72:26]
  wire [31:0] auSelector_io_alu_result; // @[CoreTop.scala 72:26]
  wire  auSelector_io_cmp_result; // @[CoreTop.scala 72:26]
  wire [31:0] auSelector_io_au_out; // @[CoreTop.scala 72:26]
  CSR CSR ( // @[CoreTop.scala 21:19]
    .clock(CSR_clock),
    .reset(CSR_reset),
    .io_cpu_state(CSR_io_cpu_state),
    .io_cur_privilege(CSR_io_cur_privilege),
    .io_write(CSR_io_write),
    .io_csr(CSR_io_csr),
    .io_write_data(CSR_io_write_data),
    .io_csr_val(CSR_io_csr_val),
    .io_mem_fault_mem_fault_type(CSR_io_mem_fault_mem_fault_type),
    .io_mem_fault_mtval(CSR_io_mem_fault_mtval),
    .io_ins_fault_ins_fault_type(CSR_io_ins_fault_ins_fault_type),
    .io_ins_fault_mtval(CSR_io_ins_fault_mtval),
    .io_pc(CSR_io_pc),
    .io_fault_state(CSR_io_fault_state),
    .io_fault_write_PC(CSR_io_fault_write_PC)
  );
  MemoryDispatch memory ( // @[CoreTop.scala 27:22]
    .clock(memory_clock),
    .reset(memory_reset),
    .io_cpu_state(memory_io_cpu_state),
    .io_fault_occurs(memory_io_fault_occurs),
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
    .io_fault_mem_fault_type(memory_io_fault_mem_fault_type),
    .io_fault_mtval(memory_io_fault_mtval),
    .io_pc(memory_io_pc)
  );
  UARTLoader uartLoader ( // @[CoreTop.scala 28:26]
    .clock(uartLoader_clock),
    .reset(uartLoader_reset),
    .io_cpu_state(uartLoader_io_cpu_state),
    .io_rxValid(uartLoader_io_rxValid),
    .io_rxData(uartLoader_io_rxData),
    .io_rxReady(uartLoader_io_rxReady),
    .io_mem_mem_write(uartLoader_io_mem_mem_write),
    .io_mem_data_to_write(uartLoader_io_mem_data_to_write),
    .io_mem_data_addr(uartLoader_io_mem_data_addr)
  );
  MemWriteSelector memInSelector ( // @[CoreTop.scala 29:29]
    .io_cpu_state(memInSelector_io_cpu_state),
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
  CPUState state ( // @[CoreTop.scala 34:21]
    .clock(state_clock),
    .reset(state_reset),
    .io_fault_state(state_io_fault_state),
    .io_cpu_state(state_io_cpu_state),
    .io_load_mode(state_io_load_mode)
  );
  PC pc ( // @[CoreTop.scala 38:18]
    .clock(pc_clock),
    .reset(pc_reset),
    .io_cpu_state(pc_io_cpu_state),
    .io_fault_occurs(pc_io_fault_occurs),
    .io_next_addr(pc_io_next_addr),
    .io_fault_write_PC(pc_io_fault_write_PC),
    .io_addr(pc_io_addr)
  );
  InstructionDecoder insDecode ( // @[CoreTop.scala 41:25]
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
  ControlUnit CU ( // @[CoreTop.scala 42:18]
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
    .io_nextPC_type(CU_io_nextPC_type),
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
    .io_fault_ins_fault_type(CU_io_fault_ins_fault_type),
    .io_fault_mtval(CU_io_fault_mtval)
  );
  Registers regs ( // @[CoreTop.scala 45:20]
    .clock(regs_clock),
    .reset(regs_reset),
    .io_cpu_state(regs_io_cpu_state),
    .io_fault_occurs(regs_io_fault_occurs),
    .io_write(regs_io_write),
    .io_rs1(regs_io_rs1),
    .io_rs2(regs_io_rs2),
    .io_rd(regs_io_rd),
    .io_write_data(regs_io_write_data),
    .io_rs1_val(regs_io_rs1_val),
    .io_rs2_val(regs_io_rs2_val)
  );
  ImmGen immGen ( // @[CoreTop.scala 46:22]
    .io_raw_imm(immGen_io_raw_imm),
    .io_unsigned(immGen_io_unsigned),
    .io_imm_width(immGen_io_imm_width),
    .io_real_imm(immGen_io_real_imm)
  );
  OperandSelector operandSelector ( // @[CoreTop.scala 47:31]
    .io_operand2Type(operandSelector_io_operand2Type),
    .io_operand1Type(operandSelector_io_operand1Type),
    .io_rs1_val(operandSelector_io_rs1_val),
    .io_rs2_val(operandSelector_io_rs2_val),
    .io_real_imm(operandSelector_io_real_imm),
    .io_csr_val(operandSelector_io_csr_val),
    .io_operand1(operandSelector_io_operand1),
    .io_operand2(operandSelector_io_operand2)
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
  NextPCGen nextPCGen ( // @[CoreTop.scala 50:25]
    .io_nextPC_type(nextPCGen_io_nextPC_type),
    .io_cmp_result(nextPCGen_io_cmp_result),
    .io_alu_result(nextPCGen_io_alu_result),
    .io_imm(nextPCGen_io_imm),
    .io_pc(nextPCGen_io_pc),
    .io_nextPC(nextPCGen_io_nextPC),
    .io_pc4(nextPCGen_io_pc4),
    .io_pcImm(nextPCGen_io_pcImm)
  );
  WriteDataSelector writeDataSelector ( // @[CoreTop.scala 71:33]
    .io_write_back_type(writeDataSelector_io_write_back_type),
    .io_imm(writeDataSelector_io_imm),
    .io_mem_out(writeDataSelector_io_mem_out),
    .io_au_out(writeDataSelector_io_au_out),
    .io_pc4(writeDataSelector_io_pc4),
    .io_pcImm(writeDataSelector_io_pcImm),
    .io_csr(writeDataSelector_io_csr),
    .io_write_data(writeDataSelector_io_write_data)
  );
  AUSelector auSelector ( // @[CoreTop.scala 72:26]
    .io_au_type(auSelector_io_au_type),
    .io_alu_result(auSelector_io_alu_result),
    .io_cmp_result(auSelector_io_cmp_result),
    .io_au_out(auSelector_io_au_out)
  );
  assign io_external_led_led = memory_io_external_led_led; // @[CoreTop.scala 163:22]
  assign io_external_seg7_seg7 = memory_io_external_seg7_seg7; // @[CoreTop.scala 163:22]
  assign io_external_uart_rxReady = state_io_cpu_state == 2'h3 ? uartLoader_io_rxReady : memory_io_external_uart_rxReady
    ; // @[CoreTop.scala 163:22 188:63 191:30]
  assign io_external_uart_txData = memory_io_external_uart_txData; // @[CoreTop.scala 163:22]
  assign io_external_uart_txStart = memory_io_external_uart_txStart; // @[CoreTop.scala 163:22]
  assign CSR_clock = clock;
  assign CSR_reset = reset;
  assign CSR_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 112:20]
  assign CSR_io_write = CU_io_csr_write; // @[CoreTop.scala 167:16]
  assign CSR_io_csr = CU_io_csr_out; // @[CoreTop.scala 113:14]
  assign CSR_io_write_data = ALU_io_result; // @[CoreTop.scala 168:21]
  assign CSR_io_mem_fault_mem_fault_type = memory_io_fault_mem_fault_type; // @[CoreTop.scala 78:20]
  assign CSR_io_mem_fault_mtval = memory_io_fault_mtval; // @[CoreTop.scala 78:20]
  assign CSR_io_ins_fault_ins_fault_type = CU_io_fault_ins_fault_type; // @[CoreTop.scala 79:20]
  assign CSR_io_ins_fault_mtval = CU_io_fault_mtval; // @[CoreTop.scala 79:20]
  assign CSR_io_pc = pc_io_addr; // @[CoreTop.scala 81:13]
  assign memory_clock = clock;
  assign memory_reset = reset;
  assign memory_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 156:23]
  assign memory_io_fault_occurs = CSR_io_fault_state; // @[CoreTop.scala 89:26]
  assign memory_io_ins_addr = pc_io_addr; // @[CoreTop.scala 97:22]
  assign memory_io_read_data = memInSelector_io_read_data; // @[CoreTop.scala 161:23]
  assign memory_io_write_data = memInSelector_io_write_data; // @[CoreTop.scala 160:24]
  assign memory_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 162:22]
  assign memory_io_data_width = memInSelector_io_data_width; // @[CoreTop.scala 159:24]
  assign memory_io_data_addr = memInSelector_io_data_addr; // @[CoreTop.scala 157:23]
  assign memory_io_data_write = memInSelector_io_data_write; // @[CoreTop.scala 158:24]
  assign memory_io_external_btn_button = io_external_btn_button; // @[CoreTop.scala 163:22]
  assign memory_io_external_switches_switches = io_external_switches_switches; // @[CoreTop.scala 163:22]
  assign memory_io_external_uart_rxData = io_external_uart_rxData; // @[CoreTop.scala 163:22]
  assign memory_io_external_uart_rxValid = io_external_uart_rxValid; // @[CoreTop.scala 163:22]
  assign memory_io_external_uart_txReady = io_external_uart_txReady; // @[CoreTop.scala 163:22]
  assign memory_io_pc = pc_io_addr; // @[CoreTop.scala 87:16]
  assign uartLoader_clock = clock;
  assign uartLoader_reset = reset;
  assign uartLoader_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 53:27]
  assign uartLoader_io_rxValid = io_external_uart_rxValid; // @[CoreTop.scala 54:25]
  assign uartLoader_io_rxData = io_external_uart_rxData; // @[CoreTop.scala 55:24]
  assign memInSelector_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 61:30]
  assign memInSelector_io_uart_in_mem_write = uartLoader_io_mem_mem_write; // @[CoreTop.scala 62:28]
  assign memInSelector_io_uart_in_data_to_write = uartLoader_io_mem_data_to_write; // @[CoreTop.scala 62:28]
  assign memInSelector_io_uart_in_data_addr = uartLoader_io_mem_data_addr; // @[CoreTop.scala 62:28]
  assign memInSelector_io_cpu_read_data = CU_io_memory_read; // @[CoreTop.scala 64:34]
  assign memInSelector_io_cpu_write_data = CU_io_memory_write; // @[CoreTop.scala 65:35]
  assign memInSelector_io_cpu_data_width = CU_io_data_width; // @[CoreTop.scala 66:35]
  assign memInSelector_io_cpu_data_addr = ALU_io_result; // @[CoreTop.scala 67:34]
  assign memInSelector_io_cpu_data_write = regs_io_rs2_val; // @[CoreTop.scala 68:35]
  assign state_clock = clock;
  assign state_reset = reset;
  assign state_io_fault_state = CSR_io_fault_state; // @[CoreTop.scala 83:24]
  assign state_io_load_mode = io_external_signal_load_data_mode; // @[CoreTop.scala 75:22]
  assign pc_clock = clock;
  assign pc_reset = reset;
  assign pc_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 94:19]
  assign pc_io_fault_occurs = CSR_io_fault_state; // @[CoreTop.scala 88:22]
  assign pc_io_next_addr = nextPCGen_io_nextPC; // @[CoreTop.scala 95:19]
  assign pc_io_fault_write_PC = CSR_io_fault_write_PC; // @[CoreTop.scala 96:24]
  assign insDecode_io_instruction = memory_io_ins_out; // @[CoreTop.scala 100:28]
  assign CU_io_instruction = memory_io_ins_out; // @[CoreTop.scala 86:21]
  assign CU_io_opcode = insDecode_io_opcode; // @[CoreTop.scala 102:16]
  assign CU_io_func3 = insDecode_io_func3; // @[CoreTop.scala 103:15]
  assign CU_io_func7 = insDecode_io_func7; // @[CoreTop.scala 104:15]
  assign CU_io_rs1 = insDecode_io_rs1; // @[CoreTop.scala 105:13]
  assign CU_io_rs2 = insDecode_io_rs2; // @[CoreTop.scala 106:13]
  assign CU_io_rd = insDecode_io_rd; // @[CoreTop.scala 107:12]
  assign CU_io_raw_imm = insDecode_io_raw_imm; // @[CoreTop.scala 108:17]
  assign CU_io_csr = insDecode_io_csr; // @[CoreTop.scala 101:13]
  assign CU_io_cur_privilege = CSR_io_cur_privilege; // @[CoreTop.scala 109:23]
  assign regs_clock = clock;
  assign regs_reset = reset;
  assign regs_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 116:21]
  assign regs_io_fault_occurs = CSR_io_fault_state; // @[CoreTop.scala 90:24]
  assign regs_io_write = CU_io_regs_write; // @[CoreTop.scala 185:17]
  assign regs_io_rs1 = CU_io_rs1_out; // @[CoreTop.scala 117:15]
  assign regs_io_rs2 = CU_io_rs2_out; // @[CoreTop.scala 118:15]
  assign regs_io_rd = CU_io_rd_out; // @[CoreTop.scala 119:14]
  assign regs_io_write_data = writeDataSelector_io_write_data; // @[CoreTop.scala 186:22]
  assign immGen_io_raw_imm = CU_io_raw_imm_out; // @[CoreTop.scala 124:21]
  assign immGen_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 125:22]
  assign immGen_io_imm_width = CU_io_imm_width_type; // @[CoreTop.scala 126:23]
  assign operandSelector_io_operand2Type = CU_io_operand2_type; // @[CoreTop.scala 133:35]
  assign operandSelector_io_operand1Type = CU_io_operand1_type; // @[CoreTop.scala 134:35]
  assign operandSelector_io_rs1_val = regs_io_rs1_val; // @[CoreTop.scala 130:30]
  assign operandSelector_io_rs2_val = regs_io_rs2_val; // @[CoreTop.scala 131:30]
  assign operandSelector_io_real_imm = immGen_io_real_imm; // @[CoreTop.scala 132:31]
  assign operandSelector_io_csr_val = CSR_io_csr_val; // @[CoreTop.scala 129:30]
  assign ALU_io_operand1 = operandSelector_io_operand1; // @[CoreTop.scala 137:19]
  assign ALU_io_operand2 = operandSelector_io_operand2; // @[CoreTop.scala 138:19]
  assign ALU_io_alu_op = CU_io_alu_type; // @[CoreTop.scala 139:17]
  assign ALU_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 140:19]
  assign CMP_io_operand1 = operandSelector_io_operand1; // @[CoreTop.scala 143:19]
  assign CMP_io_operand2 = operandSelector_io_operand2; // @[CoreTop.scala 144:19]
  assign CMP_io_cmp_op = CU_io_cmp_type; // @[CoreTop.scala 145:17]
  assign CMP_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 146:19]
  assign nextPCGen_io_nextPC_type = CU_io_nextPC_type; // @[CoreTop.scala 153:28]
  assign nextPCGen_io_cmp_result = CMP_io_result; // @[CoreTop.scala 149:27]
  assign nextPCGen_io_alu_result = ALU_io_result; // @[CoreTop.scala 150:27]
  assign nextPCGen_io_imm = immGen_io_real_imm; // @[CoreTop.scala 151:20]
  assign nextPCGen_io_pc = pc_io_addr; // @[CoreTop.scala 152:19]
  assign writeDataSelector_io_write_back_type = CU_io_write_back_type; // @[CoreTop.scala 181:40]
  assign writeDataSelector_io_imm = immGen_io_real_imm; // @[CoreTop.scala 176:28]
  assign writeDataSelector_io_mem_out = memory_io_data_out; // @[CoreTop.scala 177:32]
  assign writeDataSelector_io_au_out = auSelector_io_au_out; // @[CoreTop.scala 178:31]
  assign writeDataSelector_io_pc4 = nextPCGen_io_pc4; // @[CoreTop.scala 179:28]
  assign writeDataSelector_io_pcImm = nextPCGen_io_pcImm; // @[CoreTop.scala 180:30]
  assign writeDataSelector_io_csr = CSR_io_csr_val; // @[CoreTop.scala 182:28]
  assign auSelector_io_au_type = CU_io_au_type; // @[CoreTop.scala 173:25]
  assign auSelector_io_alu_result = ALU_io_result; // @[CoreTop.scala 171:28]
  assign auSelector_io_cmp_result = CMP_io_result; // @[CoreTop.scala 172:28]
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
  input         clock,
  input         reset,
  output        io_board_hsync,
  output        io_board_vsync,
  output [11:0] io_board_vga_rgb,
  input  [31:0] io_mmio_value
);
  wire  vga_clk; // @[VGA.scala 21:19]
  wire  vga_rst; // @[VGA.scala 21:19]
  wire [31:0] vga_digit; // @[VGA.scala 21:19]
  wire  vga_hsync; // @[VGA.scala 21:19]
  wire  vga_vsync; // @[VGA.scala 21:19]
  wire [11:0] vga_vga_rgb; // @[VGA.scala 21:19]
  vga vga ( // @[VGA.scala 21:19]
    .clk(vga_clk),
    .rst(vga_rst),
    .digit(vga_digit),
    .hsync(vga_hsync),
    .vsync(vga_vsync),
    .vga_rgb(vga_vga_rgb)
  );
  assign io_board_hsync = vga_hsync; // @[VGA.scala 25:18]
  assign io_board_vsync = vga_vsync; // @[VGA.scala 26:18]
  assign io_board_vga_rgb = vga_vga_rgb; // @[VGA.scala 27:20]
  assign vga_clk = clock; // @[VGA.scala 22:13]
  assign vga_rst = reset; // @[VGA.scala 23:13]
  assign vga_digit = io_mmio_value; // @[VGA.scala 24:16]
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
  wire  vga_clock; // @[DeviceTop.scala 51:19]
  wire  vga_reset; // @[DeviceTop.scala 51:19]
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
    .clock(vga_clock),
    .reset(vga_reset),
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
  assign vga_clock = clock;
  assign vga_reset = reset;
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
