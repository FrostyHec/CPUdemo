module ClockSeparator(
  input   clock,
  input   reset,
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
  assign ip_clock_reset = reset; // @[ClockSeparator.scala 16:22]
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
  (* MARK_DEBUG="true" *)input         io_mem_write,
  input  [31:0] io_mem_read_addr,
  input  [31:0] io_mem_write_addr,
  (* MARK_DEBUG="true" *)input  [31:0] io_mem_write_data,
  output [31:0] io_mem_read_data,
  (* MARK_DEBUG="true" *)output [31:0] io_external_led_led,
  output [31:0] io_external_seg7_seg7,
  input  [31:0] io_external_btn_button,
  input  [31:0] io_external_switches_switches
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] btn; // @[OutRegisters.scala 23:20]
  reg [31:0] switches; // @[OutRegisters.scala 26:25]
  reg [31:0] led; // @[OutRegisters.scala 30:20]
  reg [31:0] seg7; // @[OutRegisters.scala 33:21]
  wire [31:0] addr = io_mem_write ? io_mem_write_addr : io_mem_read_addr; // @[OutRegisters.scala 37:23]
  wire  _T_1 = addr == 32'h3fffffc1; // @[OutRegisters.scala 40:13]
  wire  _T_3 = ~reset; // @[OutRegisters.scala 42:13]
  wire  _T_5 = addr == 32'h3fffffc2; // @[OutRegisters.scala 46:19]
  wire [31:0] _GEN_2 = io_mem_write ? io_mem_write_data : led; // @[OutRegisters.scala 53:24 54:11 30:20]
  wire [31:0] _GEN_4 = io_mem_write ? io_mem_write_data : seg7; // @[OutRegisters.scala 59:24 60:12 33:21]
  wire [31:0] _GEN_6 = addr == 32'h3fffffc3 ? _GEN_4 : seg7; // @[OutRegisters.scala 33:21 58:65]
  wire [31:0] _GEN_9 = addr == 32'h3fffffc0 ? led : seg7; // @[OutRegisters.scala 52:64]
  wire [31:0] _GEN_11 = addr == 32'h3fffffc2 ? switches : _GEN_9; // @[OutRegisters.scala 46:67]
  assign io_mem_read_data = addr == 32'h3fffffc1 ? btn : _GEN_11; // @[OutRegisters.scala 40:58]
  assign io_external_led_led = led; // @[OutRegisters.scala 31:23]
  assign io_external_seg7_seg7 = seg7; // @[OutRegisters.scala 34:25]
  always @(posedge clock) begin
    if (reset) begin // @[OutRegisters.scala 23:20]
      btn <= 32'h0; // @[OutRegisters.scala 23:20]
    end else begin
      btn <= io_external_btn_button; // @[OutRegisters.scala 24:7]
    end
    if (reset) begin // @[OutRegisters.scala 26:25]
      switches <= 32'h0; // @[OutRegisters.scala 26:25]
    end else begin
      switches <= io_external_switches_switches; // @[OutRegisters.scala 27:12]
    end
    if (reset) begin // @[OutRegisters.scala 30:20]
      led <= 32'h0; // @[OutRegisters.scala 30:20]
    end else if (!(addr == 32'h3fffffc1)) begin // @[OutRegisters.scala 40:58]
      if (!(addr == 32'h3fffffc2)) begin // @[OutRegisters.scala 46:67]
        if (addr == 32'h3fffffc0) begin // @[OutRegisters.scala 52:64]
          led <= _GEN_2;
        end
      end
    end
    if (reset) begin // @[OutRegisters.scala 33:21]
      seg7 <= 32'h0; // @[OutRegisters.scala 33:21]
    end else if (!(addr == 32'h3fffffc1)) begin // @[OutRegisters.scala 40:58]
      if (!(addr == 32'h3fffffc2)) begin // @[OutRegisters.scala 46:67]
        if (!(addr == 32'h3fffffc0)) begin // @[OutRegisters.scala 52:64]
          seg7 <= _GEN_6;
        end
      end
    end
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_1 & io_mem_write & ~reset) begin
          $fwrite(32'h80000002,"btn cannot write"); // @[OutRegisters.scala 42:13]
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
          $fwrite(32'h80000002,"switches cannot write"); // @[OutRegisters.scala 48:13]
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
  (* MARK_DEBUG="true" *)input  [31:0] io_ins_addr,
  (* MARK_DEBUG="true" *)output [31:0] io_ins_out,
  (* MARK_DEBUG="true" *)input         io_write_data,
  input         io_unsigned,
  input  [1:0]  io_data_width,
  (* MARK_DEBUG="true" *)input  [31:0] io_data_addr,
  (* MARK_DEBUG="true" *)input  [31:0] io_data_write,
  output [31:0] io_data_out,
  output [31:0] io_external_led_led,
  output [31:0] io_external_seg7_seg7,
  input  [31:0] io_external_btn_button,
  input  [31:0] io_external_switches_switches
);
  wire  insRAM_clock; // @[MemoryDispatch.scala 36:22]
  wire  insRAM_io_write; // @[MemoryDispatch.scala 36:22]
  wire [31:0] insRAM_io_write_addr; // @[MemoryDispatch.scala 36:22]
  wire [31:0] insRAM_io_write_data; // @[MemoryDispatch.scala 36:22]
  wire [31:0] insRAM_io_read_data; // @[MemoryDispatch.scala 36:22]
  wire [31:0] insRAM_io2_read_addr; // @[MemoryDispatch.scala 36:22]
  wire [31:0] insRAM_io2_read_data; // @[MemoryDispatch.scala 36:22]
  wire  dataRAM_clock; // @[MemoryDispatch.scala 37:23]
  wire  dataRAM_io_write; // @[MemoryDispatch.scala 37:23]
  wire [31:0] dataRAM_io_read_addr; // @[MemoryDispatch.scala 37:23]
  wire [31:0] dataRAM_io_write_addr; // @[MemoryDispatch.scala 37:23]
  wire [31:0] dataRAM_io_write_data; // @[MemoryDispatch.scala 37:23]
  wire [31:0] dataRAM_io_read_data; // @[MemoryDispatch.scala 37:23]
  wire  outRegisters_clock; // @[MemoryDispatch.scala 38:28]
  wire  outRegisters_reset; // @[MemoryDispatch.scala 38:28]
  wire  outRegisters_io_mem_write; // @[MemoryDispatch.scala 38:28]
  wire [31:0] outRegisters_io_mem_read_addr; // @[MemoryDispatch.scala 38:28]
  wire [31:0] outRegisters_io_mem_write_addr; // @[MemoryDispatch.scala 38:28]
  wire [31:0] outRegisters_io_mem_write_data; // @[MemoryDispatch.scala 38:28]
  wire [31:0] outRegisters_io_mem_read_data; // @[MemoryDispatch.scala 38:28]
  wire [31:0] outRegisters_io_external_led_led; // @[MemoryDispatch.scala 38:28]
  wire [31:0] outRegisters_io_external_seg7_seg7; // @[MemoryDispatch.scala 38:28]
  wire [31:0] outRegisters_io_external_btn_button; // @[MemoryDispatch.scala 38:28]
  wire [31:0] outRegisters_io_external_switches_switches; // @[MemoryDispatch.scala 38:28]
  wire [29:0] rw_mem_addr = io_data_addr[31:2]; // @[MemoryDispatch.scala 32:34]
  wire [29:0] read_ins_addr = io_ins_addr[31:2]; // @[MemoryDispatch.scala 33:35]
  wire  is_write_clk = io_cpu_state == 2'h1 | io_cpu_state == 2'h2; // @[MemoryDispatch.scala 45:71]
  wire  _T_1 = io_data_addr <= 32'hffff; // @[MemoryDispatch.scala 79:21]
  wire  _GEN_0 = io_write_data & (is_write_clk & io_write_data); // @[MemoryDispatch.scala 81:25 82:23 MemoryPacket.scala 17:16]
  wire  _T_3 = 32'h10000 <= io_data_addr; // @[MemoryDispatch.scala 88:36]
  wire  _T_5 = _T_3 & io_data_addr <= 32'h1ffff; // @[MemoryDispatch.scala 89:5]
  wire [31:0] _havard_mem_T_1 = io_data_addr - 32'h10000; // @[MemoryDispatch.scala 91:36]
  wire [29:0] havard_mem = _havard_mem_T_1[31:2]; // @[MemoryDispatch.scala 91:61]
  wire  _T_6 = 32'hffffff00 <= io_data_addr; // @[MemoryDispatch.scala 101:38]
  wire [31:0] _GEN_3 = outRegisters_io_mem_read_data; // @[MemoryDispatch.scala 102:47 103:14]
  wire  _GEN_4 = _T_6 & _GEN_0; // @[MemoryDispatch.scala 102:47 MemoryPacket.scala 17:16]
  wire [29:0] _GEN_5 = _T_3 & io_data_addr <= 32'h1ffff ? havard_mem : rw_mem_addr; // @[MemoryDispatch.scala 89:45 92:26 MemoryPacket.scala 18:20]
  wire [31:0] _GEN_6 = _T_3 & io_data_addr <= 32'h1ffff ? dataRAM_io_read_data : _GEN_3; // @[MemoryDispatch.scala 89:45 94:14]
  wire  _GEN_7 = _T_3 & io_data_addr <= 32'h1ffff & _GEN_0; // @[MemoryDispatch.scala 89:45 MemoryPacket.scala 17:16]
  wire  _GEN_8 = _T_3 & io_data_addr <= 32'h1ffff ? 1'h0 : _GEN_4; // @[MemoryDispatch.scala 89:45 MemoryPacket.scala 17:16]
  wire [31:0] data_out = io_data_addr <= 32'hffff ? insRAM_io_read_data : _GEN_6; // @[MemoryDispatch.scala 79:44 80:14]
  wire [29:0] _GEN_11 = io_data_addr <= 32'hffff ? rw_mem_addr : _GEN_5; // @[MemoryDispatch.scala 79:44 MemoryPacket.scala 18:20]
  wire  _T_11 = 2'h0 == io_data_width; // @[MemoryDispatch.scala 118:25]
  wire [7:0] value = io_data_write[7:0]; // @[MemoryDispatch.scala 120:32]
  wire  _T_13 = 2'h0 == io_data_addr[1:0]; // @[MemoryDispatch.scala 121:34]
  wire [31:0] _data_in_T_1 = {data_out[31:8],value}; // @[Cat.scala 33:92]
  wire  _T_14 = 2'h1 == io_data_addr[1:0]; // @[MemoryDispatch.scala 121:34]
  wire [31:0] _data_in_T_4 = {data_out[31:16],value,data_out[7:0]}; // @[Cat.scala 33:92]
  wire  _T_15 = 2'h2 == io_data_addr[1:0]; // @[MemoryDispatch.scala 121:34]
  wire [31:0] _data_in_T_7 = {data_out[31:24],value,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_9 = {value,data_out[23:0]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_15 = 2'h2 == io_data_addr[1:0] ? _data_in_T_7 : _data_in_T_9; // @[MemoryDispatch.scala 121:34 129:19]
  wire [31:0] _GEN_16 = 2'h1 == io_data_addr[1:0] ? _data_in_T_4 : _GEN_15; // @[MemoryDispatch.scala 121:34 126:19]
  wire [31:0] _GEN_17 = 2'h0 == io_data_addr[1:0] ? _data_in_T_1 : _GEN_16; // @[MemoryDispatch.scala 121:34 123:19]
  wire  _T_17 = 2'h1 == io_data_width; // @[MemoryDispatch.scala 118:25]
  wire [15:0] value_1 = io_data_write[15:0]; // @[MemoryDispatch.scala 137:32]
  wire [31:0] _data_in_T_11 = {data_out[31:16],value_1}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_13 = {value_1,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_18 = ~io_data_addr[1] ? _data_in_T_11 : _data_in_T_13; // @[MemoryDispatch.scala 138:40 139:17 141:17]
  wire [31:0] _GEN_20 = 2'h1 == io_data_width ? _GEN_18 : io_data_write; // @[MemoryDispatch.scala 118:25]
  wire  _high_bit_T_1 = io_unsigned ? 1'h0 : data_out[7]; // @[MemoryDispatch.scala 155:38]
  wire [23:0] high_bit = _high_bit_T_1 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_1 = {high_bit,data_out[7:0]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_4 = io_unsigned ? 1'h0 : data_out[15]; // @[MemoryDispatch.scala 159:38]
  wire [23:0] high_bit_1 = _high_bit_T_4 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_3 = {high_bit_1,data_out[15:8]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_7 = io_unsigned ? 1'h0 : data_out[23]; // @[MemoryDispatch.scala 163:38]
  wire [23:0] high_bit_2 = _high_bit_T_7 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_5 = {high_bit_2,data_out[23:16]}; // @[Cat.scala 33:92]
  wire  _high_bit_T_10 = io_unsigned ? 1'h0 : data_out[31]; // @[MemoryDispatch.scala 167:38]
  wire [23:0] high_bit_3 = _high_bit_T_10 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_7 = {high_bit_3,data_out[31:24]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_23 = _T_15 ? _io_data_out_T_5 : _io_data_out_T_7; // @[MemoryDispatch.scala 153:34 164:23]
  wire [31:0] _GEN_24 = _T_14 ? _io_data_out_T_3 : _GEN_23; // @[MemoryDispatch.scala 153:34 160:23]
  wire [31:0] _GEN_25 = _T_13 ? _io_data_out_T_1 : _GEN_24; // @[MemoryDispatch.scala 153:34 156:23]
  wire [15:0] high_bit_4 = _high_bit_T_4 ? 16'hffff : 16'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_9 = {high_bit_4,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [15:0] high_bit_5 = _high_bit_T_10 ? 16'hffff : 16'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_11 = {high_bit_5,data_out[31:16]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_27 = _T_13 ? _io_data_out_T_9 : _io_data_out_T_11; // @[MemoryDispatch.scala 174:34 177:23]
  wire [31:0] _GEN_29 = _T_17 ? _GEN_27 : data_out; // @[MemoryDispatch.scala 151:25]
  InsRAM insRAM ( // @[MemoryDispatch.scala 36:22]
    .clock(insRAM_clock),
    .io_write(insRAM_io_write),
    .io_write_addr(insRAM_io_write_addr),
    .io_write_data(insRAM_io_write_data),
    .io_read_data(insRAM_io_read_data),
    .io2_read_addr(insRAM_io2_read_addr),
    .io2_read_data(insRAM_io2_read_data)
  );
  DataRAM dataRAM ( // @[MemoryDispatch.scala 37:23]
    .clock(dataRAM_clock),
    .io_write(dataRAM_io_write),
    .io_read_addr(dataRAM_io_read_addr),
    .io_write_addr(dataRAM_io_write_addr),
    .io_write_data(dataRAM_io_write_data),
    .io_read_data(dataRAM_io_read_data)
  );
  OutRegisters outRegisters ( // @[MemoryDispatch.scala 38:28]
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
    .io_external_switches_switches(outRegisters_io_external_switches_switches)
  );
  assign io_ins_out = insRAM_io2_read_data; // @[MemoryDispatch.scala 49:14]
  assign io_data_out = _T_11 ? _GEN_25 : _GEN_29; // @[MemoryDispatch.scala 151:25]
  assign io_external_led_led = outRegisters_io_external_led_led; // @[MemoryDispatch.scala 73:28]
  assign io_external_seg7_seg7 = outRegisters_io_external_seg7_seg7; // @[MemoryDispatch.scala 73:28]
  assign insRAM_clock = clock;
  assign insRAM_io_write = io_data_addr <= 32'hffff & _GEN_0; // @[MemoryDispatch.scala 79:44 MemoryPacket.scala 17:16]
  assign insRAM_io_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 19:21]
  assign insRAM_io_write_data = 2'h0 == io_data_width ? _GEN_17 : _GEN_20; // @[MemoryDispatch.scala 118:25]
  assign insRAM_io2_read_addr = {{2'd0}, read_ins_addr}; // @[MemoryDispatch.scala 48:24]
  assign dataRAM_clock = clock;
  assign dataRAM_io_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_7; // @[MemoryDispatch.scala 79:44 MemoryPacket.scala 17:16]
  assign dataRAM_io_read_addr = {{2'd0}, _GEN_11};
  assign dataRAM_io_write_addr = {{2'd0}, _GEN_11};
  assign dataRAM_io_write_data = 2'h0 == io_data_width ? _GEN_17 : _GEN_20; // @[MemoryDispatch.scala 118:25]
  assign outRegisters_clock = clock;
  assign outRegisters_reset = reset;
  assign outRegisters_io_mem_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_8; // @[MemoryDispatch.scala 79:44 MemoryPacket.scala 17:16]
  assign outRegisters_io_mem_read_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 18:20]
  assign outRegisters_io_mem_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 19:21]
  assign outRegisters_io_mem_write_data = 2'h0 == io_data_width ? _GEN_17 : _GEN_20; // @[MemoryDispatch.scala 118:25]
  assign outRegisters_io_external_btn_button = io_external_btn_button; // @[MemoryDispatch.scala 73:28]
  assign outRegisters_io_external_switches_switches = io_external_switches_switches; // @[MemoryDispatch.scala 73:28]
  always @(posedge clock) begin
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~_T_1 & ~_T_5 & ~_T_6 & ~reset) begin
          $fwrite(32'h80000002,"Unexpected address!"); // @[MemoryDispatch.scala 114:11]
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
  output        io_mem_mem_write,
  output [31:0] io_mem_data_to_write,
  output [31:0] io_mem_data_addr
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
`endif // RANDOMIZE_REG_INIT
  reg [1:0] cur_state; // @[UARTLoader.scala 33:26]
  reg [31:0] data_addr; // @[UARTLoader.scala 34:26]
  wire  _T_3 = io_cpu_state != 2'h2; // @[UARTLoader.scala 51:25]
  wire [1:0] _GEN_2 = ~io_rxValid ? 2'h3 : cur_state; // @[UARTLoader.scala 54:38 55:21 33:26]
  wire [31:0] _data_addr_T_1 = data_addr + 32'h1; // @[UARTLoader.scala 64:34]
  wire [31:0] _GEN_4 = io_rxValid ? _data_addr_T_1 : data_addr; // @[UARTLoader.scala 63:37 64:21 34:26]
  wire [1:0] _GEN_5 = io_rxValid ? 2'h2 : cur_state; // @[UARTLoader.scala 63:37 65:21 33:26]
  wire [1:0] _GEN_6 = _T_3 ? 2'h0 : _GEN_5; // @[UARTLoader.scala 60:61 61:19]
  wire [31:0] _GEN_7 = _T_3 ? data_addr : _GEN_4; // @[UARTLoader.scala 34:26 60:61]
  wire [1:0] _GEN_9 = io_rxValid ? cur_state : 2'h3; // @[UARTLoader.scala 33:26 73:37 76:21]
  wire [1:0] _GEN_10 = _T_3 ? 2'h0 : _GEN_9; // @[UARTLoader.scala 70:61 71:19]
  wire  _GEN_11 = _T_3 ? 1'h0 : io_rxValid; // @[UARTLoader.scala 41:20 70:61]
  wire [1:0] _GEN_12 = 2'h2 == cur_state ? _GEN_10 : cur_state; // @[UARTLoader.scala 43:21 33:26]
  wire  _GEN_16 = 2'h3 == cur_state ? 1'h0 : 2'h2 == cur_state & _GEN_11; // @[UARTLoader.scala 41:20 43:21]
  wire  _GEN_19 = 2'h1 == cur_state ? 1'h0 : _GEN_16; // @[UARTLoader.scala 41:20 43:21]
  assign io_mem_mem_write = 2'h0 == cur_state ? 1'h0 : _GEN_19; // @[UARTLoader.scala 41:20 43:21]
  assign io_mem_data_to_write = {{24'd0}, io_rxData}; // @[UARTLoader.scala 39:24]
  assign io_mem_data_addr = data_addr; // @[UARTLoader.scala 37:20]
  always @(posedge clock) begin
    if (reset) begin // @[UARTLoader.scala 33:26]
      cur_state <= 2'h0; // @[UARTLoader.scala 33:26]
    end else if (2'h0 == cur_state) begin // @[UARTLoader.scala 43:21]
      if (io_cpu_state == 2'h2) begin // @[UARTLoader.scala 45:61]
        cur_state <= 2'h1; // @[UARTLoader.scala 47:19]
      end
    end else if (2'h1 == cur_state) begin // @[UARTLoader.scala 43:21]
      if (io_cpu_state != 2'h2) begin // @[UARTLoader.scala 51:61]
        cur_state <= 2'h0; // @[UARTLoader.scala 52:19]
      end else begin
        cur_state <= _GEN_2;
      end
    end else if (2'h3 == cur_state) begin // @[UARTLoader.scala 43:21]
      cur_state <= _GEN_6;
    end else begin
      cur_state <= _GEN_12;
    end
    if (reset) begin // @[UARTLoader.scala 34:26]
      data_addr <= 32'hffffffff; // @[UARTLoader.scala 34:26]
    end else if (2'h0 == cur_state) begin // @[UARTLoader.scala 43:21]
      if (io_cpu_state == 2'h2) begin // @[UARTLoader.scala 45:61]
        data_addr <= 32'hffffffff; // @[UARTLoader.scala 46:19]
      end
    end else if (!(2'h1 == cur_state)) begin // @[UARTLoader.scala 43:21]
      if (2'h3 == cur_state) begin // @[UARTLoader.scala 43:21]
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
  input  [1:0]  io_cpu_state,
  input         io_uart_in_mem_write,
  input  [31:0] io_uart_in_data_to_write,
  input  [31:0] io_uart_in_data_addr,
  input         io_cpu_write_data,
  input  [1:0]  io_cpu_data_width,
  input  [31:0] io_cpu_data_addr,
  input  [31:0] io_cpu_data_write,
  output        io_write_data,
  output [1:0]  io_data_width,
  output [31:0] io_data_addr,
  output [31:0] io_data_write
);
  assign io_write_data = io_cpu_state == 2'h2 ? io_uart_in_mem_write : io_cpu_write_data; // @[MemWriteSelector.scala 29:54 31:18 37:18]
  assign io_data_width = io_cpu_state == 2'h2 ? 2'h0 : io_cpu_data_width; // @[MemWriteSelector.scala 29:54 32:18 38:18]
  assign io_data_addr = io_cpu_state == 2'h2 ? io_uart_in_data_addr : io_cpu_data_addr; // @[MemWriteSelector.scala 29:54 33:17 39:17]
  assign io_data_write = io_cpu_state == 2'h2 ? io_uart_in_data_to_write : io_cpu_data_write; // @[MemWriteSelector.scala 29:54 34:18 40:18]
endmodule
module CPUState(
  input        clock,
  input        reset,
  (* MARK_DEBUG="true" *)output [1:0] io_cpu_state,
  input        io_load_mode
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  reg [1:0] state; // @[CPUState.scala 13:22]
  wire  _GEN_0 = state == 2'h1 ? 1'h0 : 1'h1; // @[CPUState.scala 19:59 20:13 22:13]
  wire  _GEN_1 = state == 2'h2 | _GEN_0; // @[CPUState.scala 17:49 18:12]
  assign io_cpu_state = ~io_load_mode ? state : 2'h2; // @[CPUState.scala 15:22 16:18 25:17]
  always @(posedge clock) begin
    if (reset) begin // @[CPUState.scala 13:22]
      state <= 2'h1; // @[CPUState.scala 13:22]
    end else if (~io_load_mode) begin // @[CPUState.scala 15:22]
      state <= {{1'd0}, _GEN_1};
    end else begin
      state <= 2'h2; // @[CPUState.scala 26:10]
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
  input  [31:0] io_next_addr,
  (* MARK_DEBUG="true" *)output [31:0] io_addr
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] pc; // @[PC.scala 15:17]
  assign io_addr = pc; // @[PC.scala 16:10]
  always @(posedge clock) begin
    if (reset) begin // @[PC.scala 15:17]
      pc <= 32'h0; // @[PC.scala 15:17]
    end else if (io_cpu_state == 2'h0) begin // @[PC.scala 17:53]
      pc <= io_next_addr; // @[PC.scala 18:7]
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
  output [19:0] io_raw_imm
);
  wire [19:0] _io_raw_imm_T_1 = {8'h0,io_instruction[31:20]}; // @[Cat.scala 33:92]
  wire [11:0] _io_raw_imm_T_8 = {io_instruction[31:25],io_instruction[11:7]}; // @[Cat.scala 33:92]
  wire [19:0] _io_raw_imm_T_13 = {8'h0,io_instruction[31],io_instruction[7],io_instruction[30:25],io_instruction[11:8]}; // @[Cat.scala 33:92]
  wire [19:0] _io_raw_imm_T_20 = {io_instruction[31],io_instruction[19:12],io_instruction[20],io_instruction[30:21]}; // @[Cat.scala 33:92]
  wire [19:0] _GEN_0 = 7'h6f == io_opcode ? _io_raw_imm_T_20 : 20'h0; // @[InstructionDecoder.scala 24:14 26:27 56:18]
  wire [19:0] _GEN_1 = 7'h17 == io_opcode ? io_instruction[31:12] : _GEN_0; // @[InstructionDecoder.scala 26:27 53:18]
  wire [19:0] _GEN_2 = 7'h37 == io_opcode ? io_instruction[31:12] : _GEN_1; // @[InstructionDecoder.scala 26:27 50:18]
  wire [19:0] _GEN_3 = 7'h63 == io_opcode ? _io_raw_imm_T_13 : _GEN_2; // @[InstructionDecoder.scala 26:27 46:18]
  wire [19:0] _GEN_4 = 7'h23 == io_opcode ? {{8'd0}, _io_raw_imm_T_8} : _GEN_3; // @[InstructionDecoder.scala 26:27 43:18]
  wire [19:0] _GEN_5 = 7'h73 == io_opcode ? {{13'd0}, io_func7} : _GEN_4; // @[InstructionDecoder.scala 26:27 40:18]
  wire [19:0] _GEN_6 = 7'h67 == io_opcode ? _io_raw_imm_T_1 : _GEN_5; // @[InstructionDecoder.scala 26:27 37:18]
  wire [19:0] _GEN_7 = 7'h3 == io_opcode ? _io_raw_imm_T_1 : _GEN_6; // @[InstructionDecoder.scala 26:27 34:18]
  wire [19:0] _GEN_8 = 7'h13 == io_opcode ? _io_raw_imm_T_1 : _GEN_7; // @[InstructionDecoder.scala 26:27 31:18]
  assign io_opcode = io_instruction[6:0]; // @[InstructionDecoder.scala 17:30]
  assign io_rs1 = io_instruction[19:15]; // @[InstructionDecoder.scala 20:27]
  assign io_rs2 = io_instruction[24:20]; // @[InstructionDecoder.scala 21:27]
  assign io_rd = io_instruction[11:7]; // @[InstructionDecoder.scala 18:26]
  assign io_func3 = io_instruction[14:12]; // @[InstructionDecoder.scala 19:29]
  assign io_func7 = io_instruction[31:25]; // @[InstructionDecoder.scala 22:29]
  assign io_raw_imm = 7'h33 == io_opcode ? 20'h0 : _GEN_8; // @[InstructionDecoder.scala 26:27 28:18]
endmodule
module ControlUnit(
  input  [6:0]  io_opcode,
  input  [2:0]  io_func3,
  input  [6:0]  io_func7,
  input  [4:0]  io_rs1,
  input  [4:0]  io_rs2,
  input  [4:0]  io_rd,
  input  [19:0] io_raw_imm,
  output [4:0]  io_rs1_out,
  output [4:0]  io_rs2_out,
  output [4:0]  io_rd_out,
  output [19:0] io_raw_imm_out,
  output [2:0]  io_alu_type,
  output [1:0]  io_cmp_type,
  output        io_unsigned,
  output [1:0]  io_nextPC_type,
  output        io_regs_write,
  output [1:0]  io_imm_width_type,
  output        io_operand2_type,
  output        io_au_type,
  output [2:0]  io_write_back_type,
  output        io_memory_write,
  output [1:0]  io_data_width
);
  wire [9:0] _T_1 = {io_func3,io_func7}; // @[Cat.scala 33:92]
  wire  _GEN_2 = 10'h100 == _T_1 | 10'h180 == _T_1; // @[ControlUnit.scala 65:39 91:22]
  wire  _GEN_4 = 10'h100 == _T_1 ? 1'h0 : 10'h180 == _T_1; // @[ControlUnit.scala 64:19 65:39]
  wire  _GEN_6 = 10'h2a0 == _T_1 ? 1'h0 : _GEN_2; // @[ControlUnit.scala 59:18 65:39]
  wire  _GEN_8 = 10'h2a0 == _T_1 ? 1'h0 : _GEN_4; // @[ControlUnit.scala 64:19 65:39]
  wire [2:0] _GEN_9 = 10'h280 == _T_1 ? 3'h6 : 3'h7; // @[ControlUnit.scala 65:39 85:23]
  wire  _GEN_10 = 10'h280 == _T_1 ? 1'h0 : _GEN_6; // @[ControlUnit.scala 59:18 65:39]
  wire  _GEN_12 = 10'h280 == _T_1 ? 1'h0 : _GEN_8; // @[ControlUnit.scala 64:19 65:39]
  wire [2:0] _GEN_13 = 10'h80 == _T_1 ? 3'h5 : _GEN_9; // @[ControlUnit.scala 65:39 82:23]
  wire  _GEN_14 = 10'h80 == _T_1 ? 1'h0 : _GEN_10; // @[ControlUnit.scala 59:18 65:39]
  wire  _GEN_16 = 10'h80 == _T_1 ? 1'h0 : _GEN_12; // @[ControlUnit.scala 64:19 65:39]
  wire [2:0] _GEN_17 = 10'h380 == _T_1 ? 3'h4 : _GEN_13; // @[ControlUnit.scala 65:39 79:23]
  wire  _GEN_18 = 10'h380 == _T_1 ? 1'h0 : _GEN_14; // @[ControlUnit.scala 59:18 65:39]
  wire  _GEN_20 = 10'h380 == _T_1 ? 1'h0 : _GEN_16; // @[ControlUnit.scala 64:19 65:39]
  wire [2:0] _GEN_21 = 10'h300 == _T_1 ? 3'h3 : _GEN_17; // @[ControlUnit.scala 65:39 76:23]
  wire  _GEN_22 = 10'h300 == _T_1 ? 1'h0 : _GEN_18; // @[ControlUnit.scala 59:18 65:39]
  wire  _GEN_24 = 10'h300 == _T_1 ? 1'h0 : _GEN_20; // @[ControlUnit.scala 64:19 65:39]
  wire [2:0] _GEN_25 = 10'h200 == _T_1 ? 3'h2 : _GEN_21; // @[ControlUnit.scala 65:39 73:23]
  wire  _GEN_26 = 10'h200 == _T_1 ? 1'h0 : _GEN_22; // @[ControlUnit.scala 59:18 65:39]
  wire  _GEN_28 = 10'h200 == _T_1 ? 1'h0 : _GEN_24; // @[ControlUnit.scala 64:19 65:39]
  wire [2:0] _GEN_29 = 10'h20 == _T_1 ? 3'h1 : _GEN_25; // @[ControlUnit.scala 65:39 70:23]
  wire  _GEN_30 = 10'h20 == _T_1 ? 1'h0 : _GEN_26; // @[ControlUnit.scala 59:18 65:39]
  wire  _GEN_32 = 10'h20 == _T_1 ? 1'h0 : _GEN_28; // @[ControlUnit.scala 64:19 65:39]
  wire [2:0] _GEN_33 = 10'h0 == _T_1 ? 3'h0 : _GEN_29; // @[ControlUnit.scala 65:39 67:23]
  wire  _GEN_34 = 10'h0 == _T_1 ? 1'h0 : _GEN_30; // @[ControlUnit.scala 59:18 65:39]
  wire  _GEN_36 = 10'h0 == _T_1 ? 1'h0 : _GEN_32; // @[ControlUnit.scala 64:19 65:39]
  wire  _T_13 = 3'h0 == io_func3; // @[ControlUnit.scala 112:24]
  wire  _T_14 = 3'h4 == io_func3; // @[ControlUnit.scala 112:24]
  wire  _T_15 = 3'h6 == io_func3; // @[ControlUnit.scala 112:24]
  wire  _T_16 = 3'h7 == io_func3; // @[ControlUnit.scala 112:24]
  wire  _T_17 = 3'h1 == io_func3; // @[ControlUnit.scala 112:24]
  wire  _T_18 = 3'h5 == io_func3; // @[ControlUnit.scala 112:24]
  wire [2:0] _GEN_38 = 7'h0 == io_func7 ? 3'h6 : 3'h7; // @[ControlUnit.scala 129:28 131:27]
  wire  _T_21 = 3'h2 == io_func3; // @[ControlUnit.scala 112:24]
  wire  _GEN_41 = 3'h2 == io_func3 | 3'h3 == io_func3; // @[ControlUnit.scala 112:24 139:22]
  wire  _GEN_43 = 3'h2 == io_func3 ? 1'h0 : 3'h3 == io_func3; // @[ControlUnit.scala 111:19 112:24]
  wire  _GEN_45 = 3'h5 == io_func3 ? 1'h0 : _GEN_41; // @[ControlUnit.scala 105:18 112:24]
  wire  _GEN_47 = 3'h5 == io_func3 ? 1'h0 : _GEN_43; // @[ControlUnit.scala 111:19 112:24]
  wire [2:0] _GEN_48 = 3'h1 == io_func3 ? 3'h5 : _GEN_38; // @[ControlUnit.scala 112:24 126:23]
  wire  _GEN_49 = 3'h1 == io_func3 ? 1'h0 : _GEN_45; // @[ControlUnit.scala 105:18 112:24]
  wire  _GEN_51 = 3'h1 == io_func3 ? 1'h0 : _GEN_47; // @[ControlUnit.scala 111:19 112:24]
  wire [2:0] _GEN_52 = 3'h7 == io_func3 ? 3'h4 : _GEN_48; // @[ControlUnit.scala 112:24 123:23]
  wire  _GEN_53 = 3'h7 == io_func3 ? 1'h0 : _GEN_49; // @[ControlUnit.scala 105:18 112:24]
  wire  _GEN_55 = 3'h7 == io_func3 ? 1'h0 : _GEN_51; // @[ControlUnit.scala 111:19 112:24]
  wire [2:0] _GEN_56 = 3'h6 == io_func3 ? 3'h3 : _GEN_52; // @[ControlUnit.scala 112:24 120:23]
  wire  _GEN_57 = 3'h6 == io_func3 ? 1'h0 : _GEN_53; // @[ControlUnit.scala 105:18 112:24]
  wire  _GEN_59 = 3'h6 == io_func3 ? 1'h0 : _GEN_55; // @[ControlUnit.scala 111:19 112:24]
  wire [2:0] _GEN_60 = 3'h4 == io_func3 ? 3'h2 : _GEN_56; // @[ControlUnit.scala 112:24 117:23]
  wire  _GEN_61 = 3'h4 == io_func3 ? 1'h0 : _GEN_57; // @[ControlUnit.scala 105:18 112:24]
  wire  _GEN_63 = 3'h4 == io_func3 ? 1'h0 : _GEN_59; // @[ControlUnit.scala 111:19 112:24]
  wire [2:0] _GEN_64 = 3'h0 == io_func3 ? 3'h0 : _GEN_60; // @[ControlUnit.scala 112:24 114:23]
  wire  _GEN_65 = 3'h0 == io_func3 ? 1'h0 : _GEN_61; // @[ControlUnit.scala 105:18 112:24]
  wire  _GEN_67 = 3'h0 == io_func3 ? 1'h0 : _GEN_63; // @[ControlUnit.scala 111:19 112:24]
  wire  _GEN_70 = _T_14 ? 1'h0 : 1'h1; // @[ControlUnit.scala 161:24 172:25]
  wire  _GEN_71 = _T_14 | _T_18; // @[ControlUnit.scala 161:24 173:23]
  wire [1:0] _GEN_72 = _T_21 ? 2'h2 : {{1'd0}, _GEN_70}; // @[ControlUnit.scala 161:24 169:25]
  wire  _GEN_73 = _T_21 ? 1'h0 : _GEN_71; // @[ControlUnit.scala 159:19 161:24]
  wire [1:0] _GEN_74 = _T_17 ? 2'h1 : _GEN_72; // @[ControlUnit.scala 161:24 166:25]
  wire  _GEN_75 = _T_17 ? 1'h0 : _GEN_73; // @[ControlUnit.scala 159:19 161:24]
  wire [1:0] _GEN_76 = _T_13 ? 2'h0 : _GEN_74; // @[ControlUnit.scala 161:24 163:25]
  wire  _GEN_77 = _T_13 ? 1'h0 : _GEN_75; // @[ControlUnit.scala 159:19 161:24]
  wire [1:0] _GEN_79 = _T_17 ? 2'h1 : 2'h2; // @[ControlUnit.scala 193:24 198:25]
  wire [1:0] _GEN_80 = _T_13 ? 2'h0 : _GEN_79; // @[ControlUnit.scala 193:24 195:25]
  wire  _GEN_83 = _T_15 | _T_16; // @[ControlUnit.scala 217:24 231:23]
  wire  _GEN_84 = _T_15 ? 1'h0 : 1'h1; // @[ControlUnit.scala 217:24 232:23]
  wire  _GEN_85 = _T_18 | _GEN_84; // @[ControlUnit.scala 217:24 228:23]
  wire  _GEN_86 = _T_18 ? 1'h0 : _GEN_83; // @[ControlUnit.scala 214:19 217:24]
  wire  _GEN_87 = _T_14 ? 1'h0 : _GEN_85; // @[ControlUnit.scala 217:24 225:23]
  wire  _GEN_88 = _T_14 ? 1'h0 : _GEN_86; // @[ControlUnit.scala 214:19 217:24]
  wire [1:0] _GEN_89 = _T_17 ? 2'h3 : {{1'd0}, _GEN_87}; // @[ControlUnit.scala 217:24 222:23]
  wire  _GEN_90 = _T_17 ? 1'h0 : _GEN_88; // @[ControlUnit.scala 214:19 217:24]
  wire [1:0] _GEN_91 = _T_13 ? 2'h2 : _GEN_89; // @[ControlUnit.scala 217:24 219:23]
  wire  _GEN_92 = _T_13 ? 1'h0 : _GEN_90; // @[ControlUnit.scala 214:19 217:24]
  wire [2:0] _GEN_100 = 7'h37 == io_opcode ? 3'h3 : 3'h4; // @[ControlUnit.scala 55:21 273:26]
  wire [1:0] _GEN_101 = 7'h67 == io_opcode ? 2'h2 : 2'h0; // @[ControlUnit.scala 55:21 255:22]
  wire [2:0] _GEN_105 = 7'h67 == io_opcode ? 3'h2 : _GEN_100; // @[ControlUnit.scala 55:21 260:26]
  wire [1:0] _GEN_106 = 7'h67 == io_opcode ? 2'h0 : 2'h3; // @[ControlUnit.scala 55:21 261:25]
  wire [1:0] _GEN_107 = 7'h6f == io_opcode ? 2'h2 : _GEN_101; // @[ControlUnit.scala 55:21 242:22]
  wire [2:0] _GEN_111 = 7'h6f == io_opcode ? 3'h2 : _GEN_105; // @[ControlUnit.scala 55:21 248:26]
  wire [1:0] _GEN_112 = 7'h6f == io_opcode ? 2'h1 : _GEN_106; // @[ControlUnit.scala 55:21 249:25]
  wire [1:0] _GEN_113 = 7'h63 == io_opcode ? 2'h1 : _GEN_107; // @[ControlUnit.scala 55:21 207:22]
  wire  _GEN_114 = 7'h63 == io_opcode ? 1'h0 : 1'h1; // @[ControlUnit.scala 208:21 55:21]
  wire [1:0] _GEN_119 = 7'h63 == io_opcode ? 2'h1 : _GEN_112; // @[ControlUnit.scala 55:21 215:25]
  wire [1:0] _GEN_122 = 7'h23 == io_opcode ? 2'h0 : _GEN_113; // @[ControlUnit.scala 55:21 183:22]
  wire  _GEN_123 = 7'h23 == io_opcode ? 1'h0 : _GEN_114; // @[ControlUnit.scala 184:21 55:21]
  wire  _GEN_124 = 7'h23 == io_opcode ? 1'h0 : 7'h63 == io_opcode; // @[ControlUnit.scala 185:18 55:21]
  wire [1:0] _GEN_128 = 7'h23 == io_opcode ? 2'h0 : _GEN_119; // @[ControlUnit.scala 55:21 190:25]
  wire  _GEN_129 = 7'h23 == io_opcode ? 1'h0 : 7'h63 == io_opcode & _GEN_92; // @[ControlUnit.scala 191:19 55:21]
  wire [1:0] _GEN_133 = 7'h3 == io_opcode ? 2'h0 : _GEN_122; // @[ControlUnit.scala 55:21 151:22]
  wire  _GEN_134 = 7'h3 == io_opcode | _GEN_123; // @[ControlUnit.scala 152:21 55:21]
  wire  _GEN_135 = 7'h3 == io_opcode ? 1'h0 : _GEN_124; // @[ControlUnit.scala 153:18 55:21]
  wire  _GEN_137 = 7'h3 == io_opcode ? 1'h0 : 7'h23 == io_opcode; // @[ControlUnit.scala 55:21 155:23]
  wire [2:0] _GEN_138 = 7'h3 == io_opcode ? 3'h1 : _GEN_111; // @[ControlUnit.scala 55:21 157:26]
  wire [1:0] _GEN_139 = 7'h3 == io_opcode ? 2'h0 : _GEN_128; // @[ControlUnit.scala 55:21 158:25]
  wire  _GEN_140 = 7'h3 == io_opcode ? _GEN_77 : _GEN_129; // @[ControlUnit.scala 55:21]
  wire [1:0] _GEN_144 = 7'h13 == io_opcode ? 2'h0 : _GEN_133; // @[ControlUnit.scala 55:21 103:22]
  wire  _GEN_145 = 7'h13 == io_opcode | _GEN_134; // @[ControlUnit.scala 104:21 55:21]
  wire  _GEN_146 = 7'h13 == io_opcode ? _GEN_65 : _GEN_135; // @[ControlUnit.scala 55:21]
  wire  _GEN_148 = 7'h13 == io_opcode ? 1'h0 : _GEN_137; // @[ControlUnit.scala 55:21 107:23]
  wire  _GEN_149 = 7'h13 == io_opcode ? 1'h0 : _GEN_135; // @[ControlUnit.scala 55:21 108:24]
  wire [2:0] _GEN_151 = 7'h13 == io_opcode ? 3'h0 : _GEN_138; // @[ControlUnit.scala 55:21 110:26]
  wire  _GEN_152 = 7'h13 == io_opcode ? _GEN_67 : _GEN_140; // @[ControlUnit.scala 55:21]
  wire [2:0] _GEN_153 = 7'h13 == io_opcode ? _GEN_64 : 3'h0; // @[ControlUnit.scala 55:21]
  wire [1:0] _GEN_154 = 7'h13 == io_opcode ? 2'h0 : _GEN_91; // @[ControlUnit.scala 55:21]
  assign io_rs1_out = io_rs1; // @[ControlUnit.scala 37:14]
  assign io_rs2_out = io_rs2; // @[ControlUnit.scala 38:14]
  assign io_rd_out = io_rd; // @[ControlUnit.scala 39:13]
  assign io_raw_imm_out = io_raw_imm; // @[ControlUnit.scala 40:18]
  assign io_alu_type = 7'h33 == io_opcode ? _GEN_33 : _GEN_153; // @[ControlUnit.scala 55:21]
  assign io_cmp_type = 7'h33 == io_opcode ? 2'h0 : _GEN_154; // @[ControlUnit.scala 55:21]
  assign io_unsigned = 7'h33 == io_opcode ? _GEN_36 : _GEN_152; // @[ControlUnit.scala 55:21]
  assign io_nextPC_type = 7'h33 == io_opcode ? 2'h0 : _GEN_144; // @[ControlUnit.scala 55:21 57:22]
  assign io_regs_write = 7'h33 == io_opcode | _GEN_145; // @[ControlUnit.scala 55:21 58:21]
  assign io_imm_width_type = 7'h13 == io_opcode ? 2'h0 : _GEN_139; // @[ControlUnit.scala 55:21 109:25]
  assign io_operand2_type = 7'h33 == io_opcode | _GEN_149; // @[ControlUnit.scala 55:21 62:24]
  assign io_au_type = 7'h33 == io_opcode ? _GEN_34 : _GEN_146; // @[ControlUnit.scala 55:21]
  assign io_write_back_type = 7'h33 == io_opcode ? 3'h0 : _GEN_151; // @[ControlUnit.scala 55:21 63:26]
  assign io_memory_write = 7'h33 == io_opcode ? 1'h0 : _GEN_148; // @[ControlUnit.scala 55:21 61:23]
  assign io_data_width = 7'h3 == io_opcode ? _GEN_76 : _GEN_80; // @[ControlUnit.scala 55:21]
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
  (* MARK_DEBUG="true" *)output [31:0] io_rs1_val,
  (* MARK_DEBUG="true" *)output [31:0] io_rs2_val
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
  reg [31:0] regs_0; // @[Registers.scala 23:21]
  reg [31:0] regs_1; // @[Registers.scala 23:21]
  reg [31:0] regs_2; // @[Registers.scala 23:21]
  reg [31:0] regs_3; // @[Registers.scala 23:21]
  reg [31:0] regs_4; // @[Registers.scala 23:21]
  reg [31:0] regs_5; // @[Registers.scala 23:21]
  reg [31:0] regs_6; // @[Registers.scala 23:21]
  reg [31:0] regs_7; // @[Registers.scala 23:21]
  reg [31:0] regs_8; // @[Registers.scala 23:21]
  reg [31:0] regs_9; // @[Registers.scala 23:21]
  reg [31:0] regs_10; // @[Registers.scala 23:21]
  reg [31:0] regs_11; // @[Registers.scala 23:21]
  reg [31:0] regs_12; // @[Registers.scala 23:21]
  reg [31:0] regs_13; // @[Registers.scala 23:21]
  reg [31:0] regs_14; // @[Registers.scala 23:21]
  reg [31:0] regs_15; // @[Registers.scala 23:21]
  reg [31:0] regs_16; // @[Registers.scala 23:21]
  reg [31:0] regs_17; // @[Registers.scala 23:21]
  reg [31:0] regs_18; // @[Registers.scala 23:21]
  reg [31:0] regs_19; // @[Registers.scala 23:21]
  reg [31:0] regs_20; // @[Registers.scala 23:21]
  reg [31:0] regs_21; // @[Registers.scala 23:21]
  reg [31:0] regs_22; // @[Registers.scala 23:21]
  reg [31:0] regs_23; // @[Registers.scala 23:21]
  reg [31:0] regs_24; // @[Registers.scala 23:21]
  reg [31:0] regs_25; // @[Registers.scala 23:21]
  reg [31:0] regs_26; // @[Registers.scala 23:21]
  reg [31:0] regs_27; // @[Registers.scala 23:21]
  reg [31:0] regs_28; // @[Registers.scala 23:21]
  reg [31:0] regs_29; // @[Registers.scala 23:21]
  reg [31:0] regs_30; // @[Registers.scala 23:21]
  reg [31:0] regs_31; // @[Registers.scala 23:21]
  wire [31:0] _GEN_1 = 5'h1 == io_rs1 ? regs_1 : regs_0; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_2 = 5'h2 == io_rs1 ? regs_2 : _GEN_1; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_3 = 5'h3 == io_rs1 ? regs_3 : _GEN_2; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_4 = 5'h4 == io_rs1 ? regs_4 : _GEN_3; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_5 = 5'h5 == io_rs1 ? regs_5 : _GEN_4; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_6 = 5'h6 == io_rs1 ? regs_6 : _GEN_5; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_7 = 5'h7 == io_rs1 ? regs_7 : _GEN_6; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_8 = 5'h8 == io_rs1 ? regs_8 : _GEN_7; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_9 = 5'h9 == io_rs1 ? regs_9 : _GEN_8; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_10 = 5'ha == io_rs1 ? regs_10 : _GEN_9; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_11 = 5'hb == io_rs1 ? regs_11 : _GEN_10; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_12 = 5'hc == io_rs1 ? regs_12 : _GEN_11; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_13 = 5'hd == io_rs1 ? regs_13 : _GEN_12; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_14 = 5'he == io_rs1 ? regs_14 : _GEN_13; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_15 = 5'hf == io_rs1 ? regs_15 : _GEN_14; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_16 = 5'h10 == io_rs1 ? regs_16 : _GEN_15; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_17 = 5'h11 == io_rs1 ? regs_17 : _GEN_16; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_18 = 5'h12 == io_rs1 ? regs_18 : _GEN_17; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_19 = 5'h13 == io_rs1 ? regs_19 : _GEN_18; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_20 = 5'h14 == io_rs1 ? regs_20 : _GEN_19; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_21 = 5'h15 == io_rs1 ? regs_21 : _GEN_20; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_22 = 5'h16 == io_rs1 ? regs_22 : _GEN_21; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_23 = 5'h17 == io_rs1 ? regs_23 : _GEN_22; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_24 = 5'h18 == io_rs1 ? regs_24 : _GEN_23; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_25 = 5'h19 == io_rs1 ? regs_25 : _GEN_24; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_26 = 5'h1a == io_rs1 ? regs_26 : _GEN_25; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_27 = 5'h1b == io_rs1 ? regs_27 : _GEN_26; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_28 = 5'h1c == io_rs1 ? regs_28 : _GEN_27; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_29 = 5'h1d == io_rs1 ? regs_29 : _GEN_28; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_30 = 5'h1e == io_rs1 ? regs_30 : _GEN_29; // @[Registers.scala 24:{14,14}]
  wire [31:0] _GEN_33 = 5'h1 == io_rs2 ? regs_1 : regs_0; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_34 = 5'h2 == io_rs2 ? regs_2 : _GEN_33; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_35 = 5'h3 == io_rs2 ? regs_3 : _GEN_34; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_36 = 5'h4 == io_rs2 ? regs_4 : _GEN_35; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_37 = 5'h5 == io_rs2 ? regs_5 : _GEN_36; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_38 = 5'h6 == io_rs2 ? regs_6 : _GEN_37; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_39 = 5'h7 == io_rs2 ? regs_7 : _GEN_38; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_40 = 5'h8 == io_rs2 ? regs_8 : _GEN_39; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_41 = 5'h9 == io_rs2 ? regs_9 : _GEN_40; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_42 = 5'ha == io_rs2 ? regs_10 : _GEN_41; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_43 = 5'hb == io_rs2 ? regs_11 : _GEN_42; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_44 = 5'hc == io_rs2 ? regs_12 : _GEN_43; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_45 = 5'hd == io_rs2 ? regs_13 : _GEN_44; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_46 = 5'he == io_rs2 ? regs_14 : _GEN_45; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_47 = 5'hf == io_rs2 ? regs_15 : _GEN_46; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_48 = 5'h10 == io_rs2 ? regs_16 : _GEN_47; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_49 = 5'h11 == io_rs2 ? regs_17 : _GEN_48; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_50 = 5'h12 == io_rs2 ? regs_18 : _GEN_49; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_51 = 5'h13 == io_rs2 ? regs_19 : _GEN_50; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_52 = 5'h14 == io_rs2 ? regs_20 : _GEN_51; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_53 = 5'h15 == io_rs2 ? regs_21 : _GEN_52; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_54 = 5'h16 == io_rs2 ? regs_22 : _GEN_53; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_55 = 5'h17 == io_rs2 ? regs_23 : _GEN_54; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_56 = 5'h18 == io_rs2 ? regs_24 : _GEN_55; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_57 = 5'h19 == io_rs2 ? regs_25 : _GEN_56; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_58 = 5'h1a == io_rs2 ? regs_26 : _GEN_57; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_59 = 5'h1b == io_rs2 ? regs_27 : _GEN_58; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_60 = 5'h1c == io_rs2 ? regs_28 : _GEN_59; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_61 = 5'h1d == io_rs2 ? regs_29 : _GEN_60; // @[Registers.scala 25:{14,14}]
  wire [31:0] _GEN_62 = 5'h1e == io_rs2 ? regs_30 : _GEN_61; // @[Registers.scala 25:{14,14}]
  wire  _T = io_cpu_state == 2'h1; // @[Registers.scala 26:21]
  wire  _T_1 = _T & io_write; // @[Registers.scala 27:5]
  assign io_rs1_val = 5'h1f == io_rs1 ? regs_31 : _GEN_30; // @[Registers.scala 24:{14,14}]
  assign io_rs2_val = 5'h1f == io_rs2 ? regs_31 : _GEN_62; // @[Registers.scala 25:{14,14}]
  always @(posedge clock) begin
    if (reset) begin // @[Registers.scala 23:21]
      regs_0 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h0 == io_rd) begin // @[Registers.scala 34:17]
        regs_0 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_1 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h1 == io_rd) begin // @[Registers.scala 34:17]
        regs_1 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_2 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h2 == io_rd) begin // @[Registers.scala 34:17]
        regs_2 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_3 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h3 == io_rd) begin // @[Registers.scala 34:17]
        regs_3 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_4 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h4 == io_rd) begin // @[Registers.scala 34:17]
        regs_4 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_5 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h5 == io_rd) begin // @[Registers.scala 34:17]
        regs_5 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_6 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h6 == io_rd) begin // @[Registers.scala 34:17]
        regs_6 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_7 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h7 == io_rd) begin // @[Registers.scala 34:17]
        regs_7 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_8 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h8 == io_rd) begin // @[Registers.scala 34:17]
        regs_8 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_9 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h9 == io_rd) begin // @[Registers.scala 34:17]
        regs_9 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_10 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'ha == io_rd) begin // @[Registers.scala 34:17]
        regs_10 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_11 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'hb == io_rd) begin // @[Registers.scala 34:17]
        regs_11 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_12 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'hc == io_rd) begin // @[Registers.scala 34:17]
        regs_12 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_13 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'hd == io_rd) begin // @[Registers.scala 34:17]
        regs_13 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_14 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'he == io_rd) begin // @[Registers.scala 34:17]
        regs_14 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_15 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'hf == io_rd) begin // @[Registers.scala 34:17]
        regs_15 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_16 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h10 == io_rd) begin // @[Registers.scala 34:17]
        regs_16 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_17 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h11 == io_rd) begin // @[Registers.scala 34:17]
        regs_17 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_18 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h12 == io_rd) begin // @[Registers.scala 34:17]
        regs_18 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_19 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h13 == io_rd) begin // @[Registers.scala 34:17]
        regs_19 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_20 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h14 == io_rd) begin // @[Registers.scala 34:17]
        regs_20 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_21 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h15 == io_rd) begin // @[Registers.scala 34:17]
        regs_21 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_22 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h16 == io_rd) begin // @[Registers.scala 34:17]
        regs_22 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_23 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h17 == io_rd) begin // @[Registers.scala 34:17]
        regs_23 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_24 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h18 == io_rd) begin // @[Registers.scala 34:17]
        regs_24 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_25 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h19 == io_rd) begin // @[Registers.scala 34:17]
        regs_25 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_26 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h1a == io_rd) begin // @[Registers.scala 34:17]
        regs_26 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_27 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h1b == io_rd) begin // @[Registers.scala 34:17]
        regs_27 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_28 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h1c == io_rd) begin // @[Registers.scala 34:17]
        regs_28 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_29 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h1d == io_rd) begin // @[Registers.scala 34:17]
        regs_29 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_30 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
      if (5'h1e == io_rd) begin // @[Registers.scala 34:17]
        regs_30 <= io_write_data; // @[Registers.scala 34:17]
      end
    end
    if (reset) begin // @[Registers.scala 23:21]
      regs_31 <= 32'h0; // @[Registers.scala 23:21]
    end else if (_T_1 & io_rd != 5'h0) begin // @[Registers.scala 28:23]
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
module OperandSelector(
  input  [31:0] io_rs1_val,
  input  [31:0] io_rs2_val,
  input  [31:0] io_real_imm,
  input         io_operand2Type,
  output [31:0] io_operand1,
  output [31:0] io_operand2
);
  assign io_operand1 = io_rs1_val; // @[OperandSelector.scala 17:15]
  assign io_operand2 = ~io_operand2Type ? io_real_imm : io_rs2_val; // @[OperandSelector.scala 19:26 21:19]
endmodule
module ALU(
  input  [31:0] io_operand1,
  input  [31:0] io_operand2,
  input  [2:0]  io_alu_op,
  input         io_unsigned,
  output [31:0] io_result
);
  wire [31:0] _io_result_T_1 = io_operand1 + io_operand2; // @[ALU.scala 17:32]
  wire [31:0] _io_result_T_3 = io_operand1 - io_operand2; // @[ALU.scala 20:32]
  wire [31:0] _io_result_T_4 = io_operand1 ^ io_operand2; // @[ALU.scala 23:32]
  wire [31:0] _io_result_T_5 = io_operand1 | io_operand2; // @[ALU.scala 26:32]
  wire [31:0] _io_result_T_6 = io_operand1 & io_operand2; // @[ALU.scala 29:32]
  wire [62:0] _GEN_9 = {{31'd0}, io_operand1}; // @[ALU.scala 32:32]
  wire [62:0] _io_result_T_8 = _GEN_9 << io_operand2[4:0]; // @[ALU.scala 32:32]
  wire [31:0] _io_result_T_12 = $signed(io_operand1) >>> io_operand2[4:0]; // @[ALU.scala 36:71]
  wire [31:0] _io_result_T_14 = io_operand1 >> io_operand2[4:0]; // @[ALU.scala 38:34]
  wire [31:0] _GEN_0 = ~io_unsigned ? _io_result_T_12 : _io_result_T_14; // @[ALU.scala 35:26 36:19 38:19]
  wire [31:0] _GEN_2 = 3'h7 == io_alu_op ? _GEN_0 : _io_result_T_14; // @[ALU.scala 15:22]
  wire [62:0] _GEN_3 = 3'h5 == io_alu_op ? _io_result_T_8 : {{31'd0}, _GEN_2}; // @[ALU.scala 15:22 32:17]
  wire [62:0] _GEN_4 = 3'h4 == io_alu_op ? {{31'd0}, _io_result_T_6} : _GEN_3; // @[ALU.scala 15:22 29:17]
  wire [62:0] _GEN_5 = 3'h3 == io_alu_op ? {{31'd0}, _io_result_T_5} : _GEN_4; // @[ALU.scala 15:22 26:17]
  wire [62:0] _GEN_6 = 3'h2 == io_alu_op ? {{31'd0}, _io_result_T_4} : _GEN_5; // @[ALU.scala 15:22 23:17]
  wire [62:0] _GEN_7 = 3'h1 == io_alu_op ? {{31'd0}, _io_result_T_3} : _GEN_6; // @[ALU.scala 15:22 20:17]
  wire [62:0] _GEN_8 = 3'h0 == io_alu_op ? {{31'd0}, _io_result_T_1} : _GEN_7; // @[ALU.scala 15:22 17:17]
  assign io_result = _GEN_8[31:0];
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
  wire [31:0] _GEN_1 = 2'h2 == io_nextPC_type ? _io_pcImm_T_1 : 32'h0; // @[NextPCGen.scala 21:12 23:25 36:16]
  wire [31:0] _GEN_2 = 2'h1 == io_nextPC_type ? _io_nextPC_T_6 : io_alu_result; // @[NextPCGen.scala 23:25 30:17]
  wire [31:0] _GEN_3 = 2'h1 == io_nextPC_type ? _io_pc4_T_1 : _GEN_1; // @[NextPCGen.scala 23:25 31:16]
  assign io_nextPC = 2'h0 == io_nextPC_type ? _io_pc4_T_1 : _GEN_2; // @[NextPCGen.scala 23:25 25:17]
  assign io_pc4 = io_pc + 32'h4; // @[NextPCGen.scala 20:19]
  assign io_pcImm = 2'h0 == io_nextPC_type ? _io_pcImm_T_1 : _GEN_3; // @[NextPCGen.scala 23:25 26:16]
endmodule
module WriteDataSelector(
  input  [2:0]  io_write_back_type,
  input  [31:0] io_imm,
  input  [31:0] io_mem_out,
  input  [31:0] io_au_out,
  input  [31:0] io_pc4,
  input  [31:0] io_pcImm,
  output [31:0] io_write_data
);
  wire [31:0] _GEN_1 = 3'h2 == io_write_back_type ? io_pc4 : io_pcImm; // @[WriteDataSelector.scala 20:30 31:21]
  wire [31:0] _GEN_2 = 3'h1 == io_write_back_type ? io_mem_out : _GEN_1; // @[WriteDataSelector.scala 20:30 28:21]
  wire [31:0] _GEN_3 = 3'h3 == io_write_back_type ? io_imm : _GEN_2; // @[WriteDataSelector.scala 20:30 25:21]
  assign io_write_data = 3'h0 == io_write_back_type ? io_au_out : _GEN_3; // @[WriteDataSelector.scala 20:30 22:21]
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
  input         io_external_signal_load_data_mode
);
  wire  memory_clock; // @[CoreTop.scala 20:22]
  wire  memory_reset; // @[CoreTop.scala 20:22]
  wire [1:0] memory_io_cpu_state; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_ins_addr; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_ins_out; // @[CoreTop.scala 20:22]
  wire  memory_io_write_data; // @[CoreTop.scala 20:22]
  wire  memory_io_unsigned; // @[CoreTop.scala 20:22]
  wire [1:0] memory_io_data_width; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_data_addr; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_data_write; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_data_out; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_external_led_led; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_external_seg7_seg7; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_external_btn_button; // @[CoreTop.scala 20:22]
  wire [31:0] memory_io_external_switches_switches; // @[CoreTop.scala 20:22]
  wire  uartLoader_clock; // @[CoreTop.scala 21:26]
  wire  uartLoader_reset; // @[CoreTop.scala 21:26]
  wire [1:0] uartLoader_io_cpu_state; // @[CoreTop.scala 21:26]
  wire  uartLoader_io_rxValid; // @[CoreTop.scala 21:26]
  wire [7:0] uartLoader_io_rxData; // @[CoreTop.scala 21:26]
  wire  uartLoader_io_mem_mem_write; // @[CoreTop.scala 21:26]
  wire [31:0] uartLoader_io_mem_data_to_write; // @[CoreTop.scala 21:26]
  wire [31:0] uartLoader_io_mem_data_addr; // @[CoreTop.scala 21:26]
  wire [1:0] memInSelector_io_cpu_state; // @[CoreTop.scala 22:29]
  wire  memInSelector_io_uart_in_mem_write; // @[CoreTop.scala 22:29]
  wire [31:0] memInSelector_io_uart_in_data_to_write; // @[CoreTop.scala 22:29]
  wire [31:0] memInSelector_io_uart_in_data_addr; // @[CoreTop.scala 22:29]
  wire  memInSelector_io_cpu_write_data; // @[CoreTop.scala 22:29]
  wire [1:0] memInSelector_io_cpu_data_width; // @[CoreTop.scala 22:29]
  wire [31:0] memInSelector_io_cpu_data_addr; // @[CoreTop.scala 22:29]
  wire [31:0] memInSelector_io_cpu_data_write; // @[CoreTop.scala 22:29]
  wire  memInSelector_io_write_data; // @[CoreTop.scala 22:29]
  wire [1:0] memInSelector_io_data_width; // @[CoreTop.scala 22:29]
  wire [31:0] memInSelector_io_data_addr; // @[CoreTop.scala 22:29]
  wire [31:0] memInSelector_io_data_write; // @[CoreTop.scala 22:29]
  wire  state_clock; // @[CoreTop.scala 24:21]
  wire  state_reset; // @[CoreTop.scala 24:21]
  wire [1:0] state_io_cpu_state; // @[CoreTop.scala 24:21]
  wire  state_io_load_mode; // @[CoreTop.scala 24:21]
  wire  pc_clock; // @[CoreTop.scala 27:18]
  wire  pc_reset; // @[CoreTop.scala 27:18]
  wire [1:0] pc_io_cpu_state; // @[CoreTop.scala 27:18]
  wire [31:0] pc_io_next_addr; // @[CoreTop.scala 27:18]
  wire [31:0] pc_io_addr; // @[CoreTop.scala 27:18]
  wire [31:0] insDecode_io_instruction; // @[CoreTop.scala 30:25]
  wire [6:0] insDecode_io_opcode; // @[CoreTop.scala 30:25]
  wire [4:0] insDecode_io_rs1; // @[CoreTop.scala 30:25]
  wire [4:0] insDecode_io_rs2; // @[CoreTop.scala 30:25]
  wire [4:0] insDecode_io_rd; // @[CoreTop.scala 30:25]
  wire [2:0] insDecode_io_func3; // @[CoreTop.scala 30:25]
  wire [6:0] insDecode_io_func7; // @[CoreTop.scala 30:25]
  wire [19:0] insDecode_io_raw_imm; // @[CoreTop.scala 30:25]
  wire [6:0] CU_io_opcode; // @[CoreTop.scala 31:18]
  wire [2:0] CU_io_func3; // @[CoreTop.scala 31:18]
  wire [6:0] CU_io_func7; // @[CoreTop.scala 31:18]
  wire [4:0] CU_io_rs1; // @[CoreTop.scala 31:18]
  wire [4:0] CU_io_rs2; // @[CoreTop.scala 31:18]
  wire [4:0] CU_io_rd; // @[CoreTop.scala 31:18]
  wire [19:0] CU_io_raw_imm; // @[CoreTop.scala 31:18]
  wire [4:0] CU_io_rs1_out; // @[CoreTop.scala 31:18]
  wire [4:0] CU_io_rs2_out; // @[CoreTop.scala 31:18]
  wire [4:0] CU_io_rd_out; // @[CoreTop.scala 31:18]
  wire [19:0] CU_io_raw_imm_out; // @[CoreTop.scala 31:18]
  wire [2:0] CU_io_alu_type; // @[CoreTop.scala 31:18]
  wire [1:0] CU_io_cmp_type; // @[CoreTop.scala 31:18]
  wire  CU_io_unsigned; // @[CoreTop.scala 31:18]
  wire [1:0] CU_io_nextPC_type; // @[CoreTop.scala 31:18]
  wire  CU_io_regs_write; // @[CoreTop.scala 31:18]
  wire [1:0] CU_io_imm_width_type; // @[CoreTop.scala 31:18]
  wire  CU_io_operand2_type; // @[CoreTop.scala 31:18]
  wire  CU_io_au_type; // @[CoreTop.scala 31:18]
  wire [2:0] CU_io_write_back_type; // @[CoreTop.scala 31:18]
  wire  CU_io_memory_write; // @[CoreTop.scala 31:18]
  wire [1:0] CU_io_data_width; // @[CoreTop.scala 31:18]
  wire  regs_clock; // @[CoreTop.scala 34:20]
  wire  regs_reset; // @[CoreTop.scala 34:20]
  wire [1:0] regs_io_cpu_state; // @[CoreTop.scala 34:20]
  wire  regs_io_write; // @[CoreTop.scala 34:20]
  wire [4:0] regs_io_rs1; // @[CoreTop.scala 34:20]
  wire [4:0] regs_io_rs2; // @[CoreTop.scala 34:20]
  wire [4:0] regs_io_rd; // @[CoreTop.scala 34:20]
  wire [31:0] regs_io_write_data; // @[CoreTop.scala 34:20]
  wire [31:0] regs_io_rs1_val; // @[CoreTop.scala 34:20]
  wire [31:0] regs_io_rs2_val; // @[CoreTop.scala 34:20]
  wire [19:0] immGen_io_raw_imm; // @[CoreTop.scala 35:22]
  wire  immGen_io_unsigned; // @[CoreTop.scala 35:22]
  wire [1:0] immGen_io_imm_width; // @[CoreTop.scala 35:22]
  wire [31:0] immGen_io_real_imm; // @[CoreTop.scala 35:22]
  wire [31:0] operandSelector_io_rs1_val; // @[CoreTop.scala 36:31]
  wire [31:0] operandSelector_io_rs2_val; // @[CoreTop.scala 36:31]
  wire [31:0] operandSelector_io_real_imm; // @[CoreTop.scala 36:31]
  wire  operandSelector_io_operand2Type; // @[CoreTop.scala 36:31]
  wire [31:0] operandSelector_io_operand1; // @[CoreTop.scala 36:31]
  wire [31:0] operandSelector_io_operand2; // @[CoreTop.scala 36:31]
  wire [31:0] ALU_io_operand1; // @[CoreTop.scala 37:19]
  wire [31:0] ALU_io_operand2; // @[CoreTop.scala 37:19]
  wire [2:0] ALU_io_alu_op; // @[CoreTop.scala 37:19]
  wire  ALU_io_unsigned; // @[CoreTop.scala 37:19]
  wire [31:0] ALU_io_result; // @[CoreTop.scala 37:19]
  wire [31:0] CMP_io_operand1; // @[CoreTop.scala 38:19]
  wire [31:0] CMP_io_operand2; // @[CoreTop.scala 38:19]
  wire [1:0] CMP_io_cmp_op; // @[CoreTop.scala 38:19]
  wire  CMP_io_unsigned; // @[CoreTop.scala 38:19]
  wire  CMP_io_result; // @[CoreTop.scala 38:19]
  wire [1:0] nextPCGen_io_nextPC_type; // @[CoreTop.scala 39:25]
  wire  nextPCGen_io_cmp_result; // @[CoreTop.scala 39:25]
  wire [31:0] nextPCGen_io_alu_result; // @[CoreTop.scala 39:25]
  wire [31:0] nextPCGen_io_imm; // @[CoreTop.scala 39:25]
  wire [31:0] nextPCGen_io_pc; // @[CoreTop.scala 39:25]
  wire [31:0] nextPCGen_io_nextPC; // @[CoreTop.scala 39:25]
  wire [31:0] nextPCGen_io_pc4; // @[CoreTop.scala 39:25]
  wire [31:0] nextPCGen_io_pcImm; // @[CoreTop.scala 39:25]
  wire [2:0] writeDataSelector_io_write_back_type; // @[CoreTop.scala 58:33]
  wire [31:0] writeDataSelector_io_imm; // @[CoreTop.scala 58:33]
  wire [31:0] writeDataSelector_io_mem_out; // @[CoreTop.scala 58:33]
  wire [31:0] writeDataSelector_io_au_out; // @[CoreTop.scala 58:33]
  wire [31:0] writeDataSelector_io_pc4; // @[CoreTop.scala 58:33]
  wire [31:0] writeDataSelector_io_pcImm; // @[CoreTop.scala 58:33]
  wire [31:0] writeDataSelector_io_write_data; // @[CoreTop.scala 58:33]
  wire  auSelector_io_au_type; // @[CoreTop.scala 59:26]
  wire [31:0] auSelector_io_alu_result; // @[CoreTop.scala 59:26]
  wire  auSelector_io_cmp_result; // @[CoreTop.scala 59:26]
  wire [31:0] auSelector_io_au_out; // @[CoreTop.scala 59:26]
  MemoryDispatch memory ( // @[CoreTop.scala 20:22]
    .clock(memory_clock),
    .reset(memory_reset),
    .io_cpu_state(memory_io_cpu_state),
    .io_ins_addr(memory_io_ins_addr),
    .io_ins_out(memory_io_ins_out),
    .io_write_data(memory_io_write_data),
    .io_unsigned(memory_io_unsigned),
    .io_data_width(memory_io_data_width),
    .io_data_addr(memory_io_data_addr),
    .io_data_write(memory_io_data_write),
    .io_data_out(memory_io_data_out),
    .io_external_led_led(memory_io_external_led_led),
    .io_external_seg7_seg7(memory_io_external_seg7_seg7),
    .io_external_btn_button(memory_io_external_btn_button),
    .io_external_switches_switches(memory_io_external_switches_switches)
  );
  UARTLoader uartLoader ( // @[CoreTop.scala 21:26]
    .clock(uartLoader_clock),
    .reset(uartLoader_reset),
    .io_cpu_state(uartLoader_io_cpu_state),
    .io_rxValid(uartLoader_io_rxValid),
    .io_rxData(uartLoader_io_rxData),
    .io_mem_mem_write(uartLoader_io_mem_mem_write),
    .io_mem_data_to_write(uartLoader_io_mem_data_to_write),
    .io_mem_data_addr(uartLoader_io_mem_data_addr)
  );
  MemWriteSelector memInSelector ( // @[CoreTop.scala 22:29]
    .io_cpu_state(memInSelector_io_cpu_state),
    .io_uart_in_mem_write(memInSelector_io_uart_in_mem_write),
    .io_uart_in_data_to_write(memInSelector_io_uart_in_data_to_write),
    .io_uart_in_data_addr(memInSelector_io_uart_in_data_addr),
    .io_cpu_write_data(memInSelector_io_cpu_write_data),
    .io_cpu_data_width(memInSelector_io_cpu_data_width),
    .io_cpu_data_addr(memInSelector_io_cpu_data_addr),
    .io_cpu_data_write(memInSelector_io_cpu_data_write),
    .io_write_data(memInSelector_io_write_data),
    .io_data_width(memInSelector_io_data_width),
    .io_data_addr(memInSelector_io_data_addr),
    .io_data_write(memInSelector_io_data_write)
  );
  CPUState state ( // @[CoreTop.scala 24:21]
    .clock(state_clock),
    .reset(state_reset),
    .io_cpu_state(state_io_cpu_state),
    .io_load_mode(state_io_load_mode)
  );
  PC pc ( // @[CoreTop.scala 27:18]
    .clock(pc_clock),
    .reset(pc_reset),
    .io_cpu_state(pc_io_cpu_state),
    .io_next_addr(pc_io_next_addr),
    .io_addr(pc_io_addr)
  );
  InstructionDecoder insDecode ( // @[CoreTop.scala 30:25]
    .io_instruction(insDecode_io_instruction),
    .io_opcode(insDecode_io_opcode),
    .io_rs1(insDecode_io_rs1),
    .io_rs2(insDecode_io_rs2),
    .io_rd(insDecode_io_rd),
    .io_func3(insDecode_io_func3),
    .io_func7(insDecode_io_func7),
    .io_raw_imm(insDecode_io_raw_imm)
  );
  ControlUnit CU ( // @[CoreTop.scala 31:18]
    .io_opcode(CU_io_opcode),
    .io_func3(CU_io_func3),
    .io_func7(CU_io_func7),
    .io_rs1(CU_io_rs1),
    .io_rs2(CU_io_rs2),
    .io_rd(CU_io_rd),
    .io_raw_imm(CU_io_raw_imm),
    .io_rs1_out(CU_io_rs1_out),
    .io_rs2_out(CU_io_rs2_out),
    .io_rd_out(CU_io_rd_out),
    .io_raw_imm_out(CU_io_raw_imm_out),
    .io_alu_type(CU_io_alu_type),
    .io_cmp_type(CU_io_cmp_type),
    .io_unsigned(CU_io_unsigned),
    .io_nextPC_type(CU_io_nextPC_type),
    .io_regs_write(CU_io_regs_write),
    .io_imm_width_type(CU_io_imm_width_type),
    .io_operand2_type(CU_io_operand2_type),
    .io_au_type(CU_io_au_type),
    .io_write_back_type(CU_io_write_back_type),
    .io_memory_write(CU_io_memory_write),
    .io_data_width(CU_io_data_width)
  );
  Registers regs ( // @[CoreTop.scala 34:20]
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
  ImmGen immGen ( // @[CoreTop.scala 35:22]
    .io_raw_imm(immGen_io_raw_imm),
    .io_unsigned(immGen_io_unsigned),
    .io_imm_width(immGen_io_imm_width),
    .io_real_imm(immGen_io_real_imm)
  );
  OperandSelector operandSelector ( // @[CoreTop.scala 36:31]
    .io_rs1_val(operandSelector_io_rs1_val),
    .io_rs2_val(operandSelector_io_rs2_val),
    .io_real_imm(operandSelector_io_real_imm),
    .io_operand2Type(operandSelector_io_operand2Type),
    .io_operand1(operandSelector_io_operand1),
    .io_operand2(operandSelector_io_operand2)
  );
  ALU ALU ( // @[CoreTop.scala 37:19]
    .io_operand1(ALU_io_operand1),
    .io_operand2(ALU_io_operand2),
    .io_alu_op(ALU_io_alu_op),
    .io_unsigned(ALU_io_unsigned),
    .io_result(ALU_io_result)
  );
  CMP CMP ( // @[CoreTop.scala 38:19]
    .io_operand1(CMP_io_operand1),
    .io_operand2(CMP_io_operand2),
    .io_cmp_op(CMP_io_cmp_op),
    .io_unsigned(CMP_io_unsigned),
    .io_result(CMP_io_result)
  );
  NextPCGen nextPCGen ( // @[CoreTop.scala 39:25]
    .io_nextPC_type(nextPCGen_io_nextPC_type),
    .io_cmp_result(nextPCGen_io_cmp_result),
    .io_alu_result(nextPCGen_io_alu_result),
    .io_imm(nextPCGen_io_imm),
    .io_pc(nextPCGen_io_pc),
    .io_nextPC(nextPCGen_io_nextPC),
    .io_pc4(nextPCGen_io_pc4),
    .io_pcImm(nextPCGen_io_pcImm)
  );
  WriteDataSelector writeDataSelector ( // @[CoreTop.scala 58:33]
    .io_write_back_type(writeDataSelector_io_write_back_type),
    .io_imm(writeDataSelector_io_imm),
    .io_mem_out(writeDataSelector_io_mem_out),
    .io_au_out(writeDataSelector_io_au_out),
    .io_pc4(writeDataSelector_io_pc4),
    .io_pcImm(writeDataSelector_io_pcImm),
    .io_write_data(writeDataSelector_io_write_data)
  );
  AUSelector auSelector ( // @[CoreTop.scala 59:26]
    .io_au_type(auSelector_io_au_type),
    .io_alu_result(auSelector_io_alu_result),
    .io_cmp_result(auSelector_io_cmp_result),
    .io_au_out(auSelector_io_au_out)
  );
  assign io_external_led_led = memory_io_external_led_led; // @[CoreTop.scala 126:22]
  assign io_external_seg7_seg7 = memory_io_external_seg7_seg7; // @[CoreTop.scala 126:22]
  assign memory_clock = clock;
  assign memory_reset = reset;
  assign memory_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 119:23]
  assign memory_io_ins_addr = pc_io_addr; // @[CoreTop.scala 67:22]
  assign memory_io_write_data = memInSelector_io_write_data; // @[CoreTop.scala 123:24]
  assign memory_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 125:22]
  assign memory_io_data_width = memInSelector_io_data_width; // @[CoreTop.scala 122:24]
  assign memory_io_data_addr = memInSelector_io_data_addr; // @[CoreTop.scala 120:23]
  assign memory_io_data_write = memInSelector_io_data_write; // @[CoreTop.scala 121:24]
  assign memory_io_external_btn_button = io_external_btn_button; // @[CoreTop.scala 126:22]
  assign memory_io_external_switches_switches = io_external_switches_switches; // @[CoreTop.scala 126:22]
  assign uartLoader_clock = clock;
  assign uartLoader_reset = reset;
  assign uartLoader_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 42:27]
  assign uartLoader_io_rxValid = io_external_uart_rxValid; // @[CoreTop.scala 43:25]
  assign uartLoader_io_rxData = io_external_uart_rxData; // @[CoreTop.scala 44:24]
  assign memInSelector_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 48:30]
  assign memInSelector_io_uart_in_mem_write = uartLoader_io_mem_mem_write; // @[CoreTop.scala 49:28]
  assign memInSelector_io_uart_in_data_to_write = uartLoader_io_mem_data_to_write; // @[CoreTop.scala 49:28]
  assign memInSelector_io_uart_in_data_addr = uartLoader_io_mem_data_addr; // @[CoreTop.scala 49:28]
  assign memInSelector_io_cpu_write_data = CU_io_memory_write; // @[CoreTop.scala 52:35]
  assign memInSelector_io_cpu_data_width = CU_io_data_width; // @[CoreTop.scala 53:35]
  assign memInSelector_io_cpu_data_addr = ALU_io_result; // @[CoreTop.scala 54:34]
  assign memInSelector_io_cpu_data_write = regs_io_rs2_val; // @[CoreTop.scala 55:35]
  assign state_clock = clock;
  assign state_reset = reset;
  assign state_io_load_mode = io_external_signal_load_data_mode; // @[CoreTop.scala 62:22]
  assign pc_clock = clock;
  assign pc_reset = reset;
  assign pc_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 65:19]
  assign pc_io_next_addr = nextPCGen_io_nextPC; // @[CoreTop.scala 66:19]
  assign insDecode_io_instruction = memory_io_ins_out; // @[CoreTop.scala 70:28]
  assign CU_io_opcode = insDecode_io_opcode; // @[CoreTop.scala 71:16]
  assign CU_io_func3 = insDecode_io_func3; // @[CoreTop.scala 72:15]
  assign CU_io_func7 = insDecode_io_func7; // @[CoreTop.scala 73:15]
  assign CU_io_rs1 = insDecode_io_rs1; // @[CoreTop.scala 74:13]
  assign CU_io_rs2 = insDecode_io_rs2; // @[CoreTop.scala 75:13]
  assign CU_io_rd = insDecode_io_rd; // @[CoreTop.scala 76:12]
  assign CU_io_raw_imm = insDecode_io_raw_imm; // @[CoreTop.scala 77:17]
  assign regs_clock = clock;
  assign regs_reset = reset;
  assign regs_io_cpu_state = state_io_cpu_state; // @[CoreTop.scala 81:21]
  assign regs_io_write = CU_io_regs_write; // @[CoreTop.scala 143:17]
  assign regs_io_rs1 = CU_io_rs1_out; // @[CoreTop.scala 82:15]
  assign regs_io_rs2 = CU_io_rs2_out; // @[CoreTop.scala 83:15]
  assign regs_io_rd = CU_io_rd_out; // @[CoreTop.scala 84:14]
  assign regs_io_write_data = writeDataSelector_io_write_data; // @[CoreTop.scala 144:22]
  assign immGen_io_raw_imm = CU_io_raw_imm_out; // @[CoreTop.scala 89:21]
  assign immGen_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 90:22]
  assign immGen_io_imm_width = CU_io_imm_width_type; // @[CoreTop.scala 91:23]
  assign operandSelector_io_rs1_val = regs_io_rs1_val; // @[CoreTop.scala 94:30]
  assign operandSelector_io_rs2_val = regs_io_rs2_val; // @[CoreTop.scala 95:30]
  assign operandSelector_io_real_imm = immGen_io_real_imm; // @[CoreTop.scala 96:31]
  assign operandSelector_io_operand2Type = CU_io_operand2_type; // @[CoreTop.scala 97:35]
  assign ALU_io_operand1 = operandSelector_io_operand1; // @[CoreTop.scala 100:19]
  assign ALU_io_operand2 = operandSelector_io_operand2; // @[CoreTop.scala 101:19]
  assign ALU_io_alu_op = CU_io_alu_type; // @[CoreTop.scala 102:17]
  assign ALU_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 103:19]
  assign CMP_io_operand1 = operandSelector_io_operand1; // @[CoreTop.scala 106:19]
  assign CMP_io_operand2 = operandSelector_io_operand2; // @[CoreTop.scala 107:19]
  assign CMP_io_cmp_op = CU_io_cmp_type; // @[CoreTop.scala 108:17]
  assign CMP_io_unsigned = CU_io_unsigned; // @[CoreTop.scala 109:19]
  assign nextPCGen_io_nextPC_type = CU_io_nextPC_type; // @[CoreTop.scala 116:28]
  assign nextPCGen_io_cmp_result = CMP_io_result; // @[CoreTop.scala 112:27]
  assign nextPCGen_io_alu_result = ALU_io_result; // @[CoreTop.scala 113:27]
  assign nextPCGen_io_imm = immGen_io_real_imm; // @[CoreTop.scala 114:20]
  assign nextPCGen_io_pc = pc_io_addr; // @[CoreTop.scala 115:19]
  assign writeDataSelector_io_write_back_type = CU_io_write_back_type; // @[CoreTop.scala 140:40]
  assign writeDataSelector_io_imm = immGen_io_real_imm; // @[CoreTop.scala 135:28]
  assign writeDataSelector_io_mem_out = memory_io_data_out; // @[CoreTop.scala 136:32]
  assign writeDataSelector_io_au_out = auSelector_io_au_out; // @[CoreTop.scala 137:31]
  assign writeDataSelector_io_pc4 = nextPCGen_io_pc4; // @[CoreTop.scala 138:28]
  assign writeDataSelector_io_pcImm = nextPCGen_io_pcImm; // @[CoreTop.scala 139:30]
  assign auSelector_io_au_type = CU_io_au_type; // @[CoreTop.scala 132:25]
  assign auSelector_io_alu_result = ALU_io_result; // @[CoreTop.scala 130:28]
  assign auSelector_io_cmp_result = CMP_io_result; // @[CoreTop.scala 131:28]
endmodule
module Led(
  input  [31:0] io_mmio_led,
  output [23:0] io_board_led
);
  assign io_board_led = io_mmio_led[23:0]; // @[Led.scala 20:16]
endmodule
module HexToSeg7(
  input  [3:0] io_hexDigit,
  output [7:0] io_seg7
);
  wire [7:0] _io_seg7_seg7_T_1 = 4'h1 == io_hexDigit ? 8'h9f : 8'h3; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_3 = 4'h2 == io_hexDigit ? 8'h25 : _io_seg7_seg7_T_1; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_5 = 4'h3 == io_hexDigit ? 8'hd : _io_seg7_seg7_T_3; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_7 = 4'h4 == io_hexDigit ? 8'h9b : _io_seg7_seg7_T_5; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_9 = 4'h5 == io_hexDigit ? 8'h49 : _io_seg7_seg7_T_7; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_11 = 4'h6 == io_hexDigit ? 8'h41 : _io_seg7_seg7_T_9; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_13 = 4'h7 == io_hexDigit ? 8'h1f : _io_seg7_seg7_T_11; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_15 = 4'h8 == io_hexDigit ? 8'h1 : _io_seg7_seg7_T_13; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_17 = 4'h9 == io_hexDigit ? 8'h9 : _io_seg7_seg7_T_15; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_19 = 4'ha == io_hexDigit ? 8'h11 : _io_seg7_seg7_T_17; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_21 = 4'hb == io_hexDigit ? 8'hc1 : _io_seg7_seg7_T_19; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_23 = 4'hc == io_hexDigit ? 8'h61 : _io_seg7_seg7_T_21; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_25 = 4'hd == io_hexDigit ? 8'h85 : _io_seg7_seg7_T_23; // @[Mux.scala 81:58]
  wire [7:0] _io_seg7_seg7_T_27 = 4'he == io_hexDigit ? 8'h61 : _io_seg7_seg7_T_25; // @[Mux.scala 81:58]
  assign io_seg7 = 4'hf == io_hexDigit ? 8'h71 : _io_seg7_seg7_T_27; // @[Mux.scala 81:58]
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
`endif // RANDOMIZE_REG_INIT
  wire [3:0] seg7Module_io_hexDigit; // @[Seg7.scala 33:26]
  wire [7:0] seg7Module_io_seg7; // @[Seg7.scala 33:26]
  wire [3:0] digits_0 = io_mmio_seg7[3:0]; // @[Seg7.scala 25:57]
  wire [3:0] digits_1 = io_mmio_seg7[7:4]; // @[Seg7.scala 25:57]
  wire [3:0] digits_2 = io_mmio_seg7[11:8]; // @[Seg7.scala 25:57]
  wire [3:0] digits_3 = io_mmio_seg7[15:12]; // @[Seg7.scala 25:57]
  wire [3:0] digits_4 = io_mmio_seg7[19:16]; // @[Seg7.scala 25:57]
  wire [3:0] digits_5 = io_mmio_seg7[23:20]; // @[Seg7.scala 25:57]
  wire [3:0] digits_6 = io_mmio_seg7[27:24]; // @[Seg7.scala 25:57]
  wire [3:0] digits_7 = io_mmio_seg7[31:28]; // @[Seg7.scala 25:57]
  reg [2:0] counter; // @[Seg7.scala 27:24]
  wire [3:0] _GEN_1 = 3'h1 == counter ? digits_1 : digits_0; // @[Seg7.scala 34:{26,26}]
  wire [3:0] _GEN_2 = 3'h2 == counter ? digits_2 : _GEN_1; // @[Seg7.scala 34:{26,26}]
  wire [3:0] _GEN_3 = 3'h3 == counter ? digits_3 : _GEN_2; // @[Seg7.scala 34:{26,26}]
  wire [3:0] _GEN_4 = 3'h4 == counter ? digits_4 : _GEN_3; // @[Seg7.scala 34:{26,26}]
  wire [3:0] _GEN_5 = 3'h5 == counter ? digits_5 : _GEN_4; // @[Seg7.scala 34:{26,26}]
  wire [3:0] _GEN_6 = 3'h6 == counter ? digits_6 : _GEN_5; // @[Seg7.scala 34:{26,26}]
  wire [7:0] _io_board_an_T_1 = 3'h1 == counter ? 8'hfd : 8'hfe; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_3 = 3'h2 == counter ? 8'hfb : _io_board_an_T_1; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_5 = 3'h3 == counter ? 8'hf7 : _io_board_an_T_3; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_7 = 3'h4 == counter ? 8'hef : _io_board_an_T_5; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_9 = 3'h5 == counter ? 8'hdf : _io_board_an_T_7; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_11 = 3'h6 == counter ? 8'hbf : _io_board_an_T_9; // @[Mux.scala 81:58]
  wire [2:0] _counter_T_1 = counter + 3'h1; // @[Seg7.scala 48:22]
  HexToSeg7 seg7Module ( // @[Seg7.scala 33:26]
    .io_hexDigit(seg7Module_io_hexDigit),
    .io_seg7(seg7Module_io_seg7)
  );
  assign io_board_seg7 = seg7Module_io_seg7; // @[Seg7.scala 35:17]
  assign io_board_an = 3'h7 == counter ? 8'h7f : _io_board_an_T_11; // @[Mux.scala 81:58]
  assign seg7Module_io_hexDigit = 3'h7 == counter ? digits_7 : _GEN_6; // @[Seg7.scala 34:{26,26}]
  always @(posedge clock) begin
    if (reset) begin // @[Seg7.scala 27:24]
      counter <= 3'h0; // @[Seg7.scala 27:24]
    end else begin
      counter <= _counter_T_1; // @[Seg7.scala 48:11]
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
  counter = _RAND_0[2:0];
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
  output       io_mmio_rxValid
);
  wire  uart_clk; // @[UARTWrapper.scala 30:22]
  wire  uart_rst; // @[UARTWrapper.scala 30:22]
  wire [7:0] uart_s_axis_tdata; // @[UARTWrapper.scala 30:22]
  wire  uart_s_axis_tvalid; // @[UARTWrapper.scala 30:22]
  wire  uart_s_axis_tready; // @[UARTWrapper.scala 30:22]
  wire [7:0] uart_m_axis_tdata; // @[UARTWrapper.scala 30:22]
  wire  uart_m_axis_tvalid; // @[UARTWrapper.scala 30:22]
  wire  uart_m_axis_tready; // @[UARTWrapper.scala 30:22]
  wire  uart_rxd; // @[UARTWrapper.scala 30:22]
  wire  uart_txd; // @[UARTWrapper.scala 30:22]
  wire  uart_tx_busy; // @[UARTWrapper.scala 30:22]
  wire  uart_rx_busy; // @[UARTWrapper.scala 30:22]
  wire  uart_rx_overrun_error; // @[UARTWrapper.scala 30:22]
  wire  uart_rx_frame_error; // @[UARTWrapper.scala 30:22]
  wire [15:0] uart_prescale; // @[UARTWrapper.scala 30:22]
  uart uart ( // @[UARTWrapper.scala 30:22]
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
  assign io_board_tx = uart_txd; // @[UARTWrapper.scala 39:17]
  assign io_mmio_rxData = uart_m_axis_tdata; // @[UARTWrapper.scala 41:20]
  assign io_mmio_rxValid = uart_m_axis_tvalid; // @[UARTWrapper.scala 42:21]
  assign uart_clk = clock; // @[UARTWrapper.scala 31:26]
  assign uart_rst = reset; // @[UARTWrapper.scala 32:26]
  assign uart_s_axis_tdata = 8'h0; // @[UARTWrapper.scala 34:26]
  assign uart_s_axis_tvalid = 1'h0; // @[UARTWrapper.scala 35:27]
  assign uart_m_axis_tready = 1'h0; // @[UARTWrapper.scala 43:27]
  assign uart_rxd = io_board_rx; // @[UARTWrapper.scala 38:17]
  assign uart_prescale = 16'h516; // @[UARTWrapper.scala 45:22]
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
  output [23:0] io_board_led_led,
  output [7:0]  io_board_seg7_seg7,
  output [7:0]  io_board_seg7_an,
  input  [4:0]  io_board_btn_button,
  input  [23:0] io_board_switch_switches,
  input         io_board_uart_rx,
  output        io_board_uart_tx,
  output        io_external_signal_load_data_mode
);
  wire [31:0] led_io_mmio_led; // @[DeviceTop.scala 42:19]
  wire [23:0] led_io_board_led; // @[DeviceTop.scala 42:19]
  wire  seg7_clock; // @[DeviceTop.scala 43:20]
  wire  seg7_reset; // @[DeviceTop.scala 43:20]
  wire [31:0] seg7_io_mmio_seg7; // @[DeviceTop.scala 43:20]
  wire [7:0] seg7_io_board_seg7; // @[DeviceTop.scala 43:20]
  wire [7:0] seg7_io_board_an; // @[DeviceTop.scala 43:20]
  wire [31:0] btn_io_mmio_button; // @[DeviceTop.scala 44:19]
  wire [4:0] btn_io_board_button; // @[DeviceTop.scala 44:19]
  wire [31:0] switches_io_mmio_switches; // @[DeviceTop.scala 45:24]
  wire [23:0] switches_io_board_switches; // @[DeviceTop.scala 45:24]
  wire  uart_clock; // @[DeviceTop.scala 46:20]
  wire  uart_reset; // @[DeviceTop.scala 46:20]
  wire  uart_io_board_rx; // @[DeviceTop.scala 46:20]
  wire  uart_io_board_tx; // @[DeviceTop.scala 46:20]
  wire [7:0] uart_io_mmio_rxData; // @[DeviceTop.scala 46:20]
  wire  uart_io_mmio_rxValid; // @[DeviceTop.scala 46:20]
  Led led ( // @[DeviceTop.scala 42:19]
    .io_mmio_led(led_io_mmio_led),
    .io_board_led(led_io_board_led)
  );
  Seg7 seg7 ( // @[DeviceTop.scala 43:20]
    .clock(seg7_clock),
    .reset(seg7_reset),
    .io_mmio_seg7(seg7_io_mmio_seg7),
    .io_board_seg7(seg7_io_board_seg7),
    .io_board_an(seg7_io_board_an)
  );
  Button btn ( // @[DeviceTop.scala 44:19]
    .io_mmio_button(btn_io_mmio_button),
    .io_board_button(btn_io_board_button)
  );
  Switches switches ( // @[DeviceTop.scala 45:24]
    .io_mmio_switches(switches_io_mmio_switches),
    .io_board_switches(switches_io_board_switches)
  );
  UARTWrapper uart ( // @[DeviceTop.scala 46:20]
    .clock(uart_clock),
    .reset(uart_reset),
    .io_board_rx(uart_io_board_rx),
    .io_board_tx(uart_io_board_tx),
    .io_mmio_rxData(uart_io_mmio_rxData),
    .io_mmio_rxValid(uart_io_mmio_rxValid)
  );
  assign io_mmio_btn_button = btn_io_mmio_button; // @[DeviceTop.scala 50:15]
  assign io_mmio_switches_switches = switches_io_mmio_switches; // @[DeviceTop.scala 51:20]
  assign io_mmio_uart_rxData = uart_io_mmio_rxData; // @[DeviceTop.scala 52:16]
  assign io_mmio_uart_rxValid = uart_io_mmio_rxValid; // @[DeviceTop.scala 52:16]
  assign io_board_led_led = led_io_board_led; // @[DeviceTop.scala 55:16]
  assign io_board_seg7_seg7 = seg7_io_board_seg7; // @[DeviceTop.scala 56:17]
  assign io_board_seg7_an = seg7_io_board_an; // @[DeviceTop.scala 56:17]
  assign io_board_uart_tx = uart_io_board_tx; // @[DeviceTop.scala 59:16]
  assign io_external_signal_load_data_mode = io_board_switch_switches[23]; // @[DeviceTop.scala 62:62]
  assign led_io_mmio_led = io_mmio_led_led; // @[DeviceTop.scala 48:15]
  assign seg7_clock = clock;
  assign seg7_reset = reset;
  assign seg7_io_mmio_seg7 = io_mmio_seg7_seg7; // @[DeviceTop.scala 49:16]
  assign btn_io_board_button = io_board_btn_button; // @[DeviceTop.scala 57:16]
  assign switches_io_board_switches = io_board_switch_switches; // @[DeviceTop.scala 58:21]
  assign uart_clock = clock;
  assign uart_reset = reset;
  assign uart_io_board_rx = io_board_uart_rx; // @[DeviceTop.scala 59:16]
endmodule
module Top(
  input         clock,
  (* MARK_DEBUG="true" *) input         reset,
  output [23:0] io_led_led,
  output [7:0]  io_seg7_seg7,
  output [7:0]  io_seg7_an,
  input  [4:0]  io_btn_button,
  input  [23:0] io_switch_switches,
  input         io_uart_rx,
  output        io_uart_tx
);
  wire  clockSeparator_clock; // @[Top.scala 15:30]
  wire  clockSeparator_reset; // @[Top.scala 15:30]
  wire  clockSeparator_io_cpuClock; // @[Top.scala 15:30]
  wire  cpu_clock; // @[Top.scala 17:11]
  wire  cpu_reset; // @[Top.scala 17:11]
  wire [31:0] cpu_io_external_led_led; // @[Top.scala 17:11]
  wire [31:0] cpu_io_external_seg7_seg7; // @[Top.scala 17:11]
  wire [31:0] cpu_io_external_btn_button; // @[Top.scala 17:11]
  wire [31:0] cpu_io_external_switches_switches; // @[Top.scala 17:11]
  wire [7:0] cpu_io_external_uart_rxData; // @[Top.scala 17:11]
  wire  cpu_io_external_uart_rxValid; // @[Top.scala 17:11]
  wire  cpu_io_external_signal_load_data_mode; // @[Top.scala 17:11]
  wire  device_clock; // @[Top.scala 19:22]
  wire  device_reset; // @[Top.scala 19:22]
  wire [31:0] device_io_mmio_led_led; // @[Top.scala 19:22]
  wire [31:0] device_io_mmio_seg7_seg7; // @[Top.scala 19:22]
  wire [31:0] device_io_mmio_btn_button; // @[Top.scala 19:22]
  wire [31:0] device_io_mmio_switches_switches; // @[Top.scala 19:22]
  wire [7:0] device_io_mmio_uart_rxData; // @[Top.scala 19:22]
  wire  device_io_mmio_uart_rxValid; // @[Top.scala 19:22]
  wire [23:0] device_io_board_led_led; // @[Top.scala 19:22]
  wire [7:0] device_io_board_seg7_seg7; // @[Top.scala 19:22]
  wire [7:0] device_io_board_seg7_an; // @[Top.scala 19:22]
  wire [4:0] device_io_board_btn_button; // @[Top.scala 19:22]
  wire [23:0] device_io_board_switch_switches; // @[Top.scala 19:22]
  wire  device_io_board_uart_rx; // @[Top.scala 19:22]
  wire  device_io_board_uart_tx; // @[Top.scala 19:22]
  wire  device_io_external_signal_load_data_mode; // @[Top.scala 19:22]
  ClockSeparator clockSeparator ( // @[Top.scala 15:30]
    .clock(clockSeparator_clock),
    .reset(clockSeparator_reset),
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
    .io_board_led_led(device_io_board_led_led),
    .io_board_seg7_seg7(device_io_board_seg7_seg7),
    .io_board_seg7_an(device_io_board_seg7_an),
    .io_board_btn_button(device_io_board_btn_button),
    .io_board_switch_switches(device_io_board_switch_switches),
    .io_board_uart_rx(device_io_board_uart_rx),
    .io_board_uart_tx(device_io_board_uart_tx),
    .io_external_signal_load_data_mode(device_io_external_signal_load_data_mode)
  );
  assign io_led_led = device_io_board_led_led; // @[Top.scala 22:18]
  assign io_seg7_seg7 = device_io_board_seg7_seg7; // @[Top.scala 22:18]
  assign io_seg7_an = device_io_board_seg7_an; // @[Top.scala 22:18]
  assign io_uart_tx = device_io_board_uart_tx; // @[Top.scala 22:18]
  assign clockSeparator_clock = clock;
  assign clockSeparator_reset = reset;
  assign cpu_clock = clockSeparator_io_cpuClock;
  assign cpu_reset = reset;
  assign cpu_io_external_btn_button = device_io_mmio_btn_button; // @[Top.scala 25:18]
  assign cpu_io_external_switches_switches = device_io_mmio_switches_switches; // @[Top.scala 25:18]
  assign cpu_io_external_uart_rxData = device_io_mmio_uart_rxData; // @[Top.scala 25:18]
  assign cpu_io_external_uart_rxValid = device_io_mmio_uart_rxValid; // @[Top.scala 25:18]
  assign cpu_io_external_signal_load_data_mode = device_io_external_signal_load_data_mode; // @[Top.scala 26:25]
  assign device_clock = clock;
  assign device_reset = reset;
  assign device_io_mmio_led_led = cpu_io_external_led_led; // @[Top.scala 25:18]
  assign device_io_mmio_seg7_seg7 = cpu_io_external_seg7_seg7; // @[Top.scala 25:18]
  assign device_io_board_btn_button = io_btn_button; // @[Top.scala 22:18]
  assign device_io_board_switch_switches = io_switch_switches; // @[Top.scala 22:18]
  assign device_io_board_uart_rx = io_uart_rx; // @[Top.scala 22:18]
endmodule
