module RAM(
  input         clock,
  input         io_write,
  input  [31:0] io_read_addr,
  input  [31:0] io_write_addr,
  input  [31:0] io_write_data,
  output [31:0] io_read_data
);
`ifdef RANDOMIZE_MEM_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_MEM_INIT
  reg [31:0] mem [0:65535]; // @[MemoryPacket.scala 32:18]
  wire  mem_io_read_data_MPORT_en; // @[MemoryPacket.scala 32:18]
  wire [15:0] mem_io_read_data_MPORT_addr; // @[MemoryPacket.scala 32:18]
  wire [31:0] mem_io_read_data_MPORT_data; // @[MemoryPacket.scala 32:18]
  wire [31:0] mem_MPORT_data; // @[MemoryPacket.scala 32:18]
  wire [15:0] mem_MPORT_addr; // @[MemoryPacket.scala 32:18]
  wire  mem_MPORT_mask; // @[MemoryPacket.scala 32:18]
  wire  mem_MPORT_en; // @[MemoryPacket.scala 32:18]
  assign mem_io_read_data_MPORT_en = 1'h1;
  assign mem_io_read_data_MPORT_addr = io_read_addr[15:0];
  assign mem_io_read_data_MPORT_data = mem[mem_io_read_data_MPORT_addr]; // @[MemoryPacket.scala 32:18]
  assign mem_MPORT_data = io_write_data;
  assign mem_MPORT_addr = io_write_addr[15:0];
  assign mem_MPORT_mask = 1'h1;
  assign mem_MPORT_en = io_write;
  assign io_read_data = mem_io_read_data_MPORT_data; // @[MemoryPacket.scala 36:18]
  always @(posedge clock) begin
    if (mem_MPORT_en & mem_MPORT_mask) begin
      mem[mem_MPORT_addr] <= mem_MPORT_data; // @[MemoryPacket.scala 32:18]
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
`ifdef RANDOMIZE_MEM_INIT
  _RAND_0 = {1{`RANDOM}};
  for (initvar = 0; initvar < 65536; initvar = initvar+1)
    mem[initvar] = _RAND_0[31:0];
`endif // RANDOMIZE_MEM_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module InsRAM(
  input         clock,
  input         io_write,
  input  [31:0] io_read_addr,
  input  [31:0] io_write_addr,
  input  [31:0] io_write_data,
  output [31:0] io_read_data
);
  wire  insRAM_clock; // @[InsRAM.scala 12:30]
  wire  insRAM_io_write; // @[InsRAM.scala 12:30]
  wire [31:0] insRAM_io_read_addr; // @[InsRAM.scala 12:30]
  wire [31:0] insRAM_io_write_addr; // @[InsRAM.scala 12:30]
  wire [31:0] insRAM_io_write_data; // @[InsRAM.scala 12:30]
  wire [31:0] insRAM_io_read_data; // @[InsRAM.scala 12:30]
  RAM insRAM ( // @[InsRAM.scala 12:30]
    .clock(insRAM_clock),
    .io_write(insRAM_io_write),
    .io_read_addr(insRAM_io_read_addr),
    .io_write_addr(insRAM_io_write_addr),
    .io_write_data(insRAM_io_write_data),
    .io_read_data(insRAM_io_read_data)
  );
  assign io_read_data = insRAM_io_read_data; // @[InsRAM.scala 17:6]
  assign insRAM_clock = clock;
  assign insRAM_io_write = io_write; // @[InsRAM.scala 17:6]
  assign insRAM_io_read_addr = io_read_addr; // @[InsRAM.scala 17:6]
  assign insRAM_io_write_addr = io_write_addr; // @[InsRAM.scala 17:6]
  assign insRAM_io_write_data = io_write_data; // @[InsRAM.scala 17:6]
endmodule
module MemoryDispatch(
  input         clock,
  input         reset,
  input         io_cpu_state,
  input  [31:0] io_ins_addr,
  output [31:0] io_ins_out,
  input         io_read_data,
  input         io_write_data,
  input         io_unsigned,
  input  [1:0]  io_data_width,
  input  [31:0] io_data_addr,
  input  [31:0] io_data_write,
  output [31:0] io_data_out
);
  wire  insRAM_clock; // @[MemoryDispatch.scala 34:22]
  wire  insRAM_io_write; // @[MemoryDispatch.scala 34:22]
  wire [31:0] insRAM_io_read_addr; // @[MemoryDispatch.scala 34:22]
  wire [31:0] insRAM_io_write_addr; // @[MemoryDispatch.scala 34:22]
  wire [31:0] insRAM_io_write_data; // @[MemoryDispatch.scala 34:22]
  wire [31:0] insRAM_io_read_data; // @[MemoryDispatch.scala 34:22]
  wire  dataRAM_clock; // @[MemoryDispatch.scala 35:23]
  wire  dataRAM_io_write; // @[MemoryDispatch.scala 35:23]
  wire [31:0] dataRAM_io_read_addr; // @[MemoryDispatch.scala 35:23]
  wire [31:0] dataRAM_io_write_addr; // @[MemoryDispatch.scala 35:23]
  wire [31:0] dataRAM_io_write_data; // @[MemoryDispatch.scala 35:23]
  wire [31:0] dataRAM_io_read_data; // @[MemoryDispatch.scala 35:23]
  wire [29:0] rw_mem_addr = io_data_addr[31:2]; // @[MemoryDispatch.scala 30:34]
  wire [29:0] read_ins_addr = io_ins_addr[31:2]; // @[MemoryDispatch.scala 31:35]
  wire  _T = 2'h0 == io_data_width; // @[MemoryDispatch.scala 48:25]
  wire  _T_1 = 2'h1 == io_data_width; // @[MemoryDispatch.scala 48:25]
  wire [31:0] _GEN_1 = 2'h1 == io_data_width ? {{16'd0}, io_data_write[15:0]} : io_data_write; // @[MemoryDispatch.scala 48:25 53:15]
  wire  _T_4 = io_data_addr <= 32'hffff; // @[MemoryDispatch.scala 88:21]
  wire  _T_7 = ~reset; // @[MemoryDispatch.scala 92:13]
  wire  _GEN_3 = io_write_data & (io_cpu_state & io_write_data); // @[MemoryDispatch.scala 89:25 90:23 MemoryPacket.scala 16:16]
  wire  _T_8 = 32'h10000 <= io_data_addr; // @[MemoryDispatch.scala 96:43]
  wire  _T_10 = _T_8 & io_data_addr <= 32'h1ffff; // @[MemoryDispatch.scala 97:5]
  wire [31:0] _havard_mem_T_1 = io_data_addr - 32'h10000; // @[MemoryDispatch.scala 99:36]
  wire [29:0] havard_mem = _havard_mem_T_1[31:2]; // @[MemoryDispatch.scala 99:68]
  wire [29:0] _GEN_4 = io_read_data ? havard_mem : rw_mem_addr; // @[MemoryDispatch.scala 103:30 104:28 MemoryPacket.scala 17:20]
  wire [31:0] _GEN_5 = dataRAM_io_read_data; // @[MemoryDispatch.scala 103:30 105:16]
  wire [29:0] _GEN_6 = io_write_data ? havard_mem : rw_mem_addr; // @[MemoryDispatch.scala 100:25 101:29 MemoryPacket.scala 18:21]
  wire [29:0] _GEN_8 = io_write_data ? rw_mem_addr : _GEN_4; // @[MemoryDispatch.scala 100:25 MemoryPacket.scala 17:20]
  wire  _T_11 = 32'hffffff00 <= io_data_addr; // @[MemoryDispatch.scala 109:51]
  wire [29:0] _GEN_15 = _T_8 & io_data_addr <= 32'h1ffff ? _GEN_6 : rw_mem_addr; // @[MemoryDispatch.scala 97:52 MemoryPacket.scala 18:21]
  wire  _GEN_16 = _T_8 & io_data_addr <= 32'h1ffff & _GEN_3; // @[MemoryDispatch.scala 97:52 MemoryPacket.scala 16:16]
  wire [29:0] _GEN_17 = _T_8 & io_data_addr <= 32'h1ffff ? _GEN_8 : rw_mem_addr; // @[MemoryDispatch.scala 97:52 MemoryPacket.scala 17:20]
  wire [31:0] data_out = _T_8 & io_data_addr <= 32'h1ffff ? _GEN_5 : 32'h0; // @[MemoryDispatch.scala 97:52]
  wire [29:0] _GEN_21 = io_data_addr <= 32'hffff ? rw_mem_addr : _GEN_15; // @[MemoryDispatch.scala 88:51 MemoryPacket.scala 18:21]
  wire [29:0] _GEN_23 = io_data_addr <= 32'hffff ? rw_mem_addr : _GEN_17; // @[MemoryDispatch.scala 88:51 MemoryPacket.scala 17:20]
  wire  _high_bit_T_1 = io_unsigned ? 1'h0 : data_out[7]; // @[MemoryDispatch.scala 126:32]
  wire [23:0] high_bit = _high_bit_T_1 ? 24'hffffff : 24'h0; // @[Bitwise.scala 77:12]
  wire  _T_18 = 2'h0 == read_ins_addr[1:0]; // @[MemoryDispatch.scala 127:35]
  wire [31:0] _io_data_out_T_1 = {high_bit,data_out[7:0]}; // @[Cat.scala 33:92]
  wire [31:0] _io_data_out_T_3 = {high_bit,data_out[15:8]}; // @[Cat.scala 33:92]
  wire [31:0] _io_data_out_T_5 = {high_bit,data_out[23:16]}; // @[Cat.scala 33:92]
  wire [31:0] _io_data_out_T_7 = {high_bit,data_out[31:24]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_27 = 2'h2 == read_ins_addr[1:0] ? _io_data_out_T_5 : _io_data_out_T_7; // @[MemoryDispatch.scala 127:35 135:23]
  wire [31:0] _GEN_28 = 2'h1 == read_ins_addr[1:0] ? _io_data_out_T_3 : _GEN_27; // @[MemoryDispatch.scala 127:35 132:23]
  wire [31:0] _GEN_29 = 2'h0 == read_ins_addr[1:0] ? _io_data_out_T_1 : _GEN_28; // @[MemoryDispatch.scala 127:35 129:23]
  wire  _high_bit_T_4 = io_unsigned ? 1'h0 : data_out[15]; // @[MemoryDispatch.scala 144:32]
  wire [15:0] high_bit_1 = _high_bit_T_4 ? 16'hffff : 16'h0; // @[Bitwise.scala 77:12]
  wire [31:0] _io_data_out_T_9 = {high_bit_1,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [31:0] _io_data_out_T_11 = {high_bit_1,data_out[31:16]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_31 = _T_18 ? _io_data_out_T_9 : _io_data_out_T_11; // @[MemoryDispatch.scala 145:35 147:23]
  wire [31:0] _GEN_33 = _T_1 ? _GEN_31 : data_out; // @[MemoryDispatch.scala 124:25]
  InsRAM insRAM ( // @[MemoryDispatch.scala 34:22]
    .clock(insRAM_clock),
    .io_write(insRAM_io_write),
    .io_read_addr(insRAM_io_read_addr),
    .io_write_addr(insRAM_io_write_addr),
    .io_write_data(insRAM_io_write_data),
    .io_read_data(insRAM_io_read_data)
  );
  InsRAM dataRAM ( // @[MemoryDispatch.scala 35:23]
    .clock(dataRAM_clock),
    .io_write(dataRAM_io_write),
    .io_read_addr(dataRAM_io_read_addr),
    .io_write_addr(dataRAM_io_write_addr),
    .io_write_data(dataRAM_io_write_data),
    .io_read_data(dataRAM_io_read_data)
  );
  assign io_ins_out = insRAM_io_read_data; // @[MemoryPacket.scala 21:16]
  assign io_data_out = _T ? _GEN_29 : _GEN_33; // @[MemoryDispatch.scala 124:25]
  assign insRAM_clock = clock;
  assign insRAM_io_write = io_data_addr <= 32'hffff & _GEN_3; // @[MemoryDispatch.scala 88:51 MemoryPacket.scala 16:16]
  assign insRAM_io_read_addr = {{2'd0}, read_ins_addr}; // @[MemoryPacket.scala 17:20]
  assign insRAM_io_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 18:21]
  assign insRAM_io_write_data = 2'h0 == io_data_width ? {{24'd0}, io_data_write[7:0]} : _GEN_1; // @[MemoryDispatch.scala 48:25 50:15]
  assign dataRAM_clock = clock;
  assign dataRAM_io_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_16; // @[MemoryDispatch.scala 88:51 MemoryPacket.scala 16:16]
  assign dataRAM_io_read_addr = {{2'd0}, _GEN_23};
  assign dataRAM_io_write_addr = {{2'd0}, _GEN_21};
  assign dataRAM_io_write_data = 2'h0 == io_data_width ? {{24'd0}, io_data_write[7:0]} : _GEN_1; // @[MemoryDispatch.scala 48:25 50:15]
  always @(posedge clock) begin
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_4 & ~io_write_data & io_read_data & ~reset) begin
          $fwrite(32'h80000002,"Cant read insRAM"); // @[MemoryDispatch.scala 92:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~_T_4 & ~_T_10 & ~_T_11 & _T_7) begin
          $fwrite(32'h80000002,"Unexpected address!"); // @[MemoryDispatch.scala 119:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
endmodule
