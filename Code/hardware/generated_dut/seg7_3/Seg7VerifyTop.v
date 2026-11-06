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
  reg [31:0] _RAND_1;
`endif // RANDOMIZE_REG_INIT
  wire [3:0] seg7Module_io_hexDigit; // @[Seg7.scala 45:26]
  wire [7:0] seg7Module_io_seg7; // @[Seg7.scala 45:26]
  reg [31:0] clock_counter; // @[Seg7.scala 25:30]
  wire [31:0] _clock_counter_T_1 = clock_counter - 32'h1; // @[Seg7.scala 27:34]
  reg [2:0] counter; // @[Seg7.scala 29:24]
  wire [2:0] _counter_T_1 = counter + 3'h1; // @[Seg7.scala 33:24]
  wire [3:0] digits_0 = io_mmio_seg7[3:0]; // @[Seg7.scala 39:57]
  wire [3:0] digits_1 = io_mmio_seg7[7:4]; // @[Seg7.scala 39:57]
  wire [3:0] digits_2 = io_mmio_seg7[11:8]; // @[Seg7.scala 39:57]
  wire [3:0] digits_3 = io_mmio_seg7[15:12]; // @[Seg7.scala 39:57]
  wire [3:0] digits_4 = io_mmio_seg7[19:16]; // @[Seg7.scala 39:57]
  wire [3:0] digits_5 = io_mmio_seg7[23:20]; // @[Seg7.scala 39:57]
  wire [3:0] digits_6 = io_mmio_seg7[27:24]; // @[Seg7.scala 39:57]
  wire [3:0] digits_7 = io_mmio_seg7[31:28]; // @[Seg7.scala 39:57]
  wire [3:0] _GEN_3 = 3'h1 == counter ? digits_1 : digits_0; // @[Seg7.scala 46:{26,26}]
  wire [3:0] _GEN_4 = 3'h2 == counter ? digits_2 : _GEN_3; // @[Seg7.scala 46:{26,26}]
  wire [3:0] _GEN_5 = 3'h3 == counter ? digits_3 : _GEN_4; // @[Seg7.scala 46:{26,26}]
  wire [3:0] _GEN_6 = 3'h4 == counter ? digits_4 : _GEN_5; // @[Seg7.scala 46:{26,26}]
  wire [3:0] _GEN_7 = 3'h5 == counter ? digits_5 : _GEN_6; // @[Seg7.scala 46:{26,26}]
  wire [3:0] _GEN_8 = 3'h6 == counter ? digits_6 : _GEN_7; // @[Seg7.scala 46:{26,26}]
  wire [7:0] _io_board_an_T_1 = 3'h1 == counter ? 8'hfd : 8'hfe; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_3 = 3'h2 == counter ? 8'hfb : _io_board_an_T_1; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_5 = 3'h3 == counter ? 8'hf7 : _io_board_an_T_3; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_7 = 3'h4 == counter ? 8'hef : _io_board_an_T_5; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_9 = 3'h5 == counter ? 8'hdf : _io_board_an_T_7; // @[Mux.scala 81:58]
  wire [7:0] _io_board_an_T_11 = 3'h6 == counter ? 8'hbf : _io_board_an_T_9; // @[Mux.scala 81:58]
  HexToSeg7 seg7Module ( // @[Seg7.scala 45:26]
    .io_hexDigit(seg7Module_io_hexDigit),
    .io_seg7(seg7Module_io_seg7)
  );
  assign io_board_seg7 = seg7Module_io_seg7; // @[Seg7.scala 47:17]
  assign io_board_an = 3'h7 == counter ? 8'h7f : _io_board_an_T_11; // @[Mux.scala 81:58]
  assign seg7Module_io_hexDigit = 3'h7 == counter ? digits_7 : _GEN_8; // @[Seg7.scala 46:{26,26}]
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
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module Seg7VerifyTop(
  input         clock,
  input         reset,
  input  [15:0] io_seg7_in,
  output [7:0]  io_seg7_out_seg7,
  output [7:0]  io_seg7_out_an
);
  wire  seg7_clock; // @[Seg7VerifyTop.scala 12:20]
  wire  seg7_reset; // @[Seg7VerifyTop.scala 12:20]
  wire [31:0] seg7_io_mmio_seg7; // @[Seg7VerifyTop.scala 12:20]
  wire [7:0] seg7_io_board_seg7; // @[Seg7VerifyTop.scala 12:20]
  wire [7:0] seg7_io_board_an; // @[Seg7VerifyTop.scala 12:20]
  Seg7 seg7 ( // @[Seg7VerifyTop.scala 12:20]
    .clock(seg7_clock),
    .reset(seg7_reset),
    .io_mmio_seg7(seg7_io_mmio_seg7),
    .io_board_seg7(seg7_io_board_seg7),
    .io_board_an(seg7_io_board_an)
  );
  assign io_seg7_out_seg7 = seg7_io_board_seg7; // @[Seg7VerifyTop.scala 14:15]
  assign io_seg7_out_an = seg7_io_board_an; // @[Seg7VerifyTop.scala 14:15]
  assign seg7_clock = clock;
  assign seg7_reset = reset;
  assign seg7_io_mmio_seg7 = {16'h0,io_seg7_in}; // @[Cat.scala 33:92]
endmodule
