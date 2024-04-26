module HelloWorld(
  input   clock,
  input   reset,
  output  io_led0
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
`endif // RANDOMIZE_REG_INIT
  reg  led; // @[HelloWorld.scala 9:20]
  reg [5:0] counterWrap_c_value; // @[Counter.scala 61:40]
  wire  counterWrap_wrap_wrap = counterWrap_c_value == 6'h31; // @[Counter.scala 73:24]
  wire [5:0] _counterWrap_wrap_value_T_1 = counterWrap_c_value + 6'h1; // @[Counter.scala 77:24]
  assign io_led0 = led; // @[HelloWorld.scala 14:11]
  always @(posedge clock) begin
    if (reset) begin // @[HelloWorld.scala 9:20]
      led <= 1'h0; // @[HelloWorld.scala 9:20]
    end else if (counterWrap_wrap_wrap) begin // @[HelloWorld.scala 11:21]
      led <= ~led; // @[HelloWorld.scala 12:9]
    end
    if (reset) begin // @[Counter.scala 61:40]
      counterWrap_c_value <= 6'h0; // @[Counter.scala 61:40]
    end else if (counterWrap_wrap_wrap) begin // @[Counter.scala 87:20]
      counterWrap_c_value <= 6'h0; // @[Counter.scala 87:28]
    end else begin
      counterWrap_c_value <= _counterWrap_wrap_value_T_1; // @[Counter.scala 77:15]
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
  led = _RAND_0[0:0];
  _RAND_1 = {1{`RANDOM}};
  counterWrap_c_value = _RAND_1[5:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
