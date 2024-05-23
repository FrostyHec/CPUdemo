module UART(
  input        clock,
  input        reset,
  input        io_board_rx,
  output       io_board_tx,
  input        rxReady,
  output [7:0] io_mmio_rxData,
  output       io_mmio_rxValid
);
  wire  uart_clk; // @[UART.scala 30:22]
  wire  uart_rst; // @[UART.scala 30:22]
  wire [7:0] uart_s_axis_tdata; // @[UART.scala 30:22]
  wire  uart_s_axis_tvalid; // @[UART.scala 30:22]
  wire  uart_s_axis_tready; // @[UART.scala 30:22]
  wire [7:0] uart_m_axis_tdata; // @[UART.scala 30:22]
  wire  uart_m_axis_tvalid; // @[UART.scala 30:22]
  wire  uart_m_axis_tready; // @[UART.scala 30:22]
  wire  uart_rxd; // @[UART.scala 30:22]
  wire  uart_txd; // @[UART.scala 30:22]
  wire  uart_tx_busy; // @[UART.scala 30:22]
  wire  uart_rx_busy; // @[UART.scala 30:22]
  wire  uart_rx_overrun_error; // @[UART.scala 30:22]
  wire  uart_rx_frame_error; // @[UART.scala 30:22]
  wire [15:0] uart_prescale; // @[UART.scala 30:22]
  uart uart ( // @[UART.scala 30:22]
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
  assign io_board_tx = uart_txd; // @[UART.scala 39:17]
  assign io_mmio_rxData = uart_m_axis_tdata; // @[UART.scala 41:20]
  assign io_mmio_rxValid = uart_m_axis_tvalid; // @[UART.scala 42:21]
  assign uart_clk = clock; // @[UART.scala 31:26]
  assign uart_rst = reset; // @[UART.scala 32:26]
  assign uart_s_axis_tdata = 8'h0; // @[UART.scala 34:26]
  assign uart_s_axis_tvalid = 1'h0; // @[UART.scala 35:27]
  assign uart_m_axis_tready = rxReady; // @[UART.scala 43:27]
  assign uart_rxd = io_board_rx; // @[UART.scala 38:17]
  assign uart_prescale = 16'h516; // @[UART.scala 45:22]
endmodule
module UARTVerify(
  input        clock,
  input        reset,
  input        io_board_rx,
  output       io_board_tx,
  output [7:0] io_led,
  output       io_signal
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
`endif // RANDOMIZE_REG_INIT
  wire  uart_clock; // @[UARTVerify.scala 14:18]
  wire  uart_reset; // @[UARTVerify.scala 14:18]
  wire  uart_io_board_rx; // @[UARTVerify.scala 14:18]
  wire  uart_io_board_tx; // @[UARTVerify.scala 14:18]
  wire [7:0] uart_io_mmio_rxData; // @[UARTVerify.scala 14:18]
  wire  uart_io_mmio_rxValid; // @[UARTVerify.scala 14:18]
  reg [7:0] out_led; // @[UARTVerify.scala 19:22]
  UART uart ( // @[UARTVerify.scala 14:18]
    .clock(uart_clock),
    .reset(uart_reset),
    .io_board_rx(uart_io_board_rx),
    .io_board_tx(uart_io_board_tx),
    .io_mmio_rxData(uart_io_mmio_rxData),
    .io_mmio_rxValid(uart_io_mmio_rxValid)
  );
  assign io_board_tx = uart_io_board_tx; // @[UARTVerify.scala 15:11]
  assign io_led = out_led; // @[UARTVerify.scala 21:9]
  assign io_signal = uart_io_mmio_rxValid; // @[UARTVerify.scala 22:12]
  assign uart_clock = clock;
  assign uart_reset = reset;
  assign uart_io_board_rx = io_board_rx; // @[UARTVerify.scala 15:11]
  always @(posedge clock) begin
    if (reset) begin // @[UARTVerify.scala 19:22]
      out_led <= 8'h0; // @[UARTVerify.scala 19:22]
    end else if (uart_io_mmio_rxValid) begin // @[UARTVerify.scala 23:29]
      out_led <= uart_io_mmio_rxData; // @[UARTVerify.scala 24:12]
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
  out_led = _RAND_0[7:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
