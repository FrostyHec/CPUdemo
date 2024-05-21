module UARTrx(
  input        clock,
  input        reset,
  input        io_rx,
  output [7:0] io_data,
  output       io_valid
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
  reg [31:0] _RAND_5;
`endif // RANDOMIZE_REG_INIT
  wire [2:0] baud_cout = 3'h5 - 3'h1; // @[UARTModule.scala 530:55]
  reg [1:0] state; // @[UARTModule.scala 533:22]
  reg [3:0] bitCount; // @[UARTModule.scala 534:25]
  reg [7:0] data_queues; // @[UARTModule.scala 535:28]
  reg [15:0] baudCounter; // @[UARTModule.scala 539:28]
  wire  baudTick = baudCounter == 16'h0; // @[UARTModule.scala 540:31]
  reg  io_valid_; // @[UARTModule.scala 542:25]
  reg [7:0] io_data_; // @[UARTModule.scala 543:24]
  wire  _T_1 = ~reset; // @[UARTModule.scala 550:13]
  wire [15:0] _baudCounter_T_1 = baudCounter - 16'h1; // @[UARTModule.scala 554:32]
  wire [15:0] _GEN_0 = baudTick ? {{13'd0}, baud_cout} : _baudCounter_T_1; // @[UARTModule.scala 548:18 552:17 554:17]
  wire  _T_2 = 2'h0 == state; // @[UARTModule.scala 557:17]
  wire [2:0] _baudCounter_T_3 = baud_cout - 3'h1; // @[UARTModule.scala 563:34]
  wire  _T_4 = 2'h1 == state; // @[UARTModule.scala 557:17]
  wire  _T_7 = 2'h2 == state; // @[UARTModule.scala 557:17]
  wire [7:0] _data_queues_T_1 = {io_rx,data_queues[7:1]}; // @[Cat.scala 33:92]
  wire [3:0] _bitCount_T_1 = bitCount + 4'h1; // @[UARTModule.scala 583:30]
  wire [1:0] _GEN_8 = bitCount == 4'h7 ? 2'h3 : state; // @[UARTModule.scala 584:32 585:17 533:22]
  wire [7:0] _GEN_9 = baudTick ? _data_queues_T_1 : data_queues; // @[UARTModule.scala 577:22 582:21 535:28]
  wire [3:0] _GEN_10 = baudTick ? _bitCount_T_1 : bitCount; // @[UARTModule.scala 577:22 583:18 534:25]
  wire [1:0] _GEN_11 = baudTick ? _GEN_8 : state; // @[UARTModule.scala 533:22 577:22]
  wire  _T_13 = 2'h3 == state; // @[UARTModule.scala 557:17]
  wire [1:0] _GEN_12 = baudTick ? 2'h0 : state; // @[UARTModule.scala 590:22 594:15 533:22]
  wire  _GEN_13 = baudTick | io_valid_; // @[UARTModule.scala 590:22 595:18 542:25]
  wire [7:0] _GEN_14 = baudTick ? data_queues : io_data_; // @[UARTModule.scala 590:22 596:17 543:24]
  wire [1:0] _GEN_15 = 2'h3 == state ? _GEN_12 : state; // @[UARTModule.scala 557:17 533:22]
  wire  _GEN_16 = 2'h3 == state ? _GEN_13 : io_valid_; // @[UARTModule.scala 557:17 542:25]
  wire [7:0] _GEN_17 = 2'h3 == state ? _GEN_14 : io_data_; // @[UARTModule.scala 557:17 543:24]
  wire  _GEN_35 = ~_T_2; // @[UARTModule.scala 569:17]
  wire  _GEN_40 = _GEN_35 & ~_T_4; // @[UARTModule.scala 579:17]
  wire  _GEN_42 = _GEN_35 & ~_T_4 & _T_7 & baudTick; // @[UARTModule.scala 579:17]
  assign io_data = io_data_; // @[UARTModule.scala 545:11]
  assign io_valid = io_valid_; // @[UARTModule.scala 546:12]
  always @(posedge clock) begin
    if (reset) begin // @[UARTModule.scala 533:22]
      state <= 2'h0; // @[UARTModule.scala 533:22]
    end else if (2'h0 == state) begin // @[UARTModule.scala 557:17]
      if (~io_rx) begin // @[UARTModule.scala 559:20]
        state <= 2'h1; // @[UARTModule.scala 562:15]
      end
    end else if (2'h1 == state) begin // @[UARTModule.scala 557:17]
      if (baudTick) begin // @[UARTModule.scala 567:22]
        state <= 2'h2; // @[UARTModule.scala 571:15]
      end
    end else if (2'h2 == state) begin // @[UARTModule.scala 557:17]
      state <= _GEN_11;
    end else begin
      state <= _GEN_15;
    end
    if (reset) begin // @[UARTModule.scala 534:25]
      bitCount <= 4'h0; // @[UARTModule.scala 534:25]
    end else if (!(2'h0 == state)) begin // @[UARTModule.scala 557:17]
      if (2'h1 == state) begin // @[UARTModule.scala 557:17]
        if (baudTick) begin // @[UARTModule.scala 567:22]
          bitCount <= 4'h0; // @[UARTModule.scala 573:18]
        end
      end else if (2'h2 == state) begin // @[UARTModule.scala 557:17]
        bitCount <= _GEN_10;
      end
    end
    if (reset) begin // @[UARTModule.scala 535:28]
      data_queues <= 8'h0; // @[UARTModule.scala 535:28]
    end else if (!(2'h0 == state)) begin // @[UARTModule.scala 557:17]
      if (!(2'h1 == state)) begin // @[UARTModule.scala 557:17]
        if (2'h2 == state) begin // @[UARTModule.scala 557:17]
          data_queues <= _GEN_9;
        end
      end
    end
    if (reset) begin // @[UARTModule.scala 539:28]
      baudCounter <= 16'h0; // @[UARTModule.scala 539:28]
    end else if (2'h0 == state) begin // @[UARTModule.scala 557:17]
      if (~io_rx) begin // @[UARTModule.scala 559:20]
        baudCounter <= {{13'd0}, _baudCounter_T_3}; // @[UARTModule.scala 563:21]
      end else begin
        baudCounter <= _GEN_0;
      end
    end else if (2'h1 == state) begin // @[UARTModule.scala 557:17]
      if (baudTick) begin // @[UARTModule.scala 567:22]
        baudCounter <= {{13'd0}, baud_cout}; // @[UARTModule.scala 572:21]
      end else begin
        baudCounter <= _GEN_0;
      end
    end else begin
      baudCounter <= _GEN_0;
    end
    if (reset) begin // @[UARTModule.scala 542:25]
      io_valid_ <= 1'h0; // @[UARTModule.scala 542:25]
    end else if (2'h0 == state) begin // @[UARTModule.scala 557:17]
      if (~io_rx) begin // @[UARTModule.scala 559:20]
        io_valid_ <= 1'h0; // @[UARTModule.scala 560:18]
      end
    end else if (!(2'h1 == state)) begin // @[UARTModule.scala 557:17]
      if (!(2'h2 == state)) begin // @[UARTModule.scala 557:17]
        io_valid_ <= _GEN_16;
      end
    end
    if (reset) begin // @[UARTModule.scala 543:24]
      io_data_ <= 8'h0; // @[UARTModule.scala 543:24]
    end else if (2'h0 == state) begin // @[UARTModule.scala 557:17]
      if (~io_rx) begin // @[UARTModule.scala 559:20]
        io_data_ <= 8'h0; // @[UARTModule.scala 561:17]
      end
    end else if (!(2'h1 == state)) begin // @[UARTModule.scala 557:17]
      if (!(2'h2 == state)) begin // @[UARTModule.scala 557:17]
        io_data_ <= _GEN_17;
      end
    end
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (baudTick & ~reset) begin
          $fwrite(32'h80000002,"======Baud Tick Occurs=====\n"); // @[UARTModule.scala 550:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~_T_2 & _T_4 & baudTick & _T_1) begin
          $fwrite(32'h80000002,"---RX will goto RECV\n"); // @[UARTModule.scala 569:17]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_35 & ~_T_4 & _T_7 & baudTick & _T_1) begin
          $fwrite(32'h80000002,"RX prev data: %b\n",data_queues); // @[UARTModule.scala 579:17]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_42 & _T_1) begin
          $fwrite(32'h80000002,"Will set bits %d\n",bitCount); // @[UARTModule.scala 580:17]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_40 & ~_T_7 & _T_13 & baudTick & _T_1) begin
          $fwrite(32'h80000002,"RX STOP data: %b\n",data_queues); // @[UARTModule.scala 592:17]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_1) begin
          $fwrite(32'h80000002,"baud tick %d , value: %d ",baudTick,baudCounter); // @[UARTModule.scala 601:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_1) begin
          $fwrite(32'h80000002,"CURRENT RX: %b, cur que %b ",io_rx,data_queues); // @[UARTModule.scala 602:11]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_T_1) begin
          $fwrite(32'h80000002,"RX cur valid: %b , cur data: %b\n",io_valid,io_data); // @[UARTModule.scala 603:11]
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
  state = _RAND_0[1:0];
  _RAND_1 = {1{`RANDOM}};
  bitCount = _RAND_1[3:0];
  _RAND_2 = {1{`RANDOM}};
  data_queues = _RAND_2[7:0];
  _RAND_3 = {1{`RANDOM}};
  baudCounter = _RAND_3[15:0];
  _RAND_4 = {1{`RANDOM}};
  io_valid_ = _RAND_4[0:0];
  _RAND_5 = {1{`RANDOM}};
  io_data_ = _RAND_5[7:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module UARTtx(
  input   clock,
  input   reset,
  output  io_tx
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
`endif // RANDOMIZE_REG_INIT
  wire [2:0] baud_cout = 3'h5 - 3'h1; // @[UARTModule.scala 615:55]
  reg [1:0] state; // @[UARTModule.scala 618:22]
  reg [3:0] bitCount; // @[UARTModule.scala 619:25]
  reg [7:0] shiftReg; // @[UARTModule.scala 620:25]
  reg [15:0] baudCounter; // @[UARTModule.scala 622:28]
  wire  baudTick = baudCounter == 16'h0; // @[UARTModule.scala 623:31]
  reg  io_tx_; // @[UARTModule.scala 628:22]
  wire [15:0] _baudCounter_T_1 = baudCounter - 16'h1; // @[UARTModule.scala 635:32]
  wire [15:0] _GEN_0 = baudTick ? {{13'd0}, baud_cout} : _baudCounter_T_1; // @[UARTModule.scala 632:18 633:17 635:17]
  wire  _T = 2'h0 == state; // @[UARTModule.scala 638:17]
  wire  _T_1 = 2'h1 == state; // @[UARTModule.scala 638:17]
  wire  _T_3 = ~reset; // @[UARTModule.scala 649:17]
  wire  _T_4 = 2'h2 == state; // @[UARTModule.scala 638:17]
  wire [3:0] _bitCount_T_1 = bitCount + 4'h1; // @[UARTModule.scala 664:30]
  wire [1:0] _GEN_7 = bitCount == 4'h8 ? 2'h3 : state; // @[UARTModule.scala 665:32 666:17 618:22]
  wire  _GEN_8 = bitCount == 4'h8 | shiftReg[0]; // @[UARTModule.scala 662:15 665:32 667:16]
  wire  _GEN_9 = baudTick ? _GEN_8 : io_tx_; // @[UARTModule.scala 628:22 658:22]
  wire [7:0] _GEN_10 = baudTick ? {{1'd0}, shiftReg[7:1]} : shiftReg; // @[UARTModule.scala 658:22 663:18 620:25]
  wire [3:0] _GEN_11 = baudTick ? _bitCount_T_1 : bitCount; // @[UARTModule.scala 658:22 664:18 619:25]
  wire [1:0] _GEN_12 = baudTick ? _GEN_7 : state; // @[UARTModule.scala 618:22 658:22]
  wire  _T_8 = 2'h3 == state; // @[UARTModule.scala 638:17]
  wire [1:0] _GEN_13 = baudTick ? 2'h0 : state; // @[UARTModule.scala 673:22 677:15 618:22]
  wire  _GEN_14 = 2'h3 == state | io_tx_; // @[UARTModule.scala 638:17 672:13 628:22]
  wire [1:0] _GEN_15 = 2'h3 == state ? _GEN_13 : state; // @[UARTModule.scala 638:17 618:22]
  wire  _GEN_16 = 2'h2 == state ? _GEN_9 : _GEN_14; // @[UARTModule.scala 638:17]
  wire  _GEN_23 = 2'h1 == state ? 1'h0 : _GEN_16; // @[UARTModule.scala 638:17 655:13]
  wire  _GEN_29 = 2'h0 == state ? io_tx_ : _GEN_23; // @[UARTModule.scala 638:17 628:22]
  wire  _GEN_30 = ~_T; // @[UARTModule.scala 649:17]
  wire  _GEN_35 = _GEN_30 & ~_T_1; // @[UARTModule.scala 660:17]
  assign io_tx = io_tx_; // @[UARTModule.scala 630:9]
  always @(posedge clock) begin
    if (reset) begin // @[UARTModule.scala 618:22]
      state <= 2'h0; // @[UARTModule.scala 618:22]
    end else if (!(2'h0 == state)) begin // @[UARTModule.scala 638:17]
      if (2'h1 == state) begin // @[UARTModule.scala 638:17]
        if (baudTick) begin // @[UARTModule.scala 647:22]
          state <= 2'h2; // @[UARTModule.scala 651:15]
        end
      end else if (2'h2 == state) begin // @[UARTModule.scala 638:17]
        state <= _GEN_12;
      end else begin
        state <= _GEN_15;
      end
    end
    if (reset) begin // @[UARTModule.scala 619:25]
      bitCount <= 4'h0; // @[UARTModule.scala 619:25]
    end else if (!(2'h0 == state)) begin // @[UARTModule.scala 638:17]
      if (2'h1 == state) begin // @[UARTModule.scala 638:17]
        if (baudTick) begin // @[UARTModule.scala 647:22]
          bitCount <= 4'h0; // @[UARTModule.scala 653:18]
        end
      end else if (2'h2 == state) begin // @[UARTModule.scala 638:17]
        bitCount <= _GEN_11;
      end
    end
    if (reset) begin // @[UARTModule.scala 620:25]
      shiftReg <= 8'h0; // @[UARTModule.scala 620:25]
    end else if (!(2'h0 == state)) begin // @[UARTModule.scala 638:17]
      if (!(2'h1 == state)) begin // @[UARTModule.scala 638:17]
        if (2'h2 == state) begin // @[UARTModule.scala 638:17]
          shiftReg <= _GEN_10;
        end
      end
    end
    if (reset) begin // @[UARTModule.scala 622:28]
      baudCounter <= 16'h0; // @[UARTModule.scala 622:28]
    end else if (2'h0 == state) begin // @[UARTModule.scala 638:17]
      baudCounter <= _GEN_0;
    end else if (2'h1 == state) begin // @[UARTModule.scala 638:17]
      if (baudTick) begin // @[UARTModule.scala 647:22]
        baudCounter <= 16'h0; // @[UARTModule.scala 652:21]
      end else begin
        baudCounter <= _GEN_0;
      end
    end else begin
      baudCounter <= _GEN_0;
    end
    io_tx_ <= reset | _GEN_29; // @[UARTModule.scala 628:{22,22}]
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~_T & _T_1 & baudTick & ~reset) begin
          $fwrite(32'h80000002,"---TX will goto SEND\n"); // @[UARTModule.scala 649:17]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_30 & ~_T_1 & _T_4 & baudTick & _T_3) begin
          $fwrite(32'h80000002,"TX prev data : %b, bit cnt %d\n",shiftReg,bitCount); // @[UARTModule.scala 660:17]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (_GEN_35 & ~_T_4 & _T_8 & baudTick & _T_3) begin
          $fwrite(32'h80000002,"TX STOP data: %b\n",shiftReg); // @[UARTModule.scala 675:17]
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
  state = _RAND_0[1:0];
  _RAND_1 = {1{`RANDOM}};
  bitCount = _RAND_1[3:0];
  _RAND_2 = {1{`RANDOM}};
  shiftReg = _RAND_2[7:0];
  _RAND_3 = {1{`RANDOM}};
  baudCounter = _RAND_3[15:0];
  _RAND_4 = {1{`RANDOM}};
  io_tx_ = _RAND_4[0:0];
`endif // RANDOMIZE_REG_INIT
  `endif // RANDOMIZE
end // initial
`ifdef FIRRTL_AFTER_INITIAL
`FIRRTL_AFTER_INITIAL
`endif
`endif // SYNTHESIS
endmodule
module UART(
  input        clock,
  input        reset,
  input        io_board_rx,
  output       io_board_tx,
  output [7:0] io_mmio_rxData,
  output       io_mmio_rxValid
);
  wire  uartRx_clock; // @[UART.scala 29:22]
  wire  uartRx_reset; // @[UART.scala 29:22]
  wire  uartRx_io_rx; // @[UART.scala 29:22]
  wire [7:0] uartRx_io_data; // @[UART.scala 29:22]
  wire  uartRx_io_valid; // @[UART.scala 29:22]
  wire  uartTx_clock; // @[UART.scala 30:22]
  wire  uartTx_reset; // @[UART.scala 30:22]
  wire  uartTx_io_tx; // @[UART.scala 30:22]
  UARTrx uartRx ( // @[UART.scala 29:22]
    .clock(uartRx_clock),
    .reset(uartRx_reset),
    .io_rx(uartRx_io_rx),
    .io_data(uartRx_io_data),
    .io_valid(uartRx_io_valid)
  );
  UARTtx uartTx ( // @[UART.scala 30:22]
    .clock(uartTx_clock),
    .reset(uartTx_reset),
    .io_tx(uartTx_io_tx)
  );
  assign io_board_tx = uartTx_io_tx; // @[UART.scala 33:15]
  assign io_mmio_rxData = uartRx_io_data; // @[UART.scala 35:18]
  assign io_mmio_rxValid = uartRx_io_valid; // @[UART.scala 36:19]
  assign uartRx_clock = clock;
  assign uartRx_reset = reset;
  assign uartRx_io_rx = io_board_rx; // @[UART.scala 32:16]
  assign uartTx_clock = clock;
  assign uartTx_reset = reset;
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
  assign io_led = out_led; // @[UARTVerify.scala 22:9]
  assign io_signal = uart_io_mmio_rxValid; // @[UARTVerify.scala 23:12]
  assign uart_clock = clock;
  assign uart_reset = reset;
  assign uart_io_board_rx = io_board_rx; // @[UARTVerify.scala 15:11]
  always @(posedge clock) begin
    if (reset) begin // @[UARTVerify.scala 19:22]
      out_led <= 8'h0; // @[UARTVerify.scala 19:22]
    end else if (uart_io_mmio_rxValid) begin // @[UARTVerify.scala 24:29]
      out_led <= uart_io_mmio_rxData; // @[UARTVerify.scala 25:12]
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
