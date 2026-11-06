module UARTWrapper(
  input        clock,
  input        reset,
  input        io_board_rx,
  output       io_board_tx,
  output [7:0] io_mmio_rxData,
  output       io_mmio_rxValid,
  input        io_mmio_rxReady
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
  assign uart_clk = clock; // @[UARTWrapper.scala 33:26]
  assign uart_rst = reset; // @[UARTWrapper.scala 34:26]
  assign uart_s_axis_tdata = 8'h0; // @[UARTWrapper.scala 36:26]
  assign uart_s_axis_tvalid = 1'h0; // @[UARTWrapper.scala 37:27]
  assign uart_m_axis_tready = io_mmio_rxReady; // @[UARTWrapper.scala 45:27]
  assign uart_rxd = io_board_rx; // @[UARTWrapper.scala 40:17]
  assign uart_prescale = 16'h516; // @[UARTWrapper.scala 47:22]
endmodule
module UARTLoader(
  input         clock,
  input         reset,
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
  wire [31:0] _data_addr_T_1 = data_addr + 32'h1; // @[UARTLoader.scala 71:34]
  wire [31:0] _GEN_4 = io_rxValid ? _data_addr_T_1 : data_addr; // @[UARTLoader.scala 70:37 71:21 35:26]
  wire [1:0] _GEN_5 = io_rxValid ? 2'h2 : cur_state; // @[UARTLoader.scala 70:37 72:21 34:26]
  wire [1:0] _GEN_9 = io_rxValid ? cur_state : 2'h3; // @[UARTLoader.scala 34:26 81:37 85:21]
  wire  _GEN_12 = 2'h2 == cur_state ? io_rxValid : rxReadyReg; // @[UARTLoader.scala 47:21 44:27]
  wire [1:0] _GEN_13 = 2'h2 == cur_state ? _GEN_9 : cur_state; // @[UARTLoader.scala 47:21 34:26]
  wire  _GEN_18 = 2'h3 == cur_state ? 1'h0 : 2'h2 == cur_state & io_rxValid; // @[UARTLoader.scala 42:20 47:21]
  wire  _GEN_22 = 2'h1 == cur_state ? 1'h0 : _GEN_18; // @[UARTLoader.scala 42:20 47:21]
  assign io_rxReady = rxReadyReg; // @[UARTLoader.scala 45:13]
  assign io_mem_mem_write = 2'h0 == cur_state ? 1'h0 : _GEN_22; // @[UARTLoader.scala 42:20 47:21]
  assign io_mem_data_to_write = {{24'd0}, io_rxData}; // @[UARTLoader.scala 40:24]
  assign io_mem_data_addr = data_addr; // @[UARTLoader.scala 38:20]
  always @(posedge clock) begin
    if (reset) begin // @[UARTLoader.scala 34:26]
      cur_state <= 2'h0; // @[UARTLoader.scala 34:26]
    end else if (2'h0 == cur_state) begin // @[UARTLoader.scala 47:21]
      cur_state <= 2'h1;
    end else if (2'h1 == cur_state) begin // @[UARTLoader.scala 47:21]
      if (~io_rxValid) begin // @[UARTLoader.scala 60:38]
        cur_state <= 2'h3; // @[UARTLoader.scala 61:21]
      end
    end else if (2'h3 == cur_state) begin // @[UARTLoader.scala 47:21]
      cur_state <= _GEN_5;
    end else begin
      cur_state <= _GEN_13;
    end
    if (reset) begin // @[UARTLoader.scala 35:26]
      data_addr <= 32'hffffffff; // @[UARTLoader.scala 35:26]
    end else if (2'h0 == cur_state) begin // @[UARTLoader.scala 47:21]
      data_addr <= 32'hffffffff;
    end else if (!(2'h1 == cur_state)) begin // @[UARTLoader.scala 47:21]
      if (2'h3 == cur_state) begin // @[UARTLoader.scala 47:21]
        data_addr <= _GEN_4;
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
  output [31:0] io_mem_read_data
);
`ifdef RANDOMIZE_REG_INIT
  reg [31:0] _RAND_0;
  reg [31:0] _RAND_1;
  reg [31:0] _RAND_2;
  reg [31:0] _RAND_3;
  reg [31:0] _RAND_4;
`endif // RANDOMIZE_REG_INIT
  reg [31:0] led; // @[OutRegisters.scala 32:20]
  reg [31:0] seg7; // @[OutRegisters.scala 35:21]
  reg [31:0] rxReady; // @[OutRegisters.scala 46:24]
  reg [31:0] txData; // @[OutRegisters.scala 49:23]
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
  wire  _T_27 = addr == 32'h3fffffc9; // @[OutRegisters.scala 119:21]
  wire [31:0] _GEN_14 = io_mem_write ? io_mem_write_data : vga; // @[OutRegisters.scala 127:26 128:13 59:20]
  wire [31:0] _GEN_16 = addr == 32'h3fffffc4 ? _GEN_14 : vga; // @[OutRegisters.scala 126:68 59:20]
  wire [31:0] _GEN_18 = addr == 32'h3fffffc9 ? 32'h0 : vga; // @[OutRegisters.scala 119:70]
  wire [31:0] _GEN_19 = addr == 32'h3fffffc9 ? vga : _GEN_16; // @[OutRegisters.scala 119:70 59:20]
  wire [31:0] _GEN_21 = addr == 32'h3fffffca ? 32'h0 : _GEN_18; // @[OutRegisters.scala 113:70]
  wire [31:0] _GEN_22 = addr == 32'h3fffffca ? vga : _GEN_19; // @[OutRegisters.scala 113:70 59:20]
  wire [31:0] _GEN_23 = addr == 32'h3fffffc8 ? _GEN_10 : txData; // @[OutRegisters.scala 107:69 49:23]
  wire [31:0] _GEN_24 = addr == 32'h3fffffc8 ? txData : _GEN_21; // @[OutRegisters.scala 107:69]
  wire [31:0] _GEN_26 = addr == 32'h3fffffc8 ? vga : _GEN_22; // @[OutRegisters.scala 107:69 59:20]
  wire [31:0] _GEN_27 = addr == 32'h3fffffc7 ? _GEN_8 : rxReady; // @[OutRegisters.scala 101:74 46:24]
  wire [31:0] _GEN_28 = addr == 32'h3fffffc7 ? rxReady : _GEN_24; // @[OutRegisters.scala 101:74]
  wire [31:0] _GEN_29 = addr == 32'h3fffffc7 ? txData : _GEN_23; // @[OutRegisters.scala 101:74 49:23]
  wire [31:0] _GEN_31 = addr == 32'h3fffffc7 ? vga : _GEN_26; // @[OutRegisters.scala 101:74 59:20]
  wire [31:0] _GEN_32 = addr == 32'h3fffffc6 ? 32'h0 : _GEN_28; // @[OutRegisters.scala 95:74]
  wire [31:0] _GEN_33 = addr == 32'h3fffffc6 ? rxReady : _GEN_27; // @[OutRegisters.scala 46:24 95:74]
  wire [31:0] _GEN_34 = addr == 32'h3fffffc6 ? txData : _GEN_29; // @[OutRegisters.scala 49:23 95:74]
  wire [31:0] _GEN_36 = addr == 32'h3fffffc6 ? vga : _GEN_31; // @[OutRegisters.scala 59:20 95:74]
  wire [31:0] _GEN_37 = addr == 32'h3fffffc5 ? 32'h0 : _GEN_32; // @[OutRegisters.scala 89:69]
  wire [31:0] _GEN_38 = addr == 32'h3fffffc5 ? rxReady : _GEN_33; // @[OutRegisters.scala 46:24 89:69]
  wire [31:0] _GEN_39 = addr == 32'h3fffffc5 ? txData : _GEN_34; // @[OutRegisters.scala 49:23 89:69]
  wire [31:0] _GEN_41 = addr == 32'h3fffffc5 ? vga : _GEN_36; // @[OutRegisters.scala 59:20 89:69]
  wire [31:0] _GEN_42 = addr == 32'h3fffffc3 ? _GEN_4 : seg7; // @[OutRegisters.scala 35:21 83:67]
  wire [31:0] _GEN_43 = addr == 32'h3fffffc3 ? seg7 : _GEN_37; // @[OutRegisters.scala 83:67]
  wire [31:0] _GEN_44 = addr == 32'h3fffffc3 ? rxReady : _GEN_38; // @[OutRegisters.scala 46:24 83:67]
  wire [31:0] _GEN_45 = addr == 32'h3fffffc3 ? txData : _GEN_39; // @[OutRegisters.scala 49:23 83:67]
  wire [31:0] _GEN_47 = addr == 32'h3fffffc3 ? vga : _GEN_41; // @[OutRegisters.scala 59:20 83:67]
  wire [31:0] _GEN_49 = addr == 32'h3fffffc0 ? led : _GEN_43; // @[OutRegisters.scala 77:66]
  wire [31:0] _GEN_55 = addr == 32'h3fffffc2 ? 32'h0 : _GEN_49; // @[OutRegisters.scala 71:67]
  wire  _GEN_20 = ~_T_1; // @[OutRegisters.scala 73:15]
  wire  _GEN_69 = _GEN_20 & ~_T_5 & ~_T_9 & ~_T_11; // @[OutRegisters.scala 91:15]
  wire  _GEN_80 = _GEN_69 & ~_T_13; // @[OutRegisters.scala 97:15]
  assign io_mem_read_data = addr == 32'h3fffffc1 ? 32'h0 : _GEN_55; // @[OutRegisters.scala 65:58]
  always @(posedge clock) begin
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
        if (_GEN_20 & ~_T_5 & ~_T_9 & ~_T_11 & _T_13 & io_mem_write & _T_3) begin
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
        if (_GEN_69 & ~_T_13 & _T_17 & io_mem_write & _T_3) begin
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
        if (_GEN_80 & ~_T_17 & ~_T_21 & ~_T_23 & ~_T_25 & _T_27 & io_mem_write & _T_3) begin
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
  led = _RAND_0[31:0];
  _RAND_1 = {1{`RANDOM}};
  seg7 = _RAND_1[31:0];
  _RAND_2 = {1{`RANDOM}};
  rxReady = _RAND_2[31:0];
  _RAND_3 = {1{`RANDOM}};
  txData = _RAND_3[31:0];
  _RAND_4 = {1{`RANDOM}};
  vga = _RAND_4[31:0];
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
  input  [31:0] io_ins_addr,
  output [31:0] io_ins_out,
  input         io_write_data,
  input  [31:0] io_data_addr,
  input  [31:0] io_data_write
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
  wire [29:0] rw_mem_addr = io_data_addr[31:2]; // @[MemoryDispatch.scala 46:34]
  wire [29:0] read_ins_addr = io_ins_addr[31:2]; // @[MemoryDispatch.scala 47:35]
  wire  _T_4 = io_data_addr <= 32'hffff; // @[MemoryDispatch.scala 114:21]
  wire  _T_6 = 32'h10000 <= io_data_addr; // @[MemoryDispatch.scala 122:36]
  wire  _T_8 = _T_6 & io_data_addr <= 32'h1ffff; // @[MemoryDispatch.scala 123:5]
  wire [31:0] _havard_mem_T_1 = io_data_addr - 32'h10000; // @[MemoryDispatch.scala 125:36]
  wire [29:0] havard_mem = _havard_mem_T_1[31:2]; // @[MemoryDispatch.scala 125:61]
  wire  _T_9 = 32'hffffff00 <= io_data_addr; // @[MemoryDispatch.scala 135:38]
  wire [31:0] _GEN_6 = outRegisters_io_mem_read_data; // @[MemoryDispatch.scala 137:14 140:30 141:16]
  wire [31:0] _GEN_8 = io_write_data ? outRegisters_io_mem_read_data : _GEN_6; // @[MemoryDispatch.scala 137:14 138:25]
  wire  _T_13 = ~reset; // @[MemoryDispatch.scala 150:13]
  wire  _GEN_18 = _T_9 & io_write_data; // @[MemoryDispatch.scala 136:47 MemoryPacket.scala 17:16]
  wire [29:0] _GEN_21 = _T_6 & io_data_addr <= 32'h1ffff ? havard_mem : rw_mem_addr; // @[MemoryDispatch.scala 123:45 126:26 MemoryPacket.scala 18:20]
  wire [31:0] _GEN_22 = _T_6 & io_data_addr <= 32'h1ffff ? dataRAM_io_read_data : _GEN_8; // @[MemoryDispatch.scala 123:45 128:14]
  wire  _GEN_23 = _T_6 & io_data_addr <= 32'h1ffff & io_write_data; // @[MemoryDispatch.scala 123:45 MemoryPacket.scala 17:16]
  wire  _GEN_24 = _T_6 & io_data_addr <= 32'h1ffff ? 1'h0 : _GEN_18; // @[MemoryDispatch.scala 123:45 MemoryPacket.scala 17:16]
  wire [31:0] data_out = io_data_addr <= 32'hffff ? insRAM_io_read_data : _GEN_22; // @[MemoryDispatch.scala 114:44 115:14]
  wire [29:0] _GEN_29 = io_data_addr <= 32'hffff ? rw_mem_addr : _GEN_21; // @[MemoryDispatch.scala 114:44 MemoryPacket.scala 18:20]
  wire [7:0] value = io_data_write[7:0]; // @[MemoryDispatch.scala 163:32]
  wire [31:0] _data_in_T_1 = {data_out[31:8],value}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_4 = {data_out[31:16],value,data_out[7:0]}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_7 = {data_out[31:24],value,data_out[15:0]}; // @[Cat.scala 33:92]
  wire [31:0] _data_in_T_9 = {value,data_out[23:0]}; // @[Cat.scala 33:92]
  wire [31:0] _GEN_35 = 2'h2 == io_data_addr[1:0] ? _data_in_T_7 : _data_in_T_9; // @[MemoryDispatch.scala 164:34 172:19]
  wire [31:0] _GEN_36 = 2'h1 == io_data_addr[1:0] ? _data_in_T_4 : _GEN_35; // @[MemoryDispatch.scala 164:34 169:19]
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
    .io_mem_read_data(outRegisters_io_mem_read_data)
  );
  assign io_ins_out = insRAM_io2_read_data; // @[MemoryDispatch.scala 63:14]
  assign insRAM_clock = clock;
  assign insRAM_io_write = io_data_addr <= 32'hffff & io_write_data; // @[MemoryDispatch.scala 114:44 MemoryPacket.scala 17:16]
  assign insRAM_io_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 19:21]
  assign insRAM_io_write_data = 2'h0 == io_data_addr[1:0] ? _data_in_T_1 : _GEN_36; // @[MemoryDispatch.scala 164:34 166:19]
  assign insRAM_io2_read_addr = {{2'd0}, read_ins_addr}; // @[MemoryDispatch.scala 62:24]
  assign dataRAM_clock = clock;
  assign dataRAM_io_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_23; // @[MemoryDispatch.scala 114:44 MemoryPacket.scala 17:16]
  assign dataRAM_io_read_addr = {{2'd0}, _GEN_29};
  assign dataRAM_io_write_addr = {{2'd0}, _GEN_29};
  assign dataRAM_io_write_data = 2'h0 == io_data_addr[1:0] ? _data_in_T_1 : _GEN_36; // @[MemoryDispatch.scala 164:34 166:19]
  assign outRegisters_clock = clock;
  assign outRegisters_reset = reset;
  assign outRegisters_io_mem_write = io_data_addr <= 32'hffff ? 1'h0 : _GEN_24; // @[MemoryDispatch.scala 114:44 MemoryPacket.scala 17:16]
  assign outRegisters_io_mem_read_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 18:20]
  assign outRegisters_io_mem_write_addr = {{2'd0}, rw_mem_addr}; // @[MemoryPacket.scala 19:21]
  assign outRegisters_io_mem_write_data = 2'h0 == io_data_addr[1:0] ? _data_in_T_1 : _GEN_36; // @[MemoryDispatch.scala 164:34 166:19]
  always @(posedge clock) begin
    `ifndef SYNTHESIS
    `ifdef PRINTF_COND
      if (`PRINTF_COND) begin
    `endif
        if (~_T_4 & ~_T_8 & ~_T_9 & io_write_data & _T_13) begin
          $fwrite(32'h80000002,"Unexpected Mem address %x",io_data_addr); // @[MemoryDispatch.scala 153:13]
        end
    `ifdef PRINTF_COND
      end
    `endif
    `endif // SYNTHESIS
  end
endmodule
module LoadVerify(
  input         clock,
  input         reset,
  input         io_rx,
  output        io_tx,
  input  [15:0] io_addr,
  output [15:0] io_led
);
  wire  uart_clock; // @[LoadVerify.scala 16:20]
  wire  uart_reset; // @[LoadVerify.scala 16:20]
  wire  uart_io_board_rx; // @[LoadVerify.scala 16:20]
  wire  uart_io_board_tx; // @[LoadVerify.scala 16:20]
  wire [7:0] uart_io_mmio_rxData; // @[LoadVerify.scala 16:20]
  wire  uart_io_mmio_rxValid; // @[LoadVerify.scala 16:20]
  wire  uart_io_mmio_rxReady; // @[LoadVerify.scala 16:20]
  wire  loader_clock; // @[LoadVerify.scala 17:22]
  wire  loader_reset; // @[LoadVerify.scala 17:22]
  wire  loader_io_rxValid; // @[LoadVerify.scala 17:22]
  wire [7:0] loader_io_rxData; // @[LoadVerify.scala 17:22]
  wire  loader_io_rxReady; // @[LoadVerify.scala 17:22]
  wire  loader_io_mem_mem_write; // @[LoadVerify.scala 17:22]
  wire [31:0] loader_io_mem_data_to_write; // @[LoadVerify.scala 17:22]
  wire [31:0] loader_io_mem_data_addr; // @[LoadVerify.scala 17:22]
  wire  mem_dispatch_clock; // @[LoadVerify.scala 18:28]
  wire  mem_dispatch_reset; // @[LoadVerify.scala 18:28]
  wire [31:0] mem_dispatch_io_ins_addr; // @[LoadVerify.scala 18:28]
  wire [31:0] mem_dispatch_io_ins_out; // @[LoadVerify.scala 18:28]
  wire  mem_dispatch_io_write_data; // @[LoadVerify.scala 18:28]
  wire [31:0] mem_dispatch_io_data_addr; // @[LoadVerify.scala 18:28]
  wire [31:0] mem_dispatch_io_data_write; // @[LoadVerify.scala 18:28]
  UARTWrapper uart ( // @[LoadVerify.scala 16:20]
    .clock(uart_clock),
    .reset(uart_reset),
    .io_board_rx(uart_io_board_rx),
    .io_board_tx(uart_io_board_tx),
    .io_mmio_rxData(uart_io_mmio_rxData),
    .io_mmio_rxValid(uart_io_mmio_rxValid),
    .io_mmio_rxReady(uart_io_mmio_rxReady)
  );
  UARTLoader loader ( // @[LoadVerify.scala 17:22]
    .clock(loader_clock),
    .reset(loader_reset),
    .io_rxValid(loader_io_rxValid),
    .io_rxData(loader_io_rxData),
    .io_rxReady(loader_io_rxReady),
    .io_mem_mem_write(loader_io_mem_mem_write),
    .io_mem_data_to_write(loader_io_mem_data_to_write),
    .io_mem_data_addr(loader_io_mem_data_addr)
  );
  MemoryDispatch mem_dispatch ( // @[LoadVerify.scala 18:28]
    .clock(mem_dispatch_clock),
    .reset(mem_dispatch_reset),
    .io_ins_addr(mem_dispatch_io_ins_addr),
    .io_ins_out(mem_dispatch_io_ins_out),
    .io_write_data(mem_dispatch_io_write_data),
    .io_data_addr(mem_dispatch_io_data_addr),
    .io_data_write(mem_dispatch_io_data_write)
  );
  assign io_tx = uart_io_board_tx; // @[LoadVerify.scala 23:8]
  assign io_led = {uart_io_mmio_rxData,mem_dispatch_io_ins_out[7:0]}; // @[Cat.scala 33:92]
  assign uart_clock = clock;
  assign uart_reset = reset;
  assign uart_io_board_rx = io_rx; // @[LoadVerify.scala 22:19]
  assign uart_io_mmio_rxReady = loader_io_rxReady; // @[LoadVerify.scala 25:23]
  assign loader_clock = clock;
  assign loader_reset = reset;
  assign loader_io_rxValid = uart_io_mmio_rxValid; // @[LoadVerify.scala 28:21]
  assign loader_io_rxData = uart_io_mmio_rxData; // @[LoadVerify.scala 27:20]
  assign mem_dispatch_clock = clock;
  assign mem_dispatch_reset = reset;
  assign mem_dispatch_io_ins_addr = {{16'd0}, io_addr}; // @[LoadVerify.scala 48:27]
  assign mem_dispatch_io_write_data = loader_io_mem_mem_write; // @[LoadVerify.scala 35:29]
  assign mem_dispatch_io_data_addr = loader_io_mem_data_addr; // @[LoadVerify.scala 38:28]
  assign mem_dispatch_io_data_write = loader_io_mem_data_to_write; // @[LoadVerify.scala 36:29]
endmodule
