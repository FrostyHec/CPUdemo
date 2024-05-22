`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2024/05/22 01:15:36
// Design Name: 
// Module Name: Wrapper2
// Project Name: 
// Target Devices: 
// Tool Versions: 
// Description: 
// 
// Dependencies: 
// 
// Revision:
// Revision 0.01 - File Created
// Additional Comments:
// 
//////////////////////////////////////////////////////////////////////////////////

module UARTWrapper2(
  input        clock,
  input        reset,
  input        io_board_rx,
  output       io_board_tx,
  output [7:0] io_led,
  output       io_signal
    );
    wire [7:0] out_data;
    //baud 9600
    //prescale 1302
    uart u(
    .clock(clock),
    .rst(reset),

    /*
     * AXI input
     */
    .s_axis_tdata(0),
    .s_axis_tvalid(1'b0),
    //.s_axis_tready,

    /*
     * AXI output
     */
    .m_axis_tdata(out_data),
    .m_axis_tvalid(io_signal),
    .m_axis_tready(1'b0),

    /*
     * UART interface
     */
    .rxd(io_board_rx),
    .txd(io_board_tx),

    /*
     * Status
     */
    // output wire                   tx_busy,
    // output wire                   rx_busy,
    // output wire                   rx_overrun_error,
    // output wire                   rx_frame_error,

    /*
     * Configuration
     */
    .prescale(1302)
    );
    assign io_led=out_data;

endmodule
