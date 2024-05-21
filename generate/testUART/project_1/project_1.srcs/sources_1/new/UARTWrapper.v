`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2024/05/21 20:53:40
// Design Name: 
// Module Name: UARTWrapper
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


module UARTWrapper(
  input        clock,
  input        io_board_rx,
  output       io_board_tx,
  output [7:0] io_led,
  output       io_signal
);
    wire reset=1'b0;
    wire div_clock ;
    
    v UARTVerify(
        .clock(div_clock),
        .reset(reset),
        .io_board_rx(io_board_rx),
        .io_board_tx(io_board_tx),
        .io_led(io_led),
        .io_signal(io_signal)
    );
endmodule
