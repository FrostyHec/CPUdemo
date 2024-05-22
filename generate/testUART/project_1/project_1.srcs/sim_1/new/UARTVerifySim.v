`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2024/05/21 21:04:04
// Design Name: 
// Module Name: UARTVerifySim
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


module UARTVerifySim(

);
    reg div_clock;
    reg reset = 1'b0;
    reg io_board_rx;
    wire io_board_tx;
    wire [7:0] io_led;
    wire io_signal;
    UARTVerify v(
        .clock(div_clock),
        .reset(reset),
        .io_board_rx(io_board_rx),
        .io_board_tx(io_board_tx),
        .io_led(io_led),
        .io_signal(io_signal)
    );
    initial begin
        reset=1'b1;
        #10 reset=1'b0;
    end
    initial begin
        div_clock=1'b0;
        forever begin
            #5 div_clock=~div_clock;
        end
    end

    initial begin
        // 模拟八个bit信号的传入
        io_board_rx=1'b1;
        #30;
        #2 io_board_rx=1'b0;
        #50 io_board_rx=1'b1;//bit0 
        #50 io_board_rx=1'b0;//bit1
        #50 io_board_rx=1'b1;//2
        #50 io_board_rx=1'b0;//3
        #50 io_board_rx=1'b0;//4
        #50 io_board_rx=1'b0;//5
        #50 io_board_rx=1'b0;//6
        #50 io_board_rx=1'b0;//7
        #50 io_board_rx=1'b1;//stop
        #100
        $finish;
    end
endmodule
