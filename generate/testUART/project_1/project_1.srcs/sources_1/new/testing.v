`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2024/05/21 22:39:36
// Design Name: 
// Module Name: testing
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


module testing(
    input clock,
    input [1:0]switch,
    output reg led
    );
    assign rst=switch[1];
    always @(posedge clock) begin
        if(rst) begin
            led<=1'b0;
        end else begin
            led<=switch[0];
        end
    end
endmodule
