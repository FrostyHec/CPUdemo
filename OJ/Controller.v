`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2024/04/23 10:57:54
// Design Name: 
// Module Name: p1
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


module Controller(
input [31:0] inst,
output reg Branch,
output reg [1:0] ALUOp,
output reg ALUSrc,
output reg MemRead,
output reg MemWrite,
output reg MemtoReg,
output reg RegWrite
    );

    always@(*) begin
        case(inst[6:0])
            7'b000_0011: begin // lw
                Branch = 1'b0;
                ALUOp = 2'b00;
                ALUSrc = 1'b1;
                MemRead = 1'b1;
                MemWrite = 1'b0;
                MemtoReg = 1'b1;
                RegWrite = 1'b1;
            end
            7'b011_0011: begin // add
                Branch = 1'b0;
                ALUOp = 2'b10;
                ALUSrc = 1'b0;
                MemRead = 1'b0;
                MemWrite = 1'b0;
                MemtoReg = 1'b0;
                RegWrite = 1'b1;
            end
            7'b010_0011: begin // sw
                Branch = 1'b0;
                ALUOp = 2'b00;
                ALUSrc = 1'b1;
                MemRead = 1'b0;
                MemWrite = 1'b1;
                MemtoReg = 1'b0;
                RegWrite = 1'b0;
            end
            7'b110_0011: begin // beq
                Branch = 1'b1;
                ALUOp = 2'b01;
                ALUSrc = 1'b0;
                MemRead = 1'b0;
                MemWrite = 1'b0;
                MemtoReg = 1'b0;
                RegWrite = 1'b0;   
            end
            7'b110_0011: begin // bne
                Branch = 1'b1;
                ALUOp = 2'b01;
                ALUSrc = 1'b0;
                MemRead = 1'b0;
                MemWrite = 1'b0;
                MemtoReg = 1'b0;
                RegWrite = 1'b0;       
            end
            7'b011_0011: begin // sub
                Branch = 1'b0;
                ALUOp = 2'b10;
                ALUSrc = 1'b0;
                MemRead = 1'b0;
                MemWrite = 1'b0;
                MemtoReg = 1'b0;
                RegWrite = 1'b1;
            end
            7'b011_0011: begin // and
                Branch = 1'b0;
                ALUOp = 2'b10;
                ALUSrc = 1'b0;
                MemRead = 1'b0;
                MemWrite = 1'b0;
                MemtoReg = 1'b0;
                RegWrite = 1'b1;
            end
            7'b011_0011: begin // or
                Branch = 1'b0;
                ALUOp = 2'b10;
                ALUSrc = 1'b0;
                MemRead = 1'b0;
                MemWrite = 1'b0;
                MemtoReg = 1'b0;
                RegWrite = 1'b1;    
            end
        endcase
    end
endmodule