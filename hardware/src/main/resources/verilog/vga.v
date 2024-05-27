`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2023/12/27 11:03:05
// Design Name: 
// Module Name: vga
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

//Refer to blog https://blog.csdn.net/qq_43796199/article/details/119916049
// && https://zhuanlan.zhihu.com/p/165905962
module vga(
    input [31:0] digit, // input digit to show

    input clk,
    input rst,
    output hsync,
    output vsync,
    output [11:0] vga_rgb
);


wire vga_clk;
wire locked;
wire rst_n;

wire [9:0] x_pix;
wire [9:0] y_pix;
wire [11:0] pix_data;

assign  rst_n = (~rst && locked);

// get CLK_25M
clk_vga clk_vga_0
(
  .clk_out1(vga_clk),      
  .reset(rst),
  .locked(locked),
  .clk_in1(clk)
);

vga_ctrl vga_ctrl_0
(
    .vga_clk  (vga_clk),
    .rst(rst_n),
    .pix_data (pix_data), 
    .pix_x (x_pix),
    .pix_y (y_pix),
    .hsync (hsync),
    .vsync (vsync),
    .vga_rgb (vga_rgb)
);

vga_disp vga_disp_0
(   
    .digit(digit),
    .vga_clk(vga_clk),
    .rst(rst_n),
    .x_pix(x_pix),
    .y_pix(y_pix),
    .pix_data(pix_data)
);
endmodule
