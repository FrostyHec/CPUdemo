`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2023/12/27 11:04:02
// Design Name: 
// Module Name: vga_ctrl
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


module vga_ctrl(
    input vga_clk,
    input rst,
    input [11:0] pix_data,

    output [9:0] pix_x,
    output [9:0] pix_y,
    output hsync,
    output vsync,
    output [11:0] vga_rgb  
);


parameter h_sync   = 10'd96,
          h_back   = 10'd40,
          h_left   = 10'd8,
          h_range  = 10'd640,
          h_right  = 10'd8,
          h_front  = 10'd8,
          h_total  = 10'd800;

parameter v_sync   = 10'd2,
          v_back   = 10'd25,
          v_top    = 10'd8,
          v_range  = 10'd480,
          v_bottom = 10'd8,
          v_front  = 10'd2,
          v_total  = 10'd525;


reg [9:0] cnt_h;
reg [9:0] cnt_v;
wire rgb_valid;
wire pix_data_req;

always @(posedge vga_clk, negedge rst) begin
    if (rst == 1'b0) begin
        cnt_h <= 10'd0;
    end
    else if (cnt_h == (h_total - 1'b1)) begin
        cnt_h <= 10'd0;
    end
    else begin
        cnt_h <= cnt_h + 10'd1;
    end
end

always @(posedge vga_clk, negedge rst) begin
    if (rst == 1'b0) begin
        cnt_v <= 10'd0;
    end
    else if ((cnt_h == (h_total - 1'b1)) && (cnt_v == (v_total - 1'b1))) begin
        cnt_v <= 10'd0;
    end
    else if (cnt_h == (h_total-1'b1)) begin
        cnt_v <= cnt_v + 10'd1;
    end
    else begin
        cnt_v <= cnt_v;
    end
end

assign rgb_valid = ((cnt_h >= h_sync + h_back + h_left)
                    && (cnt_h < h_sync + h_back + h_left + h_range)
                    && (cnt_v >= v_sync + v_back + v_top)
                    && (cnt_v < v_sync + v_back + v_top + v_range))
                    ?  1'b1 : 1'b0;

assign pix_data_req = ((cnt_h >= h_sync + h_back + h_left - 1'b1)
                    && (cnt_h < h_sync + h_back + h_left + h_range - 1'b1)
                    && (cnt_v >= v_sync + v_back + v_top)
                    && (cnt_v < v_sync + v_back + v_top + v_range))
                    ?  1'b1 : 1'b0;
                    
assign pix_x = (pix_data_req == 1'b1) ? (cnt_h - (h_sync + h_back + h_left) - 1'b1) : 10'd0;
assign pix_y = (pix_data_req == 1'b1) ? (cnt_v - (v_sync + v_back + v_top)) : 10'd0;
assign hsync = (cnt_h <= h_sync - 1'b1) ? 1'b1 : 1'b0;
assign vsync = (cnt_v <= v_sync - 1'b1) ? 1'b1 : 1'b0;
assign vga_rgb = (rgb_valid == 1'b1) ? pix_data : 12'h000;
                    
endmodule
