`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/09/11 14:20:09
// Design Name: 
// Module Name: WB_stage
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

`include"mycpu.vh"

module WB_stage(
    input   wire                        clk           ,
    input   wire                        reset         ,
    //allowin
    output  wire                        ws_allowin    ,
    //from ms
    input   wire                      ms_to_ws_valid,
    input  wire[`MS_TO_WS_DATA_WD -1:0]  ms_to_ws_data  ,
    //to rf: for write back
    output wire[`WS_TO_RF_DATA_WD -1:0]  ws_to_rf_data  ,
    //trace debug interface
    output wire[31:0] debug_wb_pc     ,
    output wire[ 3:0] debug_wb_rf_we ,
    output wire[ 4:0] debug_wb_rf_wnum,
    output wire[31:0] debug_wb_rf_wdata
    );
reg         ws_valid;
wire        ws_ready_go;

reg [`MS_TO_WS_DATA_WD -1:0] ms_to_ws_data_r;
wire        ws_gr_we;
wire [ 4:0] ws_dest;
wire [31:0] ws_final_result;
wire [31:0] ws_pc;
assign {
    ws_gr_we,           //69:69
    ws_dest,            //68:64
    ws_final_result,    //63:32
    ws_pc               //31:0
} = ms_to_ws_data_r;

wire [ 3:0] rf_we;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;
assign ws_to_rf_data = {
    rf_we,      //40:37
    rf_waddr,   //36:32
    rf_wdata    //31:0
};

assign ws_ready_go = 1'b1;
assign ws_allowin  = !ws_valid || ws_ready_go;
always @(posedge clk) begin
    if (reset) begin
        ws_valid <= 1'b0;
    end
    else if (ws_allowin) begin
        ws_valid <= ms_to_ws_valid;
    end
end

always @(posedge clk) begin
    if (ms_to_ws_valid && ws_allowin) begin
        ms_to_ws_data_r <= ms_to_ws_data;
    end
end

assign rf_we    = {4{ ws_gr_we && ws_valid }};
assign rf_waddr = ws_dest;
assign rf_wdata = ws_final_result;

// debug info generate
assign debug_wb_pc       = ws_pc;
assign debug_wb_rf_we    = rf_we;
assign debug_wb_rf_wnum  = ws_dest;
assign debug_wb_rf_wdata = ws_final_result;
endmodule

