`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/09/11 13:17:34
// Design Name: 
// Module Name: IF_stage
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
`include "mycpu.vh"

module IF_stage(
    input  wire                  clk            ,
    input  wire                  reset          ,
    // allow in
    input  wire                  ds_allowin     ,
    // br_data
    input  wire[`BR_DATA_WD-1:0] br_blk_data         ,
    //to ds
    output wire                    fs_to_ds_valid ,
    output wire[`FS_TO_DS_DATA_WD -1:0] fs_to_ds_data   ,
    // inst sram interface
    output wire       inst_sram_we  ,
    output wire[31:0] inst_sram_addr ,
    output wire[31:0] inst_sram_wdata,
    input  wire[31:0] inst_sram_rdata
    );
    

reg         fs_valid;
wire        fs_ready_go;
wire        fs_allowin;
wire        to_fs_valid;
wire        to_fs_ready_go;

wire [31:0] seq_pc;
wire [31:0] next_pc;

wire         br_taken;
wire [ 31:0] br_target;
assign {blk_valid,br_taken,br_target} = br_blk_data;

wire [31:0] fs_inst;
reg  [31:0] fs_pc;
assign fs_to_ds_data = {fs_inst ,fs_pc};
                       

assign to_fs_valid    = ~reset ;
assign fs_ready_go    = 1'b1;
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;
assign fs_to_ds_valid =  fs_valid && fs_ready_go;

assign seq_pc       = fs_pc + 3'h4;
assign next_pc    =  !fs_allowin ? fs_pc:
                    br_taken ? br_target:
                                 seq_pc;

assign inst_sram_we    = 1'b0;
assign inst_sram_addr  = next_pc;
assign inst_sram_wdata = 32'b0;
assign fs_inst         = inst_sram_rdata;

always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end 
    else if(br_taken)begin
        fs_valid<=1'b0;
    end
    
end

always @(posedge clk) begin
    if (reset) begin
        fs_pc <= 32'h_1bfffffc;  //trick: to make next_pc be 0x1c000000 during reset 
    end
    else if (to_fs_valid && fs_allowin ) begin
        fs_pc <= next_pc;
    end
   
end

endmodule
