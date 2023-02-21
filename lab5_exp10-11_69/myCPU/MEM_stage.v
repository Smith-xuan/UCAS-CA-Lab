`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/09/11 14:37:11
// Design Name: 
// Module Name: MEM_stage
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
module MEM_stage(
    input   wire                       clk           ,
    input   wire                       reset         ,
    //allowin
    input   wire                       ws_allowin    ,
    output  wire                       ms_allowin    ,
    //from es
    input   wire                       es_to_ms_valid,
    input   wire [`ES_TO_MS_DATA_WD -1:0] es_to_ms_data ,
    //to ws
    output  wire                       ms_to_ws_valid,
    output wire[`MS_TO_WS_DATA_WD -1:0] ms_to_ws_data  ,
    //from data-sram
    input  wire[31                 :0] data_sram_rdata,

    // forword from es
    output wire[`MS_FWD_BLK_DATA_WD -1:0] ms_fwd_blk_data   
    );
reg         ms_valid;
wire        ms_ready_go;
reg [`ES_TO_MS_DATA_WD -1:0] es_to_ms_data_r;
wire        ms_res_from_mem;
wire        ms_gr_we;
wire [ 4:0] ms_dest;
wire [31:0] ms_exe_result;
wire [31:0] ms_pc;
wire        ms_inst_ld_b;
wire        ms_inst_ld_bu;
wire        ms_inst_ld_h;
wire        ms_inst_ld_hu;
wire        ms_inst_ld_w;

assign {
    ms_inst_ld_b    ,
    ms_inst_ld_bu   ,
    ms_inst_ld_h    ,
    ms_inst_ld_hu   ,
    ms_inst_ld_w    ,
    ms_res_from_mem ,  //70:70
    ms_gr_we        ,  //69:69
    ms_dest         ,  //68:64
    ms_exe_result   ,  //63:32
    ms_pc             //31:0
} = es_to_ms_data_r;

wire [31:0] mem_result;
wire [31:0] ms_final_result;

assign ms_to_ws_data = {
    ms_gr_we       ,  //69:69
    ms_dest        ,  //68:64
    ms_final_result,  //63:32
    ms_pc             //31:0
};

wire [ 3:0] ms_fwd_valid;
wire [ 4:0] ms_rf_dest;
wire [31:0] ms_rf_data;

assign ms_fwd_blk_data = {
    ms_fwd_valid,   // 40:37
    ms_rf_dest,     // 36:32
    ms_rf_data      // 31:0
};

assign ms_ready_go    = 1'b1;
assign ms_allowin     = !ms_valid || ms_ready_go && ws_allowin;
assign ms_to_ws_valid = ms_valid && ms_ready_go;

always @(posedge clk) begin
    if (reset) begin
        ms_valid <= 1'b0;
    end
    else if (ms_allowin) begin
        ms_valid <= es_to_ms_valid;
    end
end

always @(posedge clk) begin
    if (es_to_ms_valid && ms_allowin) begin
        es_to_ms_data_r <= es_to_ms_data;
    end
end


// load
wire [1:0] ld_addr;
wire [7:0] mem_byte;
wire [15:0] mem_half;
wire [31:0] mem_word;
wire [31:0] ld_b_res;
wire [31:0] ld_h_res;
wire [31:0] ld_w_res;

assign ld_addr = ms_exe_result[1:0];
assign mem_byte = {8{!ld_addr[0] & !ld_addr[1]}} & data_sram_rdata[7:0]   |
                  {8{ ld_addr[0] & !ld_addr[1]}} & data_sram_rdata[15:8]  |
                  {8{!ld_addr[0] &  ld_addr[1]}} & data_sram_rdata[23:16] |
                  {8{ ld_addr[0] &  ld_addr[1]}} & data_sram_rdata[31:24] ;
assign mem_half = ld_addr[1] ? data_sram_rdata[31:16] : data_sram_rdata[15:0];
assign mem_word = data_sram_rdata;
assign ld_b_res[31:8] = {24{ms_inst_ld_b & mem_byte[7]}};
assign ld_b_res[7:0]  = mem_byte;
assign ld_h_res[31:16]= {16{ms_inst_ld_h & mem_half[15]}};
assign ld_h_res[15:0] = mem_half;
assign ld_w_res       = mem_word;

assign mem_result = {32{ms_inst_ld_b || ms_inst_ld_bu}} & ld_b_res |
                    {32{ms_inst_ld_h || ms_inst_ld_hu}} & ld_h_res |
                    {32{ms_inst_ld_w}}                  & ld_w_res ;

assign ms_final_result = ms_res_from_mem ? mem_result: ms_exe_result;

assign ms_fwd_valid = {4{ ms_valid && ms_gr_we && 1'b1 }};      // 1'b1 for data_sram_rvalid
assign ms_rf_dest   = ms_dest;
assign ms_rf_data   = ms_final_result;    
    
    
endmodule
