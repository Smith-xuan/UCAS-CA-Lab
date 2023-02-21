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

    output wire                            ws_csr_we_valid  ,
    output wire [31                    :0] ws_csr_wmask     ,
    output wire [31                    :0] ws_csr_wvalue    ,
    output wire [13                    :0] ws_csr_num       ,
    input  wire [31                    :0] ws_csr_rdata     ,
    output wire                            ws_ex_valid      ,
    output wire                            ws_ertn_flush_valid,
    output wire [`WS_CSR_BLK_DATA_WD -1:0] ws_csr_blk_data  ,
    output wire [5                     :0] wb_ecode         ,
    output wire [8                     :0] wb_esubcode      ,
    output wire [31                    :0] wb_vaddr         ,

    //trace debug interface
    output wire[31:0] debug_wb_pc     ,
    output wire[ 3:0] debug_wb_rf_we  ,
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
wire [31:0] ws_csr_rvalue;
wire        ws_csr_we;
wire        ws_ertn_flush;
wire        ws_inst_csr;
wire        ws_inst_csrxchg;
wire        ws_inst_csrwr;
wire        ws_csr_blk_valid;
wire [13:0] ws_csr_num_o;
wire        ws_ex_syscall;
wire        ws_ex_break;
wire        ws_ex_adef;
wire        ws_ex_ale;
wire        ws_ex_ine;
wire        ws_has_int;
wire        ws_inst_rdcntid;
wire        ws_inst_rdcntvl_w;
wire        ws_inst_rdcntvh_w;
wire [31:0] ws_vaddr;
wire [31:0] ws_rdtimer;

assign {
    ws_inst_csrxchg,
    ws_inst_csrwr,
    ws_inst_csr,
    ws_inst_rdcntid,
    ws_inst_rdcntvl_w,
    ws_inst_rdcntvh_w,
    ws_csr_num,
    ws_ertn_flush,
    ws_ex_syscall,
    ws_ex_break,
    ws_ex_adef,
    ws_ex_ale,
    ws_ex_ine,
    ws_has_int,
    ws_rdtimer,
    ws_vaddr,
    ws_csr_we,
    ws_csr_wvalue,
    ws_csr_wmask,
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

assign wb_vaddr = ws_vaddr;
assign ws_ready_go = 1'b1;
assign ws_allowin  = !ws_valid || ws_ready_go;
assign ws_csr_we_valid = ws_csr_we && ws_valid;
assign ws_ex_valid = (ws_ex_syscall | ws_ex_adef | ws_ex_ale | ws_ex_break | ws_has_int | ws_ex_ine) && ws_valid;
assign ws_ertn_flush_valid = ws_ertn_flush && ws_valid;
assign ws_csr_blk_valid = (ws_inst_csr | ws_inst_rdcntid | ws_inst_rdcntvl_w | ws_inst_rdcntvh_w) && ws_valid;
assign ws_csr_num_o = ws_csr_num;
assign ws_csr_blk_data = {ws_csr_blk_valid, ws_csr_num_o};

assign wb_ecode = {6{ws_ex_adef}} & 6'b001000    |
                  {6{ws_ex_ale}} & 6'b001001     |
                  {6{ws_ex_syscall}} & 6'b001011 |
                  {6{ws_ex_break}} & 6'b001100   |
                  {6{ws_ex_ine}} & 6'b001101     |
                  {6{ws_has_int}} & 6'b000000    ;

assign wb_esubcode = 9'b0;

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

assign rf_we    = {4{ ws_gr_we && ws_valid && !ws_ex_valid}};
assign rf_waddr = ws_dest;
assign rf_wdata = (ws_inst_csr | ws_inst_rdcntid) ? ws_csr_rdata : 
                  (ws_inst_rdcntvl_w | ws_inst_rdcntvh_w) ? ws_rdtimer : 
                  ws_final_result;

// debug info generate
assign debug_wb_pc       = ws_pc;
assign debug_wb_rf_we    = !(ws_ex_valid) & rf_we;
assign debug_wb_rf_wnum  = ws_dest;
assign debug_wb_rf_wdata = rf_wdata;
endmodule
