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
    output wire[`MS_FWD_BLK_DATA_WD -1:0] ms_fwd_blk_data,

    input  wire                            wb_ex        ,
    input  wire                            wb_ertn_flush,
    output wire                            ms_ex_valid  ,
    output wire                            ms_ertn_valid,
    output wire [`MS_CSR_BLK_DATA_WD -1:0] ms_csr_blk_data   
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
wire [31:0] ms_csr_rvalue;
wire        ms_res_from_csr;
wire [31:0] ms_csr_wvalue;
wire [31:0] ms_csr_wmask;
wire        ms_csr_we;
wire        ms_csr_we_valid;
wire        ms_ex_syscall;
wire        ms_ex_break  ;
wire        ms_ex_adef   ;
wire        ms_ex_ale    ;
wire        ms_ex_ine    ;
wire        ms_has_int   ;
wire        ms_ex_valid_o;
wire        ms_ertn;
wire [13:0] ms_csr_num;
wire        ms_inst_csr;
wire        ms_inst_csrwr;
wire        ms_inst_csrxchg;
wire        ms_csr_blk_valid;

wire        ms_inst_rdcntid;
wire        ms_inst_rdcntvl_w;
wire        ms_inst_rdcntvh_w;
wire [31:0] ms_rdtimer;

wire [31:0] ms_vaddr;

assign {
    ms_inst_csrxchg ,
    ms_inst_csrwr   ,
    ms_inst_csr     ,
    ms_inst_rdcntid ,
    ms_inst_rdcntvl_w,
    ms_inst_rdcntvh_w,
    ms_csr_num      ,
    ms_ertn         ,
    ms_ex_syscall   ,
    ms_ex_break     ,
    ms_ex_adef      ,
    ms_ex_ale       ,
    ms_ex_ine       ,
    ms_has_int      ,
    ms_rdtimer      ,
    ms_vaddr        ,
    ms_ex_valid_o   ,
    ms_csr_we       ,
    ms_csr_wvalue   ,  //139:108
    ms_csr_wmask    ,  // 107:76
    ms_inst_ld_b    ,  //75
    ms_inst_ld_bu   ,  //74
    ms_inst_ld_h    ,  //73
    ms_inst_ld_hu   ,  //72
    ms_inst_ld_w    ,  //71
    ms_res_from_mem ,  //70:70
    ms_gr_we        ,  //69:69
    ms_dest         ,  //68:64
    ms_exe_result   ,  //63:32
    ms_pc             //31:0
} = es_to_ms_data_r;

wire [31:0] mem_result;
wire [31:0] ms_final_result;

assign ms_to_ws_data = {
    ms_inst_csrxchg,  //225
    ms_inst_csrwr  ,  //223
    ms_inst_csr    ,  //222
    ms_inst_rdcntid,  //221
    ms_inst_rdcntvl_w, //220
    ms_inst_rdcntvh_w, //219
    ms_csr_num     ,  // 218:205
    ms_ertn        ,  //204
    ms_ex_syscall  ,  //203
    ms_ex_break    ,  //202
    ms_ex_adef     ,  //201
    ms_ex_ale      ,  //200
    ms_ex_ine      ,  //199
    ms_has_int     ,  //198
    ms_rdtimer     ,  //197:166
    ms_vaddr       ,  //165:134
    ms_csr_we_valid,  //133
    ms_csr_wvalue  ,  //132:102
    ms_csr_wmask   ,  //101:70
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

assign ms_ex_valid = (ms_ex_syscall | ms_ex_adef | ms_ex_ale | ms_ex_break | ms_has_int | ms_ex_ine) && ms_valid;
assign ms_csr_we_valid = ms_csr_we && ms_valid;
assign ms_ertn_valid = ms_ertn && ms_valid;

assign ms_ready_go    = 1'b1;
assign ms_allowin     = !ms_valid || ms_ready_go && ws_allowin;
assign ms_to_ws_valid = ms_valid && ms_ready_go && !wb_ex && !wb_ertn_flush;
assign ms_csr_blk_valid = !ms_res_from_mem && (ms_inst_csr | ms_inst_rdcntid | ms_inst_rdcntvl_w | ms_inst_rdcntvh_w) && ms_valid;
assign ms_csr_blk_data = {ms_csr_blk_valid, ms_csr_num};

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
