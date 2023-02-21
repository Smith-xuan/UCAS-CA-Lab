`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/09/11 15:23:04
// Design Name: 
// Module Name: EX_stage
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
module EX_stage(
    input  wire                        clk           ,
    input  wire                        reset         ,
    //allowin
    input  wire                        ms_allowin    ,
    output wire                        es_allowin    ,
    //from ds
    input  wire                        ds_to_es_valid,
    input  wire[`DS_TO_ES_DATA_WD -1:0] ds_to_es_data,
    //to ms
    output wire                        es_to_ms_valid,
    output wire[`ES_TO_MS_DATA_WD -1:0] es_to_ms_data,
    // data sram interface
    output wire [ 3:0] data_sram_we   ,
    output wire [31:0] data_sram_addr ,
    output wire [31:0] data_sram_wdata,

    // forword & block from es
    output wire [`ES_FWD_BLK_DATA_WD -1:0] es_fwd_blk_data 
    );

reg         es_valid      ;
wire        es_ready_go   ;

reg  [`DS_TO_ES_DATA_WD -1:0] ds_to_es_data_r;

wire [11:0] es_alu_op     ;
wire        es_load_op    ;
wire        es_src1_is_sa ;  
wire        es_src1_is_pc ;
wire        es_src2_is_imm; 
wire        es_gr_we      ;
wire        es_mem_we     ;
wire [ 4:0] es_dest       ;
wire [31:0] es_imm        ;
wire [31:0] es_rj_value   ;
wire [31:0] es_rkd_value  ;
wire [31:0] es_pc         ;
wire        es_inst_mul_w ;
wire        es_inst_mulh_w;
wire        es_inst_mulh_wu;
wire        es_inst_div_w ;
wire        es_inst_mod_w ;
wire        es_inst_div_wu;
wire        es_inst_mod_wu;
wire        es_inst_st_b  ;
wire        es_inst_st_h  ;
wire        es_inst_st_w  ;
wire        es_inst_ld_b  ;
wire        es_inst_ld_bu ;
wire        es_inst_ld_h  ;
wire        es_inst_ld_hu ;
wire        es_inst_ld_w  ;

assign {
    es_inst_mul_w  ,
    es_inst_mulh_w ,
    es_inst_mulh_wu,
    es_inst_div_w  ,
    es_inst_mod_w  ,
    es_inst_div_wu ,
    es_inst_mod_wu ,
    es_inst_st_b   ,
    es_inst_st_h   ,
    es_inst_st_w   ,
    es_inst_ld_b   ,
    es_inst_ld_bu  ,
    es_inst_ld_h   ,
    es_inst_ld_hu  ,
    es_inst_ld_w   ,
    es_alu_op      ,  
    es_load_op     ,  
    es_src1_is_pc  ,  
    es_src2_is_imm ,  
    es_gr_we       ,  
    es_mem_we      ,  
    es_dest        ,  
    es_imm         ,  
    es_rj_value    ,  
    es_rkd_value   ,  
    es_pc             
} = ds_to_es_data_r;

wire [31:0] es_alu_src1   ;
wire [31:0] es_alu_src2   ;
wire [31:0] es_alu_result ;
wire [31:0] es_exe_result ;

wire    es_res_from_mem;

assign es_res_from_mem  = es_load_op;
assign es_to_ms_data = {
    es_inst_ld_b    ,
    es_inst_ld_bu   ,
    es_inst_ld_h    ,
    es_inst_ld_hu   ,
    es_inst_ld_w    ,
    es_res_from_mem ,  //70:70
    es_gr_we        ,  //69:69
    es_dest         ,  //68:64
    es_exe_result   ,  //63:32
    es_pc              //31:0
};

wire [ 3:0] es_fwd_valid;
wire [ 4:0] es_rf_dest  ;
wire [31:0] es_rf_data  ;
wire        es_blk_valid;

assign es_fwd_blk_data = {
    es_fwd_valid,   // 41:38
    es_rf_dest,     // 37:33
    es_rf_data,     // 32:1
    es_blk_valid    // 0:0
};

assign es_alu_src1 = es_src1_is_pc  ? es_pc  :
                                      es_rj_value;
assign es_alu_src2 = es_src2_is_imm ? es_imm : 
                                      es_rkd_value;
alu u_alu(
    .alu_op     (es_alu_op    ),
    .alu_src1   (es_alu_src1  ),
    .alu_src2   (es_alu_src2  ),
    .alu_result (es_alu_result)
    );
// Mul & Mulu
wire [31:0] mul_src1;
wire [31:0] mul_src2;
wire [63:0] unsigned_mul_res;
wire [63:0] signed_mul_res;

assign mul_src1 = es_rj_value;
assign mul_src2 = es_rkd_value;
assign unsigned_mul_res = mul_src1 * mul_src2;
assign signed_mul_res   = $signed(mul_src1) * $signed(mul_src2);

// store
wire [1:0] st_addr;
wire [3:0] st_op;
wire [3:0] st_b_op;
wire [3:0] st_h_op;
wire [3:0] st_w_op;
wire [31:0] st_data;

assign st_addr = es_alu_result[1:0];
assign st_b_op = {4{(!st_addr[0] & !st_addr[1])}} & 4'b0001 |
                 {4{( st_addr[0] & !st_addr[1])}} & 4'b0010 |
                 {4{(!st_addr[0] &  st_addr[1])}} & 4'b0100 |
                 {4{( st_addr[0] &  st_addr[1])}} & 4'b1000 ;
assign st_h_op = (!st_addr[0] & !st_addr[1]) ? 4'b0011 : 4'b1100;
assign st_w_op = 4'b1111;

assign st_op   = {4{es_inst_st_b}} & st_b_op |
                 {4{es_inst_st_h}} & st_h_op |
                 {4{es_inst_st_w}} & st_w_op ;

assign st_data = es_inst_st_b ? {4{es_rkd_value[7:0]}} :
                 es_inst_st_h ? {2{es_rkd_value[15:0]}}:
                                    es_rkd_value[31:0] ;


assign data_sram_we    = (es_valid & es_mem_we) ? st_op : 4'h0;
assign data_sram_addr  = es_alu_result;
assign data_sram_wdata = st_data;

// Block & Forward
assign es_fwd_valid = {4{ es_valid && es_gr_we && !es_res_from_mem  }};
assign es_rf_dest   = es_dest;
assign es_rf_data   = es_exe_result;

wire  es_res_from_mul;
wire  es_res_from_mod;
wire  es_res_from_div;
wire[31:0]  reg_mul_rdata;
wire[31:0]  reg_div_rdata;
wire[31:0]  reg_mod_rdata;
assign reg_mul_rdata = es_inst_mul_w ? signed_mul_res[31:0] :
                       es_inst_mulh_w? signed_mul_res[63:32]:
                    /*es_inst_mulh_wu*/ unsigned_mul_res[63:32];
assign reg_div_rdata = es_inst_div_w ? signed_divider_res[63:32]:
                                        unsigned_divider_res[63:32];
assign reg_mod_rdata = es_inst_mod_w ? signed_divider_res[31:0]: 
                                        unsigned_divider_res[31:0];
assign es_res_from_mul = es_inst_mul_w | es_inst_mulh_w | es_inst_mulh_wu ;
assign es_res_from_mod = es_inst_mod_w | es_inst_mod_wu;
assign es_res_from_div = es_inst_div_w | es_inst_div_wu;

assign es_exe_result = 
    es_res_from_mul  ? reg_mul_rdata  :
    es_res_from_div  ? reg_div_rdata  :
    es_res_from_mod  ? reg_mod_rdata  :
    es_alu_result;
    
assign es_blk_valid = es_valid && es_res_from_mem ;

assign es_ready_go    = 
    (es_inst_div_w|es_inst_mod_w)     ? signed_dout_tvalid   :
    (es_inst_div_wu|es_inst_mod_wu)   ? unsigned_dout_tvalid :
    1'b1;
assign es_allowin     = !es_valid || es_ready_go && ms_allowin;
assign es_to_ms_valid =  es_valid && es_ready_go;

// Div & Divu
wire [31:0] divider_dividend;
wire [31:0] divider_divisor;
wire [63:0] unsigned_divider_res;
wire [63:0] signed_divider_res;

assign divider_dividend = es_rj_value;
assign divider_divisor  = es_rkd_value;

wire unsigned_dividend_tready;
wire unsigned_dividend_tvalid;
wire unsigned_divisor_tready;
wire unsigned_divisor_tvalid;
wire unsigned_dout_tvalid;

wire signed_dividend_tready;
wire signed_dividend_tvalid;
wire signed_divisor_tready;
wire signed_divisor_tvalid;
wire signed_dout_tvalid;

u_unsigned_div u_unsigned_divider (
    .aclk                   (clk),
    .s_axis_dividend_tdata  (divider_dividend),
    .s_axis_dividend_tready (unsigned_dividend_tready),
    .s_axis_dividend_tvalid (unsigned_dividend_tvalid),
    .s_axis_divisor_tdata   (divider_divisor),
    .s_axis_divisor_tready  (unsigned_divisor_tready),
    .s_axis_divisor_tvalid  (unsigned_divisor_tvalid),
    .m_axis_dout_tdata      (unsigned_divider_res),
    .m_axis_dout_tvalid     (unsigned_dout_tvalid)
);

u_signed_div u_signed_divider (
    .aclk                   (clk),
    .s_axis_dividend_tdata  (divider_dividend),
    .s_axis_dividend_tready (signed_dividend_tready),
    .s_axis_dividend_tvalid (signed_dividend_tvalid),
    .s_axis_divisor_tdata   (divider_divisor),
    .s_axis_divisor_tready  (signed_divisor_tready),
    .s_axis_divisor_tvalid  (signed_divisor_tvalid),
    .m_axis_dout_tdata      (signed_divider_res),
    .m_axis_dout_tvalid     (signed_dout_tvalid)
);

// Divider status control
reg  unsigned_dividend_sent;
reg  unsigned_divisor_sent;

assign unsigned_dividend_tvalid = es_valid && (es_inst_div_wu | es_inst_mod_wu) && !unsigned_dividend_sent;
assign unsigned_divisor_tvalid  = es_valid && (es_inst_div_wu | es_inst_mod_wu) && !unsigned_divisor_sent;

always @ (posedge clk) begin
    if (reset) begin
        unsigned_dividend_sent <= 1'b0;
    end else if (unsigned_dividend_tready && unsigned_dividend_tvalid) begin
        unsigned_dividend_sent <= 1'b1;
    end else if (es_ready_go && ms_allowin) begin
        unsigned_dividend_sent <= 1'b0;
    end
    
    if (reset) begin
        unsigned_divisor_sent <= 1'b0;
    end else if (unsigned_divisor_tready && unsigned_divisor_tvalid) begin
        unsigned_divisor_sent <= 1'b1;
    end else if (es_ready_go && ms_allowin) begin
        unsigned_divisor_sent <= 1'b0;
    end
end

reg  signed_dividend_sent;
reg  signed_divisor_sent;

assign signed_dividend_tvalid = es_valid && (es_inst_div_w | es_inst_mod_w) && !signed_dividend_sent;
assign signed_divisor_tvalid  = es_valid && (es_inst_div_w | es_inst_mod_w) && !signed_divisor_sent;

always @ (posedge clk) begin
    if (reset) begin
        signed_dividend_sent <= 1'b0;
    end else if (signed_dividend_tready && signed_dividend_tvalid) begin
        signed_dividend_sent <= 1'b1;
    end else if (es_ready_go && ms_allowin) begin
        signed_dividend_sent <= 1'b0;
    end
    
    if (reset) begin
        signed_divisor_sent <= 1'b0;
    end else if (signed_divisor_tready && signed_divisor_tvalid) begin
        signed_divisor_sent <= 1'b1;
    end else if (es_ready_go && ms_allowin) begin
        signed_divisor_sent <= 1'b0;
    end
end

always @(posedge clk) begin
    if (reset) begin
        es_valid <= 1'b0;
    end
    else if (es_allowin) begin
        es_valid <= ds_to_es_valid;
    end
end

always @(posedge clk) begin
    if (ds_to_es_valid && es_allowin) begin
        ds_to_es_data_r <= ds_to_es_data;
    end
end
endmodule