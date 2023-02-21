`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/09/11 16:05:51
// Design Name: 
// Module Name: ID_stage
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
module ID_stage(
    input  wire                        clk           ,
    input  wire                        reset         ,
    //allowin
    input  wire                        es_allowin    ,
    output wire                        ds_allowin    ,
    //from fs
    input  wire                        fs_to_ds_valid,
    input  wire[`FS_TO_DS_DATA_WD -1:0] fs_to_ds_data  ,
    //to es
    output wire                        ds_to_es_valid,
    output wire[`DS_TO_ES_DATA_WD -1:0] ds_to_es_data  ,
    //to fs
    output wire[`BR_DATA_WD       -1:0] br_blk_data       ,
    //to rf: for write back
    input  wire[`WS_TO_RF_DATA_WD -1:0] ws_to_rf_data  ,
    
    // forword & block from es
    input  wire[`ES_FWD_BLK_DATA_WD -1:0] es_fwd_blk_data,
    // forword & block from ms
    input  wire[`MS_FWD_BLK_DATA_WD -1:0] ms_fwd_blk_data
    );
    
reg         ds_valid   ;
wire        ds_ready_go;
reg  [`FS_TO_DS_DATA_WD -1:0] fs_to_ds_data_r;
wire [31:0] ds_inst;
wire [31:0] ds_pc  ;
assign {
    ds_inst,
    ds_pc
} = fs_to_ds_data_r;

wire [ 3:0] rf_we;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;
wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;
assign {
    rf_we   ,  //40:37          ?
    rf_waddr,  //36:32
    rf_wdata   //31:0
} = ws_to_rf_data;


// forward & block

wire [ 3:0] es_fwd_valid;
wire [ 4:0] es_rf_dest;
wire [31:0] es_rf_data;
wire        es_blk_valid;
wire        es_res_from_mem;
assign {
    es_fwd_valid,   // 41:38    ?
    es_rf_dest,     // 37:33
    es_rf_data,     // 32:1
    es_blk_valid    // 0:0
} = es_fwd_blk_data;

wire [ 3:0] ms_fwd_valid;
wire [ 4:0] ms_rf_dest;
wire [31:0] ms_rf_data;
assign {
    ms_fwd_valid,   // 40:37
    ms_rf_dest,     // 36:32
    ms_rf_data      // 31:0
} = ms_fwd_blk_data;
wire        blk_valid;
wire        br_taken;
wire [31:0] br_target;

wire [11:0] alu_op;
wire        load_op;
wire        src1_is_pc;
wire        src2_is_imm;
wire        res_from_mem;
wire        gr_we;
wire        mem_we;
wire [ 4:0] dest;
wire [31:0] imm;
wire [31:0] rj_value;
wire [31:0] rkd_value;
wire [31:0] br_offs;
wire [31:0] jirl_offs;

wire [ 5:0] op_31_26;
wire [ 3:0] op_25_22;
wire [ 1:0] op_21_20;
wire [ 4:0] op_19_15;
wire [ 4:0] rd;
wire [ 4:0] rj;
wire [ 4:0] rk;
wire [11:0] i12;
wire [19:0] i20;
wire [15:0] i16;
wire [25:0] i26;

wire [63:0] op_31_26_d;
wire [15:0] op_25_22_d;
wire [ 3:0] op_21_20_d;
wire [31:0] op_19_15_d;

wire        inst_add_w;
wire        inst_sub_w;
wire        inst_slt;
wire        inst_sltu;
wire        inst_slti;
wire        inst_sltiu;
wire        inst_nor;
wire        inst_and;
wire        inst_or;
wire        inst_xor;
wire        inst_andi;
wire        inst_ori;
wire        inst_xori;
wire        inst_sll_w;
wire        inst_srl_w;
wire        inst_sra_w;
wire        inst_pcaddu12i;
wire        inst_slli_w;
wire        inst_srli_w;
wire        inst_srai_w;
wire        inst_addi_w;
wire        inst_ld_b;
wire        inst_ld_h;
wire        inst_ld_bu;
wire        inst_ld_hu;
wire        inst_ld_w;
wire        inst_st_b;
wire        inst_st_h;
wire        inst_st_w;
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_blt;
wire        inst_bge;
wire        inst_bltu;
wire        inst_bgeu;
wire        inst_lu12i_w;
wire        inst_mul_w;
wire        inst_mulh_w;
wire        inst_mulh_wu;
wire        inst_div_w;
wire        inst_mod_w;
wire        inst_div_wu;
wire        inst_mod_wu;


wire        need_ui5;
wire        need_si12;
wire        need_si16;
wire        need_si20;
wire        need_si26;
wire        src2_is_4;
wire        src_reg_is_rd;
wire        dst_is_r1;
wire        rj_eq_rd;
assign op_31_26  = ds_inst[31:26];
assign op_25_22  = ds_inst[25:22];
assign op_21_20  = ds_inst[21:20];
assign op_19_15  = ds_inst[19:15];

assign rd   = ds_inst[ 4: 0];
assign rj   = ds_inst[ 9: 5];
assign rk   = ds_inst[14:10];

assign i12  = ds_inst[21:10];
assign i20  = ds_inst[24: 5];
assign i16  = ds_inst[25:10];
assign i26  = {ds_inst[ 9: 0], ds_inst[25:10]};

decoder_6_64 u_dec0(.in(op_31_26 ), .out(op_31_26_d ));
decoder_4_16 u_dec1(.in(op_25_22 ), .out(op_25_22_d ));
decoder_2_4  u_dec2(.in(op_21_20 ), .out(op_21_20_d ));
decoder_5_32 u_dec3(.in(op_19_15 ), .out(op_19_15_d ));

assign inst_add_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h00];
assign inst_sub_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h02];
assign inst_slt    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h04];
assign inst_sltu   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h05];

assign inst_nor    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h08];
assign inst_and    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h09];
assign inst_or     = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0a];
assign inst_xor    = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0b];

assign inst_mul_w   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h18];
assign inst_mulh_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h19];
assign inst_mulh_wu = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h1a];
assign inst_div_w   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h00];
assign inst_mod_w   = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h01];
assign inst_div_wu  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h02];
assign inst_mod_wu  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h2] & op_19_15_d[5'h03];

assign inst_sll_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0e];
assign inst_srl_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h0f];
assign inst_sra_w  = op_31_26_d[6'h00] & op_25_22_d[4'h0] & op_21_20_d[2'h1] & op_19_15_d[5'h10];

assign inst_slli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h01];
assign inst_srli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h09];
assign inst_srai_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h11];

assign inst_slti   = op_31_26_d[6'h00] & op_25_22_d[4'h8];
assign inst_sltui  = op_31_26_d[6'h00] & op_25_22_d[4'h9];
assign inst_addi_w = op_31_26_d[6'h00] & op_25_22_d[4'ha];
assign inst_andi   = op_31_26_d[6'h00] & op_25_22_d[4'hd];
assign inst_ori    = op_31_26_d[6'h00] & op_25_22_d[4'he];
assign inst_xori   = op_31_26_d[6'h00] & op_25_22_d[4'hf];

assign inst_ld_b   = op_31_26_d[6'h0a] & op_25_22_d[4'h0];
assign inst_ld_h   = op_31_26_d[6'h0a] & op_25_22_d[4'h1];
assign inst_ld_bu  = op_31_26_d[6'h0a] & op_25_22_d[4'h8];
assign inst_ld_hu  = op_31_26_d[6'h0a] & op_25_22_d[4'h9];
assign inst_ld_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_b   = op_31_26_d[6'h0a] & op_25_22_d[4'h4];
assign inst_st_h   = op_31_26_d[6'h0a] & op_25_22_d[4'h5];
assign inst_st_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h6];

assign inst_jirl   = op_31_26_d[6'h13];
assign inst_b      = op_31_26_d[6'h14];
assign inst_bl     = op_31_26_d[6'h15];
assign inst_beq    = op_31_26_d[6'h16];
assign inst_bne    = op_31_26_d[6'h17];
assign inst_blt    = op_31_26_d[6'h18];
assign inst_bge    = op_31_26_d[6'h19];
assign inst_bltu   = op_31_26_d[6'h1a];
assign inst_bgeu   = op_31_26_d[6'h1b];

assign inst_lu12i_w= op_31_26_d[6'h05] & ~ds_inst[25];
assign inst_pcaddu12i= op_31_26_d[6'h07] & ~ds_inst[25];

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_st_w
                    | inst_ld_b | inst_ld_h | inst_ld_bu | inst_ld_hu
                    | inst_st_b | inst_st_h
                    | inst_jirl | inst_bl | inst_pcaddu12i;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt | inst_slti;
assign alu_op[ 3] = inst_sltu | inst_sltui;
assign alu_op[ 4] = inst_and | inst_andi;
assign alu_op[ 5] = inst_nor ;
assign alu_op[ 6] = inst_or | inst_ori;
assign alu_op[ 7] = inst_xor | inst_xori;
assign alu_op[ 8] = inst_slli_w |inst_sll_w;
assign alu_op[ 9] = inst_srli_w | inst_srl_w;
assign alu_op[10] = inst_srai_w | inst_sra_w;
assign alu_op[11] = inst_lu12i_w;

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_si12  =  inst_addi_w | inst_ld_w | inst_st_w | inst_ld_b 
                    | inst_ld_h | inst_ld_bu | inst_ld_hu | inst_st_b 
                    | inst_st_h | inst_slti |inst_sltui;
assign need_ui12  =  inst_andi | inst_ori | inst_xori;
assign need_si16  =  inst_jirl | inst_beq | inst_bne;
assign need_si20  =  inst_lu12i_w | inst_pcaddu12i;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign imm = src2_is_4 ? 32'h4                      :
             need_si20 ? {i20[19:0], 12'b0}         :
             need_ui12 ? {20'b0,i12 }               :
/*need_ui5 || need_si12*/{{20{i12[11]}}, i12[11:0]} ;

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};

assign src_reg_is_rd = inst_beq | inst_bne | inst_st_w | inst_st_b | inst_st_h | inst_blt | inst_bltu | inst_bge | inst_bgeu;

assign src1_is_pc    = inst_jirl | inst_bl | inst_pcaddu12i;

assign src2_is_imm   = inst_slli_w | inst_slti | inst_st_b | 
                       inst_srli_w | inst_sltui | inst_ld_hu |
                       inst_srai_w | inst_andi  | inst_ld_h |
                       inst_addi_w | inst_ori  | inst_ld_bu |
                       inst_ld_w   | inst_xori | inst_ld_b |
                       inst_st_w   | inst_pcaddu12i | inst_st_h |
                       inst_lu12i_w|
                       inst_jirl   |
                       inst_bl     ;
assign load_op       = inst_ld_w | inst_ld_b | inst_ld_bu | inst_ld_hu | inst_ld_h;
assign res_from_mem  = load_op;
assign dst_is_r1     = inst_bl;
assign gr_we         = ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_b & ~inst_blt &
                        ~inst_st_b & ~inst_st_h & ~inst_bltu & ~inst_bge & ~inst_bgeu;
assign mem_we        = inst_st_w | inst_st_b | inst_st_h;
assign dest          = dst_is_r1 ? 5'd1 : rd;

assign blk_valid     =  !ds_ready_go;
assign br_blk_data   = { blk_valid,br_taken,br_target};
assign ds_to_es_data = {
    inst_mul_w  ,     
    inst_mulh_w ,
    inst_mulh_wu,
    inst_div_w  ,
    inst_mod_w  ,
    inst_div_wu ,
    inst_mod_wu ,
    inst_st_b   ,
    inst_st_h   ,
    inst_st_w   ,
    inst_ld_b   ,
    inst_ld_bu  ,
    inst_ld_h   ,
    inst_ld_hu  ,
    inst_ld_w   ,
    alu_op      ,  
    load_op     ,  //137
    src1_is_pc  ,  //136
    src2_is_imm ,  //135
    gr_we       ,  //134
    mem_we      ,  //133
    dest        ,  //132:128
    imm         ,  //127:96
    rj_value    ,  //95 :64
    rkd_value   ,  //63 :32
    ds_pc          //31 :0
};


assign rf_raddr1 = rj;
assign rf_raddr2 = src_reg_is_rd ? rd :rk;

regfile u_regfile(
    .clk    (clk      ),
    .raddr1 (rf_raddr1),
    .rdata1 (rf_rdata1),
    .raddr2 (rf_raddr2),
    .rdata2 (rf_rdata2),
    .we     (rf_we    ),
    .waddr  (rf_waddr ),
    .wdata  (rf_wdata )
    );

assign rj_value = 
    (es_fwd_valid && (es_rf_dest == rf_raddr1&&es_rf_dest!=5'b0))? es_rf_data :
    (ms_fwd_valid && (ms_rf_dest == rf_raddr1&&ms_rf_dest!=5'b0))? ms_rf_data :
    (rf_we        && (rf_waddr   == rf_raddr1&&rf_waddr!=5'b0))? rf_wdata   :
    rf_rdata1;
assign rkd_value = 
    (es_fwd_valid && (es_rf_dest == rf_raddr2&&es_rf_dest!=5'b0))? es_rf_data :
    (ms_fwd_valid && (ms_rf_dest == rf_raddr2&&ms_rf_dest!=5'b0))? ms_rf_data :
    (rf_we        && (rf_waddr   == rf_raddr2&&rf_waddr!=5'b0))? rf_wdata   :
    rf_rdata2;
    
assign rj_eq_rd = (rj_value == rkd_value);



wire [32:0] sub_result;
wire        rj_lt_rd;
wire        rj_ltu_rd;

assign sub_result = {1'b0, rj_value} + {1'b0, ~rkd_value} + 1'b1;


// SLT result
assign rj_lt_rd = (rj_value[31] & ~rkd_value[31])
                        | ((rj_value[31] ~^ rkd_value[31]) & sub_result[31]);

// SLTU result
assign rj_ltu_rd = ~sub_result[32];






assign br_taken = (  (inst_beq  &  rj_eq_rd)
                   | (inst_bne  & !rj_eq_rd)
                   | inst_jirl
                   | inst_bl
                   | inst_b
                   | (inst_blt & rj_lt_rd)
                   | (inst_bltu & rj_ltu_rd)
                   | (inst_bge & !rj_lt_rd)
                   | (inst_bgeu & !rj_ltu_rd)
                  )&&ds_valid;
assign br_target = (inst_beq || inst_bne || inst_bl || inst_b || inst_bge || inst_blt || inst_bgeu || inst_bltu) ? (ds_pc + br_offs) :
                                                   /*inst_jirl*/ (rj_value + jirl_offs);
assign ds_ready_go    = !(
    es_blk_valid  && (es_rf_dest == rf_raddr1 || es_rf_dest == rf_raddr2)
);
//assign ds_ready_go    = 1'b1;
assign ds_allowin     = (!ds_valid || ds_ready_go && es_allowin);
assign ds_to_es_valid = ds_valid && ds_ready_go;


always @(posedge clk) begin
    if (reset) begin
        ds_valid <= 1'b0;
    end else if (br_taken && ds_ready_go )begin
        ds_valid<= 1'b0;
    end else if (ds_allowin) begin
        ds_valid <= fs_to_ds_valid;
    end

    if (fs_to_ds_valid && ds_allowin) begin
        fs_to_ds_data_r <= fs_to_ds_data;
    end
end
endmodule