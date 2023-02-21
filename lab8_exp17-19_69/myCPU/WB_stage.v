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

module WB_stage#(
    parameter TLBNUM = 16
)(
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
    output wire                            ws_tlbsrch_blk   ,
    output wire [5                     :0] wb_ecode         ,
    output wire [8                     :0] wb_esubcode      ,
    output wire [31                    :0] wb_vaddr         ,

    output wire                            wb_ex_r          ,

    //trace debug interface
    output wire[31:0] debug_wb_pc      ,
    output wire[ 3:0] debug_wb_rf_we   ,
    output wire[ 4:0] debug_wb_rf_wnum ,
    output wire[31:0] debug_wb_rf_wdata,

    // write port
    output wire                     we,
    output wire[$clog2(TLBNUM)-1:0] w_index,
    output wire                     w_e,
    output wire[              18:0] w_vppn,
    output wire[               5:0] w_ps,
    output wire[               9:0] w_asid,
    output wire                     w_g,
    output wire[              19:0] w_ppn0,
    output wire[               1:0] w_plv0,
    output wire[               1:0] w_mat0,
    output wire                     w_d0,
    output wire                     w_v0,
    output wire[              19:0] w_ppn1,
    output wire[               1:0] w_plv1,
    output wire[               1:0] w_mat1,
    output wire                     w_d1,
    output wire                     w_v1,
    // read port
    output wire[$clog2(TLBNUM)-1:0] r_index,
    input  wire                     r_e,
    input  wire[              18:0] r_vppn,
    input  wire[               5:0] r_ps,
    input  wire[               9:0] r_asid,
    input  wire                     r_g,
    input  wire[              19:0] r_ppn0,
    input  wire[               1:0] r_plv0,
    input  wire[               1:0] r_mat0,
    input  wire                     r_d0,
    input  wire                     r_v0,
    input  wire[              19:0] r_ppn1,     
    input  wire[               1:0] r_plv1,
    input  wire[               1:0] r_mat1,
    input  wire                     r_d1,
    input  wire                     r_v1,
    //csr tlb
    output wire  [4 : 0] tlbop_bus_ws, //tlbsrch,tlbrd,tlbwr,tlbfill,invtlb
    output wire          csr_tlbrd_re,
    output wire  [31: 0] csr_tlbidx_wvalue_ws,
    output wire  [31: 0] csr_tlbehi_wvalue,
    output wire  [31: 0] csr_tlbelo0_wvalue,
    output wire  [31: 0] csr_tlbelo1_wvalue,
    output wire  [31: 0] csr_asid_wvalue,
    input wire [31: 0] csr_tlbidx_rvalue,
    input wire [31: 0] csr_tlbehi_rvalue,
    input wire [31: 0] csr_tlbelo0_rvalue,
    input wire [31: 0] csr_tlbelo1_rvalue,
    input wire [31: 0] csr_asid_rvalue
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
wire        ws_ex_adem;
wire        ws_ex_ale;
wire        ws_ex_ine;
wire        ws_has_int;
wire [5:0]  ws_tlb_es_bus;
wire        ws_inst_rdcntid;
wire        ws_inst_rdcntvl_w;
wire        ws_inst_rdcntvh_w;
wire [31:0] ws_vaddr;
wire [31:0] ws_rdtimer;
wire [4:0]  tlb_bus;
reg  [3:0]  tlbfill_index;

wire        ws_fake_ex_reflush;

assign {
    ws_ex_adem,
    ws_tlb_es_bus,
    ws_fake_ex_reflush,//+1
    tlb_bus,//+5
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
assign ws_csr_we_valid = ws_csr_we && ws_valid && !wb_ex_r;
assign ws_ex_valid = (ws_ex_syscall | ws_ex_adef |ws_ex_adem | ws_ex_ale | ws_ex_break | ws_has_int | ws_ex_ine | ws_fake_ex_reflush | (|ws_tlb_es_bus)) && ws_valid;
assign ws_ertn_flush_valid = ws_ertn_flush && ws_valid;
assign ws_csr_blk_valid = (ws_inst_csr | ws_inst_rdcntid | ws_inst_rdcntvl_w | ws_inst_rdcntvh_w) && ws_valid;
assign ws_csr_num_o = ws_csr_num;
assign ws_csr_blk_data = {ws_csr_blk_valid, ws_csr_num_o};
assign ws_tlbsrch_blk = ((ws_inst_csrwr || ws_inst_csrxchg) && (ws_csr_num == `CSR_ASID || ws_csr_num == `CSR_TLBEHI) || tlb_bus[3]) && ws_valid;

assign wb_ex_r = (ws_ex_syscall | ws_ex_adef | ws_ex_adem | ws_ex_ale | ws_ex_break | ws_has_int | ws_ex_ine | (|ws_tlb_es_bus)) && ws_valid;

assign wb_ecode = {6{ws_ex_adef}}       & 6'b001000 |
                  {6{ws_ex_adem}}       & 6'b001000 |
                  {6{ws_ex_ale}}        & 6'b001001 |
                  {6{ws_ex_syscall}}    & 6'b001011 |
                  {6{ws_ex_break}}      & 6'b001100 |
                  {6{ws_ex_ine}}        & 6'b001101 |
                  {6{ws_has_int}}       & 6'b000000 |
                  {6{ws_tlb_es_bus[0]}} & 6'b111111 |
                  {6{ws_tlb_es_bus[1]}} & 6'b000011 |
                  {6{ws_tlb_es_bus[2]}} & 6'b000001 |
                  {6{ws_tlb_es_bus[3]}} & 6'b000010 |
                  {6{ws_tlb_es_bus[4]}} & 6'b000111 |
                  {6{ws_tlb_es_bus[5]}} & 6'b000100;

assign wb_esubcode = ws_ex_adem ? 9'b1 : 9'b0;

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


//tlb
assign tlbop_bus_ws = tlb_bus & {5{ws_valid}};
assign csr_tlbrd_re = r_e & ws_valid;

assign we = tlb_bus[1] | tlb_bus[2];//wr fill
assign w_index = tlb_bus[2]?csr_tlbidx_rvalue[3:0]:(tlb_bus[1]?tlbfill_index[3:0]:4'b0);
assign w_e  = ~csr_tlbidx_rvalue[31];
assign w_vppn =  csr_tlbehi_rvalue[31:13];
assign w_ps = csr_tlbidx_rvalue[29:24];
assign w_asid = csr_asid_rvalue[9:0];
assign w_g = csr_tlbelo1_rvalue[6] & csr_tlbelo0_rvalue[6];
assign w_v0 = csr_tlbelo0_rvalue [0];
assign w_d0 = csr_tlbelo0_rvalue [1];
assign w_plv0 = csr_tlbelo0_rvalue [3:2];
assign w_mat0 = csr_tlbelo0_rvalue [5:4];
assign w_ppn0 = csr_tlbelo0_rvalue [31:8];
assign w_v1 = csr_tlbelo1_rvalue [0];
assign w_d1 = csr_tlbelo1_rvalue [1];
assign w_plv1 = csr_tlbelo1_rvalue [3:2];
assign w_mat1 = csr_tlbelo1_rvalue [5:4];
assign w_ppn1 = csr_tlbelo1_rvalue [31:8];

assign r_index = csr_tlbidx_rvalue[3:0];
assign csr_tlbidx_wvalue_ws = {~r_e,
                            1'b0,
                            r_ps,
                            20'b0,
                            4'b0
                            };
assign csr_tlbehi_wvalue = {r_vppn,
                            13'b0
                            };
assign csr_tlbelo0_wvalue = {r_ppn0,
                            1'b0,
                            r_g,
                            r_mat0,
                            r_plv0,
                            r_d0, 
                            r_v0
                            };
assign csr_tlbelo1_wvalue = {r_ppn1,
                            1'b0,
                            r_g,
                            r_mat1,
                            r_plv1,
                            r_d1,
                            r_v1                            
                            };
assign csr_asid_wvalue[9:0] = r_asid;

always @(posedge clk)begin
    if(reset)begin
        tlbfill_index <= 4'b0;
    end
    else if(tlb_bus[1]&ws_valid) begin
        if(tlbfill_index == 4'd15) begin
            tlbfill_index <= 4'b0;
        end
        else begin
            tlbfill_index <= tlbfill_index + 4'b1;
        end
    end
end
endmodule
