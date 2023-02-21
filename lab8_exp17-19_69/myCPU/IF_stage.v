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

module IF_stage#(
    parameter TLBNUM = 16
)(
    input  wire                  clk            ,
    input  wire                  reset          ,
    // allow in
    input  wire                  ds_allowin     ,
    // br_data
    input  wire[`BR_DATA_WD-1:0] br_blk_data    ,
    //to ds
    output wire                         fs_to_ds_valid,
    output wire[`FS_TO_DS_DATA_WD -1:0] fs_to_ds_data ,
    // inst sram interface
    output wire       inst_sram_req    ,// TODO
    output wire[31:0] inst_sram_addr   ,
    input wire        inst_sram_addr_ok,// TODO
    input wire[31:0]  inst_sram_rdata  ,
    input wire        inst_sram_data_ok,// TODO

    input wire [31:0] csr_pc       ,
    input wire        wb_ex        ,
    input wire        wb_ertn_flush,

    // search port 0 
    output wire[              18:0] s0_vppn,
    output wire                     s0_va_bit12,
    output wire[               9:0] s0_asid,
    input  wire                     s0_found,
    input  wire[$clog2(TLBNUM)-1:0] s0_index,
    input  wire[              19:0] s0_ppn,
    input  wire[               5:0] s0_ps,
    input  wire[               1:0] s0_plv,
    input  wire[               1:0] s0_mat,
    input  wire                     s0_d,
    input  wire                     s0_v,

    //csr_tlb
    input wire[31:0] csr_asid_rvalue,
    input wire[31:0] csr_crmd_rvalue,
    input wire[31:0] csr_dmw0_rvalue,
    input wire[31:0] csr_dmw1_rvalue,
    //tlb_reflush
    input wire       tlb_reflush
    );
    

reg         fs_valid;
wire        fs_ready_go;
wire        fs_allowin;
wire        to_fs_valid;
wire        to_fs_ready_go;
wire dir;


wire         blk_valid;

wire         br_taken;
wire [ 31:0] br_target;
assign {blk_valid,br_taken,br_target} = br_blk_data;

wire [5:0]  tlb_ex_bus;
wire [31:0] fs_inst;
reg  [31:0] fs_pc;
reg         fake_ex_reflush;
reg         ex_adef;
assign      fs_to_ds_data = {tlb_ex_bus ,fs_inst, fs_pc, ex_adef, fake_ex_reflush};
                       
reg         fs_inst_buff_valid;
reg  [31:0] fs_inst_buff;
reg         cancel;
reg         cancel_flag;
reg         br_cancel;
reg         br_cancel_flag;
wire        pfs_valid;
assign pfs_valid     = ~reset;
assign to_fs_valid    = pfs_valid && to_fs_ready_go;
assign to_fs_ready_go = inst_sram_req & inst_sram_addr_ok ;

assign fs_ready_go    = (!cancel && !br_cancel && inst_sram_data_ok) | fs_inst_buff_valid | (wb_ex | wb_ertn_flush/* | (|tlb_ex_bus)*/);
assign fs_allowin     = !fs_valid || fs_ready_go && ds_allowin;
assign fs_to_ds_valid =  fs_valid && fs_ready_go && !wb_ex && !wb_ertn_flush;


always @(posedge clk) begin
     if(reset) begin
        cancel_flag<=1'b0;
     end else if(fs_valid && !ds_allowin && inst_sram_data_ok) begin
        cancel_flag<=1'b1;
     end else if(ds_allowin) begin
        cancel_flag<=1'b0;
     end
end

always@(posedge clk) begin
     if(reset) begin
        cancel<=1'b0;
     end else if ((wb_ex | wb_ertn_flush) && fs_valid && (!inst_sram_data_ok || inst_sram_data_ok && br_cancel) && !cancel_flag)begin
        cancel<=1'b1;
     end else if(inst_sram_data_ok)begin
        cancel<=1'b0;
     end
end

always @(posedge clk) begin
     if(reset) begin
        br_cancel_flag<=1'b0;
     end else if(fs_valid && !ds_allowin && inst_sram_data_ok) begin
        br_cancel_flag<=1'b1;
     end else if(ds_allowin) begin
        br_cancel_flag<=1'b0;
     end
end

always@(posedge clk) begin
     if(reset) begin
        br_cancel<=1'b0;
     end else if (br_taken && fs_valid && (!inst_sram_data_ok || inst_sram_data_ok && cancel) && !br_cancel_flag)begin
        br_cancel<=1'b1;
     end else if(inst_sram_data_ok && !cancel)begin
        br_cancel<=1'b0;
     end
end

// pc control
wire [31:0] seq_pc;
wire [31:0] next_pc;
reg  [31:0] pc_buff_br;
reg         pc_buff_br_valid;
reg  [31:0] pc_buff_ex;
reg         pc_buff_ex_valid;
always @ (posedge clk) begin
    if (reset) begin
        pc_buff_br_valid  <= 1'b0;
        pc_buff_br        <= 32'h0;
        pc_buff_ex_valid  <= 1'b0;
        pc_buff_ex        <= 32'h0;
    end else if ( (wb_ex|wb_ertn_flush) && !to_fs_ready_go) begin
        pc_buff_ex_valid  <= 1'b1;
        pc_buff_ex        <= csr_pc;
    end else if (br_taken && !to_fs_ready_go) begin
        pc_buff_br_valid  <= 1'b1;
        pc_buff_br        <= br_target;
    end else if (to_fs_ready_go) begin
        pc_buff_br_valid  <= 1'b0;
        pc_buff_br        <= 32'h0;
        pc_buff_ex_valid  <= 1'b0;
        pc_buff_ex        <= 32'h0;
    end
end
assign next_pc = pc_buff_ex_valid    ?      pc_buff_ex :
                 wb_ex|wb_ertn_flush ?      csr_pc  :
                 pc_buff_br_valid    ?      pc_buff_br:
                 !fs_allowin         ?      fs_pc:
                 br_taken            ?      br_target:
                                            seq_pc;

assign seq_pc = fs_pc+32'h4;
    
                                       
assign inst_sram_req   = pfs_valid & fs_allowin;
//assign inst_sram_addr  = next_pc;  in the v2p moudle
assign fs_inst         = fs_inst_buff_valid ? fs_inst_buff : inst_sram_rdata;

always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end 
    else if(br_taken && ds_allowin)begin
        fs_valid<=1'b0;
    end
    
end

always @(posedge clk) begin
    if (reset) begin
        fs_pc <= 32'h_1bfffffc;  //trick: to make next_pc be 0x1c000000 during reset 
    end
    else if (to_fs_valid && (fs_allowin || wb_ex || wb_ertn_flush || {|tlb_ex_bus})) begin
        fs_pc <= next_pc;
    end
   
    if (next_pc[1:0] != 2'b00 || next_pc >= `ADDR_MAX && !dir) begin
        ex_adef <= 1'b1;
    end
    else if (next_pc[1:0] == 2'b00 || next_pc < `ADDR_MAX || next_pc >= `ADDR_MAX && dir) begin
        ex_adef <= 1'b0;
    end

    if (reset) begin
        fake_ex_reflush <= 1'b0;
    end
    else if (tlb_reflush) begin
        fake_ex_reflush <= 1'b1;
    end
    else if (fs_ready_go & ds_allowin) begin
        fake_ex_reflush <= 1'b0;
    end
end

always @ (posedge clk) begin
    if (reset) begin
        fs_inst_buff_valid  <= 1'b0;
        fs_inst_buff        <= 32'h0;
    end else if (!fs_inst_buff_valid && fs_valid && inst_sram_data_ok && !cancel && !br_cancel && !ds_allowin) begin
        fs_inst_buff_valid  <= 1'b1;
        fs_inst_buff        <= inst_sram_rdata;
    end else if (ds_allowin || wb_ertn_flush || wb_ex) begin
        fs_inst_buff_valid  <= 1'b0;
        fs_inst_buff        <= 32'h0;
        end
end
//v2p
vaddr_transfer inst_transfer(
    .va        (next_pc),
    .inst_op   (3'b001),//{load.store,if}
    .pa        (inst_sram_addr),
    .tlb_ex_bus(tlb_ex_bus),//{PME,PPE,PIS,PIL,PIF,TLBR}
    .adem_judge(dir),
    //tlb
    .s_vppn    (s0_vppn),
    .s_va_bit12(s0_va_bit12),
    .s_asid    (s0_asid),
    .s_found   (s0_found),
    .s_index   (s0_index),
    .s_ppn     (s0_ppn),
    .s_ps      (s0_ps),
    .s_plv     (s0_plv),
    .s_mat     (s0_mat),
    .s_d       (s0_d),
    .s_v       (s0_v),
    //crmd
    .csr_asid  (csr_asid_rvalue),
    .csr_crmd  (csr_crmd_rvalue),
    //dmw
    .dmw_hit   (dmw_hit),
    .csr_dmw0  (csr_dmw0_rvalue),
    .csr_dmw1  (csr_dmw1_rvalue)
    
);

endmodule
