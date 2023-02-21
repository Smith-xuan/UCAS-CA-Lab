`timescale 1ns / 1ps
//////////////////////////////////////////////////////////////////////////////////
// Company: 
// Engineer: 
// 
// Create Date: 2022/11/16 15:32:14
// Design Name: 
// Module Name: tlb
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


module tlb
#(
    parameter TLBNUM = 16
)
(
    input wire clk,
    // search port 0 (for fetch)
    input wire [ 18:0] s0_vppn,
    input wire s0_va_bit12,
    input wire [ 9:0] s0_asid,
    output wire s0_found,
    output wire [$clog2(TLBNUM)-1:0] s0_index,
    output wire [ 19:0] s0_ppn,
    output wire [ 5:0] s0_ps,
    output wire [ 1:0] s0_plv,
    output wire [ 1:0] s0_mat,
    output wire s0_d,
    output wire s0_v,
    // search port 1 (for load/store)
    input wire [ 18:0] s1_vppn,
    input wire s1_va_bit12,
    input wire [ 9:0] s1_asid,
    output wire s1_found,
    output wire [$clog2(TLBNUM)-1:0] s1_index,
    output wire [ 19:0] s1_ppn,
    output wire [ 5:0] s1_ps,
    output wire [ 1:0] s1_plv,
    output wire [ 1:0] s1_mat,
    output wire s1_d,
    output wire s1_v,
    // invtlb opcode
    input wire invtlb_valid,
    input wire [ 4:0] invtlb_op,
    // write port
    input wire we, //w(rite) e(nable)
    input wire [$clog2(TLBNUM)-1:0] w_index,
    input wire w_e,
    input wire [ 18:0] w_vppn,
    input wire [ 5:0] w_ps,
    input wire [ 9:0] w_asid,
    input wire w_g,
    input wire [ 19:0] w_ppn0,
    input wire [ 1:0] w_plv0,
    input wire [ 1:0] w_mat0,
    input wire w_d0,
    input wire w_v0,
    input wire [ 19:0] w_ppn1,
    input wire [ 1:0] w_plv1,
    input wire [ 1:0] w_mat1,
    input wire w_d1,
    input wire w_v1,
    // read port
    input wire [$clog2(TLBNUM)-1:0] r_index,
    output wire r_e,
    output wire [ 18:0] r_vppn,
    output wire [ 5:0] r_ps,
    output wire [ 9:0] r_asid,
    output wire r_g,
    output wire [ 19:0] r_ppn0,
    output wire [ 1:0] r_plv0,
    output wire [ 1:0] r_mat0,
    output wire r_d0,
    output wire r_v0,
    output wire [ 19:0] r_ppn1,
    output wire [ 1:0] r_plv1,
    output wire [ 1:0] r_mat1,
    output wire r_d1,
    output wire r_v1
);

//TLB_regfile
reg [TLBNUM-1:0]     tlb_e;
reg [TLBNUM-1:0]     tlb_ps4MB; //pagesize 1:4MB, 0:4KB
reg [ 18:0]          tlb_vppn [TLBNUM-1:0];
reg [ 9:0]           tlb_asid [TLBNUM-1:0];
reg                  tlb_g [TLBNUM-1:0];

reg [ 19:0]          tlb_ppn0 [TLBNUM-1:0];
reg [ 1:0]           tlb_plv0 [TLBNUM-1:0];
reg [ 1:0]           tlb_mat0 [TLBNUM-1:0];
reg                  tlb_d0 [TLBNUM-1:0];
reg                  tlb_v0 [TLBNUM-1:0];

reg [ 19:0]          tlb_ppn1 [TLBNUM-1:0];
reg [ 1:0]           tlb_plv1 [TLBNUM-1:0];
reg [ 1:0]           tlb_mat1 [TLBNUM-1:0];
reg                  tlb_d1 [TLBNUM-1:0];
reg                  tlb_v1 [TLBNUM-1:0];

//serach:
wire [TLBNUM-1:0]          match0;
wire [TLBNUM-1:0]          match1;
wire [$clog2(TLBNUM)-1:0]  s0_index_arr [TLBNUM -1:0];
wire [$clog2(TLBNUM)-1:0]  s1_index_arr [TLBNUM -1:0];
wire                       pick_num0;
wire                       pick_num1;

assign s0_found = |match0;
assign s1_found = |match1;
assign s0_index = s0_index_arr[TLBNUM -1];
assign s1_index = s1_index_arr[TLBNUM -1];
assign s0_ps    = tlb_ps4MB[s0_index] ? 6'd22 : 6'd12;
assign s1_ps    = tlb_ps4MB[s1_index] ? 6'd22 : 6'd12;

assign pick_num0 = tlb_ps4MB[s0_index] ? s0_vppn[9] : s0_va_bit12;
assign pick_num1 = tlb_ps4MB[s1_index] ? s1_vppn[9] : s1_va_bit12;
assign s0_ppn = pick_num0 ? tlb_ppn1[s0_index] : tlb_ppn0[s0_index];
assign s0_mat = pick_num0 ? tlb_mat1[s0_index] : tlb_mat0[s0_index];
assign s0_plv = pick_num0 ? tlb_plv1[s0_index] : tlb_plv0[s0_index];
assign s0_d = pick_num0   ? tlb_d1[s0_index] : tlb_d0[s0_index];
assign s0_v = pick_num0   ? tlb_v1[s0_index] : tlb_v0[s0_index];

assign s1_ppn = pick_num1 ? tlb_ppn1[s1_index] : tlb_ppn0[s1_index];
assign s1_plv = pick_num1 ? tlb_plv1[s1_index] : tlb_plv0[s1_index];
assign s1_mat = pick_num1 ? tlb_mat1[s1_index] : tlb_mat0[s1_index];
assign s1_d = pick_num1   ? tlb_d1[s1_index] : tlb_d0[s1_index];
assign s1_v = pick_num1   ? tlb_v1[s1_index] : tlb_v0[s1_index];



wire [               3:0] cond[TLBNUM-1:0];
wire [TLBNUM - 1 : 0]inv_match;

genvar tlb_i;
generate for (tlb_i = 0; tlb_i < TLBNUM; tlb_i = tlb_i + 1) begin:gen_tlb

   assign match0[tlb_i] = (s0_vppn[18:10]==tlb_vppn[tlb_i][18:10])
                        && (tlb_ps4MB[tlb_i] || s0_vppn[9:0]==tlb_vppn[tlb_i][9:0])
                        && ((s0_asid==tlb_asid[tlb_i]) || tlb_g[tlb_i]);
   assign match1[tlb_i] = (s1_vppn[18:10]==tlb_vppn[tlb_i][18:10])
                        && (tlb_ps4MB[tlb_i] || s1_vppn[9:0]==tlb_vppn[tlb_i][9:0])
                        && ((s1_asid==tlb_asid[tlb_i]) || tlb_g[tlb_i]);
   if (tlb_i == 0) begin
        assign s0_index_arr[tlb_i] = {$clog2(TLBNUM){match0[tlb_i]}} & tlb_i;
        assign s1_index_arr[tlb_i] = {$clog2(TLBNUM){match1[tlb_i]}} & tlb_i;
    end else begin
        assign s0_index_arr[tlb_i] = s0_index_arr[tlb_i - 1] | ({$clog2(TLBNUM){match0[tlb_i]}} & tlb_i);
        assign s1_index_arr[tlb_i] = s1_index_arr[tlb_i - 1] | ({$clog2(TLBNUM){match1[tlb_i]}} & tlb_i);
    end
   
   //invtlb:
    assign cond[tlb_i][0] =~tlb_g[tlb_i];
    assign cond[tlb_i][1] = tlb_g[tlb_i];
    assign cond[tlb_i][2] = s1_asid == tlb_asid[tlb_i];
    assign cond[tlb_i][3] = (s1_vppn[18:10]==tlb_vppn[tlb_i][18:10]) 
                             && (tlb_ps4MB[tlb_i]||s1_vppn[9:0]==tlb_vppn[tlb_i][ 9: 0]);
    assign inv_match[tlb_i] =           ((invtlb_op==0||invtlb_op==1) & (cond[tlb_i][0] || cond[tlb_i][1]))
                                         ||((invtlb_op==2) & (cond[tlb_i][1]))
                                         ||((invtlb_op==3) & (cond[tlb_i][0]))
                                         ||((invtlb_op==4) & (cond[tlb_i][0]) & (cond[tlb_i][2]))
                                         ||((invtlb_op==5) & (cond[tlb_i][0]) & cond[tlb_i][2] & cond[tlb_i][3])
                                         ||((invtlb_op==6) & (cond[tlb_i][1] | cond[tlb_i][2]) & cond[tlb_i][3]);
   
   //Write
    always @(posedge clk) begin 
        if (we && w_index == tlb_i) begin
            tlb_vppn[tlb_i] <= w_vppn;
            tlb_asid[tlb_i] <= w_asid;    
            tlb_g   [tlb_i] <= w_g;
            tlb_e   [tlb_i] <= w_e;
            tlb_ps4MB[tlb_i]<= (w_ps == 6'd22);
            
            tlb_ppn0[tlb_i] <= w_ppn0;
            tlb_plv0  [tlb_i] <= w_plv0;
            tlb_mat0  [tlb_i] <= w_mat0;
            tlb_d0  [tlb_i] <= w_d0;
            tlb_v0  [tlb_i] <= w_v0;

            tlb_ppn1[tlb_i] <= w_ppn1;
            tlb_plv1  [tlb_i] <= w_plv1;
            tlb_mat1  [tlb_i] <= w_mat1;
            tlb_d1  [tlb_i] <= w_d1;
            tlb_v1  [tlb_i] <= w_v1;
        end else if(inv_match[tlb_i] & invtlb_valid)begin
            tlb_e[tlb_i] <= 1'b0;
        end
    end
   
end endgenerate

//Read
assign r_vppn = tlb_vppn[r_index];
assign r_asid = tlb_asid[r_index];
assign r_g    = tlb_g[r_index];
assign r_e    = tlb_e[r_index];
assign r_ps   = tlb_ps4MB[r_index]  ? 6'd22 : 6'd12;

assign r_ppn0 = tlb_ppn0[r_index];
assign r_plv0   = tlb_plv0[r_index];
assign r_mat0   = tlb_mat0[r_index];
assign r_d0   = tlb_d0[r_index];
assign r_v0   = tlb_v0[r_index];

assign r_ppn1 = tlb_ppn1[r_index];
assign r_plv1   = tlb_plv1[r_index];
assign r_mat1   = tlb_mat1[r_index];
assign r_d1   = tlb_d1[r_index];
assign r_v1   = tlb_v1[r_index];


endmodule
