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
    output wire      inst_sram_req,    // TODO
    output wire[31:0] inst_sram_addr,
    input wire        inst_sram_addr_ok,// TODO
    input wire[31:0]  inst_sram_rdata,
    input wire        inst_sram_data_ok,// TODO

    input wire [31:0] csr_pc    ,
    input wire        wb_ex     ,
    input wire        wb_ertn_flush
    );
    

reg         fs_valid;
wire        fs_ready_go;
wire        fs_allowin;
wire        to_fs_valid;
wire        to_fs_ready_go;


wire         blk_valid;

wire         br_taken;
wire [ 31:0] br_target;
assign {blk_valid,br_taken,br_target} = br_blk_data;

wire [31:0] fs_inst;
reg  [31:0] fs_pc;
reg  ex_adef;
assign fs_to_ds_data = {fs_inst, fs_pc, ex_adef};
                       
reg         fs_inst_buff_valid;
reg  [31:0] fs_inst_buff;
reg         cancel;
reg         cancel_flag;
wire        pfs_valid;


wire [31:0] seq_pc;
wire [31:0] next_pc;
reg  [31:0] pc_buff_br;
reg         pc_buff_br_valid;
reg  [31:0] pc_buff_ex;
reg         pc_buff_ex_valid;

reg cancel_req;
reg [1:0] cancel_req_count;
reg [1:0] req_count;
assign pfs_valid     = ~reset;
assign to_fs_valid    = pfs_valid && to_fs_ready_go;
assign to_fs_ready_go = (inst_sram_req | cancel_req) & inst_sram_addr_ok
                        || (pc_buff_ex_valid && !(inst_sram_data_ok && !cancel))
                        || (pc_buff_br_valid && !(inst_sram_data_ok && !cancel));

assign fs_ready_go    = | (!cancel && inst_sram_data_ok) | fs_inst_buff_valid|(wb_ex | wb_ertn_flush);
assign fs_allowin     = fs_valid && cancel || !fs_valid || fs_ready_go && ds_allowin;
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
     end else if ((wb_ex | wb_ertn_flush|br_taken)/* && fs_valid*/ && !inst_sram_data_ok && !cancel_flag)begin
        cancel<=1'b1;
     end else if(inst_sram_data_ok)begin
        cancel<=1'b0;
     end
end

always@(posedge clk)begin
    if(reset)begin
        cancel_req_count <= 2'b00;
    end else if(cancel && inst_sram_addr_ok && !inst_sram_data_ok)begin
        cancel_req_count <= cancel_req_count + 2'b01;
    end else if(cancel_req_count != 2'b00 && inst_sram_data_ok && !inst_sram_addr_ok)begin
        cancel_req_count <= cancel_req_count - 2'b01;
    end

    
end
        
always @(posedge clk)begin
    if(reset)begin
        req_count <= 2'b00;
    end else if(inst_sram_addr_ok && !inst_sram_data_ok)begin
        req_count <= req_count + 2'b01;
    end else if(!inst_sram_addr_ok && inst_sram_data_ok)begin
        req_count <= req_count - 2'b01;
    end
    
    if(reset)begin
        cancel_req <= 1'b0;
    end else if(req_count == 2'b01 && inst_sram_addr_ok && !inst_sram_data_ok)begin
        cancel_req <= 1'b1;
    end else if(req_count == 2'b01 && !inst_sram_addr_ok && inst_sram_data_ok)begin
        cancel_req <= 1'b0;
    end
end

// pc control
always @ (posedge clk) begin
    if (reset) begin
        pc_buff_br_valid  <= 1'b0;
        pc_buff_br        <= 32'h0;
    end else if (br_taken) begin
        pc_buff_br_valid  <= 1'b1;
        pc_buff_br        <= br_target;
    end else if (inst_sram_data_ok && !cancel) begin
        pc_buff_br_valid  <= 1'b0;
        pc_buff_br        <= 32'h0;
    end
        
    if (reset) begin
        pc_buff_ex_valid  <= 1'b0;
        pc_buff_ex        <= 32'h0;
    end else if (wb_ex|wb_ertn_flush) begin
        pc_buff_ex_valid  <= 1'b1;
        pc_buff_ex        <= csr_pc;
    end else if(inst_sram_data_ok && !cancel)begin
        pc_buff_ex_valid  <= 1'b0;
        pc_buff_ex        <= 32'h0;
    end
end

assign next_pc = (pc_buff_ex_valid && !(inst_sram_data_ok && !cancel)) ?         pc_buff_ex :
                 wb_ex|wb_ertn_flush ?      csr_pc  :
                 (pc_buff_br_valid && !(inst_sram_data_ok && !cancel))  ?      pc_buff_br:
                 !fs_allowin          ?     fs_pc:
                 br_taken            ?      br_target:
                                            seq_pc;

assign seq_pc = fs_pc+32'h4;
    
                                       
assign inst_sram_req   = pfs_valid & fs_allowin & !cancel_req;
assign inst_sram_addr  = next_pc;
assign fs_inst         = fs_inst_buff_valid ? fs_inst_buff : inst_sram_rdata;

always @(posedge clk) begin
    if (reset) begin
        fs_valid <= 1'b0;
    end
    else if (fs_allowin) begin
        fs_valid <= to_fs_valid;
    end 
    else if(br_taken && ds_allowin || cancel)begin
        fs_valid<=1'b0;
    end
    
end

always @(posedge clk) begin
    if (reset) begin
        fs_pc <= 32'h_1bfffffc;  //trick: to make next_pc be 0x1c000000 during reset 
    end
    else if (to_fs_valid && (fs_allowin || wb_ex || wb_ertn_flush)) begin
        fs_pc <= next_pc;
    end
   
    if (next_pc[1:0] != 2'b00) begin
        ex_adef <= 1'b1;
    end
    else if (next_pc[1:0] == 2'b00) begin
        ex_adef <= 1'b0;
    end
end

always @ (posedge clk) begin
    if (reset) begin
        fs_inst_buff_valid  <= 1'b0;
        fs_inst_buff        <= 32'h0;
    end else if (!fs_inst_buff_valid && fs_valid && inst_sram_data_ok && !cancel && !ds_allowin) begin
        fs_inst_buff_valid  <= 1'b1;
        fs_inst_buff        <= inst_sram_rdata;
    end else if (ds_allowin || wb_ertn_flush || wb_ex) begin
        fs_inst_buff_valid  <= 1'b0;
        fs_inst_buff        <= 32'h0;
        end
end

endmodule
