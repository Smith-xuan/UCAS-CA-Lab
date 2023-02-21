`include"mycpu.vh"

module mycpu_top(
    input  wire        clk,
    input  wire        resetn,
    // inst sram interface
    output wire        inst_sram_en,
    output wire [3:0]  inst_sram_we,
    output wire [31:0] inst_sram_addr,
    output wire [31:0] inst_sram_wdata,
    input  wire [31:0] inst_sram_rdata,
    // data sram interface
    output wire        data_sram_en,
    output wire [3:0]  data_sram_we,
    output wire [31:0] data_sram_addr,
    output wire [31:0] data_sram_wdata,
    input  wire [31:0] data_sram_rdata,
    // trace debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata
);

  
//pianxuan xinhao
assign inst_sram_en=1'b1; 
assign data_sram_en=1'b1;

wire         reset;
assign reset = ~resetn;

wire         ds_allowin;
wire         es_allowin;
wire         ms_allowin;
wire         ws_allowin;
wire         fs_to_ds_valid;
wire         ds_to_es_valid;
wire         es_to_ms_valid;
wire         ms_to_ws_valid;
wire [`FS_TO_DS_DATA_WD -1:0] fs_to_ds_data;
wire [`DS_TO_ES_DATA_WD -1:0] ds_to_es_data;
wire [`ES_TO_MS_DATA_WD -1:0] es_to_ms_data;
wire [`MS_TO_WS_DATA_WD -1:0] ms_to_ws_data;
wire [`WS_TO_RF_DATA_WD -1:0] ws_to_rf_data;
wire [`BR_DATA_WD       -1:0] br_blk_data;
wire [`ES_FWD_BLK_DATA_WD -1:0] es_fwd_blk_data;
wire [`MS_FWD_BLK_DATA_WD -1:0] ms_fwd_blk_data;

// IF stage
IF_stage if_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ds_allowin     (ds_allowin     ),
    //brdata
    .br_blk_data         (br_blk_data         ),
    //outputs
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_data   (fs_to_ds_data   ),
    // inst sram interface

    .inst_sram_we  (inst_sram_we  ),
    .inst_sram_addr (inst_sram_addr ),
    .inst_sram_wdata(inst_sram_wdata),
    .inst_sram_rdata(inst_sram_rdata)
);
// ID stage
ID_stage id_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .es_allowin     (es_allowin     ),
    .ds_allowin     (ds_allowin     ),
    //from fs
    .fs_to_ds_valid (fs_to_ds_valid ),
    .fs_to_ds_data   (fs_to_ds_data   ),
    //to es
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_data   (ds_to_es_data   ),
    //to fs
    .br_blk_data         (br_blk_data         ),
    //to rf: for write back
    .ws_to_rf_data   (ws_to_rf_data   ),
    // forward & block
    .es_fwd_blk_data (es_fwd_blk_data ),
    .ms_fwd_blk_data (ms_fwd_blk_data )
);
// EXE stage
EX_stage ex_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ms_allowin     (ms_allowin     ),
    .es_allowin     (es_allowin     ),
    //from ds
    .ds_to_es_valid (ds_to_es_valid ),
    .ds_to_es_data   (ds_to_es_data   ),
    //to ms
    .es_to_ms_valid (es_to_ms_valid ),
    .es_to_ms_data   (es_to_ms_data   ),
    // data sram interface

    .data_sram_we  (data_sram_we  ),
    .data_sram_addr (data_sram_addr ),
    .data_sram_wdata(data_sram_wdata),
    // forward & block
    .es_fwd_blk_data (es_fwd_blk_data )
);
// MEM stage
MEM_stage mem_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ws_allowin     (ws_allowin     ),
    .ms_allowin     (ms_allowin     ),
    //from es
    .es_to_ms_valid (es_to_ms_valid ),
    .es_to_ms_data   (es_to_ms_data   ),
    //to ws
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_data   (ms_to_ws_data   ),
    //from data-sram
    .data_sram_rdata(data_sram_rdata),
    // forward & block
    .ms_fwd_blk_data (ms_fwd_blk_data)
);
// WB stage
WB_stage wb_stage(
    .clk            (clk            ),
    .reset          (reset          ),
    //allowin
    .ws_allowin     (ws_allowin     ),
    //from ms
    .ms_to_ws_valid (ms_to_ws_valid ),
    .ms_to_ws_data   (ms_to_ws_data   ),
    //to rf: for write back
    .ws_to_rf_data   (ws_to_rf_data   ),
    //trace debug interface
    .debug_wb_pc      (debug_wb_pc      ),
    .debug_wb_rf_we (debug_wb_rf_we  ),
    .debug_wb_rf_wnum (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata(debug_wb_rf_wdata)
);

endmodule
