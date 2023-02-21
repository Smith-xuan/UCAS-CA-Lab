`include"mycpu.vh"

module mycpu_top(
    input  wire         aclk,
    input  wire         aresetn,

    output  [ 3:0]      arid,
    output  [31:0]      araddr,
    output  [ 7:0]      arlen,
    output  [ 2:0]      arsize,
    output  [ 1:0]      arburst,
    output  [ 1:0]      arlock,
    output  [ 3:0]      arcache,
    output  [ 2:0]      arprot,
    output              arvalid,
    input               arready,

    //read response
    input   [ 3:0]      rid,
    input   [31:0]      rdata,
    input   [ 1:0]      rresp,
    input               rlast,
    input               rvalid,
    output              rready,

    //write request
    output  [ 3:0]      awid,
    output  [31:0]      awaddr,
    output  [ 7:0]      awlen,
    output  [ 2:0]      awsize,
    output  [ 1:0]      awburst,
    output  [ 1:0]      awlock,
    output  [ 3:0]      awcache,
    output  [ 2:0]      awprot,
    output              awvalid,
    input               awready,

    //write data
    output  [ 3:0]      wid,
    output  [31:0]      wdata,
    output  [ 3:0]      wstrb,
    output              wlast,
    output              wvalid,
    input               wready,

    //write response
    input   [ 3:0]      bid,
    input   [ 1:0]      bresp,
    input               bvalid,
    output              bready,

    // trace debug interface
    output wire [31:0] debug_wb_pc,
    output wire [ 3:0] debug_wb_rf_we,
    output wire [ 4:0] debug_wb_rf_wnum,
    output wire [31:0] debug_wb_rf_wdata
);

wire         reset;
wire         clk;
assign reset = ~aresetn;
assign clk = aclk;

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

wire wb_ex;
wire ms_ex;
wire ms_ertn;
wire wb_csr_we;
wire wb_ertn_flush;
wire has_int;
wire [31:0] wb_vaddr;
wire [31:0] csr_pc;
wire [31:0] wb_pc;
wire [13:0] wb_csr_num;
wire [31:0] wb_csr_wmask;
wire [31:0] wb_csr_wvalue;
wire [31:0] wb_csr_rvalue;
wire [ 5:0] wb_ecode;
wire [ 8:0] wb_esubcode;
wire [`ES_CSR_BLK_DATA_WD -1:0] es_csr_blk_data;
wire [`MS_CSR_BLK_DATA_WD -1:0] ms_csr_blk_data;
wire [`WS_CSR_BLK_DATA_WD -1:0] ws_csr_blk_data;

assign debug_wb_pc = wb_pc;

// inst_sram

wire [31:0] inst_sram_addr;
wire        inst_sram_req;
wire        inst_sram_wr;     
wire [2:0]  inst_sram_size;   
wire [3:0]  inst_sram_wstrb; 
wire        inst_sram_addr_ok;
wire [31:0] inst_sram_wdata;
wire [31:0] inst_sram_rdata;
wire        inst_sram_data_ok;

wire        data_sram_req;
wire        data_sram_wr;
wire [2:0]  data_sram_size;
wire [3:0]  data_sram_wstrb;
wire [31:0] data_sram_addr;
wire        data_sram_addr_ok;
wire [31:0] data_sram_wdata;
wire [31:0] data_sram_rdata;
wire        data_sram_data_ok;

assign inst_sram_wr     = 1'b0;
assign inst_sram_size   = 3'h2;
assign inst_sram_wstrb  = 4'h0;
assign inst_sram_wdata  = 32'h0;
assign data_sram_size[2] = 1'b0;

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
    .inst_sram_req          (inst_sram_req),
    .inst_sram_addr         (inst_sram_addr),
    .inst_sram_addr_ok      (inst_sram_addr_ok),
    .inst_sram_rdata        (inst_sram_rdata),
    .inst_sram_data_ok      (inst_sram_data_ok),
    
    .csr_pc         (csr_pc         ),
    .wb_ex          (wb_ex          ),
    .wb_ertn_flush  (wb_ertn_flush  )
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
    .ms_fwd_blk_data (ms_fwd_blk_data ),

    .wb_ex           (wb_ex          ),
    .wb_ertn_flush   (wb_ertn_flush  ),
    .has_int         (has_int        ),
    .es_csr_blk_data (es_csr_blk_data),
    .ms_csr_blk_data (ms_csr_blk_data),
    .ws_csr_blk_data (ws_csr_blk_data)
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
     // data sram interface
    .data_sram_req          (data_sram_req  ),
    .data_sram_wr           (data_sram_wr   ),
    .data_sram_size         (data_sram_size ),
    .data_sram_wdata        (data_sram_wdata),
    .data_sram_wstrb        (data_sram_wstrb),
    .data_sram_addr         (data_sram_addr ),
    .data_sram_addr_ok      (data_sram_addr_ok),
    
   
    // forward & block
    .es_fwd_blk_data (es_fwd_blk_data ),

    .wb_ex           (wb_ex         ),
    .wb_ertn_flush   (wb_ertn_flush ),
    .ms_ex           (ms_ex         ),
    .ms_ertn_flush   (ms_ertn       ),
    .es_csr_blk_data (es_csr_blk_data)
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
    .data_sram_data_ok      (data_sram_data_ok),
    // forward & block
    .ms_fwd_blk_data (ms_fwd_blk_data),

    .wb_ex           (wb_ex         ),
    .wb_ertn_flush   (wb_ertn_flush ),
    .ms_ex_valid     (ms_ex         ),
    .ms_ertn_valid   (ms_ertn       ),
    .ms_csr_blk_data (ms_csr_blk_data)
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
    
    .ws_csr_we_valid (wb_csr_we     ),
    .ws_csr_wmask    (wb_csr_wmask  ),
    .ws_csr_wvalue   (wb_csr_wvalue ),
    .ws_csr_num      (wb_csr_num    ),
    .ws_csr_rdata    (wb_csr_rvalue ),
    .ws_ex_valid     (wb_ex         ),
    .ws_ertn_flush_valid (wb_ertn_flush),
    .ws_csr_blk_data (ws_csr_blk_data),
    .wb_ecode        (wb_ecode      ),
    .wb_esubcode     (wb_esubcode   ),
    .wb_vaddr        (wb_vaddr      ),
    //trace debug interface
    .debug_wb_pc      (wb_pc      ),
    .debug_wb_rf_we (debug_wb_rf_we  ),
    .debug_wb_rf_wnum (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata(debug_wb_rf_wdata)
);

CSR csr_stage(
    .clk        (clk          ),
    .reset      (reset        ),

    .csr_we     (wb_csr_we    ),
    .csr_num    (wb_csr_num   ),
    .csr_wvalue (wb_csr_wvalue),
    .csr_wmask  (wb_csr_wmask ),
    .csr_rvalue (wb_csr_rvalue),

    .wb_ex      (wb_ex        ),
    .ertn_flush (wb_ertn_flush),
    .wb_pc      (wb_pc        ),
    .wb_ecode   (wb_ecode     ),
    .wb_esubcode(wb_esubcode  ),
    .wb_vaddr   (wb_vaddr     ), 
    .csr_pc     (csr_pc       ),
    .has_int    (has_int      )
);

sram_to_axi_bridge bridge(
    
    .aclk               (aclk),
    .areset             (reset),

    .inst_sram_req      (inst_sram_req),
    .inst_sram_wr       (inst_sram_wr),     
    .inst_sram_size     (inst_sram_size),   
    .inst_sram_wstrb    (inst_sram_wstrb),  
    .inst_sram_addr     (inst_sram_addr),
    .inst_sram_addr_ok  (inst_sram_addr_ok),
    .inst_sram_wdata    (inst_sram_wdata),
    .inst_sram_rdata    (inst_sram_rdata),
    .inst_sram_data_ok  (inst_sram_data_ok),

    .data_sram_req      (data_sram_req),    
    .data_sram_wr       (data_sram_wr),     
    .data_sram_size     (data_sram_size),   
    .data_sram_wstrb    (data_sram_wstrb),  
    .data_sram_addr     (data_sram_addr),
    .data_sram_addr_ok  (data_sram_addr_ok),
    .data_sram_wdata    (data_sram_wdata),
    .data_sram_rdata    (data_sram_rdata),
    .data_sram_data_ok  (data_sram_data_ok),

    .arid               (arid),
    .araddr             (araddr),
    .arlen              (arlen),
    .arsize             (arsize),
    .arburst            (arburst),
    .arlock             (arlock),
    .arcache            (arcache),
    .arport             (arport),
    .arvalid            (arvalid),
    .arready            (arready),

    .rid                (rid),
    .rdata              (rdata),
    .rresp              (rresp),
    .rlast              (rlast),
    .rvalid             (rvalid),
    .rready             (rready),

    .awid               (awid),
    .awaddr             (awaddr),
    .awlen              (awlen),
    .awsize             (awsize),
    .awburst            (awburst),
    .awlock             (awlock),
    .awcache            (awcache),
    .awport             (awport),
    .awvalid            (awvalid),
    .awready            (awready),

    .wid                (wid),
    .wdata              (wdata),
    .wstrb              (wstrb),
    .wlast              (wlast),
    .wvalid             (wvalid),
    .wready             (wready),

    .bid                (bid),
    .bresp              (bresp),
    .bvalid             (bvalid),
    .bready             (bready)
);
endmodule
