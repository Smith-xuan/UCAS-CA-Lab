`include"mycpu.vh"

module mycpu_top
#(
    parameter TLBNUM = 16
)
(
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

wire        wb_ex_r;
wire [`ES_CSR_BLK_DATA_WD -1:0] es_csr_blk_data;
wire [`MS_CSR_BLK_DATA_WD -1:0] ms_csr_blk_data;
wire [`WS_CSR_BLK_DATA_WD -1:0] ws_csr_blk_data;
wire es_tlbsrch_blk;
wire ms_tlbsrch_blk;
wire ws_tlbsrch_blk;

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


//tlb
wire [18:0]   s0_vppn;
wire          s0_va_bit12;
wire [9:0]    s0_asid;
wire          s0_found;
wire [$clog2(TLBNUM)-1:0]    s0_index;
wire [19:0]   s0_ppn;
wire [5:0]    s0_ps;
wire [1:0]    s0_plv;
wire [1:0]    s0_mat;
wire          s0_d;
wire          s0_v;
    // search port 1 (for load/store)
wire [18:0]   s1_vppn;
wire          s1_va_bit12;
wire [9:0]    s1_asid;
wire          s1_found;
wire [$clog2(TLBNUM)-1:0]    s1_index;
wire [19:0]   s1_ppn;
wire [5:0]    s1_ps;
wire [1:0]    s1_plv;
wire [1:0]    s1_mat;
wire          s1_d;
wire          s1_v;
    // invtlb opcode
wire          invtlb_valid;
wire [4:0]    invtlb_op;
    // write port
wire          we;
wire  [$clog2(TLBNUM)-1:0]   w_index;
wire          w_e;
wire [5:0]    w_ps;
wire [18:0]   w_vppn;
wire [9:0]    w_asid;
wire          w_g;
wire [19:0]   w_ppn0;
wire [1:0]    w_plv0;
wire [1:0]    w_mat0;
wire          w_d0;
wire          w_v0;
wire [19:0]   w_ppn1;
wire [1:0]    w_plv1;
wire [1:0]    w_mat1;
wire          w_d1;
wire          w_v1;
    // read port
wire [$clog2(TLBNUM)-1:0]   r_index;
wire          r_e;
wire [18:0]   r_vppn;
wire [5:0]    r_ps;
wire [9:0]    r_asid;
wire          r_g;
wire [19:0]   r_ppn0;
wire [1:0]    r_plv0;
wire [1:0]    r_mat0;
wire          r_d0;
wire          r_v0;
wire [19:0]   r_ppn1;     
wire [1:0]    r_plv1;
wire [1:0]    r_mat1;
wire          r_d1;
wire          r_v1;

wire [4:0]    tlbop_bus_es;
wire [4:0]    tlbop_bus_ws;
wire          tlbsrch_hit;
wire          csr_tlbrd_re;
wire [31: 0] csr_tlbidx_wvalue_es;
wire [31: 0] csr_tlbidx_wvalue_ws;
wire [31: 0] csr_tlbehi_wvalue;
wire [31: 0] csr_tlbelo0_wvalue;
wire [31: 0] csr_tlbelo1_wvalue;
wire [31: 0] csr_asid_wvalue;
wire [31: 0] csr_tlbidx_rvalue;
wire [31: 0] csr_tlbehi_rvalue;
wire [31: 0] csr_tlbelo0_rvalue;
wire [31: 0] csr_tlbelo1_rvalue;
wire [31: 0] csr_asid_rvalue;
wire [31: 0] csr_crmd_rvalue;
wire [31: 0] csr_dmw0_rvalue;
wire [31: 0] csr_dmw1_rvalue;

wire         tlb_reflush;


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
    //ex
    .csr_pc         (csr_pc         ),
    .wb_ex          (wb_ex          ),
    .wb_ertn_flush  (wb_ertn_flush  ),
    //tlb
    // search port 0
    .s0_vppn       (s0_vppn        ),
    .s0_va_bit12   (s0_va_bit12    ),
    .s0_asid       (s0_asid        ),
    .s0_found      (s0_found       ),
    .s0_index      (s0_index       ),
    .s0_ppn        (s0_ppn         ),  
    .s0_ps         (s0_ps          ),
    .s0_plv        (s0_plv         ),
    .s0_mat        (s0_mat         ),
    .s0_d          (s0_d           ),
    .s0_v          (s0_v           ),
    //csr_tlb
    .csr_asid_rvalue    (csr_asid_rvalue),
    .csr_crmd_rvalue    (csr_crmd_rvalue),
    .csr_dmw0_rvalue    (csr_dmw0_rvalue),
    .csr_dmw1_rvalue    (csr_dmw1_rvalue),
    //tlb_reflush
    .tlb_reflush        (tlb_reflush)
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
    .ws_csr_blk_data (ws_csr_blk_data),
    .es_tlbsrch_blk  (es_tlbsrch_blk ),
    .ms_tlbsrch_blk  (ms_tlbsrch_blk ),
    .ws_tlbsrch_blk  (ws_tlbsrch_blk ),
    //tlb_reflush
    .tlb_reflush     (tlb_reflush)
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
    .es_csr_blk_data (es_csr_blk_data),
    .es_tlbsrch_blk  (es_tlbsrch_blk ),

    //tlb
    // search port 1
    .s1_vppn       (s1_vppn        ),
    .s1_va_bit12   (s1_va_bit12    ),
    .s1_asid       (s1_asid        ),
    .s1_found      (s1_found       ),
    .s1_index      (s1_index       ),
    .s1_ppn        (s1_ppn         ),  
    .s1_ps         (s1_ps          ),
    .s1_plv        (s1_plv         ),
    .s1_mat        (s1_mat         ),
    .s1_d          (s1_d           ),
    .s1_v          (s1_v           ),
    //invtlb
    .invtlb_valid  (invtlb_valid),
    .invtlb_op     (invtlb_op),
    //csr_tlb
    .tlbop_bus_es       (tlbop_bus_es),
    .tlbsrch_hit        (tlbsrch_hit),
    .csr_tlbidx_wvalue_es  (csr_tlbidx_wvalue_es),
    .csr_tlbehi_rvalue  (csr_tlbehi_rvalue),
    .csr_asid_rvalue    (csr_asid_rvalue),
    .csr_crmd_rvalue    (csr_crmd_rvalue),
    .csr_dmw0_rvalue    (csr_dmw0_rvalue),
    .csr_dmw1_rvalue    (csr_dmw1_rvalue)
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
    .ms_csr_blk_data (ms_csr_blk_data),
    .ms_tlbsrch_blk  (ms_tlbsrch_blk )
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
    .ws_tlbsrch_blk  (ws_tlbsrch_blk ),
    .wb_ecode        (wb_ecode      ),
    .wb_esubcode     (wb_esubcode   ),
    .wb_vaddr        (wb_vaddr      ),

    .wb_ex_r         (wb_ex_r),
    //trace debug interface
    .debug_wb_pc      (wb_pc      ),
    .debug_wb_rf_we (debug_wb_rf_we  ),
    .debug_wb_rf_wnum (debug_wb_rf_wnum ),
    .debug_wb_rf_wdata(debug_wb_rf_wdata),

    // write port
    .we            (we             ), //w(rite) e(nable)
    .w_index       (w_index        ),
    .w_e           (w_e            ),
    .w_vppn        (w_vppn         ),
    .w_ps          (w_ps           ),
    .w_asid        (w_asid         ),
    .w_g           (w_g            ),
    .w_ppn0        (w_ppn0         ),
    .w_plv0        (w_plv0         ),
    .w_mat0        (w_mat0         ),
    .w_d0          (w_d0           ),
    .w_v0          (w_v0           ),
    .w_ppn1        (w_ppn1         ),
    .w_plv1        (w_plv1         ),
    .w_mat1        (w_mat1         ),
    .w_d1          (w_d1           ),
    .w_v1          (w_v1           ),
    // read port
    .r_index       (r_index        ),
    .r_e           (r_e            ),
    .r_vppn        (r_vppn         ),
    .r_ps          (r_ps           ),
    .r_asid        (r_asid         ),
    .r_g           (r_g            ),
    .r_ppn0        (r_ppn0         ),
    .r_plv0        (r_plv0         ),
    .r_mat0        (r_mat0         ),
    .r_d0          (r_d0           ),
    .r_v0          (r_v0           ),
    .r_ppn1        (r_ppn1         ),     
    .r_plv1        (r_plv1         ),
    .r_mat1        (r_mat1         ),
    .r_d1          (r_d1           ),
    .r_v1          (r_v1           ),

    //csr
    //tlb
    .tlbop_bus_ws       (tlbop_bus_ws), //tlbsrch,tlbrd,tlbwr,tlbfill,invtlb
    .csr_tlbrd_re       (csr_tlbrd_re),
    .csr_tlbidx_wvalue_ws  (csr_tlbidx_wvalue_ws),
    .csr_tlbehi_wvalue  (csr_tlbehi_wvalue),
    .csr_tlbelo0_wvalue (csr_tlbelo0_wvalue),
    .csr_tlbelo1_wvalue (csr_tlbelo1_wvalue),
    .csr_asid_wvalue    (csr_asid_wvalue),
    .csr_tlbidx_rvalue  (csr_tlbidx_rvalue),
    .csr_tlbehi_rvalue  (csr_tlbehi_rvalue),
    .csr_tlbelo0_rvalue (csr_tlbelo0_rvalue),
    .csr_tlbelo1_rvalue (csr_tlbelo1_rvalue),
    .csr_asid_rvalue    (csr_asid_rvalue)
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
    .has_int    (has_int      ),

    .wb_ex_r    (wb_ex_r      ),

    //tlb
    .tlbop_bus_es       (tlbop_bus_es), //tlbsrch,tlbrd,tlbwr,tlbfill,invtlb
    .tlbop_bus_ws       (tlbop_bus_ws),
    .tlbsrch_hit        (tlbsrch_hit),
    .csr_tlbrd_re       (csr_tlbrd_re),
    .csr_tlbidx_wvalue_es  (csr_tlbidx_wvalue_es),
    .csr_tlbidx_wvalue_ws  (csr_tlbidx_wvalue_ws),
    .csr_tlbehi_wvalue  (csr_tlbehi_wvalue),
    .csr_tlbelo0_wvalue (csr_tlbelo0_wvalue),
    .csr_tlbelo1_wvalue (csr_tlbelo1_wvalue),
    .csr_asid_wvalue    (csr_asid_wvalue),
    .csr_tlbidx_rvalue  (csr_tlbidx_rvalue),
    .csr_tlbehi_rvalue  (csr_tlbehi_rvalue),
    .csr_tlbelo0_rvalue (csr_tlbelo0_rvalue),
    .csr_tlbelo1_rvalue (csr_tlbelo1_rvalue),
    .csr_asid_rvalue    (csr_asid_rvalue),
    .csr_crmd_rvalue    (csr_crmd_rvalue),
    .csr_dmw0_rvalue    (csr_dmw0_rvalue),
    .csr_dmw1_rvalue    (csr_dmw1_rvalue)
);

tlb tlb(
    .clk           (clk            ),
    // search port 0 (for fetch)
    .s0_vppn       (s0_vppn        ),
    .s0_va_bit12   (s0_va_bit12    ),
    .s0_asid       (s0_asid        ),
    .s0_found      (s0_found       ),
    .s0_index      (s0_index       ),
    .s0_ppn        (s0_ppn         ),  
    .s0_ps         (s0_ps          ),
    .s0_plv        (s0_plv         ),
    .s0_mat        (s0_mat         ),
    .s0_d          (s0_d           ),
    .s0_v          (s0_v           ),
    // search port 1 (for load/store)
    .s1_vppn       (s1_vppn        ),
    .s1_va_bit12   (s1_va_bit12    ),
    .s1_asid       (s1_asid        ),
    .s1_found      (s1_found       ),
    .s1_index      (s1_index       ),
    .s1_ppn        (s1_ppn         ),
    .s1_ps         (s1_ps          ),
    .s1_plv        (s1_plv         ),
    .s1_mat        (s1_mat         ),
    .s1_d          (s1_d           ),
    .s1_v          (s1_v           ),
    // invtlb opcode
    .invtlb_valid  (invtlb_valid   ),
    .invtlb_op     (invtlb_op      ),
    // write port
    .we            (we             ), //w(rite) e(nable)
    .w_index       (w_index        ),
    .w_e           (w_e            ),
    .w_vppn        (w_vppn         ),
    .w_ps          (w_ps           ),
    .w_asid        (w_asid         ),
    .w_g           (w_g            ),
    .w_ppn0        (w_ppn0         ),
    .w_plv0        (w_plv0         ),
    .w_mat0        (w_mat0         ),
    .w_d0          (w_d0           ),
    .w_v0          (w_v0           ),
    .w_ppn1        (w_ppn1         ),
    .w_plv1        (w_plv1         ),
    .w_mat1        (w_mat1         ),
    .w_d1          (w_d1           ),
    .w_v1          (w_v1           ),
    // read port
    .r_index       (r_index        ),
    .r_e           (r_e            ),
    .r_vppn        (r_vppn         ),
    .r_ps          (r_ps           ),
    .r_asid        (r_asid         ),
    .r_g           (r_g            ),
    .r_ppn0        (r_ppn0         ),
    .r_plv0        (r_plv0         ),
    .r_mat0        (r_mat0         ),
    .r_d0          (r_d0           ),
    .r_v0          (r_v0           ),
    .r_ppn1        (r_ppn1         ),     
    .r_plv1        (r_plv1         ),
    .r_mat1        (r_mat1         ),
    .r_d1          (r_d1           ),
    .r_v1          (r_v1           )
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

module vaddr_transfer(
    input  [31:0] va,
    input  [ 2:0] inst_op,//{load.store,if}
    output [31:0] pa,
    output [ 5:0] tlb_ex_bus,//{PME,PPE,PIS,PIL,PIF,TLBR}
    output wire adem_judge,
    //tlb
    output [18:0] s_vppn,
    output        s_va_bit12,
    output [ 9:0] s_asid,
    input         s_found,
    input  [ 3:0] s_index,
    input  [19:0] s_ppn,
    input  [ 5:0] s_ps,
    input  [ 1:0] s_plv,
    input  [ 1:0] s_mat,
    input         s_d,
    input         s_v,
    //crmd
    input  [31:0] csr_asid,
    input  [31:0] csr_crmd,
    //dmw
    output dmw_hit,
    input  [31:0] csr_dmw0,
    input  [31:0] csr_dmw1
    
);
    parameter ps4k = 12;
    wire mapping;
    wire direct;
    //direct
    wire dmw_hit0;
    wire dmw_hit1;
    wire [31:0] dmw_pa0;
    wire [31:0] dmw_pa1;
    wire [31:0] tlb_pa;
    wire [31:0] tlb_pa4k;
    wire [31:0] tlb_pa4m;
    assign direct = csr_crmd[3] & ~csr_crmd[4];
    //direct map
    assign dmw_hit0 = csr_dmw0[csr_crmd[1:0]] && (csr_dmw0[31:29]==va[31:29]);
    assign dmw_hit1 = csr_dmw1[csr_crmd[1:0]] && (csr_dmw1[31:29]==va[31:29]);
    assign dmw_pa0  = {csr_dmw0[27:25],va[28:0]};
    assign dmw_pa1  = {csr_dmw1[27:25],va[28:0]};
     //mapping
     assign s_vppn =  va[31:13];
     assign s_va_bit12 = va[12];
     assign s_asid =  csr_asid[9:0];
     assign tlb_ex_bus = {direct ? 1'b0 : ~dmw_hit & inst_op[1] & s_found & s_v & csr_crmd[1:0] <= s_plv & ~s_d,//PME
                          direct ? 1'b0 : ~dmw_hit & {|inst_op} & s_found & s_v & csr_crmd[1:0] > s_plv,//PPE
                          direct ? 1'b0 : ~dmw_hit & inst_op[1] & s_found & ~s_v,//PIS
                          direct ? 1'b0 : ~dmw_hit & inst_op[2] & s_found & ~s_v,//PIL
                          direct ? 1'b0 : ~dmw_hit & inst_op[0] & s_found & ~s_v,//PIF
                          direct ? 1'b0 : ~dmw_hit & {|inst_op} & ~s_found//TLBR
                          };
     assign tlb_pa4k = {s_ppn[19:0],va[11:0]};
     assign tlb_pa4m = {s_ppn[19:10],va[21:0]};
     assign tlb_pa = (s_ps==ps4k)? tlb_pa4k:tlb_pa4m;
     assign dmw_hit = dmw_hit0 | dmw_hit1;
     assign pa = direct ? va:(dmw_hit0 ? dmw_pa0 : (dmw_hit1 ? dmw_pa1 : tlb_pa));
     assign adem_judge = direct | dmw_hit;
endmodule
