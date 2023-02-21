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
reg reset;
always @(posedge clk) reset <= ~resetn;
//wire         reset;
//assign reset = ~resetn;

wire [31:0] seq_pc;
wire [31:0] nextpc;
wire        br_taken;
wire [31:0] br_target;
wire [31:0] inst;
reg  [31:0] pc;

wire [11:0] alu_op;
wire        load_op;
wire        src1_is_pc;
wire        src2_is_imm;
wire        res_from_mem;
wire        dst_is_r1;
wire        gr_we;
wire        mem_we;
wire        src_reg_is_rd;
wire [4: 0] dest;
wire [31:0] rj_value;
wire [31:0] rkd_value;
wire [31:0] imm;
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
wire        inst_nor;
wire        inst_and;
wire        inst_or;
wire        inst_xor;
wire        inst_slli_w;
wire        inst_srli_w;
wire        inst_srai_w;
wire        inst_addi_w;
wire        inst_ld_w;
wire        inst_st_w;
wire        inst_jirl;
wire        inst_b;
wire        inst_bl;
wire        inst_beq;
wire        inst_bne;
wire        inst_lu12i_w;

wire        need_ui5;
wire        need_si12;
wire        need_si16;
wire        need_si20;
wire        need_si26;
wire        src2_is_4;

wire [ 4:0] rf_raddr1;
wire [31:0] rf_rdata1;
wire [ 4:0] rf_raddr2;
wire [31:0] rf_rdata2;
wire        rf_we   ;
wire [ 4:0] rf_waddr;
wire [31:0] rf_wdata;

wire [31:0] alu_src1   ;
wire [31:0] alu_src2   ;
wire [31:0] alu_result ;

wire [31:0] mem_result;
wire [31:0] final_result;

wire      validin;
reg       valid;

reg [31:0] pc_ID;
reg        valid_ID;
reg [31:0] inst_ID;

wire       sel_rf_res = res_from_mem;
reg [31:0] pc_EXE;
reg        valid_EXE;
reg        gr_we_EXE;
reg [4:0]  dest_EXE;
reg        sel_rf_res_EXE;
reg [31:0] alu_src1_EXE;
reg [31:0] alu_src2_EXE;   
reg [11:0] alu_op_EXE;
reg [3:0]  data_sram_we_EXE;
reg        data_sram_en_EXE;
reg [31:0] rkd_value_EXE;

reg [31:0] pc_MEM;
reg        valid_MEM;
reg        gr_we_MEM;
reg [4:0]  dest_MEM;
reg        sel_rf_res_MEM;
reg [31:0] alu_result_MEM;

reg [31:0] pc_WB;
reg        valid_WB;
reg        gr_we_WB;
reg [4:0]  dest_WB;
reg [31:0] final_result_WB;


wire IFreg_ready_go = 1'b1;
wire IDreg_ready_go = 1'b1;
wire EXEreg_ready_go = 1'b1;
wire MEMreg_ready_go = 1'b1;
wire WBreg_ready_go = 1'b1;
wire IFreg_allowin;
wire IDreg_allowin;
wire EXEreg_allowin;
wire MEMreg_allowin;
wire WBreg_allowin;
wire allowout = 1'b1;
assign IFreg_allowin = !valid || (IFreg_ready_go && IDreg_allowin);
assign IDreg_allowin = !valid_ID || (IDreg_ready_go && EXEreg_allowin);
assign EXEreg_allowin = !valid_EXE || (EXEreg_ready_go && MEMreg_allowin);
assign MEMreg_allowin = !valid_MEM || (MEMreg_ready_go && WBreg_allowin);
assign WBreg_allowin = !valid_WB || (WBreg_ready_go && allowout);
wire to_IF_valid;
wire IF_to_ID_valid;
wire ID_to_EXE_valid;
wire EXE_to_MEM_valid;
wire MEM_to_WB_valid;
assign validin = ~reset;
assign to_IF_valid = validin;
assign IF_to_ID_valid = valid && IFreg_ready_go;
assign ID_to_EXE_valid = valid_ID && IDreg_ready_go;
assign EXE_to_MEM_valid = valid_EXE && EXEreg_ready_go;
assign MEM_to_WB_valid = valid_MEM && MEMreg_ready_go;


assign seq_pc       = pc + 3'h4;
assign nextpc       = br_taken ? br_target : seq_pc;


always @(posedge clk) begin
    if (reset) begin
        pc <= 32'h1bfffffc;
        valid <= 1'b0;
    end
    else if(IFreg_allowin)begin
        valid <= validin;
    end
    else if(br_taken)begin
        valid <= 1'b0;
    end

    if(to_IF_valid && IFreg_allowin) begin
        pc <= nextpc;
    end

end


always @(posedge clk) begin
    if(reset) begin
        valid_ID <= 1'b0;
    end
    else if(br_taken) begin
        valid_ID <= 1'b0;
    end
    else if(IDreg_allowin)begin
        valid_ID <= IF_to_ID_valid;
    end

    if(IF_to_ID_valid && IDreg_allowin)begin
        pc_ID <= pc;
        inst_ID <= inst;
    end

end


always @(posedge clk) begin
    if(reset) begin
        valid_EXE <= 1'b0;
    end
    else if (EXEreg_allowin)begin
        valid_EXE <= ID_to_EXE_valid;
    end

    if(ID_to_EXE_valid && EXEreg_allowin)begin
        pc_EXE <= pc_ID;
        gr_we_EXE <= gr_we;
        dest_EXE <= dest;
        sel_rf_res_EXE <= sel_rf_res;
        alu_src1_EXE <= alu_src1;
        alu_src2_EXE <= alu_src2;
        alu_op_EXE <= alu_op;
        data_sram_we_EXE <= {4{mem_we}};
        data_sram_en_EXE <= (mem_we || inst_ld_w);
        rkd_value_EXE <= rkd_value;
    end
end


always @(posedge clk) begin
    if(reset) begin
        valid_MEM <= 1'b0;
    end
    else if (MEMreg_allowin) begin
        valid_MEM <= EXE_to_MEM_valid;
    end

    if(EXE_to_MEM_valid && MEMreg_allowin)begin
        pc_MEM <= pc_EXE;
        gr_we_MEM <= gr_we_EXE;
        dest_MEM <= dest_EXE;
        sel_rf_res_MEM <= sel_rf_res_EXE;
        alu_result_MEM <= alu_result;
    end
end


always @(posedge clk) begin
    if(reset) begin
        valid_WB <= 1'b0;
    end
    else if (WBreg_allowin) begin
        valid_WB <= MEM_to_WB_valid;
    end

    if(MEM_to_WB_valid && WBreg_allowin) begin
        pc_WB <= pc_MEM;
        gr_we_WB <= gr_we_MEM;
        dest_WB <= dest_MEM;
        final_result_WB <= final_result;
    end
end


assign inst_sram_en    = 1'b1;
assign inst_sram_we    = 4'b0;
assign inst_sram_addr  = nextpc;
assign inst_sram_wdata = 32'b0;
assign inst            = inst_sram_rdata;

assign op_31_26  = inst_ID[31:26];
assign op_25_22  = inst_ID[25:22];
assign op_21_20  = inst_ID[21:20];
assign op_19_15  = inst_ID[19:15];

assign rd   = inst_ID[ 4: 0];
assign rj   = inst_ID[ 9: 5];
assign rk   = inst_ID[14:10];

assign i12  = inst_ID[21:10];
assign i20  = inst_ID[24: 5];
assign i16  = inst_ID[25:10];
assign i26  = {inst_ID[ 9: 0], inst_ID[25:10]};

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
assign inst_slli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h01];
assign inst_srli_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h09];
assign inst_srai_w = op_31_26_d[6'h00] & op_25_22_d[4'h1] & op_21_20_d[2'h0] & op_19_15_d[5'h11];
assign inst_addi_w = op_31_26_d[6'h00] & op_25_22_d[4'ha];
assign inst_ld_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h2];
assign inst_st_w   = op_31_26_d[6'h0a] & op_25_22_d[4'h6];
assign inst_jirl   = op_31_26_d[6'h13];
assign inst_b      = op_31_26_d[6'h14];
assign inst_bl     = op_31_26_d[6'h15];
assign inst_beq    = op_31_26_d[6'h16];
assign inst_bne    = op_31_26_d[6'h17];
assign inst_lu12i_w= op_31_26_d[6'h05] & ~inst_ID[25];

assign alu_op[ 0] = inst_add_w | inst_addi_w | inst_ld_w | inst_st_w
                    | inst_jirl | inst_bl;
assign alu_op[ 1] = inst_sub_w;
assign alu_op[ 2] = inst_slt;
assign alu_op[ 3] = inst_sltu;
assign alu_op[ 4] = inst_and;
assign alu_op[ 5] = inst_nor;
assign alu_op[ 6] = inst_or;
assign alu_op[ 7] = inst_xor;
assign alu_op[ 8] = inst_slli_w;
assign alu_op[ 9] = inst_srli_w;
assign alu_op[10] = inst_srai_w;
assign alu_op[11] = inst_lu12i_w;

assign need_ui5   =  inst_slli_w | inst_srli_w | inst_srai_w;
assign need_si12  =  inst_addi_w | inst_ld_w | inst_st_w;
assign need_si16  =  inst_jirl | inst_beq | inst_bne;
assign need_si20  =  inst_lu12i_w;
assign need_si26  =  inst_b | inst_bl;
assign src2_is_4  =  inst_jirl | inst_bl;

assign imm = src2_is_4 ? 32'h4                      :
             need_si20 ? {i20[19:0], 12'b0}         :
/*need_ui5 || need_si12*/{{20{i12[11]}}, i12[11:0]} ;

assign br_offs = need_si26 ? {{ 4{i26[25]}}, i26[25:0], 2'b0} :
                             {{14{i16[15]}}, i16[15:0], 2'b0} ;

assign jirl_offs = {{14{i16[15]}}, i16[15:0], 2'b0};

assign src_reg_is_rd = inst_beq | inst_bne | inst_st_w;

assign src1_is_pc    = inst_jirl | inst_bl;

assign src2_is_imm   = inst_slli_w |
                       inst_srli_w |
                       inst_srai_w |
                       inst_addi_w |
                       inst_ld_w   |
                       inst_st_w   |
                       inst_lu12i_w|
                       inst_jirl   |
                       inst_bl     ;

assign res_from_mem  = inst_ld_w;
assign dst_is_r1     = inst_bl;
assign gr_we         = ~inst_st_w & ~inst_beq & ~inst_bne & ~inst_b;
assign mem_we        = inst_st_w;
assign dest          = dst_is_r1 ? 5'd1 : rd;

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

assign rj_value  = rf_rdata1;
assign rkd_value = rf_rdata2;

assign rj_eq_rd = (rj_value == rkd_value);
assign br_taken = (   inst_beq  &&  rj_eq_rd
                   || inst_bne  && !rj_eq_rd
                   || inst_jirl
                   || inst_bl
                   || inst_b
                  ) && valid_ID;
assign br_target = (inst_beq || inst_bne || inst_bl || inst_b) ? (pc_ID + br_offs) :
                                                   /*inst_jirl*/ (rj_value + jirl_offs);

assign alu_src1 = src1_is_pc  ? pc_ID[31:0] : rj_value;
assign alu_src2 = src2_is_imm ? imm : rkd_value;

wire [31:0] alu_src1_in = alu_src1_EXE;
wire [31:0] alu_src2_in = alu_src2_EXE;
wire [11:0] alu_op_in = alu_op_EXE;

alu u_alu(
    .alu_op     (alu_op_in    ),
    .alu_src1   (alu_src1_in  ),
    .alu_src2   (alu_src2_in  ),
    .alu_result (alu_result)
    );

assign data_sram_en    = data_sram_en_EXE;
assign data_sram_we    = data_sram_we_EXE;
assign data_sram_addr  = alu_result;
assign data_sram_wdata = rkd_value_EXE;

assign mem_result   = data_sram_rdata;
assign final_result = sel_rf_res_MEM ? mem_result : alu_result_MEM;

assign rf_we    = (valid_WB) ? gr_we_WB : 1'b0;
assign rf_waddr = dest_WB;
assign rf_wdata = final_result_WB;

// debug info generate
assign debug_wb_pc[31:0]      = pc_WB;
assign debug_wb_rf_we[3:0]   = {4{rf_we}};
assign debug_wb_rf_wnum[4:0]  = dest_WB;
assign debug_wb_rf_wdata[31:0] = final_result_WB;

endmodule
