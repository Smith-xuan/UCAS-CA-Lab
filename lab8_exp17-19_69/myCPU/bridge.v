`define DATA_ID 4'h1
`define INST_ID 4'h0


module sram_to_axi_bridge(
    input  wire     aclk,
    input  wire     areset,

    input  wire       inst_sram_req,
    input  wire       inst_sram_wr,     // TODO
    input  wire[ 1:0] inst_sram_size,   // TODO
    input  wire[ 3:0] inst_sram_wstrb,  // TODO
    input  wire[31:0] inst_sram_addr,
    output wire       inst_sram_addr_ok,// TODO
    input  wire[31:0] inst_sram_wdata,
    output wire[31:0] inst_sram_rdata,
    output wire       inst_sram_data_ok,// TODO

    input  wire       data_sram_req,    // TODO
    input  wire       data_sram_wr,     // TODO
    input  wire[ 1:0] data_sram_size,   // TODO
    input  wire[ 3:0] data_sram_wstrb,  // TODO
    input  wire[31:0] data_sram_addr,
    output wire       data_sram_addr_ok,// TODO
    input  wire[31:0] data_sram_wdata,
    output wire[31:0] data_sram_rdata,
    output wire       data_sram_data_ok,// TODO

    output wire [3:0] arid,
    output wire [31:0] araddr,
    output wire [7:0] arlen,//
    output wire [2:0] arsize,
    output wire [1:0] arburst,//
    output wire [1:0] arlock,//
    output wire [3:0] arcache,//
    output wire [2:0] arport,//
    output wire       arvalid,
    input  wire       arready,

    input  wire [3:0] rid,
    input  wire [31:0] rdata,
    input  wire [1:0] rresp,
    input  wire       rlast,
    input  wire       rvalid,
    output wire       rready,

    output wire [3:0] awid,//
    output wire [31:0] awaddr,
    output wire [7:0] awlen,//
    output wire [2:0] awsize,
    output wire [1:0] awburst,//
    output wire [1:0] awlock,//
    output wire [3:0] awcache,//
    output wire [2:0] awport,//
    output wire       awvalid,
    input  wire       awready,

    output wire [3:0] wid,//
    output wire [31:0] wdata,
    output wire [3:0] wstrb,
    output wire       wlast,//
    output wire       wvalid,
    input  wire       wready,

    input  wire [3:0] bid,//
    input  wire [1:0] bresp,//
    input  wire       bvalid,
    output wire       bready
);

//constant
assign arlen    = 0;
assign arburst  = 2'b01;
assign arlock   = 2'b00;
assign arcache  = 4'b0000;
assign arprot   = 3'b000;

assign awid     = 4'b0001;
assign awlen    = 8'b00000000;
assign awburst  = 2'b01;
assign awlock   = 2'b00;
assign awcache  = 4'b0000;
assign awprot   = 3'b000;

assign wid      = 4'b0001;
assign wlast    = 1;

reg         arvalid_r;
reg [3 :0]  arid_r;
reg [2 :0]  arsize_r;
reg [31:0]  araddr_r;

reg [31:0] araddr_buff;
reg [2 :0] arsize_buff;
reg ar_buff_valid;

reg [31:0] araddr_wfr;//waiting for read
reg [2 :0] arsize_wfr;
reg ar_wfr_valid;

wire axi_rreq;
wire axi_read_from_data;
wire [3:0]  rreq_id;
wire [2:0]  rreq_size;
wire [31:0] rreq_addr;

wire write_finish;

assign arvalid = arvalid_r;
assign arid = arid_r;
assign arsize = arsize_r;
assign araddr = araddr_r;
assign axi_rreq = inst_sram_req && !inst_sram_wr || data_sram_req && !data_sram_wr;
assign axi_read_from_data = data_sram_req && !data_sram_wr;
assign axi_read_from_inst = inst_sram_req && !axi_read_from_data;
assign rreq_id = axi_read_from_data ? `DATA_ID : `INST_ID;
assign rreq_size = axi_read_from_data ? data_sram_size : inst_sram_size;
assign rreq_addr = axi_read_from_data ? data_sram_addr : inst_sram_addr;

always @(posedge aclk)begin
    if(areset)begin
        arvalid_r <= 1'b0;
        arid_r    <= 4'b0010;
        arsize_r  <= 3'h0;
        araddr_r  <= 32'h0;
    end else if(!arvalid_r && axi_rreq && write_finish)begin
        arvalid_r <= 1'b1;
        arid_r    <= rreq_id;
        arsize_r  <= rreq_size;
        araddr_r  <= rreq_addr;
    end else if(arvalid_r && arready)begin
        arvalid_r <= 1'b0;
        arid_r    <= 4'b0010;
        arsize_r  <= 3'h0;
        araddr_r  <= 32'h0;
    end
end

assign rready = 1'b1;
assign inst_sram_data_ok = rready && rvalid && rid == `INST_ID;
assign data_sram_data_ok = rready && rvalid && rid == `DATA_ID || bready && bvalid;
assign inst_sram_rdata   = rdata;
assign data_sram_rdata   = rdata;


reg         awvalid_r;
reg         wvalid_r;
reg [2 :0]  awsize_r;
reg [31:0]  awaddr_r;
reg [31:0]  wdata_r;
reg [3 :0]  wstrb_r;

wire axi_wreq;
wire data_sram_waddr_shake;
wire data_sram_wdata_shake;

assign awvalid = awvalid_r;
assign wvalid = wvalid_r;
assign awsize = awsize_r;
assign awaddr = awaddr_r;
assign wdata = wdata_r;
assign wstrb = wstrb_r;
assign axi_wreq = data_sram_req && data_sram_wr;

assign inst_sram_addr_ok = axi_read_from_inst && !arvalid_r && !inst_sram_wr && write_finish;

always @(posedge aclk)begin
    if(areset)begin
        awvalid_r <= 1'b0;
        awsize_r  <= 3'h0;
        awaddr_r  <= 32'h0;
    end else if(!awvalid_r && axi_wreq && !wvalid_r)begin
        awvalid_r <= 1'b1;
        awsize_r  <= data_sram_size;
        awaddr_r  <= data_sram_addr;
    end else if(awvalid_r && awready)begin
        awvalid_r <= 1'b0;
        awsize_r  <= 3'h0;
        awaddr_r  <= 32'h0;
    end

    if(areset)begin
        wvalid_r <= 1'b0;
        wdata_r <= 32'b0;
        wstrb_r <= 4'b0;
    end else if(!awvalid_r && axi_wreq)begin
        wvalid_r <= 1'b1;
        wdata_r <= data_sram_wdata;
        wstrb_r <= data_sram_wstrb;
    end else if(wvalid_r && wready)begin
        wvalid_r <= 1'b0;
        wdata_r <= 32'b0;
        wstrb_r <= 4'b0;
    end
end

assign bready = 1'b1;
//assign data_sram_addr_ok = awvalid_r && awready || arvalid_r && arready && axi_read_from_data ;
assign data_sram_addr_ok = axi_read_from_data && !arvalid_r && !data_sram_wr && write_finish || data_sram_req && !awvalid_r && data_sram_wr;
assign data_sram_waddr_shake = awvalid_r && awready;
assign data_sram_wdata_shake = bvalid && bready;

reg [2:0] write_count;
assign write_finish = write_count == 3'b0;

always @(posedge aclk)begin
    if(areset)begin
        write_count <= 3'b0;
    end else if(data_sram_waddr_shake && !data_sram_wdata_shake)begin
        write_count <= write_count + 1;
    end else if(!data_sram_waddr_shake && data_sram_wdata_shake)begin
        write_count <= write_count - 1;
    end
end

endmodule