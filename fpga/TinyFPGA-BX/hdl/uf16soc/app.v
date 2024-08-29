
`define max(a,b)((a) > (b) ? (a) : (b))

module app
  #(parameter GPIO_NUM = 'd16)
   (
    input                 clk_i,
    input                 rstn_i,

    // ---- to/from TOP ----------------------------------------------
    input [GPIO_NUM-1:0]  gpio_i,
    output [GPIO_NUM-1:0] gpio_o,
    output [GPIO_NUM-1:0] gpio_oe_o,
    output                sleep_o,

    // ---- to/from USB_CDC ------------------------------------------
    output [7:0]          in_data_o,
    output                in_valid_o,
    // While in_valid_o is high, in_data_o shall be valid.
    input                 in_ready_i,
    // When both in_ready_i and in_valid_o are high, in_data_o shall
    //   be consumed.
    input [7:0]           out_data_i,
    input                 out_valid_i,
    // While out_valid_i is high, the out_data_i shall be valid and both
    //   out_valid_i and out_data_i shall not change until consumed.
    output                out_ready_o
    // When both out_valid_i and out_ready_o are high, the out_data_i shall
    //   be consumed.
    );

   function integer ceil_log2;
      input [31:0] arg;
      integer      i;
      begin
         ceil_log2 = 0;
         for (i = 0; i < 32; i = i + 1) begin
            if (arg > (1 << i))
              ceil_log2 = ceil_log2 + 1;
         end
      end
   endfunction

   function integer gcd; // Greatest common divisior
      input integer       arg1;
      input integer       arg2;
      integer             temp1, temp2;
      integer             remainder;
      begin
         temp1 = arg1;
         temp2 = arg2;
         while (temp2 != 0) begin
            remainder = temp1 % temp2;
            temp1 = temp2;
            temp2 = remainder;
         end
         gcd = temp1;
      end
   endfunction

   reg [1:0]         rstn_sq;

   wire              rstn;

   assign rstn = rstn_sq[0];

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         rstn_sq <= 2'd0;
      end else begin
         rstn_sq <= {1'b1, rstn_sq[1]};
      end
   end

   localparam       ADDR_WIDTH = 'd15;
   localparam       CELL_WIDTH = 'd16;
   localparam       WAKEUPID_WIDTH = 'd3;
   localparam       IRQ_NUM = 'd4;
   localparam       DSTACK_DEPTH = 'd4;
   localparam       DCACHE_DEPTH = 'd1;
   localparam       RSTACK_DEPTH = 'd2;
   localparam       RCACHE_DEPTH = 'd1;
   localparam       SADDR_WIDTH = 'd6;
   localparam       REGS_BASE = 16'hFFF0;  // byte address
   localparam       EXC_ADDR = 16'h0002;  // byte address
   localparam       ROM_BASE = 16'h0000;  // byte address
   localparam       RAM_BASE = 16'h8000;  // byte address
   localparam       FIFO_BASE = 16'hF800;  // byte address
   localparam       TIMER_BASE= 16'hF808;  // byte address
   localparam       GPIO_BASE = 16'hF810;  // byte address
   localparam       IRQCK0_BASE = 16'hF820;  // byte address
   localparam       IRQCK1_BASE = 16'hF828;  // byte address
   localparam       VIC_BASE = 16'hF840;  // byte address
   localparam       ROM_SIZE = 8*1024;  // byte size
   localparam       RAM_SIZE = 8*1024;  // byte size
   localparam       FIFO_SIZE = 2**3;  // byte size
   localparam       TIMER_SIZE = 2**3;  // byte size
   localparam       GPIO_SIZE = 2**3;  // byte size
   localparam       IRQCK_SIZE = 2**3;  // byte size
   localparam       VIC_SIZE = 64;  // byte size
   localparam       ROM_ADDR_WIDTH = ceil_log2(ROM_SIZE/(CELL_WIDTH/8));
   localparam       RAM_ADDR_WIDTH = ceil_log2(RAM_SIZE/(CELL_WIDTH/8));
   localparam       FIFO_ADDR_WIDTH = ceil_log2(FIFO_SIZE/(CELL_WIDTH/8));
   localparam       TIMER_ADDR_WIDTH = ceil_log2(TIMER_SIZE/(CELL_WIDTH/8));
   localparam       GPIO_ADDR_WIDTH = ceil_log2(GPIO_SIZE/(CELL_WIDTH/8));
   localparam       IRQCK_ADDR_WIDTH = ceil_log2(IRQCK_SIZE/(CELL_WIDTH/8));
   localparam       VIC_ADDR_WIDTH = ceil_log2(VIC_SIZE/(CELL_WIDTH/8));
   localparam [63:0] PRBS31_INIT = 64'h49F42E2E127235D4;

   localparam [3:0]  SEL_NONE = 4'd0,
                     SEL_ROM = 4'd1,
                     SEL_RAM = 4'd2,
                     SEL_FIFO = 4'd3,
                     SEL_TIMER = 4'd4,
                     SEL_GPIO = 4'd5,
                     SEL_IRQCK0 = 4'd6,
                     SEL_IRQCK1 = 4'd7,
                     SEL_VIC = 4'd8;
   reg [3:0]         sel, rdsel_q;
   localparam [0:0]  SELWAKEUP_NONE = 1'd0,
                     SELWAKEUP_FIFO = 1'd1;

   reg               rom_sel;
   reg               ram_sel;
   reg               fifo_sel;
   reg               fifo_wakeup_q;
   reg               timer_sel;
   reg               gpio_sel;
   reg [1:0]         irqck_sel;
   reg               vic_sel;

   wire [IRQ_NUM-1:0] irqs;
   wire [0:0]         selwakeup;
   wire [WAKEUPID_WIDTH-1:0] uf16_wakeupid;
   wire                      uf16_wakeup;
   wire [CELL_WIDTH-1:0]     uf16_rddata;
   wire [2*CELL_WIDTH-1:0]   irqck_rddata;
   wire [1:0]                irqck_irq;
   wire [CELL_WIDTH-1:0]     rom_rddata;
   wire [CELL_WIDTH-1:0]     ram_rddata;
   wire [CELL_WIDTH-1:0]     fifo_rddata;
   wire [CELL_WIDTH-1:0]     timer_rddata;
   wire [CELL_WIDTH-1:0]     gpio_rddata;
   wire [CELL_WIDTH-1:0]     vic_rddata;
   wire                      vic_irq, uf16_irqack;
   wire                      uf16_wakeupack;
   wire                      uf16_rd, uf16_wr;
   wire                      uf16_sleep;
   wire [ADDR_WIDTH-1:0]     uf16_addr;
   wire [CELL_WIDTH-1:0]     uf16_wrdata;
   wire                      fifo_out_irq, fifo_in_irq;
   wire                      timer_irq;
   wire                      gpio_irq;
   wire [CELL_WIDTH-1:0]     vic_irqaddr;

   assign sleep_o = uf16_sleep;
   assign irqs = {irqck_irq, gpio_irq, timer_irq};

   // wakeup priority:
   assign selwakeup = (fifo_wakeup_q) ? SELWAKEUP_FIFO : SELWAKEUP_NONE;
   assign uf16_wakeupid = (selwakeup == SELWAKEUP_FIFO) ? 'h0002 : 'h0000;
   assign uf16_wakeup = fifo_wakeup_q;

   assign uf16_rddata = (rdsel_q == SEL_ROM) ? rom_rddata :
                        (rdsel_q == SEL_RAM) ? ram_rddata :
                        (rdsel_q == SEL_FIFO) ? fifo_rddata :
                        (rdsel_q == SEL_TIMER) ? timer_rddata :
                        (rdsel_q == SEL_GPIO) ? gpio_rddata :
                        (rdsel_q == SEL_IRQCK0) ? irqck_rddata[CELL_WIDTH*0 +:CELL_WIDTH] :
                        (rdsel_q == SEL_IRQCK1) ? irqck_rddata[CELL_WIDTH*1 +:CELL_WIDTH] :
                        (rdsel_q == SEL_VIC) ? vic_rddata :
                        'd0;

   always @(posedge clk_i or negedge rstn) begin
      if (~rstn) begin
         fifo_wakeup_q <= 1'b0;
         rdsel_q <= SEL_NONE;
      end else begin
         if (fifo_out_irq | fifo_in_irq)
           fifo_wakeup_q <= 1'b1;
         else if (uf16_wakeupack == 1'b1 && selwakeup == SELWAKEUP_FIFO)
           fifo_wakeup_q <= 1'b0;
         rdsel_q <= (uf16_rd) ? sel : SEL_NONE;
      end
   end

   always @(/*AS*/uf16_addr) begin
      sel = SEL_NONE;
      rom_sel = 1'b0;
      ram_sel = 1'b0;
      fifo_sel = 1'b0;
      timer_sel = 1'b0;
      gpio_sel = 1'b0;
      irqck_sel = 2'd0;
      vic_sel = 1'b0;

      if ((((ROM_BASE+ROM_SIZE)/gcd(ROM_BASE, ROM_SIZE) - ROM_BASE/gcd(ROM_BASE, ROM_SIZE)) == 1) ?
          {uf16_addr, 1'b0}/gcd(ROM_BASE, ROM_SIZE) == ROM_BASE/gcd(ROM_BASE, ROM_SIZE) :
          (ROM_BASE/gcd(ROM_BASE, ROM_SIZE) <= {uf16_addr, 1'b0}/gcd(ROM_BASE, ROM_SIZE) &&
           {uf16_addr, 1'b0}/gcd(ROM_BASE, ROM_SIZE) < (ROM_BASE+ROM_SIZE)/gcd(ROM_BASE, ROM_SIZE))) begin
         sel     = SEL_ROM;
         rom_sel = 1'b1;
      end else if ((((RAM_BASE+RAM_SIZE)/gcd(RAM_BASE, RAM_SIZE) - RAM_BASE/gcd(RAM_BASE, RAM_SIZE)) == 1) ?
                   {uf16_addr, 1'b0}/gcd(RAM_BASE, RAM_SIZE) == RAM_BASE/gcd(RAM_BASE, RAM_SIZE) :
                   (RAM_BASE/gcd(RAM_BASE, RAM_SIZE) <= {uf16_addr, 1'b0}/gcd(RAM_BASE, RAM_SIZE) &&
                    {uf16_addr, 1'b0}/gcd(RAM_BASE, RAM_SIZE) < (RAM_BASE+RAM_SIZE)/gcd(RAM_BASE, RAM_SIZE))) begin
         sel     = SEL_RAM;
         ram_sel = 1'b1;
      end else if ((((FIFO_BASE+FIFO_SIZE)/gcd(FIFO_BASE, FIFO_SIZE) - FIFO_BASE/gcd(FIFO_BASE, FIFO_SIZE)) == 1) ?
                   {uf16_addr, 1'b0}/gcd(FIFO_BASE, FIFO_SIZE) == FIFO_BASE/gcd(FIFO_BASE, FIFO_SIZE) :
                   (FIFO_BASE/gcd(FIFO_BASE, FIFO_SIZE) <= {uf16_addr, 1'b0}/gcd(FIFO_BASE, FIFO_SIZE) &&
                    {uf16_addr, 1'b0}/gcd(FIFO_BASE, FIFO_SIZE) < (FIFO_BASE+FIFO_SIZE)/gcd(FIFO_BASE, FIFO_SIZE))) begin
         sel      = SEL_FIFO;
         fifo_sel = 1'b1;
      end else if ((((TIMER_BASE+TIMER_SIZE)/gcd(TIMER_BASE, TIMER_SIZE) - TIMER_BASE/gcd(TIMER_BASE, TIMER_SIZE)) == 1) ?
                   {uf16_addr, 1'b0}/gcd(TIMER_BASE, TIMER_SIZE) == TIMER_BASE/gcd(TIMER_BASE, TIMER_SIZE) :
                   (TIMER_BASE/gcd(TIMER_BASE, TIMER_SIZE) <= {uf16_addr, 1'b0}/gcd(TIMER_BASE, TIMER_SIZE) &&
                    {uf16_addr, 1'b0}/gcd(TIMER_BASE, TIMER_SIZE) < (TIMER_BASE+TIMER_SIZE)/gcd(TIMER_BASE, TIMER_SIZE))) begin
         sel       = SEL_TIMER;
         timer_sel = 1'b1;
      end else if ((((GPIO_BASE+GPIO_SIZE)/gcd(GPIO_BASE, GPIO_SIZE) - GPIO_BASE/gcd(GPIO_BASE, GPIO_SIZE)) == 1) ?
                   {uf16_addr, 1'b0}/gcd(GPIO_BASE, GPIO_SIZE) == GPIO_BASE/gcd(GPIO_BASE, GPIO_SIZE) :
                   (GPIO_BASE/gcd(GPIO_BASE, GPIO_SIZE) <= {uf16_addr, 1'b0}/gcd(GPIO_BASE, GPIO_SIZE) &&
                    {uf16_addr, 1'b0}/gcd(GPIO_BASE, GPIO_SIZE) < (GPIO_BASE+GPIO_SIZE)/gcd(GPIO_BASE, GPIO_SIZE))) begin
         sel      = SEL_GPIO;
         gpio_sel = 1'b1;
      end else if ((((IRQCK0_BASE+IRQCK_SIZE)/gcd(IRQCK0_BASE, IRQCK_SIZE) - IRQCK0_BASE/gcd(IRQCK0_BASE, IRQCK_SIZE)) == 1) ?
                   {uf16_addr, 1'b0}/gcd(IRQCK0_BASE, IRQCK_SIZE) == IRQCK0_BASE/gcd(IRQCK0_BASE, IRQCK_SIZE) :
                   (IRQCK0_BASE/gcd(IRQCK0_BASE, IRQCK_SIZE) <= {uf16_addr, 1'b0}/gcd(IRQCK0_BASE, IRQCK_SIZE) &&
                    {uf16_addr, 1'b0}/gcd(IRQCK0_BASE, IRQCK_SIZE) < (IRQCK0_BASE+IRQCK_SIZE)/gcd(IRQCK0_BASE, IRQCK_SIZE))) begin
         sel          = SEL_IRQCK0;
         irqck_sel[0] = 1'b1;
      end else if ((((IRQCK1_BASE+IRQCK_SIZE)/gcd(IRQCK1_BASE, IRQCK_SIZE) - IRQCK1_BASE/gcd(IRQCK1_BASE, IRQCK_SIZE)) == 1) ?
                   {uf16_addr, 1'b0}/gcd(IRQCK1_BASE, IRQCK_SIZE) == IRQCK1_BASE/gcd(IRQCK1_BASE, IRQCK_SIZE) :
                   (IRQCK1_BASE/gcd(IRQCK1_BASE, IRQCK_SIZE) <= {uf16_addr, 1'b0}/gcd(IRQCK1_BASE, IRQCK_SIZE) &&
                    {uf16_addr, 1'b0}/gcd(IRQCK1_BASE, IRQCK_SIZE) < (IRQCK1_BASE+IRQCK_SIZE)/gcd(IRQCK1_BASE, IRQCK_SIZE))) begin
         sel          = SEL_IRQCK1;
         irqck_sel[1] = 1'b1;
      end else if ((((VIC_BASE+VIC_SIZE)/gcd(VIC_BASE, VIC_SIZE) - VIC_BASE/gcd(VIC_BASE, VIC_SIZE)) == 1) ?
                   {uf16_addr, 1'b0}/gcd(VIC_BASE, VIC_SIZE) == VIC_BASE/gcd(VIC_BASE, VIC_SIZE) :
                   (VIC_BASE/gcd(VIC_BASE, VIC_SIZE) <= {uf16_addr, 1'b0}/gcd(VIC_BASE, VIC_SIZE) &&
                    {uf16_addr, 1'b0}/gcd(VIC_BASE, VIC_SIZE) < (VIC_BASE+VIC_SIZE)/gcd(VIC_BASE, VIC_SIZE))) begin
         sel     = SEL_VIC;
         vic_sel = 1'b1;
      end else begin
         sel = SEL_NONE;
      end
   end

   uf16 #(.ADDR_WIDTH(ADDR_WIDTH),
          .CELL_WIDTH(CELL_WIDTH),
          .WAKEUPID_WIDTH(WAKEUPID_WIDTH),
          .DSTACK_DEPTH(DSTACK_DEPTH),
          .DCACHE_DEPTH(DCACHE_DEPTH),
          .RSTACK_DEPTH(RSTACK_DEPTH),
          .RCACHE_DEPTH(RCACHE_DEPTH),
          .SADDR_WIDTH(SADDR_WIDTH),
          .REGS_BASE(REGS_BASE),
          .EXCEPTION_ADDR(EXC_ADDR))
   u_uf16 (.rstn_i(rstn),
           .clk_i(clk_i),
           .irq_i(vic_irq),
           .wakeup_i(uf16_wakeup),
           .rddata_i(uf16_rddata),
           .irqaddr_i(vic_irqaddr[CELL_WIDTH-1:1]),
           .wakeupid_i(uf16_wakeupid),
           .irqack_o(uf16_irqack),
           .wakeupack_o(uf16_wakeupack),
           .read_o(uf16_rd),
           .write_o(uf16_wr),
           .sleep_o(uf16_sleep),
           .addr_o(uf16_addr),
           .wrdata_o(uf16_wrdata));

   fifo_if u_fifo_if (.rstn_i(rstn),
                      .clk_i(clk_i),
                      .sel_i(fifo_sel),
                      .read_i(uf16_rd),
                      .write_i(uf16_wr),
                      .addr_i(uf16_addr[FIFO_ADDR_WIDTH-1:0]),
                      .data_i(uf16_wrdata),
                      .out_data_i(out_data_i),
                      .out_valid_i(out_valid_i),
                      .in_ready_i(in_ready_i),
                      .data_o(fifo_rddata),
                      .out_irq_o(fifo_out_irq),
                      .in_irq_o(fifo_in_irq),
                      .out_ready_o(out_ready_o),
                      .in_data_o(in_data_o),
                      .in_valid_o(in_valid_o));

   timer u_timer (.rstn_i(rstn),
                  .clk_i(clk_i),
                  .sel_i(timer_sel),
                  .read_i(uf16_rd),
                  .write_i(uf16_wr),
                  .addr_i(uf16_addr[TIMER_ADDR_WIDTH-1:0]),
                  .data_i(uf16_wrdata),
                  .data_o(timer_rddata),
                  .irq_o(timer_irq));

   gpio #(.GPIO_NUM(GPIO_NUM))
   u_gpio (.rstn_i(rstn),
           .clk_i(clk_i),
           .sel_i(gpio_sel),
           .read_i(uf16_rd),
           .write_i(uf16_wr),
           .addr_i(uf16_addr[GPIO_ADDR_WIDTH-1:0]),
           .data_i(uf16_wrdata),
           .gpio_i(gpio_i),
           .data_o(gpio_rddata),
           .gpio_o(gpio_o),
           .gpio_oe_o(gpio_oe_o),
           .irq_o(gpio_irq));

   genvar i;

   generate
      for (i = 0; i <= 1; i = i+1) begin : gen_irqck
         irqck #(.PRBS31_INIT(PRBS31_INIT[32*i +:31]))
         u_irqck (.rstn_i(rstn_i),
                  .clk_i(clk_i),
                  .sel_i(irqck_sel[i]),
                  .read_i(uf16_rd),
                  .write_i(uf16_wr),
                  .addr_i(uf16_addr[IRQCK_ADDR_WIDTH-1:0]),
                  .data_i(uf16_wrdata),
                  .data_o(irqck_rddata[CELL_WIDTH*i +:CELL_WIDTH]),
                  .irq_o(irqck_irq[i]));
      end
   endgenerate

   vic #(.IRQ_NUM(IRQ_NUM),
         .IRQ_SYNC(16'h0000))
   u_vic (.rstn_i(rstn_i),
          .clk_i(clk_i),
          .sel_i(vic_sel),
          .read_i(uf16_rd),
          .write_i(uf16_wr),
          .addr_i(uf16_addr[VIC_ADDR_WIDTH-1:0]),
          .data_i(uf16_wrdata),
          .irq_i(irqs),
          .irqack_i(uf16_irqack),
          .irq_o(vic_irq),
          .irqaddr_o(vic_irqaddr),
          .data_o(vic_rddata));

   soc_rom #(.VECTOR_LENGTH(ROM_SIZE/(CELL_WIDTH/8)),
             .WORD_WIDTH(CELL_WIDTH),
             .ADDR_WIDTH(ROM_ADDR_WIDTH))
   u_rom (.clk_i(clk_i),
          .sel_i(rom_sel),
          .read_i(uf16_rd),
          .addr_i(uf16_addr[ROM_ADDR_WIDTH-1:0]),
          .data_o(rom_rddata));

   soc_ram #(.VECTOR_LENGTH(RAM_SIZE/(CELL_WIDTH/8)),
             .WORD_WIDTH(CELL_WIDTH),
             .ADDR_WIDTH(RAM_ADDR_WIDTH))
   u_ram (.clk_i(clk_i),
          .sel_i(ram_sel),
          .read_i(uf16_rd),
          .write_i(uf16_wr),
          .addr_i(uf16_addr[RAM_ADDR_WIDTH-1:0]),
          .mask_i('d0),
          .data_i(uf16_wrdata),
          .data_o(ram_rddata));
endmodule
