// VIC module shall implement a Vectored Interrupt Controller for the MCUs

module vic
  #(parameter IRQ_NUM = 16,
    parameter IRQ_SYNC = {IRQ_NUM{1'b0}})
   (
    input               clk_i,
    input               rstn_i,

    // ---- to/from uC -----------------------------------------------
    input               sel_i,
    input               read_i,
    input               write_i,
    input [4:0]         addr_i,
    input [15:0]        data_i,
    input [IRQ_NUM-1:0] irq_i,
    input               irqack_i,
    output [15:0]       data_o,
    output [15:0]       irqaddr_o,
    output              irq_o
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

   reg [4:0]  addr_q;
   reg [16*IRQ_NUM-1:0] irqaddr_q;
   reg [IRQ_NUM-1:0]    en_q;
   reg [IRQ_NUM-1:0]    status_q;
   reg [IRQ_NUM-1:0]    sw_q;
   reg [IRQ_NUM-1:0]    mask_q, mask_d, mask;
   reg                  irq_q, irq_d;
   reg [ceil_log2(IRQ_NUM)-1:0] index_q, index_d, index_qq;

   wire                         mask_set;
   wire                         mask_clear;

   assign irq_o = irq_q;
   assign irqaddr_o = irqaddr_q[{index_q, 4'd0} +:16];
   assign mask_set = (irqack_i) ? 1'b1 : 1'b0;
   assign mask_clear = (addr_i == 5'b00001 && write_i == 1'b1 && sel_i == 1'b1) ? 1'b1 : 1'b0;

   genvar i;

   generate
      for (i = 0; i < IRQ_NUM; i = i+1) begin : u_status
         if (IRQ_SYNC[i] == 1'b0) begin : u_nosync
            always @(posedge clk_i or negedge rstn_i) begin
               if (~rstn_i)
                 status_q[i] <= 1'b0;
               else
                 status_q[i] <= (irq_i[i] | sw_q[i]) & en_q[i] & ~mask[i];
            end
         end else begin : u_async_app
            reg status_sq;

            always @(posedge clk_i or negedge rstn_i) begin
               if (~rstn_i) begin
                  status_sq <= 1'b0;
                  status_q[i] <= 1'b0;
               end else begin
                  status_sq <= (irq_i[i] | sw_q[i]) & en_q[i] & ~mask[i];
                  status_q[i] <= status_sq;
               end
            end
         end
      end
   endgenerate

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         irq_q <= 1'b0;
         index_q <= 'd0;
         index_qq <= 'd0;
         mask_q <= {IRQ_NUM{1'b0}};
      end else begin
         irq_q <= irq_d;
         index_q <= index_d;
         index_qq <= index_q;
         if (mask_set)
           mask_q[index_qq] <= 1'b1;
         else if (mask_clear)
           mask_q <= mask_d;
      end
   end

   always @(/*AS*/status_q) begin : u_irq
      integer j;

      irq_d = 1'b0;
      index_d = 'd0;
      for (j = 0; j < IRQ_NUM; j = j + 1) begin
         if (status_q[j]) begin
            irq_d = 1'b1;
            index_d = j[ceil_log2(IRQ_NUM)-1:0];
         end
      end
   end

   always @(/*AS*/mask_q) begin : u_mask
      integer j;
      reg mask_found_var;

      mask_found_var = 1'b0;
      mask_d = mask_q;
      mask = 'd0;
      for (j = IRQ_NUM-1; j >= 0; j = j - 1) begin
         if (mask_q[j] & ~mask_found_var) begin
            mask_found_var = 1'b1;
            mask_d[j] = 1'b0;
         end
         mask[j] = mask_found_var;
      end
   end

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         addr_q <= 5'd0;
      end else begin
         if (read_i & sel_i)
           addr_q <= addr_i;
      end
   end

   reg [15:0] rdata;
   assign data_o = rdata;

   always @(/*AS*/addr_q or en_q or index_q or irqaddr_q or status_q) begin
      rdata = 16'd0;
      case (addr_q) 
        5'b00000: begin
           rdata[IRQ_NUM-1:0] = status_q;
        end
        5'b00001: begin
           rdata[ceil_log2(IRQ_NUM)-1:0] = index_q;
        end
        5'b00010: begin
           rdata[IRQ_NUM-1:0] = en_q;
        end
        default: begin
           if (addr_q[4] == 1'b1 && {1'b0, addr_q[3:0]} < IRQ_NUM)
             rdata = irqaddr_q[{addr_q[3:0], 4'd0} +:16];
        end
      endcase
   end

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         en_q <= {IRQ_NUM{1'b0}};
         sw_q <= {IRQ_NUM{1'b0}};
         irqaddr_q <= {IRQ_NUM{16'd0}};
      end else begin
         if (write_i & sel_i) begin
            case (addr_i) 
              5'b00010: begin
                 en_q <= data_i[IRQ_NUM-1:0];
              end
              5'b00011: begin
                 sw_q <= sw_q | data_i[IRQ_NUM-1:0];
              end
              5'b00100: begin
                 sw_q <= sw_q & ~data_i[IRQ_NUM-1:0];
              end
              default: begin
                 if (addr_i[4] == 1'b1 && {1'b0, addr_i[3:0]} < IRQ_NUM)
                   irqaddr_q[{addr_i[3:0], 4'd0} +:16] <= data_i;
              end
            endcase
         end
      end
   end
endmodule
