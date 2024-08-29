
module irqck
  #(parameter PRBS31_INIT = 31'h00000000)
   (
    input         clk_i,
    input         rstn_i,

    // ---- to/from uC -----------------------------------------------
    input         sel_i,
    input         read_i,
    input         write_i,
    input [1:0]   addr_i,
    input [15:0]  data_i,
    output [15:0] data_o,
    output        irq_o
    );

   function [30:0] prbs31;
      input [30:0] lfsr;
      localparam [31:0] POLY31 = 32'h90000001;
      localparam        SHIFTS = 8;
      reg [30:0]        lfsr;
      reg [5:0]         i;
      begin
         prbs31 = lfsr;
         for (i = 0; i <= SHIFTS-1; i = i + 1) begin
            prbs31 = {prbs31[29:0], ^(prbs31 & POLY31[31:1])};
         end
      end
   endfunction

   reg [15:0]    irq_counter_q;
   reg           irq_q;
   reg           irq_en_q;
   reg [30:0]    rnd_q;
   reg [7:0]     rnd_limit_q;

   assign irq_o = irq_q;

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         irq_counter_q <= 16'd0;
         irq_q <= 1'b0;
         irq_en_q <= 1'b0;
         rnd_q <= PRBS31_INIT;
         rnd_limit_q <= 8'd0;
      end else begin
         if (write_i & sel_i) begin
            case (addr_i) 
              2'b00: begin
                 rnd_limit_q <= data_i[7:0];
              end
              2'b01: begin
                 irq_q <= 1'b0;
              end
              2'b10: begin
                 irq_en_q <= data_i[0];
              end
              default: begin
              end
            endcase
         end
	       if (~irq_q & irq_en_q) begin
            rnd_q <= prbs31(rnd_q);
            if (rnd_limit_q >= rnd_q[7:0]) begin
               irq_q <= 1'b1;
               irq_counter_q <= irq_counter_q + 1;
            end
         end
      end
   end

   reg [1:0] addr_q;

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         addr_q <= 2'd0;
      end else begin
         if (read_i & sel_i)
           addr_q <= addr_i;
      end
   end

   reg [15:0] rdata;
   assign data_o = rdata;

   always @(/*AS*/addr_q or irq_counter_q or irq_en_q or irq_q) begin
      rdata = 16'b0;
      case (addr_q) 
        2'b00: begin
           rdata = irq_counter_q;
        end
        2'b01: begin
           rdata[0] = irq_q;
        end
        2'b10: begin
           rdata[0] = irq_en_q;
        end
        default: begin
        end
      endcase
   end
endmodule
