// TIMER module shall implement a 32bit timer for the MCUs
// TIMER shall:
//   - Provide latched IRQ to signal when timer reaches max value.
//   - Provide overflow information when timer reaches 32'hFFFFFFFF value.
//   - Allow read timer, overflow and irq values.

module timer
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

   reg [1:0]  addr_q;
   reg [31:0] counter_q;
   reg [31:0] counter_buf_q;
   reg [31:0] max_counter_q;
   reg        irq_q;
   reg        irq_en_q;
   reg        counter_buf_valid_q;
   reg        counter_en_q;
   reg        overflow_q;

   assign irq_o = irq_q;

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         addr_q <= 2'd0;
         counter_buf_q <= 32'd0;
         counter_buf_valid_q <= 1'b0;
      end else begin
         if (read_i & sel_i) begin
            addr_q <= addr_i;
            if ((addr_i == 2'b00 || addr_i == 2'b01) &&
                (counter_buf_valid_q == 1'b0 || addr_i[0] == addr_q[0])) begin
               counter_buf_q <= counter_q;
               counter_buf_valid_q <= 1'b1;
            end else begin
               counter_buf_valid_q <= 1'b0;
            end
         end
      end
   end

   reg [15:0] rdata;
   assign data_o = rdata;

   always @(/*AS*/addr_q or counter_buf_q or counter_en_q or irq_en_q
            or irq_q or overflow_q) begin
      rdata = 16'd0;
      case (addr_q) 
        2'b00: begin
           rdata = counter_buf_q[15:0];
        end
        2'b01: begin
           rdata = counter_buf_q[31:16];
        end
        2'b10: begin
           rdata[1:0] = {overflow_q, irq_q};
        end
        default: begin
           rdata[1:0] = {counter_en_q, irq_en_q};
        end
      endcase
   end

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         counter_q <= 32'd0;
         counter_en_q <= 1'b0;
         max_counter_q <= {32{1'b1}};
         irq_q <= 1'b0;
         irq_en_q <= 1'b0;
         overflow_q <= 1'b0;
      end else begin
         if (counter_en_q) begin
            if (counter_q == max_counter_q) begin
               counter_q <= 32'd0;
               irq_q <= irq_en_q;
            end else begin
               counter_q <= counter_q + 1;
            end
            if (counter_q == {32{1'b1}})
              overflow_q <= 1'b1;
         end
         if (write_i & sel_i) begin
            case (addr_i) 
              2'b00: begin
                 max_counter_q[15:0] <= data_i;
                 counter_q <= 32'd0;
                 overflow_q <= 1'b0;
              end
              2'b01: begin
                 max_counter_q[31:16] <= data_i;
                 counter_q <= 32'd0;
                 overflow_q <= 1'b0;
              end
              2'b10: begin
                 if (data_i[0])
                   irq_q <= 1'b0;
                 if (data_i[1])
                   overflow_q <= 1'b0;
                 if (data_i[2])
                   counter_q <= 32'd0;
              end
              default: begin
                 counter_en_q <= data_i[1];
                 irq_en_q <= data_i[0];
              end
            endcase
         end
      end
   end
endmodule
