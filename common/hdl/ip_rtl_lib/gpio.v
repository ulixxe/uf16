// GPIO module shall implement a gpio interface for the MCUs

module gpio
  #(parameter GPIO_NUM = 16)
   (
    input                 clk_i,
    input                 rstn_i,

   // ---- to/from uC -----------------------------------------------
    input                 sel_i,
    input                 read_i,
    input                 write_i,
    input [1:0]           addr_i,
    input [15:0]          data_i,
    input [GPIO_NUM-1:0]  gpio_i,
    output [15:0]         data_o,
    output [GPIO_NUM-1:0] gpio_o,
    output [GPIO_NUM-1:0] gpio_oe_o,
    output                irq_o
   );

   reg [1:0]  addr_q;
   reg [GPIO_NUM-1:0] gpio_q;
   reg [GPIO_NUM-1:0] gpio_oe_q;
   reg [GPIO_NUM-1:0] gpio_siq;
   reg [GPIO_NUM-1:0] gpio_soq;
   reg [GPIO_NUM-1:0] gpio_sqq;
   reg                irq_q;
   reg [GPIO_NUM-1:0] irq_en_q;

   assign gpio_o = gpio_q;
   assign gpio_oe_o = gpio_oe_q;
   assign irq_o = irq_q;

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

   always @(/*AS*/addr_q or gpio_oe_q or gpio_soq or irq_en_q or irq_q) begin
      rdata = 16'd0;
      case (addr_q) 
        2'b00: begin
           rdata[GPIO_NUM-1:0] = gpio_soq;
        end
        2'b01: begin
           rdata[GPIO_NUM-1:0] = gpio_oe_q;
        end
        2'b10: begin
           rdata[0] = irq_q;
        end
        default: begin
           rdata[GPIO_NUM-1:0] = irq_en_q;
        end
      endcase
   end

   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         gpio_q <= {GPIO_NUM{1'b0}};
         gpio_oe_q <= {GPIO_NUM{1'b0}};
         gpio_siq <= {GPIO_NUM{1'b0}};
         gpio_soq <= {GPIO_NUM{1'b0}};
         gpio_sqq <= {GPIO_NUM{1'b0}};
         irq_q <= 1'b0;
         irq_en_q <= {GPIO_NUM{1'b0}};
      end else begin
         if (write_i & sel_i) begin
            case (addr_i) 
              2'b00: begin
                 gpio_q <= data_i[GPIO_NUM-1:0];
              end
              2'b01: begin
                 gpio_oe_q <= data_i[GPIO_NUM-1:0];
              end
              2'b10: begin
                 irq_q <= 1'b0;
              end
              default: begin
                 irq_en_q <= data_i[GPIO_NUM-1:0];
              end
            endcase
         end
         gpio_siq <= gpio_i;
         gpio_soq <= gpio_siq;
         gpio_sqq <= gpio_soq;
         if ((gpio_sqq & irq_en_q) != (gpio_soq & irq_en_q))
           irq_q <= 1'b1;
      end
   end
endmodule
