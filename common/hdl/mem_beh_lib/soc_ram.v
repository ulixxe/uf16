
module soc_ram
  #(parameter VECTOR_LENGTH = 'd256,
    parameter WORD_WIDTH = 'd16,
    parameter ADDR_WIDTH = ceil_log2(VECTOR_LENGTH))
   (
    output [WORD_WIDTH-1:0] data_o,
    input                   clk_i,
    input                   sel_i,
    input                   read_i,
    input                   write_i,
    input [ADDR_WIDTH-1:0]  addr_i,
    input [WORD_WIDTH-1:0]  mask_i,
    input [WORD_WIDTH-1:0]  data_i
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

   reg [WORD_WIDTH-1:0] mem [(VECTOR_LENGTH-1):0];
   reg [WORD_WIDTH-1:0] rdata_word;

   assign data_o = rdata_word;

   always @(posedge clk_i)
     if (sel_i == 1) begin
        if (write_i == 1)
          mem[addr_i] <= (data_i & ~mask_i) | (mem[addr_i] & mask_i);
        else if (read_i == 1)
          rdata_word <= mem[addr_i];
     end

endmodule
