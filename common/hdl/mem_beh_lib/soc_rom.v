
module soc_rom
  #(parameter VECTOR_LENGTH = 'd256,
    parameter WORD_WIDTH = 'd16,
    parameter ADDR_WIDTH = ceil_log2(VECTOR_LENGTH),
    parameter INIT_FILE = "rom.hex")
   (
    output [WORD_WIDTH-1:0] data_o,
    input                   clk_i,
    input                   sel_i,
    input                   read_i,
    input [ADDR_WIDTH-1:0]  addr_i
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

   reg [WORD_WIDTH-1:0] mem [VECTOR_LENGTH-1:0];
   reg [WORD_WIDTH-1:0] data_word;

   assign data_o = data_word;

   initial
     $readmemh (INIT_FILE, mem, 0, VECTOR_LENGTH-1);

   always @(posedge clk_i)
     if (sel_i == 1 && read_i == 1)
       data_word <= mem[addr_i];

endmodule
