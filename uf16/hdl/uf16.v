// UF16 module

module uf16
  #(parameter ADDR_WIDTH = 15,           // address bus width
    parameter CELL_WIDTH = 16,           // data bus width
    parameter WAKEUPID_WIDTH = 3,        // wakeupid bus width
    parameter DSTACK_DEPTH = 4,          // data stack depth
    parameter DCACHE_DEPTH = 1,          // data stack cache depth
    parameter RSTACK_DEPTH = 2,          // return stack depth
    parameter RCACHE_DEPTH = 1,          // return stack cache depth
    parameter SADDR_WIDTH = 6,           // stack address width
    parameter REGS_BASE = 16'hFFF0,      // byte address
    parameter EXCEPTION_ADDR = 16'h0002) // byte address
   (
    input                      rstn_i,
    input                      clk_i,
    input                      irq_i,
    input                      wakeup_i,
    input [CELL_WIDTH-1:0]     rddata_i,
    input [ADDR_WIDTH-1:0]     irqaddr_i,
    input [WAKEUPID_WIDTH-1:0] wakeupid_i,
    output                     irqack_o,
    output                     wakeupack_o,
    output                     read_o,
    output                     write_o,
    output                     sleep_o,
    output [ADDR_WIDTH-1:0]    addr_o,
    output [CELL_WIDTH-1:0]    wrdata_o
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

   initial begin
      if (DSTACK_DEPTH - DCACHE_DEPTH < 3) begin
         $fatal("At least 3 fixed (not cached) registers are necessary for DSTACK");
      end
      if (RSTACK_DEPTH - RCACHE_DEPTH < 1) begin
         $fatal("At least 1 fixed (not cached) register is necessary for RSTACK");
      end
   end

   localparam DS0 = {ADDR_WIDTH{1'b0}};
   localparam RS0 = {ADDR_WIDTH{1'b0}} + 1;

   // ALU extended microcode constants ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER=[17:16]
   localparam ALU_EXTUCODE_RANGEL = 17;
   localparam ALU_EXTUCODE_RANGER = 16;
   localparam [1:0] NOP_ALU_EXTUCODE = 2'b00,
                    C1_ALU_EXTUCODE  = 2'b01,
                    C2_ALU_EXTUCODE  = 2'b10,
                    C3_ALU_EXTUCODE  = 2'b11;

   // RAM extended microcode constants RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER=[15:14]
   localparam RAM_EXTUCODE_RANGEL = 15;
   localparam RAM_EXTUCODE_RANGER = 14;
   localparam [1:0] NOP_RAM_EXTUCODE = 2'b00,
                    N_PUSH_EXTUCODE  = 2'b01,
                    R_PUSH_EXTUCODE  = 2'b10,
                    N_RAM_EXTUCODE   = 2'b11;

   // N microcode constants N_UCODE_RANGEL:N_UCODE_RANGER=[13:13]
   localparam N_UCODE_RANGEL = 13;
   localparam N_UCODE_RANGER = 13;
   localparam [0:0] N_N_UCODE = 1'b0,
                    T_N_UCODE = 1'b1;

   // T microcode constants T_UCODE_RANGEL:T_UCODE_RANGER=[12:11]
   localparam T_UCODE_RANGEL = 12;
   localparam T_UCODE_RANGER = 11;
   localparam [1:0] T_T_UCODE  = 2'b00,
                    N_T_UCODE  = 2'b01,
                    R_T_UCODE  = 2'b10,
                    IR_T_UCODE = 2'b11;

   // R microcode constants R_UCODE_RANGEL:R_UCODE_RANGER=[10:9]
   localparam R_UCODE_RANGEL = 10;
   localparam R_UCODE_RANGER = 9;
   localparam [1:0] R_R_UCODE  = 2'b00,
                    M1_R_UCODE = 2'b01,
                    PC_R_UCODE = 2'b10,
                    T_R_UCODE  = 2'b11;

   // PC microcode constants PC_UCODE_RANGEL:PC_UCODE_RANGER=[8:7]
   localparam PC_UCODE_RANGEL = 8;
   localparam PC_UCODE_RANGER = 7;
   localparam [1:0] P1_PC_UCODE  = 2'b00,
                    R_PC_UCODE   = 2'b01,
                    IR_PC_UCODE  = 2'b10,
                    QIR_PC_UCODE = 2'b11;

   // RAM microcode constants RAM_UCODE_RANGEL:RAM_UCODE_RANGER=[6:4]
   localparam RAM_UCODE_RANGEL = 6;
   localparam RAM_UCODE_RANGER = 4;
   localparam [2:0] NOP_RAM_UCODE = 3'b000,
                    POP_N_UCODE   = 3'b001,
                    POP_R_UCODE   = 3'b010,
                    RAM_N_UCODE   = 3'b011,
                    RAM_T_UCODE   = 3'b100,
                    N_PUSH_UCODE  = 3'b101,
                    R_PUSH_UCODE  = 3'b110,
                    N_RAM_UCODE   = 3'b111;

   // ALU microcode constants ALU_UCODE_RANGEL:ALU_UCODE_RANGER=[3:0]
   localparam ALU_UCODE_RANGEL = 3;
   localparam ALU_UCODE_RANGER = 0;
   localparam [3:0] NOP_ALU_UCODE   = 4'h0,
                    NOT_ALU_UCODE   = 4'h1,
                    AND_ALU_UCODE   = 4'h2,
                    OR_ALU_UCODE    = 4'h3,
                    XOR_ALU_UCODE   = 4'h4,
                    SHL_ALU_UCODE   = 4'h5,
                    SHR_ALU_UCODE   = 4'h6,
                    ADD_ALU_UCODE   = 4'h7,
                    SUB_ALU_UCODE   = 4'h8,
                    MUL_ALU_UCODE   = 4'h9,
                    DIV_ALU_UCODE   = 4'hA,
                    ADD1_ALU_UCODE  = 4'hB,
                    ADD2_ALU_UCODE  = 4'hC,
                    XXX_ALU_UCODE   = 4'hD,
                    BYTE_ALU_UCODE  = 4'hE,
                    CARRY_ALU_UCODE = 4'hF;

   // internal addresses
   localparam [CELL_WIDTH-1:0] DSP_ADDR = {REGS_BASE[CELL_WIDTH-1:4], 4'hE},
                               DSB_ADDR = {REGS_BASE[CELL_WIDTH-1:4], 4'hC},
                               RSP_ADDR = {REGS_BASE[CELL_WIDTH-1:4], 4'hA},
                               RSB_ADDR = {REGS_BASE[CELL_WIDTH-1:4], 4'h8};

   // exception codes
   localparam                  RS_UNDERFLOW_BIT  = 0;   // return stack underflow
   localparam                  RS_OVERFLOW_BIT   = 1;   // return stack overflow
   localparam                  DS_UNDERFLOW_BIT  = 2;   // data stack underflow
   localparam                  DS_OVERFLOW_BIT   = 3;   // data stack overflow
   localparam                  INVALID_T_BIT     = 4;   // invalid T
   localparam                  INVALID_N_BIT     = 5;   // invalid N
   localparam                  INVALID_R_BIT     = 6;   // invalid R
   localparam                  NSWAP_ERROR_BIT   = 7;   // N SWAP error
   localparam                  RSWAP_ERROR_BIT   = 8;   // R SWAP error
   localparam                  MUL_ERROR_BIT     = 9;   // MUL error
   localparam                  DIV_ERROR_BIT     = 10;  // DIV error
   localparam                  IRQ_ERROR_BIT     = 11;  // IRQ error
   localparam                  INVALID_INSTR_BIT = 15;  // invalid instruction error

   // invalid instruction
   localparam [17:0]           INVALID_INSTR = {NOP_ALU_EXTUCODE,
                                                NOP_RAM_EXTUCODE,
                                                N_N_UCODE,
                                                N_T_UCODE,
                                                R_R_UCODE,
                                                P1_PC_UCODE,
                                                NOP_RAM_UCODE,
                                                NOT_ALU_UCODE};

   localparam [1:0]            IDLE_STATE     = 2'd0,
                               FETCHING_STATE = 2'd1,
                               WAIT_STATE     = 2'd2;

   reg [1:0]                   fetch_status_q;
   reg [CELL_WIDTH*DSTACK_DEPTH-1:0] dsbuf_q, dsbuf_d;
   reg [CELL_WIDTH*RSTACK_DEPTH-1:0] rsbuf_q, rsbuf_d;
   reg [ceil_log2(DSTACK_DEPTH+1)-1:0] dsbuf_valid_q, dsbuf_valid_d;
   reg [ceil_log2(RSTACK_DEPTH+1)-1:0] rsbuf_valid_q, rsbuf_valid_d;
   reg [SADDR_WIDTH:0]                 dsp_q, dsp_d;
   reg [SADDR_WIDTH:0]                 rsp_q, rsp_d;
   reg [ADDR_WIDTH-1:0]                dsb_q, rsb_q;
   reg                                 dsmem_empty_q, rsmem_empty_q;
   reg [ADDR_WIDTH-1:0]                pc_q, pc_d;
   reg [ADDR_WIDTH-1:0]                fetch_addr_q, fetch_addr_d;
   reg [CELL_WIDTH-1:0]                ir_q;
   reg                                 ir_fetched_q;
   reg [CELL_WIDTH-1:0]                irbuf_q;
   reg                                 irbuf_valid_q;
   reg [CELL_WIDTH-1:0]                lit_q, lit_d;
   reg                                 lit_valid_q;
   reg                                 lit_fetch;
   reg                                 lit_fetched_q;
   reg [17:0]                          uir_q, uir_d;
   reg                                 uir_fetched_q, uir_fetched_d;
   reg                                 irqen_q, irqen_d;
   reg                                 carry_q, carry_d;
   reg                                 irqack_q;
   reg                                 early_call_q;
   wire                                fetch;
   reg [ADDR_WIDTH-1:0]                jump_addr;
   reg                                 jump;
   wire                                unext_hit;
   reg                                 sleep;
   wire                                wakeup;
   wire                                interrupt;
   reg                                 exception;
   reg [CELL_WIDTH-1:0]                exception_code;
   reg                                 ret;
   reg                                 reg_write;
   reg                                 mem_write;
   reg                                 mem_read;
   wire                                dec_wait;
   wire                                exe_wait;
   reg                                 pushr;
   reg                                 pushd;
   reg                                 popr;
   reg                                 popd;
   wire                                dsmem_empty;
   wire                                rsmem_empty;
   reg                                 irqdis;
   reg                                 call_dec;
   reg                                 tail_call;

   assign wakeup = wakeup_i | (irqen_q & irq_i);
   assign sleep_o = sleep & ~wakeup;
   assign wakeupack_o = sleep & wakeup_i;
   assign irqack_o = irqack_q;
   assign interrupt = irq_i & irqen_q & ~irqdis &
                      ~(wakeup_i & sleep) &
                      uir_fetched_q;  // to maximize job done by interrupting execution of only "real" instructions

   assign wrdata_o = (pushd == 1'b1) ? dsbuf_q[CELL_WIDTH*(DSTACK_DEPTH-1) +:CELL_WIDTH] :
                     (pushr == 1'b1) ? rsbuf_q[CELL_WIDTH*(RSTACK_DEPTH-1) +:CELL_WIDTH] :
                     dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH];  // mem_write = '1'

   assign write_o = (mem_write == 1'b1 && exe_wait == 1'b0) ? 1'b1 : 1'b0;

   assign read_o = (exe_wait == 1'b0 && (interrupt == 1'b1 || exception == 1'b1)) ? 1'b0 :
                   ((mem_read == 1'b1 && jump == 1'b0 && dec_wait == 1'b0) || fetch == 1'b1) ? 1'b1 :
                   1'b0;

   assign addr_o = (pushd == 1'b1 && exe_wait == 1'b0 && dsp_q[SADDR_WIDTH] == 1'b1) ? {dsb_q[ADDR_WIDTH-1:SADDR_WIDTH+1], dsp_q} :
                   (pushd == 1'b1 && exe_wait == 1'b0) ? {rsb_q[ADDR_WIDTH-1:SADDR_WIDTH+1], dsp_q} :
                   (pushr == 1'b1 && exe_wait == 1'b0 && rsp_q[SADDR_WIDTH] == 1'b1) ? {dsb_q[ADDR_WIDTH-1:SADDR_WIDTH+1], rsp_q} :
                   (pushr == 1'b1 && exe_wait == 1'b0) ? {rsb_q[ADDR_WIDTH-1:SADDR_WIDTH+1], rsp_q} :
                   (mem_write == 1'b1 && exe_wait == 1'b0) ? dsbuf_q[CELL_WIDTH*0+1 +:ADDR_WIDTH] :
                   (popd == 1'b1 && jump == 1'b0 && dec_wait == 1'b0 && dsp_q[SADDR_WIDTH] == 1'b1) ? {dsb_q[ADDR_WIDTH-1:SADDR_WIDTH+1], dsp_q} :
                   (popd == 1'b1 && jump == 1'b0 && dec_wait == 1'b0) ? {rsb_q[ADDR_WIDTH-1:SADDR_WIDTH+1], dsp_q} :
                   (popr == 1'b1 && jump == 1'b0 && dec_wait == 1'b0 && rsp_q[SADDR_WIDTH] == 1'b1) ? {dsb_q[ADDR_WIDTH-1:SADDR_WIDTH+1], rsp_q} :
                   (popr == 1'b1 && jump == 1'b0 && dec_wait == 1'b0) ? {rsb_q[ADDR_WIDTH-1:SADDR_WIDTH+1], rsp_q} :
                   (mem_read == 1'b1 && jump == 1'b0 && dec_wait == 1'b0) ? dsbuf_d[CELL_WIDTH*0+1 +:ADDR_WIDTH] :
                   fetch_addr_d;

   assign exe_wait = (sleep == 1'b1 && wakeup == 1'b0) ? 1'b1 : 1'b0;

   assign dec_wait = (exe_wait == 1'b1) ? 1'b1 :
                     (mem_write == 1'b1 && mem_read == 1'b1 && jump == 1'b0) ? 1'b1 :
                     (reg_write == 1'b1) ? 1'b1 :
                     (jump == 1'b0 && lit_fetch == 1'b1 && lit_valid_q == 1'b0 && fetch_status_q != FETCHING_STATE) ? 1'b1 :
                     (jump == 1'b1 && unext_hit == 1'b1) ? 1'b1 :
                     1'b0;

   assign fetch = (mem_write == 1'b1 && exe_wait == 1'b0) ? 1'b0 :
                  (jump == 1'b1 && unext_hit == 1'b1 && exe_wait == 1'b0) ? 1'b0 :
                  (mem_read == 1'b1 && jump == 1'b0 && dec_wait == 1'b0) ? 1'b0 :
                  (dec_wait == 1'b1 && ((fetch_status_q == FETCHING_STATE && lit_fetch == 1'b0) ||
                                        (fetch_status_q == FETCHING_STATE && lit_valid_q == 1'b1) ||
                                        (irbuf_valid_q == 1'b1))) ? 1'b0 :
                  1'b1;

   assign dsmem_empty = (dsp_q == dsb_q[SADDR_WIDTH:0]) ? 1'b1 : 1'b0;
   assign rsmem_empty = (rsp_q == rsb_q[SADDR_WIDTH:0]) ? 1'b1 : 1'b0;
   assign unext_hit = (jump_addr == pc_q) ? 1'b1 : 1'b0;

   // Fetch logic
   always @(/*AS*/call_dec or dec_wait or early_call_q or exe_wait
            or fetch_addr_q or fetch_status_q or ir_q or jump
            or jump_addr or pc_q) begin
      if (exe_wait == 1'b0 && jump == 1'b1 && early_call_q == 1'b0) begin
         fetch_addr_d = jump_addr;
      end else if (fetch_status_q == WAIT_STATE) begin
         fetch_addr_d = pc_q;
      end else if (dec_wait == 1'b0 && call_dec == 1'b1 && early_call_q == 1'b0) begin
         fetch_addr_d = ir_q[ADDR_WIDTH-1:0];
      end else begin
         fetch_addr_d = fetch_addr_q + 1;
      end
   end

   // Fetch logic
   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         fetch_status_q <= IDLE_STATE;
         fetch_addr_q <= {ADDR_WIDTH{1'b1}};
         ir_q <= {CELL_WIDTH{1'b0}};
         irbuf_q <= {CELL_WIDTH{1'b0}};
         ir_fetched_q <= 1'b0;
         irbuf_valid_q <= 1'b0;
         lit_valid_q <= 1'b0;
         early_call_q <= 1'b0;
      end else begin
         if (~exe_wait & (interrupt | exception)) begin
            fetch_status_q <= IDLE_STATE;
            ir_q <= {CELL_WIDTH{1'b0}};
            ir_fetched_q <= 1'b0;
            irbuf_valid_q <= 1'b0;
            lit_valid_q <= 1'b0;
            early_call_q <= 1'b0;
         end else begin
            if (~exe_wait & mem_write & jump & ~early_call_q & ~unext_hit) begin
               fetch_status_q <= WAIT_STATE;
            end else if (fetch) begin
               fetch_status_q <= FETCHING_STATE;
               fetch_addr_q <= fetch_addr_d;
            end else if (fetch_status_q != WAIT_STATE) begin
               fetch_status_q <= IDLE_STATE;
            end
            if (fetch_status_q == FETCHING_STATE) begin
               if (lit_fetch & ~lit_valid_q) begin
                  lit_valid_q <= 1'b1;
               end else if (dec_wait & ~irbuf_valid_q) begin
                  irbuf_q <= rddata_i;
                  irbuf_valid_q <= 1'b1;
               end
            end
            if (~dec_wait) begin
               if (jump & ~early_call_q) begin
                  ir_q <= {CELL_WIDTH{1'b0}};
                  ir_fetched_q <= 1'b0;
               end else if (fetch_status_q == FETCHING_STATE &&
                            (early_call_q | ~lit_fetch | (lit_fetch & lit_valid_q)) == 1'b1) begin
                  ir_q <= rddata_i;
                  ir_fetched_q <= 1'b1;
               end else if (irbuf_valid_q) begin
                  ir_q <= irbuf_q;
                  ir_fetched_q <= 1'b1;
               end else begin
                  ir_q <= {CELL_WIDTH{1'b0}};
                  ir_fetched_q <= 1'b0;
               end
               lit_valid_q <= 1'b0;
               irbuf_valid_q <= 1'b0;
            end
            if (~dec_wait & fetch & call_dec & ~jump) begin
               early_call_q <= 1'b1;
            end
            if (~exe_wait & early_call_q) begin
               early_call_q <= 1'b0;
            end
         end
      end
   end

   // Instruction decoding logic
   always @(/*AS*/dsbuf_d or dsbuf_valid_d or dsmem_empty or dsp_q
            or ir_fetched_q or ir_q or lit_q or rsbuf_valid_d
            or rsmem_empty or rsp_q or uir_q) begin : u_dec
      reg ram_ext_v;

      lit_d         = lit_q;
      uir_d         = uir_q;
      uir_fetched_d = ir_fetched_q;
      dsp_d         = dsp_q;
      rsp_d         = rsp_q;
      lit_fetch     = 1'b0;
      mem_read      = 1'b0;
      popr          = 1'b0;
      popd          = 1'b0;
      call_dec      = 1'b0;
      ram_ext_v     = 1'b0;

      if (ir_q[15] == 1'b1) begin  // call
         lit_d = {ir_q[14:0], 1'b0};
         uir_d = {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, N_N_UCODE, T_T_UCODE, PC_R_UCODE, IR_PC_UCODE, R_PUSH_UCODE, NOP_ALU_UCODE};
         if (rsbuf_valid_d == RSTACK_DEPTH) begin
            rsp_d = rsp_q - 1;
         end
         call_dec = 1'b1;
      end else if (ir_q[15:12] == 4'b0100) begin  // branch
         if (ir_q[11:10] == 2'b11) begin
            uir_d = INVALID_INSTR;
         end else begin
            lit_d = {3'b000, ir_q[11:0], 1'b1};
            uir_d = {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, N_N_UCODE, T_T_UCODE, R_R_UCODE, IR_PC_UCODE, NOP_RAM_UCODE, NOP_ALU_UCODE};
         end
      end else if (ir_q[15:12] == 4'b0101) begin  // 0branch
         if (ir_q[11:10] == 2'b11) begin
            uir_d = INVALID_INSTR;
         end else begin
            lit_d = {3'b000, ir_q[11:0], 1'b1};
            uir_d = {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, N_N_UCODE, N_T_UCODE, R_R_UCODE, QIR_PC_UCODE, POP_N_UCODE, NOP_ALU_UCODE};
            if (dsmem_empty == 1'b0 && dsbuf_valid_d == DSTACK_DEPTH-DCACHE_DEPTH) begin
               mem_read = 1'b1;
               popd     = 1'b1;
               dsp_d    = dsp_q - 1;
            end
         end
      end else if (ir_q[15:12] == 4'b0110) begin  // next
         if (ir_q[11:10] == 2'b11) begin
            uir_d = INVALID_INSTR;
         end else begin
            lit_d = {3'b000, ir_q[11:0], 1'b1};
            uir_d = {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, N_N_UCODE, T_T_UCODE, M1_R_UCODE, QIR_PC_UCODE, NOP_RAM_UCODE, NOP_ALU_UCODE};
         end
      end else if (ir_q[15:12] == 4'b0111) begin  // lit
         lit_d = {ir_q[11], ir_q[11], ir_q[11], ir_q[11], ir_q[11:0]};
         uir_d = {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, T_N_UCODE, IR_T_UCODE, R_R_UCODE, P1_PC_UCODE, N_PUSH_UCODE, NOP_ALU_UCODE};
         if (dsbuf_valid_d == DSTACK_DEPTH) begin
            dsp_d = dsp_q + 1;
         end
      end else begin  // microcode
         uir_d = {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, ir_q[13:0]};
         if ((ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == POP_R_UCODE &&
              (ir_q[R_UCODE_RANGEL:R_UCODE_RANGER] == PC_R_UCODE || ir_q[R_UCODE_RANGEL:R_UCODE_RANGER] == M1_R_UCODE)) ||
             (ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == RAM_T_UCODE &&
              (ir_q[T_UCODE_RANGEL:T_UCODE_RANGER] == R_T_UCODE || ir_q[T_UCODE_RANGEL:T_UCODE_RANGER] == IR_T_UCODE ||
               (ir_q[T_UCODE_RANGEL:T_UCODE_RANGER] == T_T_UCODE && ir_q[ALU_UCODE_RANGEL:ALU_UCODE_RANGER] != NOP_ALU_UCODE)))) begin
            uir_d = INVALID_INSTR;
         end else begin
            if (ir_q[T_UCODE_RANGEL:T_UCODE_RANGER] == IR_T_UCODE || ir_q[PC_UCODE_RANGEL:PC_UCODE_RANGER] == IR_PC_UCODE || ir_q[PC_UCODE_RANGEL:PC_UCODE_RANGER] == QIR_PC_UCODE) begin
               lit_fetch = 1'b1;
            end
            if ((ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == POP_N_UCODE || ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == RAM_N_UCODE) &&
                ir_q[N_UCODE_RANGEL:N_UCODE_RANGER] == T_N_UCODE) begin
               uir_d[N_UCODE_RANGEL:N_UCODE_RANGER]      = N_N_UCODE;
               uir_d[ALU_UCODE_RANGEL:ALU_UCODE_RANGER]    = NOP_ALU_UCODE;
               uir_d[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] = ir_q[1:0];
               ram_ext_v                 = 1'b1;
            end else if (ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == POP_R_UCODE && ir_q[R_UCODE_RANGEL:R_UCODE_RANGER] == T_R_UCODE) begin
               uir_d[R_UCODE_RANGEL:R_UCODE_RANGER]      = R_R_UCODE;
               uir_d[ALU_UCODE_RANGEL:ALU_UCODE_RANGER]    = NOP_ALU_UCODE;
               uir_d[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] = ir_q[1:0];
               ram_ext_v                 = 1'b1;
            end else if (ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == RAM_T_UCODE && ir_q[T_UCODE_RANGEL:T_UCODE_RANGER] == N_T_UCODE) begin
               uir_d[T_UCODE_RANGEL:T_UCODE_RANGER]      = T_T_UCODE;
               uir_d[ALU_UCODE_RANGEL:ALU_UCODE_RANGER]    = NOP_ALU_UCODE;
               uir_d[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] = ir_q[1:0];
               ram_ext_v                 = 1'b1;
            end else if (ir_q[ALU_UCODE_RANGEL:ALU_UCODE_RANGER] != NOP_ALU_UCODE) begin
               uir_d[T_UCODE_RANGEL:T_UCODE_RANGER]      = T_T_UCODE;
               uir_d[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] = ir_q[T_UCODE_RANGEL:T_UCODE_RANGER];
               if (ir_q[T_UCODE_RANGEL:T_UCODE_RANGER] == IR_T_UCODE && ir_q[PC_UCODE_RANGEL:PC_UCODE_RANGER] != IR_PC_UCODE && ir_q[PC_UCODE_RANGEL:PC_UCODE_RANGER] != QIR_PC_UCODE) begin
                  lit_fetch = 1'b0;
               end
            end
            case (ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER])
              POP_N_UCODE: begin
                 if (dsmem_empty == 1'b0 && dsbuf_valid_d == DSTACK_DEPTH-DCACHE_DEPTH &&
                     !(ram_ext_v == 1'b1 && ir_q[1:0] == N_PUSH_EXTUCODE)) begin
                    mem_read = 1'b1;
                    popd     = 1'b1;
                    dsp_d    = dsp_q - 1;
                 end
              end
              POP_R_UCODE: begin
                 if (rsmem_empty == 1'b0 && rsbuf_valid_d == RSTACK_DEPTH-RCACHE_DEPTH) begin
                    mem_read = 1'b1;
                    popr     = 1'b1;
                    if (!(ram_ext_v == 1'b1 && ir_q[1:0] == R_PUSH_EXTUCODE && RCACHE_DEPTH == 0)) begin
                       rsp_d = rsp_q + 1;
                    end
                 end
              end
              RAM_N_UCODE: begin
                 if (dsbuf_d[CELL_WIDTH*0+4 +:CELL_WIDTH-4] != REGS_BASE[15:4]) begin
                    mem_read = 1'b1;
                 end
              end
              RAM_T_UCODE: begin
                 if (dsbuf_d[CELL_WIDTH*0+4 +:CELL_WIDTH-4] != REGS_BASE[15:4]) begin
                    mem_read = 1'b1;
                 end
              end
            endcase
            if (ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == N_PUSH_UCODE || (ram_ext_v == 1'b1 && ir_q[1:0] == N_PUSH_EXTUCODE)) begin
               if (dsbuf_valid_d == DSTACK_DEPTH && ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != POP_N_UCODE) begin
                  dsp_d = dsp_q + 1;
               end
            end else if (ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == R_PUSH_UCODE || (ram_ext_v == 1'b1 && ir_q[1:0] == R_PUSH_EXTUCODE)) begin
               if (rsbuf_valid_d == RSTACK_DEPTH && ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != POP_R_UCODE) begin
                  rsp_d = rsp_q - 1;
               end
            end else if (ir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == N_RAM_UCODE || (ram_ext_v == 1'b1 && ir_q[1:0] == N_RAM_EXTUCODE)) begin
               // no operation
            end
         end
      end
   end

   // Instruction decoding logic
   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         uir_q         <= 0;
         uir_fetched_q <= 1'b0;
         lit_q         <= 0;
         lit_fetched_q <= 1'b0;
         dsp_q         <= DS0[SADDR_WIDTH:0];
         rsp_q         <= RS0[SADDR_WIDTH:0];
         dsb_q         <= DS0;
         rsb_q         <= RS0;
         dsmem_empty_q <= 1'b0;
         rsmem_empty_q <= 1'b0;
      end else begin
         if (fetch_status_q == FETCHING_STATE && lit_fetch == 1'b1 && lit_valid_q == 1'b0) begin
            lit_q <= rddata_i;
         end
         if (exe_wait == 1'b0) begin
            if (exception == 1'b1) begin
               uir_q         <= {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, N_N_UCODE, T_T_UCODE, PC_R_UCODE, IR_PC_UCODE, NOP_RAM_UCODE, NOP_ALU_UCODE};
               uir_fetched_q <= 1'b0;
               lit_q         <= 0;
               lit_q[ADDR_WIDTH:0] <= {EXCEPTION_ADDR[ADDR_WIDTH:1], 1'b0};
               lit_fetched_q <= 1'b0;
               dsp_q         <= dsb_q[SADDR_WIDTH:0];
               rsp_q         <= rsb_q[SADDR_WIDTH:0];
            end else begin
               if (interrupt == 1'b1) begin
                  uir_q         <= {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, N_N_UCODE, T_T_UCODE, PC_R_UCODE, IR_PC_UCODE, R_PUSH_UCODE, NOP_ALU_UCODE};
                  uir_fetched_q <= 1'b0;
                  lit_q         <= 0;
                  lit_q[ADDR_WIDTH:0] <= {irqaddr_i, 1'b0};
                  lit_fetched_q <= 1'b0;
                  if (rsbuf_valid_d == RSTACK_DEPTH && tail_call == 1'b0) begin
                     rsp_q <= rsp_q - 1;
                  end
               end else if (jump == 1'b1) begin
                  if (unext_hit == 1'b1) begin
                     lit_q <= lit_q;
                  end else begin
                     uir_q         <= 0;
                     uir_fetched_q <= 1'b0;
                     lit_fetched_q <= 1'b0;
                  end
                  if (rsbuf_valid_q == RSTACK_DEPTH && tail_call == 1'b1) begin
                     rsp_q <= rsp_q + 1;  // revert return stack pointer
                  end
               end else begin
                  if (dec_wait == 1'b0) begin
                     uir_q         <= uir_d;
                     uir_fetched_q <= uir_fetched_d;
                     if (lit_fetch == 1'b0) begin
                        lit_q <= lit_d;
                     end
                     lit_fetched_q <= lit_fetch;
                     dsmem_empty_q <= dsmem_empty;
                     rsmem_empty_q <= rsmem_empty;
                     dsp_q         <= dsp_d;
                     rsp_q         <= rsp_d;
                  end else begin
                     uir_q         <= 0;
                     uir_fetched_q <= 1'b0;
                     lit_fetched_q <= 1'b0;
                  end
               end
               if (reg_write == 1'b1) begin
                  if (dsbuf_q[CELL_WIDTH*0 +:4] == DSP_ADDR[3:0]) begin
                     dsp_q <= dsbuf_q[CELL_WIDTH*1+1 +:SADDR_WIDTH+1];
                  end else if (dsbuf_q[CELL_WIDTH*0 +:4] == RSP_ADDR[3:0]) begin
                     rsp_q <= dsbuf_q[CELL_WIDTH*1+1 +:SADDR_WIDTH+1];
                  end else if (dsbuf_q[CELL_WIDTH*0 +:4] == DSB_ADDR[3:0]) begin
                     dsb_q <= dsbuf_q[CELL_WIDTH*1+1 +:ADDR_WIDTH];
                  end else if (dsbuf_q[CELL_WIDTH*0 +:4] == RSB_ADDR[3:0]) begin
                     rsb_q <= dsbuf_q[CELL_WIDTH*1+1 +:ADDR_WIDTH];
                  end
               end
            end
         end
      end
   end

   // Micro code (uir_q) execution logic
   always @(/*AS*/carry_q or dsb_q or dsbuf_q or dsbuf_valid_q
            or dsmem_empty_q or dsp_q or interrupt or ir_q or irqen_q
            or lit_fetched_q or lit_q or pc_q or rddata_i or rsb_q
            or rsbuf_q or rsbuf_valid_q or rsmem_empty_q or rsp_q
            or uir_fetched_q or uir_q or wakeup_i or wakeupid_i) begin : u_exe
      reg tail_call_v;
      reg [CELL_WIDTH-1:0] rddata_v;
      reg [CELL_WIDTH:0]   sum_v;
      reg [ADDR_WIDTH-1:0] next_pc_v;
      reg [ADDR_WIDTH-1:0] irpc_v;
      integer              i;

      // Tail call logic
      if (ir_q == 16'h00A0 && uir_fetched_q == 1'b1 && lit_fetched_q == 1'b0 &&
          uir_q == {NOP_ALU_EXTUCODE, NOP_RAM_EXTUCODE, N_N_UCODE, T_T_UCODE, PC_R_UCODE, IR_PC_UCODE, R_PUSH_UCODE, NOP_ALU_UCODE}) begin
         tail_call_v = 1'b1;
      end else begin
         tail_call_v = 1'b0;
      end
      tail_call = tail_call_v;

      // Next PC logic
      if (uir_fetched_q == 1'b1) begin
         next_pc_v = pc_q + 1;
         if (lit_fetched_q == 1'b1) begin
            next_pc_v = pc_q + 2;
         end
      end else begin
         next_pc_v = pc_q;
      end
      pc_d = next_pc_v;

      dsbuf_d = dsbuf_q;
      dsbuf_valid_d = dsbuf_valid_q;
      rsbuf_d = rsbuf_q;
      rsbuf_valid_d = rsbuf_valid_q;
      carry_d = carry_q;
      irqen_d = irqen_q;
      exception = 1'b0;
      exception_code = 0;
      jump_addr = 0;
      jump = 1'b0;
      ret = 1'b0;
      mem_write = 1'b0;
      reg_write = 1'b0;
      pushr = 1'b0;
      pushd = 1'b0;
      sleep = 1'b0;
      irqdis = 1'b0;

      // N case
      case (uir_q[N_UCODE_RANGEL:N_UCODE_RANGER]) 
        T_N_UCODE: begin
           if (dsbuf_valid_q >= 1) begin
              dsbuf_d[CELL_WIDTH*1 +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH];
              if (dsbuf_valid_q == 1) begin
                 dsbuf_valid_d = 2;
              end
           end else if (uir_q[T_UCODE_RANGEL:T_UCODE_RANGER] != R_T_UCODE && uir_q[T_UCODE_RANGEL:T_UCODE_RANGER] != IR_T_UCODE && uir_q[ALU_UCODE_RANGEL:ALU_UCODE_RANGER] != CARRY_ALU_UCODE) begin
              exception_code[INVALID_T_BIT] = 1'b1;
              exception = 1'b1; // error signaling
              $display("Exception: N<-T not valid");
           end
        end
        default: ;
      endcase

      // T case
      case (uir_q[T_UCODE_RANGEL:T_UCODE_RANGER])
        N_T_UCODE: begin
           if (dsbuf_valid_q >= 2) begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH];
              if (dsbuf_valid_q == 2 && uir_q[N_UCODE_RANGEL:N_UCODE_RANGER] != T_N_UCODE && uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != RAM_N_UCODE) begin
                 dsbuf_valid_d = 1;
              end
           end else if (dsbuf_valid_q == 1 && uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == POP_N_UCODE) begin
              dsbuf_valid_d = 0;
           end else begin
              exception = 1'b1; // error signaling
              if (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == POP_N_UCODE) begin
                 exception_code[DS_UNDERFLOW_BIT] = 1'b1;
              end else begin
                 exception_code[INVALID_N_BIT] = 1'b1;
              end
              $display("Exception: T<-N not valid");
           end
        end
        R_T_UCODE: begin
           if (rsbuf_valid_q >= 1) begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = rsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH];
              if (dsbuf_valid_q == 0) begin
                 dsbuf_valid_d = 1;
              end
           end else begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_R_BIT] = 1'b1;
              $display("Exception: T<-R not valid");
           end
        end
        IR_T_UCODE: begin
           dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = lit_q;
           if (dsbuf_valid_q == 0) begin
              dsbuf_valid_d = 1;
           end
        end
        default: ;
      endcase

      // R case
      case (uir_q[R_UCODE_RANGEL:R_UCODE_RANGER])
        M1_R_UCODE: begin
           if (rsbuf_valid_q >= 1) begin
              if (rsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH] != 0) begin
                 rsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = rsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH] - 1;
              end
           end else begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_R_BIT] = 1'b1;
              $display("Exception: R<-M1 not valid");
           end
        end
        PC_R_UCODE: begin
           if (~tail_call_v) begin
              rsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = 0;
              rsbuf_d[CELL_WIDTH*0+1 +:ADDR_WIDTH] = next_pc_v;
              if (rsbuf_valid_q == 0) begin
                 rsbuf_valid_d = 1;
              end
           end
        end
        T_R_UCODE: begin
           if (dsbuf_valid_q >= 1) begin
              rsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH];
              if (rsbuf_valid_q == 0) begin
                 rsbuf_valid_d = 1;
              end
           end else begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_T_BIT] = 1'b1;
              $display("Exception: R<-T not valid");
           end
        end
        default: ;
      endcase

      // PC logic
      irpc_v = lit_q[ADDR_WIDTH:1];
      if (lit_q[0]) begin
         if (lit_q[12:11] == 2'b00) begin
            irpc_v[ADDR_WIDTH-1:10] = pc_q[ADDR_WIDTH-1:10];
         end else if (lit_q[12:11] == 2'b01) begin
            irpc_v[ADDR_WIDTH-1:10] = pc_q[ADDR_WIDTH-1:10] + 1;
         end else if (lit_q[12:11] == 2'b10) begin
            irpc_v[ADDR_WIDTH-1:10] = pc_q[ADDR_WIDTH-1:10] - 1;
         end
      end
      jump_addr = irpc_v;

      // PC cases
      case (uir_q[PC_UCODE_RANGEL:PC_UCODE_RANGER])
        R_PC_UCODE: begin // return
           if (rsbuf_valid_q >= 1) begin
              pc_d = rsbuf_q[CELL_WIDTH*0+1 +:ADDR_WIDTH];
              ret = 1'b1;
              jump_addr = rsbuf_q[CELL_WIDTH*0+1 +:ADDR_WIDTH];
              jump = 1'b1;
           end else begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_R_BIT] = 1'b1;
              $display("Exception: PC<-R not valid");
           end
        end
        IR_PC_UCODE: begin // call/branch
           pc_d = irpc_v;
           jump = 1'b1;
        end
        QIR_PC_UCODE: begin // 0branch/next
           if (uir_q[R_UCODE_RANGEL:R_UCODE_RANGER] == M1_R_UCODE) begin
              if (rsbuf_valid_q >= 1) begin
                 if (rsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH] != 0) begin
                    pc_d = irpc_v;
                    jump = 1'b1;
                 end
              end else begin
                 exception = 1'b1; // error signaling
                 exception_code[INVALID_R_BIT] = 1'b1;
                 $display("Exception: PC<-?IR not valid");
              end
           end else begin
              if (dsbuf_valid_q >= 1) begin
                 if (dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH] == 0) begin
                    pc_d = irpc_v;
                    jump = 1'b1;
                 end
              end else begin
                 exception = 1'b1; // error signaling
                 exception_code[INVALID_T_BIT] = 1'b1;
                 $display("Exception: PC<-?IR not valid");
              end
           end
        end
        default: ;
      endcase

      // RAM access
      rddata_v = 0;
      if (dsbuf_q[CELL_WIDTH*0+4 +:CELL_WIDTH-4] == REGS_BASE/16) begin
         if (dsbuf_q[CELL_WIDTH*0 +:4] == DSP_ADDR[3:0]) begin
            rddata_v[SADDR_WIDTH+1:1] = dsp_q;
         end else if (dsbuf_q[CELL_WIDTH*0 +:4] == RSP_ADDR[3:0]) begin
            rddata_v[SADDR_WIDTH+1:1] = rsp_q;
         end else if (dsbuf_q[CELL_WIDTH*0 +:4] == DSB_ADDR[3:0]) begin
            rddata_v[ADDR_WIDTH:1] = dsb_q;
         end else if (dsbuf_q[CELL_WIDTH*0 +:4] == RSB_ADDR[3:0]) begin
            rddata_v[ADDR_WIDTH:1] = rsb_q;
         end
      end else begin
         rddata_v = rddata_i;
      end

      // RAM case
      case (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER])
        POP_N_UCODE: begin
           if (uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] != N_PUSH_EXTUCODE) begin
              if (dsbuf_valid_q >= 3 || (dsbuf_valid_q == 2 && dsmem_empty_q == 1'b1)) begin
                 dsbuf_valid_d = dsbuf_valid_q - 1;
              end
              if (DSTACK_DEPTH >= 4) begin
                 for (i = DSTACK_DEPTH-2; i >= 2; i = i - 1) begin
                    if (dsbuf_valid_q > i+1) begin
                       dsbuf_d[CELL_WIDTH*(i) +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*(i+1) +:CELL_WIDTH];
                    end
                 end
              end
           end
           if (dsbuf_valid_q >= 3) begin
              dsbuf_d[CELL_WIDTH*1 +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*2 +:CELL_WIDTH];
           end
           if (dsbuf_valid_q == DSTACK_DEPTH-DCACHE_DEPTH) begin
              if (DSTACK_DEPTH-DCACHE_DEPTH == 2 || uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] != N_PUSH_EXTUCODE) begin
                 if (~dsmem_empty_q) begin
                    dsbuf_d[CELL_WIDTH*(DSTACK_DEPTH-DCACHE_DEPTH-1) +:CELL_WIDTH] = rddata_i;
                    dsbuf_valid_d = dsbuf_valid_q;
                 end
              end
           end
           if (uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] == N_PUSH_EXTUCODE) begin
              if ((DSTACK_DEPTH-DCACHE_DEPTH == 2 && ((dsbuf_valid_q == 2 && dsmem_empty_q == 1'b1) || dsbuf_valid_q < 2)) || 
                  (DSTACK_DEPTH-DCACHE_DEPTH > 2 && dsbuf_valid_q < 3)) begin
                 exception = 1'b1; // error signaling
                 exception_code[NSWAP_ERROR_BIT] = 1'b1;
                 $display("Exception: PUSH<-N<-POP not valid");
              end
           end else if (~dsmem_empty_q) begin
              if (dsbuf_valid_q < DSTACK_DEPTH-DCACHE_DEPTH) begin
                 exception = 1'b1; // error signaling
                 if (dsbuf_valid_q == 0) begin
                    exception_code[INVALID_T_BIT] = 1'b1;
                 end
                 exception_code[INVALID_N_BIT] = 1'b1;
                 $display("Exception: N<-POP not valid");
              end
           end else if (dsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[DS_UNDERFLOW_BIT] = 1'b1;
              $display("Exception: N<-POP not valid");
           end
        end
        POP_R_UCODE: begin
           if (uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] != R_PUSH_EXTUCODE) begin
              if (rsbuf_valid_q >= 2 || (rsbuf_valid_q == 1 && rsmem_empty_q == 1'b1)) begin
                 rsbuf_valid_d = rsbuf_valid_q - 1;
              end
              if (RSTACK_DEPTH >= 3) begin
                 for (i = RSTACK_DEPTH-2; i >= 1; i = i - 1) begin
                    if (rsbuf_valid_q > i+1) begin
                       rsbuf_d[CELL_WIDTH*(i) +:CELL_WIDTH] = rsbuf_q[CELL_WIDTH*(i+1) +:CELL_WIDTH];
                    end
                 end
              end
           end
           if (rsbuf_valid_q >= 2) begin
              rsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = rsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH];
           end
           if (rsbuf_valid_q == RSTACK_DEPTH-RCACHE_DEPTH) begin
              if (RSTACK_DEPTH-RCACHE_DEPTH == 1 || uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] != R_PUSH_EXTUCODE) begin
                 if (~rsmem_empty_q) begin
                    rsbuf_d[CELL_WIDTH*(RSTACK_DEPTH-RCACHE_DEPTH-1) +:CELL_WIDTH] = rddata_i;
                    rsbuf_valid_d = rsbuf_valid_q;
                 end
              end
           end
           if (uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] == R_PUSH_EXTUCODE) begin
              if ((RSTACK_DEPTH-RCACHE_DEPTH == 1 && ((rsbuf_valid_q == 1 && rsmem_empty_q == 1'b1) || rsbuf_valid_q < 1)) || 
                  (RSTACK_DEPTH-RCACHE_DEPTH > 1 && rsbuf_valid_q < 2)) begin
                 exception = 1'b1; // error signaling
                 exception_code[RSWAP_ERROR_BIT] = 1'b1;
                 $display("Exception: PUSH<-R<-POP not valid");
              end
           end else if (~rsmem_empty_q) begin
              if (rsbuf_valid_q < RSTACK_DEPTH-RCACHE_DEPTH) begin
                 exception = 1'b1; // error signaling
                 exception_code[INVALID_R_BIT] = 1'b1;
                 $display("Exception: R<-POP not valid");
              end
           end else if (rsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[RS_UNDERFLOW_BIT] = 1'b1;
              $display("Exception: R<-POP not valid");
           end
        end
        RAM_N_UCODE: begin
           if (dsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_T_BIT] = 1'b1;
              $display("Exception: N<-RAM not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*1 +:CELL_WIDTH] = rddata_v;
              if (dsbuf_valid_q == 1) begin
                 dsbuf_valid_d = 2;
              end
           end
        end
        RAM_T_UCODE: begin
           if (dsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_T_BIT] = 1'b1;
              $display("Exception: T<-RAM not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = rddata_v;
           end
        end
        default: ;
      endcase

      // N_PUSH and N_PUSH_EXTUCODE logic
      if (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == N_PUSH_UCODE || uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] == N_PUSH_EXTUCODE) begin
         if (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != POP_N_UCODE && 
             ((dsbuf_valid_q < 2 && uir_q[N_UCODE_RANGEL:N_UCODE_RANGER] != T_N_UCODE && uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != RAM_N_UCODE) || 
              (dsbuf_valid_q == DSTACK_DEPTH && dsp_q == rsp_q))) begin
            if (dsbuf_valid_q == DSTACK_DEPTH && dsp_q == rsp_q) begin
               exception_code[DS_OVERFLOW_BIT] = 1'b1;
            end else begin
               exception_code[INVALID_N_BIT] = 1'b1;
            end
            exception = 1'b1; // error signaling
            $display("Exception: PUSH<-N not valid");
         end else begin
            if (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != POP_N_UCODE) begin
               if (dsbuf_valid_q >= 2 && dsbuf_valid_q < DSTACK_DEPTH) begin
                  dsbuf_valid_d = dsbuf_valid_q + 1;
               end
               if (DSTACK_DEPTH >= 4) begin
                  for (i = DSTACK_DEPTH-1; i >= 3; i = i - 1) begin
                     if (dsbuf_valid_q > i-1) begin
                        dsbuf_d[CELL_WIDTH*(i) +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*(i-1) +:CELL_WIDTH];
                     end
                  end
               end
            end
            if (dsbuf_valid_q >= 2) begin
               dsbuf_d[CELL_WIDTH*2 +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH];
               if (dsbuf_valid_q == 2 && uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == POP_N_UCODE) begin
                  dsbuf_valid_d = dsbuf_valid_q + 1;
               end
            end
            if (dsbuf_valid_q == DSTACK_DEPTH) begin
               if (DSTACK_DEPTH == 2 || uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != POP_N_UCODE) begin
                  mem_write = 1'b1;
                  pushd = 1'b1;
               end
            end
         end
      end else if (tail_call_v == 1'b0 && (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == R_PUSH_UCODE || uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] == R_PUSH_EXTUCODE)) begin
         if (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != POP_R_UCODE && 
             ((rsbuf_valid_q < 1 && uir_q[R_UCODE_RANGEL:R_UCODE_RANGER] == R_R_UCODE) || 
              (rsbuf_valid_q == RSTACK_DEPTH && dsp_q == rsp_q))) begin
            if (rsbuf_valid_q == RSTACK_DEPTH && dsp_q == rsp_q) begin
               exception_code[RS_OVERFLOW_BIT] = 1'b1;
            end else begin
               exception_code[INVALID_R_BIT] = 1'b1;
            end
            exception = 1'b1; // error signaling
            $display("Exception: PUSH<-R not valid");
         end else begin
            if (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != POP_R_UCODE) begin
               if (rsbuf_valid_q >= 1 && rsbuf_valid_q < RSTACK_DEPTH) begin
                  rsbuf_valid_d = rsbuf_valid_q + 1;
               end
               if (RSTACK_DEPTH >= 3) begin
                  for (i = RSTACK_DEPTH-1; i >= 2; i = i - 1) begin
                     if (rsbuf_valid_q > i-1) begin
                        rsbuf_d[CELL_WIDTH*(i) +:CELL_WIDTH] = rsbuf_q[CELL_WIDTH*(i-1) +:CELL_WIDTH];
                     end
                  end
               end
            end
            if (rsbuf_valid_q >= 1) begin
               rsbuf_d[CELL_WIDTH*1 +:CELL_WIDTH] = rsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH];
               if (rsbuf_valid_q == 1 && uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == POP_R_UCODE) begin
                  rsbuf_valid_d = rsbuf_valid_q + 1;
               end
            end
            if (rsbuf_valid_q == RSTACK_DEPTH) begin
               if (RSTACK_DEPTH == 1 || uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] != POP_R_UCODE) begin
                  mem_write = 1'b1;
                  pushr = 1'b1;
               end
            end
         end
      end else if (uir_q[RAM_UCODE_RANGEL:RAM_UCODE_RANGER] == N_RAM_UCODE || uir_q[RAM_EXTUCODE_RANGEL:RAM_EXTUCODE_RANGER] == N_RAM_EXTUCODE) begin
         if (dsbuf_valid_q < 2) begin
            exception = 1'b1; // error signaling
            if (dsbuf_valid_q == 0) begin
               exception_code[INVALID_T_BIT] = 1'b1;
            end
            exception_code[INVALID_N_BIT] = 1'b1;
            $display("Exception: RAM<-N not valid");
         end else if (dsbuf_q[CELL_WIDTH*0+4 +:CELL_WIDTH-4] == REGS_BASE/16) begin
            if (interrupt == 1'b1) begin
               exception = 1'b1; // error signaling
               exception_code[IRQ_ERROR_BIT] = 1'b1;
               $display("Exception: interrupt and REGS modification at the same time");
            end else begin
               reg_write = 1;
               if (dsbuf_q[CELL_WIDTH*0 +:4] == DSP_ADDR[3:0]) begin
                  dsbuf_valid_d = 3;
               end else if (dsbuf_q[CELL_WIDTH*0 +:4] == RSP_ADDR[3:0]) begin
                  rsbuf_valid_d = 1;
               end
            end
         end else begin
            mem_write = 1'b1;
         end
      end

      // ALU logic
      case (uir_q[ALU_UCODE_RANGEL:ALU_UCODE_RANGER]) 
        NOP_ALU_UCODE: ;
        NOT_ALU_UCODE: begin
           if (dsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_T_BIT] = 1'b1;
              $display("Exception: NOT_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = ~dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH];
           end
        end
        AND_ALU_UCODE: begin
           if (dsbuf_valid_q < 2) begin
              exception = 1'b1; // error signaling
              if (dsbuf_valid_q == 0) begin
                 exception_code[INVALID_T_BIT] = 1'b1;
              end
              exception_code[INVALID_N_BIT] = 1'b1;
              $display("Exception: AND_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH] & dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH];
           end
        end
        OR_ALU_UCODE: begin
           if (dsbuf_valid_q < 2) begin
              exception = 1'b1; // error signaling
              if (dsbuf_valid_q == 0) begin
                 exception_code[INVALID_T_BIT] = 1'b1;
              end
              exception_code[INVALID_N_BIT] = 1'b1;
              $display("Exception: OR_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH] | dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH];
           end
        end
        XOR_ALU_UCODE: begin
           if (dsbuf_valid_q < 2) begin
              exception = 1'b1; // error signaling
              if (dsbuf_valid_q == 0) begin
                 exception_code[INVALID_T_BIT] = 1'b1;
              end
              exception_code[INVALID_N_BIT] = 1'b1;
              $display("Exception: XOR_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH] ^ dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH];
           end
        end
        SHL_ALU_UCODE: begin
           if (dsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_T_BIT] = 1'b1;
              $display("Exception: SHL_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = {dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH-1], 1'b0};
              if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C1_ALU_EXTUCODE) begin
                 dsbuf_d[CELL_WIDTH*0 +:1] = carry_q;
              end
              carry_d = dsbuf_q[CELL_WIDTH*0+CELL_WIDTH-1 +:1];
           end
        end
        SHR_ALU_UCODE: begin
           if (dsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_T_BIT] = 1'b1;
              $display("Exception: SHR_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = {dsbuf_q[CELL_WIDTH*0+CELL_WIDTH-1 +:1], dsbuf_q[CELL_WIDTH*0+1 +:CELL_WIDTH-1]};
              if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C1_ALU_EXTUCODE) begin
                 dsbuf_d[CELL_WIDTH*0+CELL_WIDTH-1 +:1] = carry_q;
              end
              carry_d = dsbuf_q[CELL_WIDTH*0 +:1];
           end
        end
        ADD_ALU_UCODE: begin
           if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C1_ALU_EXTUCODE && carry_q) begin
              sum_v = {1'b0, dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + 1 + {1'b0, dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH]};
           end else begin
              sum_v = {1'b0, dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + {1'b0, dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH]};
           end
           if (dsbuf_valid_q < 2) begin
              exception = 1'b1; // error signaling
              if (dsbuf_valid_q == 0) begin
                 exception_code[INVALID_T_BIT] = 1'b1;
              end
              exception_code[INVALID_N_BIT] = 1'b1;
              $display("Exception: ADD_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = sum_v[CELL_WIDTH-1:0];
              carry_d = sum_v[CELL_WIDTH];
           end
        end
        SUB_ALU_UCODE: begin
           if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C1_ALU_EXTUCODE && carry_q) begin
              sum_v = {1'b1, ~dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + {1'b0, dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH]};
           end else begin
              sum_v = {1'b1, ~dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + 1 + {1'b0, dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH]};
           end
           if (dsbuf_valid_q < 2) begin
              exception = 1'b1; // error signaling
              if (dsbuf_valid_q == 0) begin
                 exception_code[INVALID_T_BIT] = 1'b1;
              end
              exception_code[INVALID_N_BIT] = 1'b1;
              $display("Exception: SUB_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = sum_v[CELL_WIDTH-1:0];
              carry_d = sum_v[CELL_WIDTH];
           end
        end
        MUL_ALU_UCODE: begin
           sum_v = {1'b0, dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + {1'b0, dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH]};
           if (dsbuf_valid_q < 3) begin
              exception = 1; // error signaling
              if (dsbuf_valid_q == 0) begin
                 exception_code[INVALID_T_BIT] = 1'b1;
              end
              if (dsbuf_valid_q <= 1) begin
                 exception_code[INVALID_N_BIT] = 1'b1;
              end
              exception_code[MUL_ERROR_BIT] = 1'b1;
              $display("Exception: MUL_ALU not valid");
           end else begin
              if (dsbuf_q[CELL_WIDTH*2 +:1]) begin
                 dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = sum_v[CELL_WIDTH:1];
                 dsbuf_d[CELL_WIDTH*2 +:CELL_WIDTH] = {sum_v[0], dsbuf_q[CELL_WIDTH*2+1 +:CELL_WIDTH-1]};
              end else begin
                 dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = {1'b0, dsbuf_q[CELL_WIDTH*0+1 +:CELL_WIDTH-1]};
                 dsbuf_d[CELL_WIDTH*2 +:CELL_WIDTH] = {dsbuf_q[CELL_WIDTH*0 +:1], dsbuf_q[CELL_WIDTH*2+1 +:CELL_WIDTH-1]};
              end
           end
        end
        DIV_ALU_UCODE: begin
           sum_v = {carry_q, dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + 1 + {1'b1, ~dsbuf_q[CELL_WIDTH*1 +:CELL_WIDTH]};
           if (dsbuf_valid_q < 3) begin
              exception = 1'b1; // error signaling
              if (dsbuf_valid_q == 0) begin
                 exception_code[INVALID_T_BIT] = 1'b1;
              end
              if (dsbuf_valid_q <= 1) begin
                 exception_code[INVALID_N_BIT] = 1'b1;
              end
              exception_code[DIV_ERROR_BIT] = 1'b1;
              $display("Exception: DIV_ALU not valid");
           end else if (carry_q & sum_v[CELL_WIDTH]) begin
              exception = 1'b1; // error signaling
              exception_code[DIV_ERROR_BIT] = 1'b1;
              $display("Exception: DIV_ALU dividend shifted-out");
           end else begin
              dsbuf_d[CELL_WIDTH*2 +:CELL_WIDTH] = {dsbuf_q[CELL_WIDTH*2 +:CELL_WIDTH-1], ~sum_v[CELL_WIDTH]};
              if (~sum_v[CELL_WIDTH]) begin
                 carry_d = sum_v[CELL_WIDTH-1];
                 dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = {sum_v[CELL_WIDTH-2:0], dsbuf_q[CELL_WIDTH*2+CELL_WIDTH-1 +:1]};
              end else begin
                 carry_d = dsbuf_q[CELL_WIDTH*0+CELL_WIDTH-1 +:1];
                 dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = {dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH-1], dsbuf_q[CELL_WIDTH*2+CELL_WIDTH-1 +:1]};
              end
           end
        end
        ADD1_ALU_UCODE: begin
           if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C1_ALU_EXTUCODE && carry_q) begin
              sum_v = {1'b0, dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + 2;
           end else begin
              sum_v = {1'b0, dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + 1;
           end
           if (dsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_T_BIT] = 1'b1;
              $display("Exception: ADD1_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = sum_v[CELL_WIDTH-1:0];
              carry_d = sum_v[CELL_WIDTH];
           end
        end
        ADD2_ALU_UCODE: begin
           if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C1_ALU_EXTUCODE && carry_q) begin
              sum_v = {1'b0, dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + 3;
           end else begin
              sum_v = {1'b0, dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH]} + 2;
           end
           if (dsbuf_valid_q == 0) begin
              exception = 1'b1; // error signaling
              exception_code[INVALID_T_BIT] = 1'b1;
              $display("Exception: ADD2_ALU not valid");
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = sum_v[CELL_WIDTH-1:0];
              carry_d = sum_v[CELL_WIDTH];
           end
        end
        BYTE_ALU_UCODE: begin
           if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C2_ALU_EXTUCODE) begin
              irqen_d = dsbuf_q[CELL_WIDTH*0 +:1];
              irqdis = ~dsbuf_q[CELL_WIDTH*0 +:1];
           end else if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C1_ALU_EXTUCODE) begin
              irqen_d = 1'b1;
           end
        end
        CARRY_ALU_UCODE: begin
           if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C2_ALU_EXTUCODE) begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = {CELL_WIDTH{irqen_q}};
              irqen_d = 1'b0;
              irqdis = 1'b1;
              if (dsbuf_valid_q == 0) begin
                 dsbuf_valid_d = 1;
              end
           end else if (uir_q[ALU_EXTUCODE_RANGEL:ALU_EXTUCODE_RANGER] == C1_ALU_EXTUCODE) begin
              sleep = 1'b1;
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = 0;
              if (wakeup_i) begin
                 dsbuf_d[CELL_WIDTH*0 +:WAKEUPID_WIDTH] = wakeupid_i;
              end
              if (dsbuf_valid_q == 0) begin
                 dsbuf_valid_d = 1;
              end
           end else begin
              dsbuf_d[CELL_WIDTH*0 +:CELL_WIDTH] = {CELL_WIDTH{carry_q}};
              if (dsbuf_valid_q == 0) begin
                 dsbuf_valid_d = 1;
              end
           end
        end
        default: ;
      endcase

      if (uir_q[ALU_UCODE_RANGEL:ALU_UCODE_RANGER] != NOP_ALU_UCODE && uir_q[T_UCODE_RANGEL:T_UCODE_RANGER] != T_T_UCODE) begin
         exception_code[INVALID_INSTR_BIT] = 1'b1;
         exception = 1'b1; // error signaling
         $display("Exception: invalid instruction");
      end
   end

   // Micro code (uir_q) execution logic
   always @(posedge clk_i or negedge rstn_i) begin
      if (~rstn_i) begin
         pc_q          <= 0;
         dsbuf_q       <= {(DSTACK_DEPTH*CELL_WIDTH){1'b0}};
         dsbuf_valid_q <= 0;
         rsbuf_q       <= {(RSTACK_DEPTH*CELL_WIDTH){1'b0}};
         rsbuf_valid_q <= 0;
         carry_q       <= 1'b0;
         irqen_q       <= 1'b0;
         irqack_q      <= 1'b0;
      end else begin
         irqack_q <= 1'b0;
         if (~exe_wait) begin
            dsbuf_q <= dsbuf_d;
            rsbuf_q <= rsbuf_d;
            if (exception) begin
               dsbuf_q[CELL_WIDTH*0 +:CELL_WIDTH] <= exception_code;
               dsbuf_valid_q <= 1;
               rsbuf_valid_q <= 0;
               carry_q       <= 1'b0;
               irqen_q       <= 1'b0;
            end else begin
               pc_q          <= pc_d;
               dsbuf_valid_q <= dsbuf_valid_d;
               rsbuf_valid_q <= rsbuf_valid_d;
               carry_q       <= carry_d;
               if (interrupt) begin
                  irqen_q  <= 1'b0;
                  irqack_q <= 1'b1;
               end else begin
                  irqen_q <= irqen_d;
               end
            end
         end
      end
   end
endmodule
