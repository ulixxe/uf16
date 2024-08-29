// LogicCell simulation model
// SEQ_MODE is the timing paramer and base on the cbit[19:16] of logic cell
// C_ON parameter is the nontiming parameter base on cbit[20] of logic cell and used for carry logic
// LUT_INIT parameter is the nontiming parameter base on cbit[15:0] of the logic cell. It is same as the LUT_INIT parameter in EDIF file.
module LogicCell ( carryout, lcout, carryin, clk, clkb, in0,
     in1, in2, in3, sr );

   parameter SEQ_MODE = 4'b0000; //cbit[19:16]  timing parameter
   parameter C_ON = 1'b0; //cbit[20]
   parameter LUT_INIT = 16'b0000000000000000; //cbit[15:0]

output  carryout, lcout;

input  carryin, clk, clkb, in0, in1, in2, in3, sr;

pulldown(gnd_);

    logic_cell LC (
            .cbit({C_ON,SEQ_MODE,LUT_INIT}),
            .carry_in(carryin),
            .carry_out(carryout),
            .clk(clk),
            .clkb(clkb),
            .in0(in0),
            .in1(in1),
            .in2(in2),
            .in3(in3),
            .lc_out(lcout),
            .prog(gnd_),
            .purst(gnd_),
            .s_r(sr));
`ifdef TIMINGCHECK
specify
   (carryin *> carryout) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	(in0 *> lcout) = (1.0, 1.0);
  // (negedge in0 *> lc_out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in1 *> lcout) = (1.0, 1.0);
  // (negedge in1 *> out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in2 *> lcout) = (1.0, 1.0);
 //  (negedge in2 *> out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in3 *> lcout) = (1.0, 1.0);
 //  (negedge in3 *> out) = (1.0, 1.0);
    if (SEQ_MODE != 4'b0000)
   ( clk *> lcout ) = (1.0, 1.0);
      if (SEQ_MODE == 4'b0000)
   ( clkb *> lcout ) = (1.0, 1.0);
      if (SEQ_MODE == 4'b0000)
   (sr *> lcout) = (1.0,1.0);
   
   (in1 *> carryout) = (1.0, 1.0);
   (in2 *> carryout) = (1.0, 1.0);
   $setup(posedge in0, posedge clk, 1.0);
   $hold(posedge clk, posedge in0, 1.0);
   $setup(negedge in0, posedge clk, 1.0);
   $hold(posedge clk, negedge in0, 1.0);
   $setup(posedge in1, posedge clk, 1.0);
   $hold(posedge clk, posedge in1, 1.0);
   $setup(negedge in1, posedge clk, 1.0);
   $hold(posedge clk, negedge in1, 1.0);
   $setup(posedge in2, posedge clk, 1.0);
   $hold(posedge clk, posedge in2, 1.0);
   $setup(negedge in2, posedge clk, 1.0);
   $hold(posedge clk, negedge in2, 1.0);
   $setup(posedge in3, posedge clk, 1.0);
   $hold(posedge clk, posedge in3, 1.0);
   $setup(negedge in3, posedge clk, 1.0);
   $hold(posedge clk, negedge in3, 1.0);
   $setup(posedge sr, posedge clk, 1.0);
   $setup(negedge sr, posedge clk, 1.0);
   $hold(posedge clk, posedge sr, 1.0);
   $hold(posedge clk, negedge sr, 1.0);
   $recovery(posedge sr, posedge clk, 1.0);
   $recovery(negedge sr, posedge clk, 1.0);
   $removal(posedge sr, posedge clk, 1.0);
   $removal(negedge sr, posedge clk, 1.0);
endspecify
`endif


endmodule

module LogicCell2 ( carryout, lcout, carryin, clk, in0,
     in1, in2, in3, sr, ce );

   parameter SEQ_MODE = 4'b0000; //cbit[19:16]  timing parameter
   parameter C_ON = 1'b0; //cbit[20]
   parameter LUT_INIT = 16'b0000000000000000; //cbit[15:0]

output  carryout, lcout;

input  carryin, clk, in0, in1, in2, in3, sr, ce;

wire clkb = 0;
pulldown(gnd_);

    logic_cell2 LC (
            .cbit({C_ON,SEQ_MODE,LUT_INIT}),
            .carry_in(carryin),
            .carry_out(carryout),
            .clk(clk),
            .clkb(clkb),
            .in0(in0),
            .in1(in1),
            .in2(in2),
            .in3(in3),
            .lc_out(lcout),
            .prog(gnd_),
            .purst(gnd_),
            .s_r(sr),
            .ce(ce));
`ifdef TIMINGCHECK
specify
   (carryin *> carryout) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	(in0 *> lcout) = (1.0, 1.0);
  // (negedge in0 *> lc_out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in1 *> lcout) = (1.0, 1.0);
  // (negedge in1 *> out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in2 *> lcout) = (1.0, 1.0);
 //  (negedge in2 *> out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in3 *> lcout) = (1.0, 1.0);
 //  (negedge in3 *> out) = (1.0, 1.0);
    if (SEQ_MODE != 4'b0000)
   ( clk *> lcout ) = (1.0, 1.0);
      if (SEQ_MODE == 4'b0000)
   (sr *> lcout) = (1.0,1.0);
   
   (in1 *> carryout) = (1.0, 1.0);
   (in2 *> carryout) = (1.0, 1.0);
   $setup(posedge ce, posedge clk, 1.0);
   $hold(posedge clk, posedge ce, 1.0);
   $setup(negedge ce, posedge clk, 1.0);
   $hold(posedge clk, negedge ce, 1.0);
   $setup(posedge in0, posedge clk, 1.0);
   $hold(posedge clk, posedge in0, 1.0);
   $setup(negedge in0, posedge clk, 1.0);
   $hold(posedge clk, negedge in0, 1.0);
   $setup(posedge in1, posedge clk, 1.0);
   $hold(posedge clk, posedge in1, 1.0);
   $setup(negedge in1, posedge clk, 1.0);
   $hold(posedge clk, negedge in1, 1.0);
   $setup(posedge in2, posedge clk, 1.0);
   $hold(posedge clk, posedge in2, 1.0);
   $setup(negedge in2, posedge clk, 1.0);
   $hold(posedge clk, negedge in2, 1.0);
   $setup(posedge in3, posedge clk, 1.0);
   $hold(posedge clk, posedge in3, 1.0);
   $setup(negedge in3, posedge clk, 1.0);
   $hold(posedge clk, negedge in3, 1.0);
   $setup(posedge sr, posedge clk, 1.0);
   $setup(negedge sr, posedge clk, 1.0);
   $hold(posedge clk, posedge sr, 1.0);
   $hold(posedge clk, negedge sr, 1.0);
   $recovery(posedge sr, posedge clk, 1.0);
   $recovery(negedge sr, posedge clk, 1.0);
   $removal(posedge sr, posedge clk, 1.0);
   $removal(negedge sr, posedge clk, 1.0);
endspecify
`endif


endmodule //LogicCell2 

module LogicCell40 ( carryout, lcout, ltout, carryin, clk, in0,
     in1, in2, in3, sr, ce );

   parameter SEQ_MODE = 4'b0000; //cbit[19:16]  timing parameter
   parameter C_ON = 1'b0; //cbit[20]
   parameter LUT_INIT = 16'b0000000000000000; //cbit[15:0]

output  carryout, lcout, ltout;

input  carryin, clk, in0, in1, in2, in3, sr, ce;

wire clkb = 0;

pulldown(gnd_);

    logic_cell40 LC (
            .cbit({C_ON,SEQ_MODE,LUT_INIT}),
            .carry_in(carryin),
            .carry_out(carryout),
            .clk(clk),
            .clkb(clkb),
            .in0(in0),
            .in1(in1),
            .in2(in2),
            .in3(in3),
            .lc_out(lcout),
            .lt_out(ltout),
            .prog(gnd_),
            .purst(gnd_),
            .s_r(sr),
            .ce(ce));
`ifdef TIMINGCHECK
specify
   (carryin *> carryout) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	(in0 *> lcout) = (1.0, 1.0);
  // (negedge in0 *> lc_out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in1 *> lcout) = (1.0, 1.0);
  // (negedge in1 *> out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in2 *> lcout) = (1.0, 1.0);
 //  (negedge in2 *> out) = (1.0, 1.0);
   if (SEQ_MODE == 4'b0000)
   	( in3 *> lcout) = (1.0, 1.0);
 //  (negedge in3 *> out) = (1.0, 1.0);
    if (SEQ_MODE != 4'b0000)
   ( clk *> lcout ) = (1.0, 1.0);
      if (SEQ_MODE == 4'b0000)
   (sr *> lcout) = (1.0,1.0);
   
   (in1 *> carryout) = (1.0, 1.0);
   (in2 *> carryout) = (1.0, 1.0);
   (in0 *> ltout) = (1.0, 1.0);
   (in1 *> ltout) = (1.0, 1.0);
   (in2 *> ltout) = (1.0, 1.0);
   (in3 *> ltout) = (1.0, 1.0);
   $setup(posedge ce, posedge clk, 1.0);
   $hold(posedge clk, posedge ce, 1.0);
   $setup(negedge ce, posedge clk, 1.0);
   $hold(posedge clk, negedge ce, 1.0);
   $setup(posedge in0, posedge clk &&& ce, 1.0);
   $hold(posedge clk, posedge in0, 1.0);
   $setup(negedge in0, posedge clk &&& ce, 1.0);
   $hold(posedge clk, negedge in0, 1.0);
   $setup(posedge in1, posedge clk &&& ce, 1.0);
   $hold(posedge clk, posedge in1, 1.0);
   $setup(negedge in1, posedge clk &&& ce, 1.0);
   $hold(posedge clk, negedge in1, 1.0);
   $setup(posedge in2, posedge clk &&& ce, 1.0);
   $hold(posedge clk, posedge in2, 1.0);
   $setup(negedge in2, posedge clk &&& ce, 1.0);
   $hold(posedge clk, negedge in2, 1.0);
   $setup(posedge in3, posedge clk &&& ce, 1.0);
   $hold(posedge clk, posedge in3, 1.0);
   $setup(negedge in3, posedge clk &&& ce, 1.0);
   $hold(posedge clk, negedge in3, 1.0);
   $setup(posedge sr, posedge clk, 1.0);
   $setup(negedge sr, posedge clk, 1.0);
   $hold(posedge clk, posedge sr, 1.0);
   $hold(posedge clk, negedge sr, 1.0);
   $recovery(posedge sr, posedge clk, 1.0);
   $recovery(negedge sr, posedge clk, 1.0);
   $removal(posedge sr, posedge clk, 1.0);
   $removal(negedge sr, posedge clk, 1.0);
endspecify
`endif

endmodule //LogicCell40

module INV (I, O);
input I;
output O;
   assign O = ~I ;
   
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif
endmodule

module AND2 (A, B, O);
input A, B;
output O;
   assign O = A & B ;

`ifdef TIMINGCHECK
specify
   (A *> O) = (1.0, 1.0);
   (B *> O) = (1.0, 1.0);
endspecify
`endif
endmodule


module inv_hvt (Y, A);
    output Y;
    input A;

	assign Y = !A;

endmodule

module logic_cell ( carry_out, lc_out, carry_in, cbit, clk, clkb, in0,
     in1, in2, in3, prog, purst, s_r );
output  carry_out, lc_out;

input  carry_in, clk, clkb, in0, in1, in2, in3, prog, purst, s_r;

input [20:0]  cbit;
supply0 gnd_;
supply1 vdd_;



coredffr REG ( .purst(purst), .d(LUT4_outd), .q(rego),
     .cbit(cbit[17:16]), .clkb(clkb), .clk(clk), .S_R(s_r));
carry_logic ICARRY_LOGIC ( .b_bar(in1b1), .carry_in(carry_in), .b(in1),
     .cout(carry_out), .a(in2), .a_bar(in2b1), .vg_en(cbit[20]));
o_mux Iomux ( .in1(rego), .O(lc_out), .cbit(cbit[19]), .prog(prog),
     .in0(LUT4_outd));
clut4 iclut4 ( .in0b(in0b1), .in3b(in3b1), .in2b(in2b1),
     .lut4(LUT4_outd), .in1b(in1b1), .in2(in2), .in1(in1), .in0(in0),
     .in3(in3), .cbit(cbit[15:0]));
inv_hvt I163 ( .A(in3), .Y(in3b1));
inv_hvt I164 ( .A(in1), .Y(in1b1));
inv_hvt I162 ( .A(in2), .Y(in2b1));
inv_hvt I161 ( .A(in0), .Y(in0b1));

endmodule

module logic_cell2 ( carry_out, lc_out, carry_in, cbit, clk, clkb, in0,
     in1, in2, in3, prog, purst, s_r, ce);
output  carry_out, lc_out;

input  carry_in, clk, clkb, in0, in1, in2, in3, prog, purst, s_r, ce;

input [20:0]  cbit;
supply0 gnd_;
supply1 vdd_;



coredffr2 REG ( .purst(purst), .d(LUT4_outd), .q(rego),
     .cbit(cbit[17:16]), .clkb(clkb), .clk(clk), .S_R(s_r), .ce(ce));
carry_logic ICARRY_LOGIC ( .b_bar(in1b1), .carry_in(carry_in), .b(in1),
     .cout(carry_out), .a(in2), .a_bar(in2b1), .vg_en(cbit[20]));
o_mux Iomux ( .in1(rego), .O(lc_out), .cbit(cbit[19]), .prog(prog),
     .in0(LUT4_outd));
clut4 iclut4 ( .in0b(in0b1), .in3b(in3b1), .in2b(in2b1),
     .lut4(LUT4_outd), .in1b(in1b1), .in2(in2), .in1(in1), .in0(in0),
     .in3(in3), .cbit(cbit[15:0]));
inv_hvt I163 ( .A(in3), .Y(in3b1));
inv_hvt I164 ( .A(in1), .Y(in1b1));
inv_hvt I162 ( .A(in2), .Y(in2b1));
inv_hvt I161 ( .A(in0), .Y(in0b1));

endmodule //logic_cell2

module logic_cell40 ( carry_out, lc_out, lt_out, carry_in, cbit, clk, clkb, in0,
     in1, in2, in3, prog, purst, s_r, ce);
output  carry_out, lc_out, lt_out;

input  carry_in, clk, clkb, in0, in1, in2, in3, prog, purst, s_r, ce;

input [20:0]  cbit;
supply0 gnd_;
supply1 vdd_;

coredffr2 REG ( .purst(purst), .d(LUT4_outd), .q(rego),
     .cbit(cbit[17:16]), .clkb(clkb), .clk(clk), .S_R(s_r), .ce(ce));
carry_logic ICARRY_LOGIC ( .b_bar(in1b1), .carry_in(carry_in), .b(in1),
     .cout(carry_out), .a(in2), .a_bar(in2b1), .vg_en(cbit[20]));
o_mux Iomux ( .in1(rego), .O(lc_out), .cbit(cbit[19]), .prog(prog),
     .in0(LUT4_outd));
clut4 iclut4 ( .in0b(in0b1), .in3b(in3b1), .in2b(in2b1),
     .lut4(LUT4_outd), .in1b(in1b1), .in2(in2), .in1(in1), .in0(in0),
     .in3(in3), .cbit(cbit[15:0]));
inv_hvt I163 ( .A(in3), .Y(in3b1));
inv_hvt I164 ( .A(in1), .Y(in1b1));
inv_hvt I162 ( .A(in2), .Y(in2b1));
inv_hvt I161 ( .A(in0), .Y(in0b1));

assign lt_out = LUT4_outd;

endmodule //logic_cell40

//************************************************
//Title:    coredffr
//Design:   cordeffr.v
//Author:  
//Company:  SiliconBlue Technologies, Inc.
//Version:  D0.2
//
//INIT: March 15, 2007
//
//Revision : June 9, 2007  Modify name, add purst.
//           and change polarity
//************************************************
`timescale 10ps/1ps
module coredffr (q, d, purst, S_R, cbit, clk, clkb);
output q;
input d, purst, S_R, clk, clkb;
input [1:0] cbit;


`ifdef TIMINGCHECK
reg NOTIFIER;

  specify
    specparam
    tplh$sr$q = 1.0,
    tphl$sr$q = 1.0,
    tplh$purst$q = 1.0,
    tphl$purst$q  = 1.0,
    tplh$clk$q = 1.0,
    tphl$clk$q = 1.0,
    tsetup$d$clk = 1.0,
    thold$d$clk	= 1.0,
    tsetup$srsr$clk = 1.0,
    thold$srsr$clk = 1.0,
    tminpwh$sr     = 1.0,
    tminpwl$clk    = 1.0,
    tminpwh$clk    = 1.0;

//path delays
    if (S_R == 1'b0)
      (posedge clk *> (q +: d)) = (tplh$clk$q, tphl$clk$q);
    (posedge purst *> (q  +: 0) ) = (tplh$purst$q, tphl$purst$q);
    if (cbit[1:0] == 2'b10)
      (posedge S_R *> (q  +: 1) ) = (tplh$sr$q, tphl$sr$q);
    if (cbit[1:0] == 2'b11)
      (posedge S_R *> (q  +: 0) ) = (tplh$sr$q, tphl$sr$q);
    if (cbit[1:0] == 2'b00)
      (posedge clk *> (q +: 1)) = (tplh$sr$q, tphl$sr$q);
    if (cbit[1:0] == 2'b01)
      (posedge clk *> (q +: 0)) = (tplh$sr$q, tphl$sr$q);

//timing checks     
    $setuphold(posedge clk &&& (S_R == 1'b0), posedge d, tsetup$d$clk, thold$d$clk, NOTIFIER);
    $setuphold(posedge clk &&& (S_R == 1'b0), negedge d, tsetup$d$clk, thold$d$clk, NOTIFIER);    
    $setuphold(posedge clk &&& (cbit[1] == 1'b0), posedge S_R , tsetup$srsr$clk, thold$srsr$clk, NOTIFIER);
    $width(posedge S_R &&& (cbit[1] == 1'b1), tminpwh$sr, 0, NOTIFIER);
    $width(negedge clk, tminpwl$clk, 0, NOTIFIER);
    $width(posedge clk, tminpwh$clk, 0, NOTIFIER); 
   endspecify
`endif

reg qint = 0;  
wire intasr;
assign intasr = purst || (S_R && cbit[1]);

assign q = (((purst ^ S_R ^ cbit[0] ^ cbit[1])==1) || ((purst ^ S_R ^ cbit[0] ^ cbit[1])==0)) ? qint : 1'b0;

always @(posedge intasr or posedge clk)
   if (S_R && cbit===2'b11) qint <= 1;
   else if (!purst && S_R && cbit===2'b10) qint <= 0;
   else if (purst && !S_R) qint <= 0;
   else begin
      if (!purst && S_R && cbit===2'b00) qint <= 1'b0;
      else if (!purst && S_R && cbit===2'b01) qint <= 1'b1;
      else if (!purst&& !S_R) qint <= d;
      end
      
endmodule // coredffr

//************************************************
//Title:    coredffr2
//Design:   cordeffr.v
//Author:  
//Company:  SiliconBlue Technologies, Inc.
//Version:  D0.2
//
//INIT: March 15, 2007
//
//Revision : October 1, 2010  added ce.
//           
//************************************************
`timescale 10ps/1ps
module coredffr2 (q, d, purst, S_R, cbit, clk, clkb, ce);
output q;
input d, purst, S_R, clk, clkb, ce;
input [1:0] cbit;


`ifdef TIMINGCHECK
reg NOTIFIER;
  
assign cond_D = (S_R == 1'b0 && ce == 1'b1);

  specify
    specparam
    tplh$sr$q = 1.0,
    tphl$sr$q = 1.0,
    tplh$purst$q = 1.0,
    tphl$purst$q  = 1.0,
    tplh$clk$q = 1.0,
    tphl$clk$q = 1.0,
    tsetup$d$clk = 1.0,
    thold$d$clk	= 1.0,
    tsetup$ce$clk = 1.0,
    thold$ce$clk	= 1.0,
    tsetup$srsr$clk = 1.0,
    thold$srsr$clk = 1.0,
    tminpwh$sr     = 1.0,
    tminpwl$clk    = 1.0,
    tminpwh$clk    = 1.0;

//path delays
    if (S_R == 1'b0)
      (posedge clk *> (q +: d)) = (tplh$clk$q, tphl$clk$q);
    (posedge purst *> (q  +: 0) ) = (tplh$purst$q, tphl$purst$q);
    if (cbit[1:0] == 2'b10)
      (posedge S_R *> (q  +: 1) ) = (tplh$sr$q, tphl$sr$q);
    if (cbit[1:0] == 2'b11)
      (posedge S_R *> (q  +: 0) ) = (tplh$sr$q, tphl$sr$q);
    if (cbit[1:0] == 2'b00)
      (posedge clk *> (q +: 1)) = (tplh$sr$q, tphl$sr$q);
    if (cbit[1:0] == 2'b01)
      (posedge clk *> (q +: 0)) = (tplh$sr$q, tphl$sr$q);

//timing checks     
    $setuphold(posedge clk &&& cond_D, posedge d, tsetup$d$clk, thold$d$clk, NOTIFIER);
    $setuphold(posedge clk &&& cond_D, negedge d, tsetup$d$clk, thold$d$clk, NOTIFIER);    
    $setuphold(posedge clk &&& (cbit[1] == 1'b0), posedge S_R , tsetup$srsr$clk, thold$srsr$clk, NOTIFIER);
    $width(posedge S_R &&& (cbit[1] == 1'b1), tminpwh$sr, 0, NOTIFIER);
    $width(negedge clk, tminpwl$clk, 0, NOTIFIER);
    $width(posedge clk, tminpwh$clk, 0, NOTIFIER); 
   endspecify
`endif

reg qint = 0;  
wire intasr;
assign intasr = purst || (S_R && cbit[1]);

assign q = (((purst ^ S_R ^ cbit[0] ^ cbit[1])==1) || ((purst ^ S_R ^ cbit[0] ^ cbit[1])==0)) ? qint : 1'b0;
assign (weak0, weak1) ce = 1'b1 ;

always @(posedge intasr or posedge clk)
   if (S_R && cbit===2'b11) qint <= 1;
   else if (!purst && S_R && cbit===2'b10) qint <= 0;
   else if (purst && !S_R) qint <= 0;
   else begin
      if (!purst && S_R && cbit===2'b00) qint <= 1'b0;
      else if (!purst && S_R && cbit===2'b01) qint <= 1'b1;
      else if (!purst && !S_R && ce) qint <= d;
      end
      
endmodule // coredffr2


//************************************************
//Title:    clut4
//Design:   clut4.v
//Author:  
//Company:  SiliconBlue Technologies, Inc.
//Version:  D0.2
//
//INIT: March 15, 2007
//
//Revision : June 9, 2007  Modify port names
//************************************************
`timescale 10ps/1ps
module clut4 (lut4, in0, in1, in2, in3, in0b, in1b, in2b, in3b, cbit);

//the output signal
output lut4;

//the input signals
input in0, in1, in2, in3, in0b, in1b, in2b, in3b;
input [15:0] cbit;

reg lut4;
      
`ifdef TIMINGCHECK
  specify
    // delay parameters
    specparam
      tplh$cbit0$lut4= 1.0,
      tphl$cbit0$lut4= 1.0,
      tplh$cbit1$lut4= 1.0,
      tphl$cbit1$lut4= 1.0,
      tplh$cbit2$lut4= 1.0,
      tphl$cbit2$lut4= 1.0,
      tplh$cbit3$lut4= 1.0,
      tphl$cbit3$lut4= 1.0,
      tplh$cbit4$lut4= 1.0,
      tphl$cbit4$lut4= 1.0,
      tplh$cbit5$lut4= 1.0,
      tphl$cbit5$lut4= 1.0,
      tplh$cbit6$lut4= 1.0,
      tphl$cbit6$lut4= 1.0,
      tplh$cbit7$lut4= 1.0,
      tphl$cbit7$lut4= 1.0,
      tplh$cbit8$lut4= 1.0,
      tphl$cbit8$lut4= 1.0,
      tplh$cbit9$lut4= 1.0,
      tphl$cbit9$lut4= 1.0,
      tplh$cbit10$lut4= 1.0,
      tphl$cbit10$lut4= 1.0,
      tplh$cbit11$lut4= 1.0,
      tphl$cbit11$lut4= 1.0,
      tplh$cbit12$lut4= 1.0,
      tphl$cbit12$lut4= 1.0,
      tplh$cbit13$lut4= 1.0,
      tphl$cbit13$lut4= 1.0,
      tplh$cbit14$lut4= 1.0,
      tphl$cbit14$lut4= 1.0,
      tplh$cbit15$lut4= 1.0,
      tphl$cbit15$lut4= 1.0,
      tplh$in3$lut4= 1.0,
      tphl$in3$lut4= 1.0,
      tplh$in2$lut4= 1.0,
      tphl$in2$lut4= 1.0,
      tplh$in1$lut4= 1.0,
      tphl$in1$lut4= 1.0,
      tplh$in0$lut4= 1.0,
      tphl$in0$lut4= 1.0,
      tplh$in3b$lut4= 1.0,
      tphl$in3b$lut4= 1.0,
      tplh$in2b$lut4= 1.0,
      tphl$in2b$lut4= 1.0,
      tplh$in1b$lut4= 1.0,
      tphl$in1b$lut4= 1.0,
      tplh$in0b$lut4= 1.0,
      tphl$in0b$lut4= 1.0;

    // path delays
     (cbit[15] *> lut4) = (tplh$cbit15$lut4, tphl$cbit15$lut4);
     (cbit[14] *> lut4) = (tplh$cbit14$lut4, tphl$cbit14$lut4);
     (cbit[13] *> lut4) = (tplh$cbit13$lut4, tphl$cbit13$lut4);
     (cbit[12] *> lut4) = (tplh$cbit12$lut4, tphl$cbit12$lut4);
     (cbit[11] *> lut4) = (tplh$cbit11$lut4, tphl$cbit11$lut4);
     (cbit[10] *> lut4) = (tplh$cbit10$lut4, tphl$cbit10$lut4);
     (cbit[9] *> lut4) = (tplh$cbit9$lut4, tphl$cbit9$lut4);
     (cbit[8] *> lut4) = (tplh$cbit8$lut4, tphl$cbit8$lut4);
     (cbit[7] *> lut4) = (tplh$cbit7$lut4, tphl$cbit7$lut4);
     (cbit[6] *> lut4) = (tplh$cbit6$lut4, tphl$cbit6$lut4);
     (cbit[5] *> lut4) = (tplh$cbit5$lut4, tphl$cbit5$lut4);
     (cbit[4] *> lut4) = (tplh$cbit4$lut4, tphl$cbit4$lut4);
     (cbit[3] *> lut4) = (tplh$cbit3$lut4, tphl$cbit3$lut4);
     (cbit[2] *> lut4) = (tplh$cbit2$lut4, tphl$cbit2$lut4);
     (cbit[1] *> lut4) = (tplh$cbit1$lut4, tphl$cbit1$lut4);
     (cbit[0] *> lut4) = (tplh$cbit0$lut4, tphl$cbit0$lut4);
     (in3 *> lut4) = (tplh$in3$lut4, tphl$in3$lut4);
     (in2 *> lut4) = (tplh$in2$lut4, tphl$in2$lut4);
     (in1 *> lut4) = (tplh$in1$lut4, tphl$in1$lut4);
     (in0 *> lut4) = (tplh$in0$lut4, tphl$in0$lut4);     
     (in3b *> lut4) = (tplh$in3b$lut4, tphl$in3b$lut4);
     (in2b *> lut4) = (tplh$in2b$lut4, tphl$in2b$lut4);
     (in1b *> lut4) = (tplh$in1b$lut4, tphl$in1b$lut4);
     (in0b *> lut4) = (tplh$in0b$lut4, tphl$in0b$lut4);     
  endspecify
`endif

   reg tmp;

   always @(in0 or in1 or in2 or in3 or in0b or in1b or in2b or in3b or cbit) begin

      tmp = in0 ^ in1 ^ in2 ^ in3;

      if ({in3, in2, in1, in0} != ~{in3b, in2b, in1b, in0b})
         lut4 = 1'bx;
      else if (tmp == 0 || tmp == 1)
         lut4 = cbit[{in3, in2, in1, in0}];
      else
         lut4 = lut_mux ({lut_mux (cbit[15:12], {in1, in0}), lut_mux (cbit[11:8], {in1, in0}), lut_mux (cbit[7:4], {in1, in0}), lut_mux (cbit[3:0], {in1, in0})}, {in3, in2});

   end


   function lut_mux;
   input [3:0] d;
   input [1:0] s;

      begin

         if ((s[1]^s[0] ==1) || (s[1]^s[0] ==0))
            lut_mux = d[s];
         else if ((d[0] ^ d[1]) == 0 && (d[2] ^ d[3]) == 0 && (d[0] ^ d[2]) == 0)
            lut_mux = d[0];
         else if ((s[1] == 0) && (d[0] == d[1]))
            lut_mux = d[0];
         else if ((s[1] == 1) && (d[2] == d[3]))
            lut_mux = d[2];
         else if ((s[0] == 0) && (d[0] == d[2]))
            lut_mux = d[0];
         else if ((s[0] == 1) && (d[1] == d[3]))
            lut_mux = d[1];
         else
            lut_mux = 1'bx;

      end

   endfunction


endmodule // clut4

//************************************************
//Title:    carry_logic
//Design:   carry_logic.v
//Author:  
//Company:  SiliconBlue Technologies, Inc.
//Version:  D0.1
//
//INIT: June 8, 2007
//************************************************
// `timescale 10ps/1ps
`celldefine
module carry_logic (cout, carry_in, a, a_bar, b, b_bar, vg_en);

//the output signal
output cout;

//the input signals
input carry_in, a, a_bar, b, b_bar, vg_en;

`ifdef TIMINGCHECK  
  specify
    // delay parameters
    specparam
      tplh$carry_in$cout= 1.0,
      tphl$carry_in$cout= 1.0,
      tplh$a$cout= 1.0,
      tphl$a$cout= 1.0,
      tplh$a_bar$cout= 1.0,
      tphl$a_bar$cout= 1.0,
      tplh$b$cout= 1.0,
      tphl$b$cout= 1.0,
      tplh$b_bar$cout= 1.0,
      tphl$b_bar$cout= 1.0,
      tplh$vg_en$cout= 1.0,
      tphl$vg_en$cout= 1.0;

    // path delays
     (carry_in *> cout) = (tplh$carry_in$cout, tphl$carry_in$cout);
     (a *> cout) = (tplh$a$cout, tphl$a$cout);
     (a_bar *> cout) = (tplh$a_bar$cout, tphl$a_bar$cout);
     (b *> cout) = (tplh$b$cout, tphl$b$cout);
     (b_bar *> cout) = (tplh$b_bar$cout, tphl$b_bar$cout);
     (vg_en *> cout) = (tplh$vg_en$cout, tphl$vg_en$cout); 
  endspecify
`endif
  
  primit_carry_logic (cout, vg_en, carry_in, a, b );

endmodule // carry_logic
`endcelldefine

//************************************************
//Title:    primit_carry_logic
//Design:   carry_logic.v
//Author:  
//Company: SiliconBlue technologies, Inc.
//revision: June 9, 2007
//************************************************

primitive primit_carry_logic (y, v, c, a, b);
   output y;  
   input  v, c, a, b;

   table

// v  c  a  b  :  y
//
   0  ?  ?  ?  :  1;
   1  0  0  0  :  0;
   1  0  0  1  :  0;
   1  0  1  0  :  0;
   1  0  1  1  :  1;
   1  1  0  0  :  0;
   1  1  0  1  :  1;
   1  1  1  0  :  1;
   1  1  1  1  :  1;
   endtable
endprimitive // primit_o_mux

//************************************************
//Title:    o_mux
//Design:   o_mux.v
//Author:  
//Company:  SiliconBlue Technologies, Inc.
//Version:  D0.2
//
//INIT: March 15, 2007
//
//Revision : June 9, 2007  Modify module name and
//             change function
//************************************************
`timescale 10ps/1ps
`celldefine
module o_mux (O, in0, in1, cbit, prog);

//the output signal
output O;

//the input signals
input in0, in1, cbit, prog;

  primit_o_mux (O, in0, in1, cbit, prog);

`ifdef TIMINGCHECK
  specify
    // delay parameters
    specparam
      tplh$in0$out = 1.0,
      tphl$in0$out = 1.0,
      tplh$in1$out = 1.0,
      tphl$in1$out = 1.0,
      tplh$prog$out = 1.0,
      tphl$prog$out = 1.0,
      tplh$cbit$out = 1.0,
      tphl$cbit$out = 1.0;

    // path delays
     (prog *> O) = (tplh$prog$out, tphl$prog$out);
     (cbit *> O) = (tplh$cbit$out, tphl$cbit$out);
     (in0 *> O) = (tplh$in0$out, tphl$in0$out);
     (in1 *> O) = (tplh$in1$out, tphl$in1$out);
  endspecify
`endif

endmodule // o_mux
`endcelldefine

//************************************************
//Title:    primit_o_mux
//Design:   o_mux.v
//Author:  
//Company: SiliconBlue technologies, Inc.
//revision: March 15, 2007
//************************************************

primitive primit_o_mux (y, a0, a1, c, p);
   output y;  
   input  a0, a1, c, p;

   table

// a0 a1 c  p  :  y
//
   ?  0  ?  1  :  0;
   ?  1  ?  1  :  1;
   ?  x  ?  1  :  x;
   0  ?  0  0  :  0;
   1  ?  0  0  :  1;
   ?  0  1  0  :  0;
   ?  1  1  0  :  1;
   ?  0  1  ?  :  0;
   ?  1  1  ?  :  1;
   endtable
endprimitive // primit_o_mux

// additional cells for router delay
/***************************************************************/
/* clock mux buffer                                            */
/***************************************************************/
module ClkMux (I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* s_r mux buffer                                              */
/***************************************************************/
module SRMux(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule


/* added to make post-routing netlist compliant with vital naming convention */
module ICE_GB (	
GLOBALBUFFEROUTPUT,
USERSIGNALTOGLOBALBUFFER);

input USERSIGNALTOGLOBALBUFFER;			
output GLOBALBUFFEROUTPUT;	

assign GLOBALBUFFEROUTPUT = USERSIGNALTOGLOBALBUFFER;

`ifdef TIMINGCHECK
specify
   (USERSIGNALTOGLOBALBUFFER *> GLOBALBUFFEROUTPUT) = (1.0, 1.0);
endspecify
`endif

endmodule	//ICE_GB


/* added such that post routing netlist becomes compliant to Vital port name */
module ICE_CARRY_IN_MUX (carryinitout, carryinitin);
   parameter C_INIT = 2'b00;  //c[1:0]
output carryinitout;
input carryinitin;
assign carryinitout = (C_INIT == 2'b00) ? 1'b0 :
                      ((C_INIT == 2'b01) ? 1'b1 :
                       ((C_INIT == 2'b10) ? carryinitin : 1'b0));

`ifdef TIMINGCHECK
specify
   (carryinitin *> carryinitout) = (1.0, 1.0);

endspecify
`endif
endmodule

module PAD_BANK0(PAD, PADIN, PADOUT, PADOEN);
inout PAD;
input PADOUT, PADOEN;
output PADIN;
parameter IO_STANDARD = "SB_LVCMOS"; 
parameter PULLUP = 1'b0; // by default the IO will have NO pullup, this parameter is used only on bank 0, 1, and 2. Will be ignored when it is placed at bank 3

assign PAD = (~PADOEN) ? PADOUT : 1'bz;
assign PADIN = PAD ;


`ifdef TIMINGCHECK
specify
   (PADOUT *> PAD) = (1.0, 1.0);
   (PADOEN *> PAD) = (1.0, 1.0);
   (PAD *> PADIN) = (1.0, 1.0);
endspecify
`endif

endmodule

module PAD_BANK1(PAD, PADIN, PADOUT, PADOEN);
inout PAD;
input PADOUT, PADOEN;
output PADIN;
parameter IO_STANDARD = "SB_LVCMOS";
parameter PULLUP = 1'b0; // by default the IO will have NO pullup, this parameter is used only on bank 0, 1, and 2. Will be ignored when it is placed at bank 3

assign PAD = (~PADOEN) ? PADOUT : 1'bz;
assign PADIN = PAD ;

`ifdef TIMINGCHECK
specify
   (PADOUT *> PAD) = (1.0, 1.0);
   (PADOEN *> PAD) = (1.0, 1.0);
   (PAD *> PADIN) = (1.0, 1.0);
endspecify
`endif

endmodule

module PAD_BANK2(PAD, PADIN, PADOUT, PADOEN);
inout PAD;
input PADOUT, PADOEN;
output PADIN;
parameter IO_STANDARD = "SB_LVCMOS";
parameter PULLUP = 1'b0; // by default the IO will have NO pullup, this parameter is used only on bank 0, 1, and 2. Will be ignored when it is placed at bank 3

assign PAD = (~PADOEN) ? PADOUT : 1'bz;
assign PADIN = PAD ;

`ifdef TIMINGCHECK
specify
   (PADOUT *> PAD) = (1.0, 1.0);
   (PADOEN *> PAD) = (1.0, 1.0);
   (PAD *> PADIN) = (1.0, 1.0);
endspecify
`endif

endmodule

module PAD_BANK3(PAD, PADIN, PADOUT, PADOEN);
inout PAD;
input PADOUT, PADOEN;
output PADIN;
parameter IO_STANDARD = "SB_LVCMOS25_8"; // SB_SSTL2_CLASS_2, SB_SSTL2_CLASS_1, SB_SSTL18_FULL, SB_SSTL18_HALF, SB_MDDR10, SB_MDDR8, SB_MDDR4, SB_MDDR2

assign PAD = (~PADOEN) ? PADOUT : 1'bz;
assign PADIN = PAD ;

`ifdef TIMINGCHECK
specify
   (PADOUT *> PAD) = (1.0, 1.0);
   (PADOEN *> PAD) = (1.0, 1.0);
   (PAD *> PADIN) = (1.0, 1.0);
endspecify
`endif

endmodule


module PREIO (
	PADIN,
	PADOUT,
	PADOEN, 
	LATCHINPUTVALUE, 
	CLOCKENABLE, 
	INPUTCLK, 
	OUTPUTCLK, 
	OUTPUTENABLE, 
	DOUT1, 
	DOUT0, 
	DIN1, 
	DIN0
 );

parameter PIN_TYPE			= 6'b000000;	  // The default is set to report IO macros that do not define what IO type is used. 
// parameter PULLUP = 1'b0; // by default the IO will have NO pullup, this parameter is used only on bank 0, 1, and 2. Will be ignored when it is placed at bank 3
parameter NEG_TRIGGER = 1'b0; // specify the polarity of all FFs in the IO to be falling edge when NEG_TRIGGER = 1, default is rising edge
// parameter IO_STANDARD = "SB_LVCMOS"; // more standards are supported in bank 3 only: SB_SSTL2_CLASS_2, SB_SSTL2_CLASS_1, SB_SSTL18_FULL, SB_SSTL18_HALF
						 // SB_MDDR10, SB_MDDR8, SB_MDDR4, SB_MDDR2

input DOUT1;  		// Input output 1
input DOUT0;  		// Input output 0

input CLOCKENABLE;    		// Clock enables NEW - common to in/out clocks

output DIN1;    		// Output input 1
output DIN0;    		// Output input 0

input OUTPUTENABLE;   		// Ouput-Enable 
input LATCHINPUTVALUE;    		// Input control
input INPUTCLK;   		// Input clock
input OUTPUTCLK;  		// Output clock

input 	PADIN;
output	PADOUT;
output	PADOEN;

//------------- Main Body of verilog ----------------------------------------------------
wire inclk_, outclk_;
wire inclk, outclk;

assign (weak0, weak1) CLOCKENABLE =1'b1 ;
assign inclk_ = (INPUTCLK ^ NEG_TRIGGER); // change the input clock phase
assign outclk_ = (OUTPUTCLK ^ NEG_TRIGGER); // change the output clock phase
assign inclk = (inclk_ & CLOCKENABLE);
assign outclk = (outclk_ & CLOCKENABLE);

wire bs_en;   //Boundary scan enable
wire shift;   //Boundary scan shift
wire tclk;    //Boundary scan clock
wire update;  //Boundary scan update
wire sdi;     //Boundary scan serial data in
wire mode;    //Boundary scan mode
wire hiz_b;   //Boundary scan tristate control
wire sdo;     //Boundary scan serial data out

//wire rstio; disabled as this a power on only signal   	//Normal Input reset
assign  bs_en = 1'b0;	//Boundary scan enable
assign  shift = 1'b0;	//Boundary scan shift
assign  tclk = 1'b0;	//Boundary scan clock
assign  update = 1'b0;	//Boundary scan update
assign  sdi = 1'b0;	//Boundary scan serial data in
assign  mode = 1'b0;	//Boundary scan mode
assign  hiz_b = 1'b1;	//Boundary scan Tristate control

//parameter Pin_Type  MUST be defined when instantiated
wire hold, oepin;							  // The required package pin type must be set when io_macro is instantiated.
assign hold = LATCHINPUTVALUE;
assign oepin = OUTPUTENABLE;
 
 preio_physical preiophysical_i (	//original names unchanged
 	.hold(hold),
	.rstio(1'b0),			//Disabled as this is power on only.
	.bs_en(bs_en),
	.shift(shift),
	.tclk(tclk),
	.inclk(inclk),
	.outclk(outclk),
	.update(update),
	.oepin(oepin),
	.sdi(sdi),
	.mode(mode),
	.hiz_b(hiz_b),
	.sdo(sdo),
	.dout1(DIN1),
	.dout0(DIN0),
	.ddr1(DOUT1),
	.ddr0(DOUT0),
	.padin(PADIN),
	.padout(PADOUT),
	.padoen(PADOEN),
	.cbit(PIN_TYPE)
	);

`ifdef TIMINGCHECK
specify
   (PADIN *> DIN0) = (1.0, 1.0);
   //(PADIN *> DIN1) = (1.0, 1.0);
   (INPUTCLK *> DIN0) = (1.0, 1.0);
   (INPUTCLK *> DIN1) = (1.0, 1.0);
   (DOUT0 *> PADOUT) = (1.0, 1.0);
   (DOUT1 *> PADOUT) = (1.0, 1.0);
   (OUTPUTENABLE *> PADOEN) = (1.0, 1.0);
   (OUTPUTCLK *> PADOUT) = (1.0, 1.0);
   (OUTPUTCLK *> PADOEN) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN0) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN1) = (1.0, 1.0);
   $setup(posedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge INPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PADIN, posedge INPUTCLK, 1.0);
   $setup(negedge PADIN, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge PADIN, 1.0);
   $hold(posedge INPUTCLK, negedge PADIN, 1.0);
   $setup(posedge PADIN, negedge INPUTCLK, 1.0);
   $setup(negedge PADIN, negedge INPUTCLK, 1.0);
   $hold(negedge INPUTCLK, posedge PADIN, 1.0);
   $hold(negedge INPUTCLK, negedge PADIN, 1.0);
   $setup(posedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PADOUT, posedge OUTPUTCLK, 1.0);
   $setup(negedge PADOUT, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge PADOUT, 1.0);
   $hold(posedge OUTPUTCLK, negedge PADOUT, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   //$setup(posedge DOUT1, posedge OUTPUTCLK, 1.0);
   //$setup(negedge DOUT1, posedge OUTPUTCLK, 1.0);
   //$hold(posedge OUTPUTCLK, posedge DOUT1, 1.0);
   //$hold(posedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT1, negedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, negedge OUTPUTCLK, 1.0);
   $hold(negedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(negedge OUTPUTCLK, negedge DOUT1, 1.0);
   //$setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   //$setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   //$hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   //$hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge OUTPUTENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge OUTPUTENABLE, 1.0);

endspecify
`endif

endmodule

/* added such that post routing netlist becomes compliant to Vital port name */
`timescale 10ps/1ps
module ICE_IO (
	PACKAGEPIN, 
	LATCHINPUTVALUE, 
	CLOCKENABLE, 
	INPUTCLK, 
	OUTPUTCLK, 
	OUTPUTENABLE, 
	DOUT1, 
	DOUT0, 
	DIN1, 
	DIN0
 );

parameter PIN_TYPE			= 6'b000000;	  // The default is set to report IO macros that do not define what IO type is used. 
parameter PULLUP = 1'b0; // by default the IO will have NO pullup, this parameter is used only on bank 0, 1, and 2. Will be ignored when it is placed at bank 3
parameter NEG_TRIGGER = 1'b0; // specify the polarity of all FFs in the IO to be falling edge when NEG_TRIGGER = 1, default is rising edge
parameter IO_STANDARD = "SB_LVCMOS"; // more standards are supported in bank 3 only: SB_SSTL2_CLASS_2, SB_SSTL2_CLASS_1, SB_SSTL18_FULL, SB_SSTL18_HALF
						 // SB_MDDR10, SB_MDDR8, SB_MDDR4, SB_MDDR2

input DOUT1;  		// Input output 1
input DOUT0;  		// Input output 0

input CLOCKENABLE;    		// Clock enables NEW - common to in/out clocks

output DIN1;    		// Output input 1
output DIN0;    		// Output input 0

input OUTPUTENABLE;   		// Ouput-Enable 
input LATCHINPUTVALUE;    		// Input control
input INPUTCLK;   		// Input clock
input OUTPUTCLK;  		// Output clock

inout 	PACKAGEPIN; 		//' User's package pin - 'PAD' output

//------------- Main Body of verilog ----------------------------------------------------
wire inclk_, outclk_;
wire inclk, outclk;
reg INCLKE_sync, OUTCLKE_sync; 

assign (weak0, weak1) CLOCKENABLE =1'b1 ;
assign inclk_ = (INPUTCLK ^ NEG_TRIGGER); // change the input clock phase
assign outclk_ = (OUTPUTCLK ^ NEG_TRIGGER); // change the output clock phase
//assign inclk = (inclk_ & CLOCKENABLE);
//assign outclk = (outclk_ & CLOCKENABLE);


////// CLKEN sync ////// 
always@(inclk_ or CLOCKENABLE)
begin 
    if(~inclk_)
	INCLKE_sync =CLOCKENABLE;
end

always@(outclk_ or CLOCKENABLE)
begin 
	if(~outclk_)
	OUTCLKE_sync =CLOCKENABLE;
end 

assign inclk =(inclk_ & INCLKE_sync); 
assign outclk =(outclk_ & OUTCLKE_sync); 


wire bs_en;   //Boundary scan enable
wire shift;   //Boundary scan shift
wire tclk;    //Boundary scan clock
wire update;  //Boundary scan update
wire sdi;     //Boundary scan serial data in
wire mode;    //Boundary scan mode
wire hiz_b;   //Boundary scan tristate control
wire sdo;     //Boundary scan serial data out

//wire rstio; disabled as this a power on only signal   	//Normal Input reset
assign  bs_en = 1'b0;	//Boundary scan enable
assign  shift = 1'b0;	//Boundary scan shift
assign  tclk = 1'b0;	//Boundary scan clock
assign  update = 1'b0;	//Boundary scan update
assign  sdi = 1'b0;	//Boundary scan serial data in
assign  mode = 1'b0;	//Boundary scan mode
assign  hiz_b = 1'b1;	//Boundary scan Tristate control
  
wire padoen, padout, padin;
assign PACKAGEPIN = (~padoen) ? padout : 1'bz;
assign padin = PACKAGEPIN ;


//parameter Pin_Type  MUST be defined when instantiated
wire hold, oepin;							  // The required package pin type must be set when io_macro is instantiated.
assign hold = LATCHINPUTVALUE;
assign oepin = OUTPUTENABLE;
 
 preio_physical preiophysical_i (	//original names unchanged
 	.hold(hold),
	.rstio(1'b0),			//Disabled as this is power on only.
	.bs_en(bs_en),
	.shift(shift),
	.tclk(tclk),
	.inclk(inclk),
	.outclk(outclk),
	.update(update),
	.oepin(oepin),
	.sdi(sdi),
	.mode(mode),
	.hiz_b(hiz_b),
	.sdo(sdo),
	.dout1(DIN1),
	.dout0(DIN0),
	.ddr1(DOUT1),
	.ddr0(DOUT0),
	.padin(padin),
	.padout(padout),
	.padoen(padoen),
	.cbit(PIN_TYPE)
	);

`ifdef TIMINGCHECK
specify
   (PACKAGEPIN *> DIN0) = (1.0, 1.0);
   (PACKAGEPIN *> DIN1) = (1.0, 1.0);
   (INPUTCLK *> DIN0) = (1.0, 1.0);
   (INPUTCLK *> DIN1) = (1.0, 1.0);
   (DOUT0 *> PACKAGEPIN) = (1.0, 1.0);
   (DOUT1 *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTENABLE *> PACKAGEPIN) = (1.0, 1.0);
   // (INPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN0) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN1) = (1.0, 1.0);
   $setup(posedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge INPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $hold(negedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(negedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PACKAGEPIN, posedge OUTPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge OUTPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge DOUT1, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT1, negedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, negedge OUTPUTCLK, 1.0);
   $hold(negedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(negedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge OUTPUTENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge OUTPUTENABLE, 1.0);

endspecify
`endif

endmodule

/* added such that post routing netlist becomes compliant to Vital port name */
`timescale 10ps/1ps
module ICE_GB_IO (
	PACKAGEPIN, 
	LATCHINPUTVALUE, 
	CLOCKENABLE, 
	INPUTCLK, 
	OUTPUTCLK, 
	OUTPUTENABLE, 
	DOUT1, 
	DOUT0, 
	DIN1, 
	DIN0,
	GLOBALBUFFEROUTPUT
 );

parameter PIN_TYPE			= 6'b000000;	  // The default is set to report IO macros that do not define what IO type is used. 
parameter PULLUP = 1'b0; // by default the IO will have NO pullup, this parameter is used only on bank 0, 1, and 2. Will be ignored when it is placed at bank 3
parameter NEG_TRIGGER = 1'b0; // specify the polarity of all FFs in the IO to be falling edge when NEG_TRIGGER = 1, default is rising edge
parameter IO_STANDARD = "SB_LVCMOS"; // more standards are supported in bank 3 only: SB_SSTL2_CLASS_2, SB_SSTL2_CLASS_1, SB_SSTL18_FULL, SB_SSTL18_HALF
						 // SB_MDDR10, SB_MDDR8, SB_MDDR4, SB_MDDR2

input DOUT1;  		// Input output 1
input DOUT0;  		// Input output 0

input CLOCKENABLE;    		// Clock enables NEW - common to in/out clocks

output DIN1;    		// Output input 1
output DIN0;    		// Output input 0

input OUTPUTENABLE;   		// Ouput-Enable 
input LATCHINPUTVALUE;    		// Input control
input INPUTCLK;   		// Input clock
input OUTPUTCLK;  		// Output clock

inout 	PACKAGEPIN; 		//' User's package pin - 'PAD' output
output GLOBALBUFFEROUTPUT;

//------------- Main Body of verilog ----------------------------------------------------
wire inclk_, outclk_;
wire inclk, outclk;
reg INCLKE_sync, OUTCLKE_sync;

assign (weak0, weak1) CLOCKENABLE =1'b1 ;
assign inclk_ = (INPUTCLK ^ NEG_TRIGGER);
assign outclk_ = (OUTPUTCLK ^ NEG_TRIGGER);
//assign inclk = (inclk_ & CLOCKENABLE);
//assign outclk = (outclk_ & CLOCKENABLE);


////// CLKEN sync ////// 
always@(inclk_ or CLOCKENABLE)
begin 
    if(~inclk_)
	INCLKE_sync =CLOCKENABLE;
end

always@(outclk_ or CLOCKENABLE)
begin 
	if(~outclk_)
	OUTCLKE_sync =CLOCKENABLE;
end 

assign inclk =(inclk_ & INCLKE_sync); 
assign outclk =(outclk_ & OUTCLKE_sync); 

wire bs_en;   //Boundary scan enable
wire shift;   //Boundary scan shift
wire tclk;    //Boundary scan clock
wire update;  //Boundary scan update
wire sdi;     //Boundary scan serial data in
wire mode;    //Boundary scan mode
wire hiz_b;   //Boundary scan tristate control
wire sdo;     //Boundary scan serial data out

//wire rstio; disabled as this a power on only signal   	//Normal Input reset

assign  bs_en = 1'b0;	//Boundary scan enable
assign  shift = 1'b0;	//Boundary scan shift
assign  tclk = 1'b0;	//Boundary scan clock
assign  update = 1'b0;	//Boundary scan update
assign  sdi = 1'b0;	//Boundary scan serial data in
assign  mode = 1'b0;	//Boundary scan mode
assign  hiz_b = 1'b1;	//Boundary scan Tristate control
  
wire padoen, padout, padin;
assign PACKAGEPIN = (~padoen) ? padout : 1'bz;
assign padin = PACKAGEPIN ;

assign GLOBALBUFFEROUTPUT = padin;


wire hold, oepin;							  // The required package pin type must be set when io_macro is instantiated.
assign hold = LATCHINPUTVALUE;
assign oepin = OUTPUTENABLE;
 
 preio_physical preiophysical_i (	//original names unchanged
 	.hold(hold),
	.rstio(1'b0),			//Disabled as this is power on only.
	.bs_en(bs_en),
	.shift(shift),
	.tclk(tclk),
	.inclk(inclk),
	.outclk(outclk),
	.update(update),
	.oepin(oepin),
	.sdi(sdi),
	.mode(mode),
	.hiz_b(hiz_b),
	.sdo(sdo),
	.dout1(DIN1),
	.dout0(DIN0),
	.ddr1(DOUT1),
	.ddr0(DOUT0),
	.padin(padin),
	.padout(padout),
	.padoen(padoen),
	.cbit(PIN_TYPE)
	);

`ifdef TIMINGCHECK
specify
   (PACKAGEPIN *> GLOBALBUFFEROUTPUT) = (1.0, 1.0);
   (PACKAGEPIN *> DIN0) = (1.0, 1.0);
   (PACKAGEPIN *> DIN1) = (1.0, 1.0);
   (INPUTCLK *> DIN0) = (1.0, 1.0);
   (INPUTCLK *> DIN1) = (1.0, 1.0);
   (DOUT0 *> PACKAGEPIN) = (1.0, 1.0);
   (DOUT1 *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTENABLE *> PACKAGEPIN) = (1.0, 1.0);
   // (INPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN0) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN1) = (1.0, 1.0);
   $setup(posedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge INPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $hold(negedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(negedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PACKAGEPIN, posedge OUTPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge OUTPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge DOUT1, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT1, negedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, negedge OUTPUTCLK, 1.0);
   $hold(negedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(negedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge OUTPUTENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge OUTPUTENABLE, 1.0);
endspecify
`endif

endmodule

/* added such that post routing netlist becomes compliant to Vital port name */
// Differential signaling IO
module ICE_IO_DS (
	PACKAGEPIN, 
	PACKAGEPINB, 
	LATCHINPUTVALUE, 
	CLOCKENABLE, 
	INPUTCLK, 
	OUTPUTCLK, 
	OUTPUTENABLE, 
	DOUT1, 
	DOUT0, 
	DIN1, 
	DIN0
 );

parameter PIN_TYPE			= 6'b000000;	  // The default is set to report IO macros that do not define what IO type is used. 
parameter NEG_TRIGGER = 1'b0; // specify the polarity of all FFs in the IO to be falling edge when NEG_TRIGGER = 1, default is rising edge
parameter IO_STANDARD = "SB_LVDS_INPUT"; // another support standard is SB_SUBLVDS_INPUT

input DOUT1;  		// Input output 1
input DOUT0;  		// Input output 0

input CLOCKENABLE;    		// Clock enables NEW - common to in/out clocks

output DIN1;    		// Output input 1
output DIN0;    		// Output input 0

input OUTPUTENABLE;   		// Ouput-Enable 
input LATCHINPUTVALUE;    		// Input control
input INPUTCLK;   		// Input clock
input OUTPUTCLK;  		// Output clock

inout 	PACKAGEPIN; 		//' User's package pin - 'PAD' output
inout 	PACKAGEPINB; 		//' User's package pin - 'PAD' output


//------------- Main Body of verilog ----------------------------------------------------
wire inclk_, outclk_;
wire inclk, outclk;
reg INCLKE_sync,OUTCLKE_sync;

assign (weak0, weak1) CLOCKENABLE =1'b1 ;
assign inclk_ = (INPUTCLK ^ NEG_TRIGGER); // change the input clock phase
assign outclk_ = (OUTPUTCLK ^ NEG_TRIGGER); // change the output clock phase
//assign inclk = (inclk_ & CLOCKENABLE);
//assign outclk = (outclk_ & CLOCKENABLE);

////// CLKEN sync ////// 
always@(inclk_ or CLOCKENABLE)
begin 
    if(~inclk_)
	INCLKE_sync =CLOCKENABLE;
end

always@(outclk_ or CLOCKENABLE)
begin 
	if(~outclk_)
	OUTCLKE_sync =CLOCKENABLE;
end 

assign inclk =(inclk_ & INCLKE_sync); 
assign outclk =(outclk_ & OUTCLKE_sync); 


wire bs_en;   //Boundary scan enable
wire shift;   //Boundary scan shift
wire tclk;    //Boundary scan clock
wire update;  //Boundary scan update
wire sdi;     //Boundary scan serial data in
wire mode;    //Boundary scan mode
wire hiz_b;   //Boundary scan tristate control
wire sdo;     //Boundary scan serial data out

//wire rstio; disabled as this a power on only signal   	//Normal Input reset

assign  bs_en = 1'b0;	//Boundary scan enable
assign  shift = 1'b0;	//Boundary scan shift
assign  tclk = 1'b0;	//Boundary scan clock
assign  update = 1'b0;	//Boundary scan update
assign  sdi = 1'b0;	//Boundary scan serial data in
assign  mode = 1'b0;	//Boundary scan mode
assign  hiz_b = 1'b1;	//Boundary scan Tristate control
  
wire padoen, padout, padin;
assign PACKAGEPIN = (~padoen) ? padout : 1'bz;
assign PACKAGEPINB = (~padoen) ? ~padout : 1'bz;

assign padin = PACKAGEPIN ;


//parameter Pin_Type  MUST be defined when instantiated
wire hold, oepin;							  // The required package pin type must be set when io_macro is instantiated.
assign hold = LATCHINPUTVALUE;
assign oepin = OUTPUTENABLE;
 
 preio_physical preiophysical_i (	//original names unchanged
 	.hold(hold),
	.rstio(1'b0),			//Disabled as this is power on only.
	.bs_en(bs_en),
	.shift(shift),
	.tclk(tclk),
	.inclk(inclk),
	.outclk(outclk),
	.update(update),
	.oepin(oepin),
	.sdi(sdi),
	.mode(mode),
	.hiz_b(hiz_b),
	.sdo(sdo),
	.dout1(DIN1),
	.dout0(DIN0),
	.ddr1(DOUT1),
	.ddr0(DOUT0),
	.padin(padin),
	.padout(padout),
	.padoen(padoen),
	.cbit(PIN_TYPE)
	);

`ifdef TIMINGCHECK
specify
   (PACKAGEPIN *> DIN0) = (1.0, 1.0);
   (PACKAGEPINB *> DIN0) = (1.0, 1.0);
   (INPUTCLK *> DIN0) = (1.0, 1.0);
   (INPUTCLK *> DIN1) = (1.0, 1.0);
   // (INPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   // (INPUTCLK *> PACKAGEPINB) = (1.0, 1.0);
   (DOUT0 *> PACKAGEPIN) = (1.0, 1.0);
   (DOUT0 *> PACKAGEPINB) = (1.0, 1.0);
   (DOUT1 *> PACKAGEPIN) = (1.0, 1.0);
   (DOUT1 *> PACKAGEPINB) = (1.0, 1.0);
   (OUTPUTENABLE *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTENABLE *> PACKAGEPINB) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN0) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN1) = (1.0, 1.0);
   (OUTPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTCLK *> PACKAGEPINB) = (1.0, 1.0);

   (LATCHINPUTVALUE *> DIN0) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN1) = (1.0, 1.0);
   $setup(posedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge INPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $hold(negedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(negedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PACKAGEPIN, posedge OUTPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge OUTPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge DOUT1, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT1, negedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, negedge OUTPUTCLK, 1.0);
   $hold(negedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(negedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge OUTPUTENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge OUTPUTENABLE, 1.0);

endspecify
`endif

endmodule

//----------------------------------------------//
//---  ICE_IO_DLY Physical Primitive -----------// 
//---------------------------------------------// 

`timescale 10ps/1ps

module ICE_IO_DLY (
	PACKAGEPIN, 
	LATCHINPUTVALUE, 
	CLOCKENABLE, 
	INPUTCLK, 
	OUTPUTCLK, 
	OUTPUTENABLE, 
	DOUT1, 
	DOUT0, 
	DIN1, 
	DIN0,
	SCLK,
	SDI,
	CRSEL,
	SDO
 );


parameter NEG_TRIGGER 	= 1'b0; 	   // When set to 1'b1 the polarity of all FFs in the IO is set work at falling edge, default is rising edge Flops
parameter PIN_TYPE      = 6'b000000;       // The required package pin type must be set when io_macro is instantiated.
parameter PULLUP 	= 1'b0;	
parameter IO_STANDARD 	= "SB_LVCMOS";     
parameter INDELAY_VAL   = 6'b000000;       // Set input  line delay value 
parameter OUTDELAY_VAL  = 6'b000000;       // Set output line delay value 


inout 	PACKAGEPIN; 		//' User's package pin - 'PAD' output
input 	CLOCKENABLE;    	// Clock enables in & out clocks
input 	LATCHINPUTVALUE;    	// Input Latch data control
input	INPUTCLK;   		// Input clock
input 	OUTPUTCLK;  		// Output clock


output 	DIN1;    		// Data to Core from PAD     - input 1   (ddrin1)
output	DIN0;    		// Data to Core from PAD     - input 0   (ddrin0)

input 	DOUT1;  		// Data to PAD from core - output 1 (ddrout1)
input 	DOUT0;  		// Data to PAD from core - output 0 (ddrout0)  
input 	OUTPUTENABLE;   	// Ouput-Enable 

input  SCLK;			// Delay serial register clock  
input  SDI;                     // Serial data input to serial delay registers  
input  CRSEL;                 //'0' selects IN/OUT static delay parameters, '1' selects serial register data 
output SDO;                     // Serial data out from serial registers 


//------------- Main Body of verilog ----------------------------------------------------
wire inclk_, outclk_;
wire inclk, outclk;
reg INCLKE_sync,OUTCLKE_sync;

assign (weak0, weak1) CLOCKENABLE =1'b1 ;
assign inclk_ = (INPUTCLK ^ NEG_TRIGGER); // change the input clock phase
assign outclk_ = (OUTPUTCLK ^ NEG_TRIGGER); // change the output clock phase
//assign inclk = (inclk_ & CLOCKENABLE);
//assign outclk = (outclk_ & CLOCKENABLE);

////// CLKEN sync ////// 
always@(inclk_ or CLOCKENABLE)
begin 
    if(~inclk_)
	INCLKE_sync =CLOCKENABLE;
end

always@(outclk_ or CLOCKENABLE)
begin 
	if(~outclk_)
	OUTCLKE_sync =CLOCKENABLE;
end 

assign inclk =(inclk_ & INCLKE_sync); 
assign outclk =(outclk_ & OUTCLKE_sync); 


wire bs_en;   //Boundary scan enable
wire shift;   //Boundary scan shift
wire tclk;    //Boundary scan clock
wire update;  //Boundary scan update
wire sdi;     //Boundary scan serial data in
wire mode;    //Boundary scan mode
wire hiz_b;   //Boundary scan tristate control
wire sdo;     //Boundary scan serial data out

//wire rstio; disabled as this a power on only signal   	//Normal Input reset
assign  bs_en = 1'b0;	//Boundary scan enable
assign  shift = 1'b0;	//Boundary scan shift
assign  tclk = 1'b0;	//Boundary scan clock
assign  update = 1'b0;	//Boundary scan update
assign  sdi = 1'b0;	//Boundary scan serial data in
assign  mode = 1'b0;	//Boundary scan mode
assign  hiz_b = 1'b1;	//Boundary scan Tristate control
  
wire padoen, padout, padin;
wire padinout_delayed; 
assign PACKAGEPIN = (~padoen) ? padinout_delayed : 1'bz;
assign padin = PACKAGEPIN ;

wire hold, oepin;							  
assign hold = LATCHINPUTVALUE;
assign oepin = OUTPUTENABLE;
 
 preio_physical preiophysical_i (	//original names unchanged
 	.hold(hold),
	.rstio(1'b0),			//Disabled as this is power on only.
	.bs_en(bs_en),
	.shift(shift),
	.tclk(tclk),
	.inclk(inclk),
	.outclk(outclk),
	.update(update),
	.oepin(oepin),
	.sdi(sdi),
	.mode(mode),
	.hiz_b(hiz_b),
	.sdo(sdo),
	.dout1(DIN1),
	.dout0(DIN0),
	.ddr1(DOUT1),
	.ddr0(DOUT0),
	.padin(padinout_delayed),
	.padout(padout),
	.padoen(padoen),
	.cbit(PIN_TYPE)
	);

   inoutdly64 iodly64_i ( 
	// dynamic delay test thru sdi,sdo pins are disabled // 
	.sclk(),
        .serialreg_rst(),
        .sdi(),
        .c_r_sel(),
        .in_datain(padin),
        .out_datain(padout),
        .delay_direction(padoen),
        .delayed_dataout(padinout_delayed),
        .sdo()
	); 
   defparam iodly64_i.INDELAY  =INDELAY_VAL; 
   defparam iodly64_i.OUTDELAY =OUTDELAY_VAL; 
    	        

`ifdef TIMINGCHECK
specify
   // tp,tCQ to din0/din1 
   (PACKAGEPIN *> DIN0) = (1.0, 1.0);
   (INPUTCLK *> DIN0) = (1.0, 1.0);
   (INPUTCLK *> DIN1) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN0) = (1.0, 1.0);
   //tp,tCQ to PP  
   (DOUT0 *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTENABLE *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   //CE-INCLK 	
   $setup(posedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge INPUTCLK, negedge CLOCKENABLE, 1.0);
   //CE-OUTCLK
   $setup(posedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge CLOCKENABLE, 1.0);
   //setup/hold wrt posedge iclk (DIN0 reg)  
   $setup(posedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   //setup/hold wrt negedge iclk (DIN1 reg)  
   $setup(posedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $hold(negedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(negedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   // setup/hold wrt to posedge oclk (DOUT0 reg) 
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   // setup/hold wrt to negedge oclk (DOUT1 reg) 
   $setup(posedge DOUT1, negedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, negedge OUTPUTCLK, 1.0);
   $hold(negedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(negedge OUTPUTCLK, negedge DOUT1, 1.0);
   // setup/hold wrt to posedge oclk (OE reg)
   $setup(posedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge OUTPUTENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge OUTPUTENABLE, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* global to local track mux buffer                                              */
/***************************************************************/
module Glb2LocalMux(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* ce mux buffer                                              */
/***************************************************************/
module CEMux(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* span12 mux buffer                                              */
/***************************************************************/
module Span12Mux(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule


/***************************************************************/
/* span12to4 buffer                                              */
/***************************************************************/
module Sp12to4(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule


/***************************************************************/
/* odrv4 buffer                                              */
/***************************************************************/
module Odrv4(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule


/***************************************************************/
/* odrv12 buffer                                              */
/***************************************************************/
module Odrv12(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* local mux buffer                                              */
/***************************************************************/
module LocalMux(I, O);
input I;
output O;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

	assign O = I;
endmodule

/***************************************************************/
/* span4 mux buffer                                              */
/***************************************************************/
module Span4Mux(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule


/***************************************************************/
/*  in mux buffer                                              */
/***************************************************************/
module InMux(I, O);
input I;
output O;

	assign O = I;
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*  ioin mux buffer                                              */
/***************************************************************/
module IoInMux(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*  Cascade Mux Buffer                                          */
/***************************************************************/
module CascadeMux(I, O);
input I;
output O;

assign O = I;

`ifdef TIMINGCHECK
specify
  (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*  Cascade Buffer                                              */
/***************************************************************/
module CascadeBuf(I, O);
input I;
output O;

assign O = I;

`ifdef TIMINGCHECK
specify
  (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*  Ip In Mux Buffer                                          */
/***************************************************************/
module IpInMux(I, O);
input I;
output O;

assign O = I;

`ifdef TIMINGCHECK
specify
  (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*  Ip Out Mux Buffer                                          */
/***************************************************************/
module IpOutMux(I, O);
input I;
output O;

assign O = I;

`ifdef TIMINGCHECK
specify
  (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*global to input of central mux buffer                       */
/***************************************************************/
module gio2CtrlBuf(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*The central muxes buffer for global networks                 */
/***************************************************************/
module GlobalMux(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*The quadrent muxes buffer for global networks                */
/***************************************************************/
module QuadClkMux(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/*The column clock control buffer for global networks          */
/***************************************************************/
module ColCtrlBuf(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/*****************************************************************/
/*The Dummy Buffer for global networks    		        */
/*****************************************************************/

module DummyBuf(I, O);
input I;
output O;

	assign O = I;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* span4 mux buffer at IO                                            */
/***************************************************************/
module IoSpan4Mux(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* span4 mux buffer and its stubs in vertical direction        */
/***************************************************************/
module Span4Mux_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span4Mux_s0_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span4Mux_s1_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span4Mux_s2_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span4Mux_s3_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* span4 mux buffer and its stubs in horizontal direction      */
/***************************************************************/
module Span4Mux_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span4Mux_s0_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span4Mux_s1_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span4Mux_s2_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span4Mux_s3_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

/***************************************************************/
/* span12 mux buffer and its stubs in horizontal direction      */
/***************************************************************/
module Span12Mux_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s0_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s1_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s2_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s3_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s4_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s5_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s6_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s7_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s8_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s9_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s10_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s11_h(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule


/***************************************************************/
/* span12 mux buffer and its stubs in vertical direction      */
/***************************************************************/
module Span12Mux_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s0_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s1_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s2_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s3_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s4_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s5_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s6_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s7_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s8_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s9_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s10_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

module Span12Mux_s11_v(I, O);
input I;
output O;

	assign O = I;
	
`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

// used for synchronizing clock enable signal in post routing netlist
module sync_clk_enable (D, NC,Q);
input D, NC;
output Q;
reg Q;

always @(NC or D)
	if (NC == 1'b0)
		Q <= D;

`ifdef TIMINGCHECK
specify
   (D *> Q) = (1.0, 1.0);
   (NC *> Q) = (1.0, 1.0);
endspecify
`endif

endmodule

// used to model clock skew, X and Y are the X and Y coordinates of logic and IO tiles. 
module SB_G2TBuf (I, O);
parameter X = 1;
parameter Y = 1;
input I;
output O;
   assign O = I ;

`ifdef TIMINGCHECK
specify
   (I *> O) = (1.0, 1.0);
endspecify
`endif

endmodule

// ICE_IO/ICE_GB_IO/PAD PLL breakup sub components models  
/*****************************************************************/
/*      		     IO_PAD 				 */ 
/*****************************************************************/
`timescale 10ps/1ps 
module IO_PAD ( 
	PACKAGEPIN,
 	DOUT,
	DIN,
	OE	   	
 ); 

parameter PULLUP = 1'b0; // by default the IO will have NO pullup, 
			//  this parameter is used only on bank 0, 1, and 2. Will be ignored when it is placed at bank 3
parameter IO_STANDARD = "SB_LVCMOS"; 
				// bank 0,1,2 supports SB_LVCMOS standard only 
				// bank 3 supports :SB_LVCMOS,SB_SSTL2_CLASS_2, SB_SSTL2_CLASS_1, SB_SSTL18_FULL, SB_SSTL18_HALF

input 	DIN;            // Data from core to PAD  
input 	OE;		// Output Data Enable (tristate) 
output 	DOUT;           // Data from PAD to core 
inout 	PACKAGEPIN; 	//' User's package pin - 'PAD' output

assign PACKAGEPIN = (~OE)? DIN : 1'bz;
assign DOUT       = PACKAGEPIN ;

`ifdef TIMINGCHECK
specify
   (PACKAGEPIN *> DOUT) = (1.0, 1.0);
   (DIN *> PACKAGEPIN) = (1.0, 1.0);
   (OE  *> PACKAGEPIN) = (1.0,1.0); 	
endspecify
`endif 

endmodule   //IO_PAD 


/*****************************************************************/
/*      		     IO_PAD_OD (Open Drain Pad) 	 */ 
/*****************************************************************/

`timescale 10ps/1ps 
module IO_PAD_OD ( 
	PACKAGEPIN,
 	DOUT,
	DIN,
	OE	   	
 ); 

/* ------------------NO PULLUP and IO STANDARD parameters for Open Drain Pads ----
//parameter PULLUP = 1'b0;			
//parameter IO_STANDARD = "SB_LVCMOS"; 
-------------------------------------------------------------------------------*/
input 	DIN;            // Data from core to PAD  
input 	OE;		// Output Data Enable (tristate) 
output 	DOUT;           // Data from PAD to core 
inout 	PACKAGEPIN; 	//' User's package pin - 'PAD' output

assign PACKAGEPIN = ( (~OE) && (~DIN)) ? 1'b0 : 1'bz;
assign DOUT       = PACKAGEPIN ;

`ifdef TIMINGCHECK
specify
   (PACKAGEPIN *> DOUT) = (1.0, 1.0);
   (DIN *> PACKAGEPIN) = (1.0, 1.0);
   (OE  *> PACKAGEPIN) = (1.0,1.0); 	
endspecify
`endif 

endmodule   //IO_PAD_OD


/*****************************************************************/
/*			PRE_IO   				 */
/*****************************************************************/
`timescale 10ps/1ps
module PRE_IO (
	PADIN,
	PADOUT,
	PADOEN, 
	LATCHINPUTVALUE, 
	CLOCKENABLE, 
	INPUTCLK, 
	OUTPUTCLK, 
	OUTPUTENABLE, 
	DOUT1, 
	DOUT0, 
	DIN1, 
	DIN0
);

parameter PIN_TYPE = 6'b000000;  // The default is set to report IO macros that do not define what IO type is used. 
parameter NEG_TRIGGER = 1'b0; 	// specify the polarity of all FFs in the IO to be falling edge when NEG_TRIGGER = 1, default is rising edge

input 	PADIN; 			// Data to  preio       - from io_pad   
output PADOUT;                  // Data to  io_pad      - from preio 
output PADOEN ;                 // OE control to io_pad - from preio 

input DOUT0;  			// Input to preio(0)    - from core logics 
input DOUT1;  			// Input to preio(1)    - from core logics 
output DIN0;    		// Output from preio(0) - to core logics 
output DIN1;    		// Output from preio(1) - to core logics 

input OUTPUTENABLE;   		// Ouput-Enable  
input LATCHINPUTVALUE;    	// Input data latch  control
input CLOCKENABLE;    		// Clock enable -common to in/out clocks
input INPUTCLK;   		// Input clock
input OUTPUTCLK;  		// Output clock

//------------- Main Body of verilog ----------------------------------------------------
wire inclk_, outclk_;
wire inclk, outclk;
reg INCLKE_sync , OUTCLKE_sync; 

assign (weak0, weak1) CLOCKENABLE =1'b1 ;
assign inclk_ = (INPUTCLK ^ NEG_TRIGGER); // change the input clock phase
assign outclk_ = (OUTPUTCLK ^ NEG_TRIGGER); // change the output clock phase
//assign inclk = (inclk_ & CLOCKENABLE);
//assign outclk = (outclk_ & CLOCKENABLE);

////// CLKEN sync ////// 
always@(inclk_ or CLOCKENABLE)
begin 
    if(~inclk_)
	INCLKE_sync =CLOCKENABLE;
end

always@(outclk_ or CLOCKENABLE)
begin 
	if(~outclk_)
	OUTCLKE_sync =CLOCKENABLE;
end 

assign inclk =(inclk_ & INCLKE_sync); 
assign outclk =(outclk_ & OUTCLKE_sync); 

wire bs_en;   //Boundary scan enable
wire shift;   //Boundary scan shift
wire tclk;    //Boundary scan clock
wire update;  //Boundary scan update
wire sdi;     //Boundary scan serial data in
wire mode;    //Boundary scan mode
wire hiz_b;   //Boundary scan tristate control
wire sdo;     //Boundary scan serial data out

//wire rstio; disabled as this a power on only signal   	//Normal Input reset
assign  bs_en = 1'b0;	//Boundary scan enable
assign  shift = 1'b0;	//Boundary scan shift
assign  tclk = 1'b0;	//Boundary scan clock
assign  update = 1'b0;	//Boundary scan update
assign  sdi = 1'b0;	//Boundary scan serial data in
assign  mode = 1'b0;	//Boundary scan mode
assign  hiz_b = 1'b1;	//Boundary scan Tristate control
  

wire hold, oepin;			  
assign hold = LATCHINPUTVALUE;
assign oepin = OUTPUTENABLE;
 
 preio_physical preiophysical_i (	//original names unchanged
 	.hold(hold),
	.rstio(1'b0),			//Disabled as this is power on only.
	.bs_en(bs_en),
	.shift(shift),
	.tclk(tclk),
	.inclk(inclk),
	.outclk(outclk),
	.update(update),
	.oepin(oepin),
	.sdi(sdi),
	.mode(mode),
	.hiz_b(hiz_b),
	.sdo(sdo),
	.dout1(DIN1),
	.dout0(DIN0),
	.ddr1(DOUT1),
	.ddr0(DOUT0),
	.padin(PADIN),
	.padout(PADOUT),
	.padoen(PADOEN),
	.cbit(PIN_TYPE)
	);

`ifdef TIMINGCHECK
specify
   // tp,tcQ to din0/din1 	
   (PADIN *> DIN0) = (0.0, 0.0);
   (INPUTCLK *> DIN0) = (0.0, 0.0);
   (LATCHINPUTVALUE *> DIN0) = (0.0, 0.0);
   (INPUTCLK *> DIN1) = (0.0, 0.0);
   // tp,tcQ to padout 	
   (DOUT0 *> PADOUT) = (0.0, 0.0);
   (OUTPUTCLK *> PADOUT) = (0.0,0.0);   	
   // tp,tcQ to padoen  	   	
   (OUTPUTENABLE *> PADOEN) = (0.0, 0.0);
   (OUTPUTCLK *> PADOEN) = (0.0, 0.0);
   //CE-INCLK 	
   $setup(posedge CLOCKENABLE, posedge INPUTCLK, 0.0);
   $setup(negedge CLOCKENABLE, posedge INPUTCLK, 0.0);
   $hold(posedge INPUTCLK, posedge CLOCKENABLE, 0.0);
   $hold(posedge INPUTCLK, negedge CLOCKENABLE, 0.0);
   //CE-OCLK 	
   $setup(posedge CLOCKENABLE, posedge OUTPUTCLK, 0.0);
   $setup(negedge CLOCKENABLE, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge CLOCKENABLE, 0.0);
   $hold(posedge OUTPUTCLK, negedge CLOCKENABLE, 0.0);
   //DIN0 reg setup/hold wrt posedge iclk  	
   $setup(posedge PADIN, posedge INPUTCLK, 0.0);
   $setup(negedge PADIN, posedge INPUTCLK, 0.0);
   $hold(posedge INPUTCLK, posedge PADIN, 0.0);
   $hold(posedge INPUTCLK, negedge PADIN, 0.0);
   //DIN1 reg setup/hold  wrt negedge iclk  
   $setup(posedge PADIN, negedge INPUTCLK, 0.0);
   $setup(negedge PADIN, negedge INPUTCLK, 0.0);
   $hold(negedge INPUTCLK, posedge PADIN, 0.0);
   $hold(negedge INPUTCLK, negedge PADIN, 0.0);
   // DOUT0 reg setup/hold wrt posedge oclk    	
   $setup(posedge DOUT0, posedge OUTPUTCLK, 0.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 0.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 0.0);
   // DOUT1 reg setup/hold wrt posedge oclk  	
   $setup(posedge DOUT1, posedge OUTPUTCLK, 0.0);
   $setup(negedge DOUT1, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge DOUT1, 0.0);
   $hold(posedge OUTPUTCLK, negedge DOUT1, 0.0);
   // DOUT1 reg setup/hold wrt  negedge oclk  	
   $setup(posedge DOUT1, negedge OUTPUTCLK, 0.0);
   $setup(negedge DOUT1, negedge OUTPUTCLK, 0.0);
   $hold(negedge OUTPUTCLK, posedge DOUT1, 0.0);
   $hold(negedge OUTPUTCLK, negedge DOUT1, 0.0);
   // OE reg setup/hold wrt posedge oclk 	
   $setup(posedge OUTPUTENABLE, posedge OUTPUTCLK, 0.0);
   $setup(negedge OUTPUTENABLE, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge OUTPUTENABLE, 0.0);
   $hold(posedge OUTPUTCLK, negedge OUTPUTENABLE, 0.0);
endspecify
`endif

endmodule

/*****************************************************************/
/*               PRE_IO_GBUF                                     */    
/*****************************************************************/
`timescale 10ps/1ps
module PRE_IO_GBUF (	
	GLOBALBUFFEROUTPUT,
	PADSIGNALTOGLOBALBUFFER
	);

input PADSIGNALTOGLOBALBUFFER;			
output GLOBALBUFFEROUTPUT;	

assign GLOBALBUFFEROUTPUT = PADSIGNALTOGLOBALBUFFER;

`ifdef TIMINGCHECK
specify
   (PADSIGNALTOGLOBALBUFFER *> GLOBALBUFFEROUTPUT) = (0.0, 0.0);
endspecify
`endif

endmodule  //PRE_IO_GBUF 



/*****************************************************************/
//                        PLL40                                  */ 
/*****************************************************************/

`timescale 1ps/1ps
module PLL40  (
		PLLIN,				//Input from IO PAD associtaed with PAD PLL. 
		PLLOUTCORE,			//PLL output to core logic
		PLLOUTGLOBAL,	   		//PLL output to global network
		EXTFEEDBACK,  			//Driven by core logic
		DYNAMICDELAY,			//Driven by core logic
		LOCK,				//Output of PLL
		BYPASS,				//Driven by core logic
		RESETB,				//Driven by core logic
		SDI,				//Driven by core logic. Test Pin
		SDO,				//Output to RB Logic Tile. Test Pin
		SCLK,				//Driven by core logic. Test Pin
		LATCHINPUTVALUE 		//iCEGate signal
);
input  	PLLIN;		
output 	PLLOUTCORE;				//PLL output to core logic
output	PLLOUTGLOBAL;	   			//PLL output to global network
input	EXTFEEDBACK;  				//Driven by core logic
input	[7:0] DYNAMICDELAY;  			//Driven by core logic
output	LOCK;					//Output of PLL
input	BYPASS;					//Driven by core logic
input	RESETB;					//Driven by core logic
input	LATCHINPUTVALUE; 			//iCEGate signal
//Test/Dynamic PLL configuration Pins
output	SDO;					//Output of PLL to core logic 
input	SDI;					//Driven by core logic
input	SCLK;					//Driven by core logic
wire SPLLOUT1net;

// Parameters 
parameter FEEDBACK_PATH = "SIMPLE";		//String  (simple, delay, phase_and_delay, external) 
parameter DELAY_ADJUSTMENT_MODE_FEEDBACK = "FIXED"; 
parameter DELAY_ADJUSTMENT_MODE_RELATIVE = "FIXED"; 
parameter SHIFTREG_DIV_MODE = 2'b00; 		//0-->Divide by 4, 1-->Divide by 7, 3 -->Divide by 5
parameter FDA_FEEDBACK = 4'b0000; 		//Integer. 

parameter FDA_RELATIVE = 4'b0000; 		//Integer. 
parameter PLLOUT_SELECT = "GENCLK"; 		

//Use the Spreadsheet to populate the values below.
parameter DIVR = 4'b0000; 			//determine a good default value
parameter DIVF = 7'b0000000; 			//determine a good default value
parameter DIVQ = 3'b000; 			//determine a good default value
parameter FILTER_RANGE = 3'b000; 		//determine a good default value


parameter ENABLE_ICEGATE = 1'b0;		//Additional cbits
parameter TEST_MODE = 1'b0;			//Test Mode parameter.Used for test/Dynamic PLL configuration.  
parameter EXTERNAL_DIVIDE_FACTOR = 1; 		//Not used by model. Added for PLL Config GUI.	 


generate
if (TEST_MODE==1'b1) begin

	Sbt_DS_PLL40 instSbtSPLL (
		.CORE_REF_CLK (),
        	.PACKAGEPIN (PLLIN),
		.EXTFEEDBACK (EXTFEEDBACK),  	
		.DYNAMICDELAY (DYNAMICDELAY),		
		.BYPASS (BYPASS),
		.RESETB (~RESETB),	
		.PLL_SCK(SCLK),
	        .PLL_SDI(SDI),
        	.PLL_SDO(SDO),
		.PLLOUT1 (SPLLOUT1net),	
		.PLLOUT2 (SPLLOUT2net),		
		.LOCK (LOCK)   	

	);	   
	defparam instSbtSPLL.DIVR = DIVR;	
	defparam instSbtSPLL.DIVF = DIVF;
	defparam instSbtSPLL.DIVQ = DIVQ;
	defparam instSbtSPLL.FILTER_RANGE = FILTER_RANGE;
	defparam instSbtSPLL.FEEDBACK_PATH = FEEDBACK_PATH;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_RELATIVE = DELAY_ADJUSTMENT_MODE_RELATIVE;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_FEEDBACK = DELAY_ADJUSTMENT_MODE_FEEDBACK;
	defparam instSbtSPLL.SHIFTREG_DIV_MODE = SHIFTREG_DIV_MODE;
	defparam instSbtSPLL.FDA_RELATIVE = FDA_RELATIVE; 
	defparam instSbtSPLL.FDA_FEEDBACK = FDA_FEEDBACK; 
	defparam instSbtSPLL.PLLOUT_SELECT_PORTA = PLLOUT_SELECT;
	defparam instSbtSPLL.PLLOUT_SELECT_PORTB = "GENCLK";
	defparam instSbtSPLL.TEST_MODE = TEST_MODE;

end else begin

	SbtSPLL40 instSbtSPLL (
		.REFERENCECLK (PLLIN),
		.EXTFEEDBACK (EXTFEEDBACK),  	
		.DYNAMICDELAY (DYNAMICDELAY),		
		.BYPASS (BYPASS),
		.RESETB (~RESETB),			
		.PLLOUT1 (SPLLOUT1net),	
		.PLLOUT2 (SPLLOUT2net),		
		.LOCK (LOCK)   	
	);	  

	defparam instSbtSPLL.DIVR = DIVR;	
	defparam instSbtSPLL.DIVF = DIVF;
	defparam instSbtSPLL.DIVQ = DIVQ;
	defparam instSbtSPLL.FILTER_RANGE = FILTER_RANGE;
	defparam instSbtSPLL.FEEDBACK_PATH = FEEDBACK_PATH;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_RELATIVE = DELAY_ADJUSTMENT_MODE_RELATIVE;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_FEEDBACK = DELAY_ADJUSTMENT_MODE_FEEDBACK;
	defparam instSbtSPLL.SHIFTREG_DIV_MODE = SHIFTREG_DIV_MODE;
	defparam instSbtSPLL.FDA_RELATIVE = FDA_RELATIVE; 
	defparam instSbtSPLL.FDA_FEEDBACK = FDA_FEEDBACK; 
	defparam instSbtSPLL.PLLOUT_SELECT_PORTA = PLLOUT_SELECT;
	defparam instSbtSPLL.PLLOUT_SELECT_PORTB = "GENCLK";
	//defparam instSbtSPLL.TEST_MODE = TEST_MODE;

end

endgenerate

assign PLLOUTCORE = ((ENABLE_ICEGATE != 0) && LATCHINPUTVALUE) ? PLLOUTCORE : SPLLOUT1net;
assign PLLOUTGLOBAL = ((ENABLE_ICEGATE != 0) && LATCHINPUTVALUE)  ? PLLOUTGLOBAL : SPLLOUT1net;

`ifdef TIMINGCHECK
specify
   (PLLIN *> PLLOUTGLOBAL) = (0.0, 0.0);
   (PLLIN *> PLLOUTCORE) = (0.0, 0.0);
   (SCLK *> SDO) = (1.0, 1.0);
   $setup(posedge SDI, posedge SCLK, 1.0);
   $setup(negedge SDI, posedge SCLK, 1.0);
   $setup(posedge SDI, negedge SCLK, 1.0);
   $setup(negedge SDI, negedge SCLK, 1.0);
   $hold(posedge SCLK, posedge SDI, 1.0);
   $hold(posedge SCLK, negedge SDI, 1.0);
   $hold(negedge SCLK, posedge SDI, 1.0);
   $hold(negedge SCLK, negedge SDI, 1.0);
endspecify
`endif

endmodule // PLL40

/*****************************************************************/
/*                PLL40_2                                        */    
/*****************************************************************/

`timescale 1ps/1ps
module PLL40_2  (
		PLLIN,				//Input from IO PAD. 
		PLLOUTCOREA,			//PLL output to core logic
		PLLOUTGLOBALA,	   		//GLOBALOUTPUTBUFFER
	        PLLOUTCOREB,			//PLL output to core logic
		PLLOUTGLOBALB,	   		//PLL output to global network
		EXTFEEDBACK,  			//Driven by core logic
		DYNAMICDELAY,			//Driven by core logic
		LOCK,				//Output of PLL
		BYPASS,				//Driven by core logic
		RESETB,				//Driven by core logic
		SDI,				//Driven by core logic. Test Pin
		SDO,				//Output to RB Logic Tile. Test Pin
		SCLK,				//Driven by core logic. Test Pin
		LATCHINPUTVALUE 		//iCEGate signal
);
input 	PLLIN;		
output  PLLOUTCOREA;				//PLL output to core logic
output	PLLOUTGLOBALA;	   			//PLL output to global network
output  PLLOUTCOREB;				//PLL output to core logic
output	PLLOUTGLOBALB;	   			//PLL output to global network
input	EXTFEEDBACK;  				//Driven by core logic
input	[7:0] DYNAMICDELAY;  			//Driven by core logic
output	LOCK;					//Output of PLL
input	BYPASS;					//Driven by core logic
input	RESETB;					//Driven by core logic
input	LATCHINPUTVALUE; 			//iCEGate signal
//Test/Dynamic PLL configuration Pins
output	SDO;					//Output of PLL to core logic. 
input	SDI;					//Driven by core logic
input	SCLK;					//Driven by core logic
wire SPLLOUT2net;

// Parameters 
parameter FEEDBACK_PATH = "SIMPLE";			//String  (simple, delay, phase_and_delay, external) 
parameter DELAY_ADJUSTMENT_MODE_FEEDBACK = "FIXED"; 
parameter DELAY_ADJUSTMENT_MODE_RELATIVE = "FIXED"; 
parameter SHIFTREG_DIV_MODE = 2'b00; 			//0-->Divide by 4, 1-->Divide by 7, 3 -->Divide by 5
parameter FDA_FEEDBACK = 4'b0000; 			//Integer. 

//Output 
parameter FDA_RELATIVE = 4'b0000; 			//Integer. 
parameter PLLOUT_SELECT_PORTB = "GENCLK"; 		

//Use the Spreadsheet to populate the values below.
parameter DIVR = 4'b0000; 				//determine a good default value
parameter DIVF = 7'b0000000; 				//determine a good default value
parameter DIVQ = 3'b000; 				//determine a good default value
parameter FILTER_RANGE = 3'b000; 			//determine a good default value


parameter ENABLE_ICEGATE_PORTA = 1'b0;			//Additional cbits
parameter ENABLE_ICEGATE_PORTB = 1'b0;
parameter TEST_MODE = 1'b0;				//Test Mode parameter.Used for test/Dynamic PLL configuration.  
parameter EXTERNAL_DIVIDE_FACTOR = 1; 			//Not used by model. Added for PLL Config GUI.

generate

if(TEST_MODE==1'b1) begin

	Sbt_DS_PLL40 instSbtSPLL (
		.CORE_REF_CLK (),
        	.PACKAGEPIN (PLLIN),
		.EXTFEEDBACK (EXTFEEDBACK),  	
		.DYNAMICDELAY (DYNAMICDELAY),		
		.BYPASS (BYPASS),
		.RESETB (~RESETB),	
		.PLL_SCK(SCLK),
	        .PLL_SDI(SDI),
        	.PLL_SDO(SDO),
		.PLLOUT1 (SPLLOUT1net),	
		.PLLOUT2 (SPLLOUT2net),		
		.LOCK (LOCK)   	

	);	 
	defparam instSbtSPLL.DIVR = DIVR;	
	defparam instSbtSPLL.DIVF = DIVF;
	defparam instSbtSPLL.DIVQ = DIVQ;
	defparam instSbtSPLL.FILTER_RANGE = FILTER_RANGE;
	defparam instSbtSPLL.FEEDBACK_PATH = FEEDBACK_PATH;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_RELATIVE = DELAY_ADJUSTMENT_MODE_RELATIVE;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_FEEDBACK = DELAY_ADJUSTMENT_MODE_FEEDBACK;
	defparam instSbtSPLL.SHIFTREG_DIV_MODE = SHIFTREG_DIV_MODE;
	defparam instSbtSPLL.FDA_RELATIVE = FDA_RELATIVE; 
	defparam instSbtSPLL.FDA_FEEDBACK = FDA_FEEDBACK; 
	defparam instSbtSPLL.PLLOUT_SELECT_PORTA = "GENCLK";
	defparam instSbtSPLL.PLLOUT_SELECT_PORTB = PLLOUT_SELECT_PORTB;
	defparam instSbtSPLL.TEST_MODE = TEST_MODE;

end  else begin

	SbtSPLL40 instSbtSPLL (
		.REFERENCECLK (PLLIN),
		.EXTFEEDBACK (EXTFEEDBACK),  	
		.DYNAMICDELAY (DYNAMICDELAY),		
		.BYPASS (BYPASS),
		.RESETB (~RESETB),			
		.PLLOUT1 (SPLLOUT1net),	
		.PLLOUT2 (SPLLOUT2net),		
		.LOCK (LOCK)   	
	);	
	defparam instSbtSPLL.DIVR = DIVR;	
	defparam instSbtSPLL.DIVF = DIVF;
	defparam instSbtSPLL.DIVQ = DIVQ;
	defparam instSbtSPLL.FILTER_RANGE = FILTER_RANGE;
	defparam instSbtSPLL.FEEDBACK_PATH = FEEDBACK_PATH;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_RELATIVE = DELAY_ADJUSTMENT_MODE_RELATIVE;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_FEEDBACK = DELAY_ADJUSTMENT_MODE_FEEDBACK;
	defparam instSbtSPLL.SHIFTREG_DIV_MODE = SHIFTREG_DIV_MODE;
	defparam instSbtSPLL.FDA_RELATIVE = FDA_RELATIVE; 
	defparam instSbtSPLL.FDA_FEEDBACK = FDA_FEEDBACK; 
	defparam instSbtSPLL.PLLOUT_SELECT_PORTA = "GENCLK";
	defparam instSbtSPLL.PLLOUT_SELECT_PORTB = PLLOUT_SELECT_PORTB;
	//defparam instSbtSPLL.TEST_MODE = TEST_MODE;
end

endgenerate

assign PLLOUTCOREA = ((ENABLE_ICEGATE_PORTA != 0) && LATCHINPUTVALUE) ? PLLOUTCOREA : PLLIN;
assign PLLOUTGLOBALA = ((ENABLE_ICEGATE_PORTA != 0) && LATCHINPUTVALUE)  ? PLLOUTGLOBALA : PLLIN;
assign PLLOUTCOREB = ((ENABLE_ICEGATE_PORTB != 0) && LATCHINPUTVALUE) ? PLLOUTCOREB : SPLLOUT2net;
assign PLLOUTGLOBALB = ((ENABLE_ICEGATE_PORTB != 0) && LATCHINPUTVALUE)  ? PLLOUTGLOBALB : SPLLOUT2net;

`ifdef TIMINGCHECK
specify
   (PLLIN *> PLLOUTGLOBALA) = (1.0, 1.0);
   (PLLIN *> PLLOUTCOREA) = (1.0, 1.0);
   (PLLIN *> PLLOUTGLOBALB) = (1.0, 1.0);
   (PLLIN *> PLLOUTCOREB) = (1.0, 1.0);
   (SCLK *> SDO) = (1.0, 1.0);
   $setup(posedge SDI, posedge SCLK, 1.0);
   $setup(negedge SDI, posedge SCLK, 1.0);
   $setup(posedge SDI, negedge SCLK, 1.0);
   $setup(negedge SDI, negedge SCLK, 1.0);
   $hold(posedge SCLK, posedge SDI, 1.0);
   $hold(posedge SCLK, negedge SDI, 1.0);
   $hold(negedge SCLK, posedge SDI, 1.0);
   $hold(negedge SCLK, negedge SDI, 1.0);
endspecify
`endif

endmodule // PLL40_2

/*****************************************************************/
/*                PLL40_2F                                       */   
/*****************************************************************/

`timescale 1ps/1ps
module PLL40_2F (
		PLLIN,				//Input from IO PAD. 
		PLLOUTCOREA,			//PLL output to core logic
		PLLOUTGLOBALA,	   		//PLL output to global network
        	PLLOUTCOREB,			//PLL output to core logic
		PLLOUTGLOBALB,	   		//PLL output to global network
		EXTFEEDBACK,  			//Driven by core logic
		DYNAMICDELAY,			//Driven by core logic
		LOCK,				//Output of PLL
		BYPASS,				//Driven by core logic
		RESETB,				//Driven by core logic
		SDI,				//Driven by core logic. Test Pin
		SDO,				//Output to RB Logic Tile. Test Pin
		SCLK,				//Driven by core logic. Test Pin
		LATCHINPUTVALUE 		//iCEGate signal
);

input 	PLLIN;					// Input from IO PAD 
output  PLLOUTCOREA;				//PLL output to core logic
output	PLLOUTGLOBALA;	   			//PLL output to global network
output  PLLOUTCOREB;				//PLL output to core logic
output	PLLOUTGLOBALB;	   			//PLL output to global network
input	EXTFEEDBACK;  				//Driven by core logic
input	[7:0] DYNAMICDELAY;  			//Driven by core logic
output	LOCK;					//Output of PLL
input	BYPASS;					//Driven by core logic
input	RESETB;					//Driven by core logic
input	LATCHINPUTVALUE; 			//iCEGate signal
//Test/Dynamic PLL configuration Pins 
output	SDO;					//Output of PLL
input	SDI;					//Driven by core logic
input	SCLK;					//Driven by core logic
wire SPLLOUT1net;
wire SPLLOUT2net;

// Parameters 
parameter FEEDBACK_PATH = "SIMPLE";		//String  (simple, delay, phase_and_delay, external) 
parameter DELAY_ADJUSTMENT_MODE_FEEDBACK = "FIXED"; 
parameter DELAY_ADJUSTMENT_MODE_RELATIVE = "FIXED"; 
parameter SHIFTREG_DIV_MODE = 2'b00; 		//0-->Divide by 4, 1-->Divide by 7, 3 -->Divide by 5
parameter FDA_FEEDBACK = 4'b0000; 		//Integer. 
parameter FDA_RELATIVE = 4'b0000; 		//Integer. 
parameter PLLOUT_SELECT_PORTA = "GENCLK"; 	//
parameter PLLOUT_SELECT_PORTB = "GENCLK"; 	//
//Use the Spreadsheet to populate the values below.
parameter DIVR = 4'b0000; 			//determine a good default value
parameter DIVF = 7'b0000000; 			//determine a good default value
parameter DIVQ = 3'b000; 			//determine a good default value
parameter FILTER_RANGE = 3'b000; 		//determine a good default value
parameter ENABLE_ICEGATE_PORTA = 1'b0;		//Additional cbits
parameter ENABLE_ICEGATE_PORTB = 1'b0;
parameter TEST_MODE = 1'b0;			//Test Mode parameter.Used for test/Dynamic PLL configuration.
parameter EXTERNAL_DIVIDE_FACTOR = 1; 		//Not used by model. Added for PLL Config GUI.

generate
if(TEST_MODE==1) begin
	Sbt_DS_PLL40 instSbtSPLL (
		.CORE_REF_CLK (),
	        .PACKAGEPIN (PLLIN),
		.EXTFEEDBACK (EXTFEEDBACK),  	
		.DYNAMICDELAY (DYNAMICDELAY),		
		.BYPASS (BYPASS),
		.RESETB (~RESETB),	
		.PLL_SCK(SCLK),
        	.PLL_SDI(SDI),
        	.PLL_SDO(SDO),
		.PLLOUT1 (SPLLOUT1net),	
		.PLLOUT2 (SPLLOUT2net),		
		.LOCK (LOCK)   
		);	

	defparam instSbtSPLL.DIVR = DIVR;	
	defparam instSbtSPLL.DIVF = DIVF;
	defparam instSbtSPLL.DIVQ = DIVQ;
	defparam instSbtSPLL.FILTER_RANGE = FILTER_RANGE;
	defparam instSbtSPLL.FEEDBACK_PATH = FEEDBACK_PATH;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_RELATIVE = DELAY_ADJUSTMENT_MODE_RELATIVE;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_FEEDBACK = DELAY_ADJUSTMENT_MODE_FEEDBACK;
	defparam instSbtSPLL.SHIFTREG_DIV_MODE = SHIFTREG_DIV_MODE;
	defparam instSbtSPLL.FDA_RELATIVE = FDA_RELATIVE; 
	defparam instSbtSPLL.FDA_FEEDBACK = FDA_FEEDBACK; 
	defparam instSbtSPLL.PLLOUT_SELECT_PORTA = PLLOUT_SELECT_PORTA;
	defparam instSbtSPLL.PLLOUT_SELECT_PORTB = PLLOUT_SELECT_PORTB;
	defparam instSbtSPLL.TEST_MODE = TEST_MODE;

end else begin
	SbtSPLL40 instSbtSPLL (
		.REFERENCECLK (PLLIN),
		.EXTFEEDBACK (EXTFEEDBACK),  	
		.DYNAMICDELAY (DYNAMICDELAY),		
		.BYPASS (BYPASS),
		.RESETB (~RESETB),			
		.PLLOUT1 (SPLLOUT1net),	
		.PLLOUT2 (SPLLOUT2net),		
		.LOCK (LOCK)   	
	);

	defparam instSbtSPLL.DIVR = DIVR;	
	defparam instSbtSPLL.DIVF = DIVF;
	defparam instSbtSPLL.DIVQ = DIVQ;
	defparam instSbtSPLL.FILTER_RANGE = FILTER_RANGE;
	defparam instSbtSPLL.FEEDBACK_PATH = FEEDBACK_PATH;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_RELATIVE = DELAY_ADJUSTMENT_MODE_RELATIVE;
	defparam instSbtSPLL.DELAY_ADJUSTMENT_MODE_FEEDBACK = DELAY_ADJUSTMENT_MODE_FEEDBACK;
	defparam instSbtSPLL.SHIFTREG_DIV_MODE = SHIFTREG_DIV_MODE;
	defparam instSbtSPLL.FDA_RELATIVE = FDA_RELATIVE; 
	defparam instSbtSPLL.FDA_FEEDBACK = FDA_FEEDBACK; 
	defparam instSbtSPLL.PLLOUT_SELECT_PORTA = PLLOUT_SELECT_PORTA;
	defparam instSbtSPLL.PLLOUT_SELECT_PORTB = PLLOUT_SELECT_PORTB;

end	 
endgenerate	

assign PLLOUTCOREA = ((ENABLE_ICEGATE_PORTA != 0) && LATCHINPUTVALUE) ? PLLOUTCOREA : SPLLOUT1net;
assign PLLOUTGLOBALA = ((ENABLE_ICEGATE_PORTA != 0) && LATCHINPUTVALUE)  ? PLLOUTGLOBALA : SPLLOUT1net;
assign PLLOUTCOREB = ((ENABLE_ICEGATE_PORTB != 0) && LATCHINPUTVALUE) ? PLLOUTCOREB : SPLLOUT2net;
assign PLLOUTGLOBALB = ((ENABLE_ICEGATE_PORTB != 0) && LATCHINPUTVALUE)  ? PLLOUTGLOBALB : SPLLOUT2net;

 
`ifdef TIMINGCHECK
specify
   (PLLIN *> PLLOUTGLOBALA) = (1.0, 1.0);
   (PLLIN *> PLLOUTCOREA) = (1.0, 1.0);
   (PLLIN *> PLLOUTGLOBALB) = (1.0, 1.0);
   (PLLIN *> PLLOUTCOREB) = (1.0, 1.0);
   (SCLK *> SDO) = (1.0, 1.0);
   $setup(posedge SDI, posedge SCLK, 1.0);
   $setup(negedge SDI, posedge SCLK, 1.0);
   $setup(posedge SDI, negedge SCLK, 1.0);
   $setup(negedge SDI, negedge SCLK, 1.0);
   $hold(posedge SCLK, posedge SDI, 1.0);
   $hold(posedge SCLK, negedge SDI, 1.0);
   $hold(negedge SCLK, posedge SDI, 1.0);
   $hold(negedge SCLK, negedge SDI, 1.0);
endspecify
`endif

endmodule // PLL40_2F

///  -------iCE5LP ----------------------/// 

`timescale 10ps/1ps
module ICE_IO_OD (
	PACKAGEPIN, 
	LATCHINPUTVALUE, 
	CLOCKENABLE, 
	INPUTCLK, 
	OUTPUTCLK, 
	OUTPUTENABLE, 
	DOUT1, 
	DOUT0, 
	DIN1, 
	DIN0
 );

parameter PIN_TYPE			= 6'b000000;	  // The default is set to report IO macros that do not define what IO type is used. 
//parameter PULLUP = 1'b0; // by default the IO will have NO pullup, this parameter is used only on bank 0, 1, and 2. Will be ignored when it is placed at bank 3
parameter NEG_TRIGGER = 1'b0; // specify the polarity of all FFs in the IO to be falling edge when NEG_TRIGGER = 1, default is rising edge
//parameter IO_STANDARD = "SB_LVCMOS"; // more standards are supported in bank 3 only: SB_SSTL2_CLASS_2, SB_SSTL2_CLASS_1, SB_SSTL18_FULL, SB_SSTL18_HALF
						 // SB_MDDR10, SB_MDDR8, SB_MDDR4, SB_MDDR2

input DOUT1;  		// Input output 1
input DOUT0;  		// Input output 0

input CLOCKENABLE;    		// Clock enables NEW - common to in/out clocks

output DIN1;    		// Output input 1
output DIN0;    		// Output input 0

input OUTPUTENABLE;   		// Ouput-Enable 
input LATCHINPUTVALUE;    		// Input control
input INPUTCLK;   		// Input clock
input OUTPUTCLK;  		// Output clock

inout 	PACKAGEPIN; 		//' User's package pin - 'PAD' output

//------------- Main Body of verilog ----------------------------------------------------
wire inclk_, outclk_;
wire inclk, outclk;
reg INCLKE_sync, OUTCLKE_sync; 

assign (weak0, weak1) CLOCKENABLE =1'b1 ;
assign inclk_ = (INPUTCLK ^ NEG_TRIGGER); // change the input clock phase
assign outclk_ = (OUTPUTCLK ^ NEG_TRIGGER); // change the output clock phase
//assign inclk = (inclk_ & CLOCKENABLE);
//assign outclk = (outclk_ & CLOCKENABLE);


////// CLKEN sync ////// 
always@(inclk_ or CLOCKENABLE)
begin 
    if(~inclk_)
	INCLKE_sync =CLOCKENABLE;
end

always@(outclk_ or CLOCKENABLE)
begin 
	if(~outclk_)
	OUTCLKE_sync =CLOCKENABLE;
end 

assign inclk =(inclk_ & INCLKE_sync); 
assign outclk =(outclk_ & OUTCLKE_sync); 


wire bs_en;   //Boundary scan enable
wire shift;   //Boundary scan shift
wire tclk;    //Boundary scan clock
wire update;  //Boundary scan update
wire sdi;     //Boundary scan serial data in
wire mode;    //Boundary scan mode
wire hiz_b;   //Boundary scan tristate control
wire sdo;     //Boundary scan serial data out

//wire rstio; disabled as this a power on only signal   	//Normal Input reset
assign  bs_en = 1'b0;	//Boundary scan enable
assign  shift = 1'b0;	//Boundary scan shift
assign  tclk = 1'b0;	//Boundary scan clock
assign  update = 1'b0;	//Boundary scan update
assign  sdi = 1'b0;	//Boundary scan serial data in
assign  mode = 1'b0;	//Boundary scan mode
assign  hiz_b = 1'b1;	//Boundary scan Tristate control
  
wire padoen, padout, padin;
//assign PACKAGEPIN = (~padoen) ? padout : 1'bz;
// -- Open drain IO --  
assign PACKAGEPIN = ((~padoen) && (~padout)) ? 1'b0 : 1'bz;
assign padin = PACKAGEPIN ;


//parameter Pin_Type  MUST be defined when instantiated
wire hold, oepin;							  // The required package pin type must be set when io_macro is instantiated.
assign hold = LATCHINPUTVALUE;
assign oepin = OUTPUTENABLE;
 
 preio_physical preiophysical_i (	//original names unchanged
 	.hold(hold),
	.rstio(1'b0),			//Disabled as this is power on only.
	.bs_en(bs_en),
	.shift(shift),
	.tclk(tclk),
	.inclk(inclk),
	.outclk(outclk),
	.update(update),
	.oepin(oepin),
	.sdi(sdi),
	.mode(mode),
	.hiz_b(hiz_b),
	.sdo(sdo),
	.dout1(DIN1),
	.dout0(DIN0),
	.ddr1(DOUT1),
	.ddr0(DOUT0),
	.padin(padin),
	.padout(padout),
	.padoen(padoen),
	.cbit(PIN_TYPE)
	);

`ifdef TIMINGCHECK
specify
   (PACKAGEPIN *> DIN0) = (0.0, 0.0);
   (PACKAGEPIN *> DIN1) = (0.0, 0.0);
   (INPUTCLK *> DIN0) = (0.0, 0.0);
   (INPUTCLK *> DIN1) = (0.0, 0.0);
   (DOUT0 *> PACKAGEPIN) = (0.0, 0.0);
   (DOUT1 *> PACKAGEPIN) = (0.0, 0.0);
   (OUTPUTENABLE *> PACKAGEPIN) = (0.0, 0.0);
   // (INPUTCLK *> PACKAGEPIN) = (0.0, 0.0);
   (OUTPUTCLK *> PACKAGEPIN) = (0.0, 0.0);
   (LATCHINPUTVALUE *> DIN0) = (0.0, 0.0);
   (LATCHINPUTVALUE *> DIN1) = (0.0, 0.0);
   $setup(posedge CLOCKENABLE, posedge INPUTCLK, 0.0);
   $setup(negedge CLOCKENABLE, posedge INPUTCLK, 0.0);
   $hold(posedge INPUTCLK, posedge CLOCKENABLE, 0.0);
   $hold(posedge INPUTCLK, negedge CLOCKENABLE, 0.0);
   $setup(posedge PACKAGEPIN, posedge INPUTCLK, 0.0);
   $setup(negedge PACKAGEPIN, posedge INPUTCLK, 0.0);
   $hold(posedge INPUTCLK, posedge PACKAGEPIN, 0.0);
   $hold(posedge INPUTCLK, negedge PACKAGEPIN, 0.0);
   $setup(posedge PACKAGEPIN, negedge INPUTCLK, 0.0);
   $setup(negedge PACKAGEPIN, negedge INPUTCLK, 0.0);
   $hold(negedge INPUTCLK, posedge PACKAGEPIN, 0.0);
   $hold(negedge INPUTCLK, negedge PACKAGEPIN, 0.0);
   $setup(posedge CLOCKENABLE, posedge OUTPUTCLK, 0.0);
   $setup(negedge CLOCKENABLE, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge CLOCKENABLE, 0.0);
   $hold(posedge OUTPUTCLK, negedge CLOCKENABLE, 0.0);
   $setup(posedge PACKAGEPIN, posedge OUTPUTCLK, 0.0);
   $setup(negedge PACKAGEPIN, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge PACKAGEPIN, 0.0);
   $hold(posedge OUTPUTCLK, negedge PACKAGEPIN, 0.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 0.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 0.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 0.0);
   $setup(posedge DOUT1, posedge OUTPUTCLK, 0.0);
   $setup(negedge DOUT1, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge DOUT1, 0.0);
   $hold(posedge OUTPUTCLK, negedge DOUT1, 0.0);
   $setup(posedge DOUT1, negedge OUTPUTCLK, 0.0);
   $setup(negedge DOUT1, negedge OUTPUTCLK, 0.0);
   $hold(negedge OUTPUTCLK, posedge DOUT1, 0.0);
   $hold(negedge OUTPUTCLK, negedge DOUT1, 0.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 0.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 0.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 0.0);
   $setup(posedge OUTPUTENABLE, posedge OUTPUTCLK, 0.0);
   $setup(negedge OUTPUTENABLE, posedge OUTPUTCLK, 0.0);
   $hold(posedge OUTPUTCLK, posedge OUTPUTENABLE, 0.0);
   $hold(posedge OUTPUTCLK, negedge OUTPUTENABLE, 0.0);

endspecify
`endif

endmodule


//--------------------------------------
// ----  ICE_IR500_DRV ------ 
//--------------------------------------
`timescale 1ps/1ps
module ICE_IR500_DRV   (
	IRLEDEN,
	IRPWM,
	CURREN,
	IRLEDEN2,
	IRPWM2,
	IRLED1,
	IRLED2
);

parameter IR500_CURRENT = "0b000000000000";
parameter CURRENT_MODE = "0b0";

	input IRLEDEN;
	input IRPWM;
	input CURREN;
	input IRLEDEN2;
	input IRPWM2;
	output IRLED1;
	output IRLED2;

 ICE_IR500_DRV_CORE #(.IR500_CURRENT(IR500_CURRENT),.CURRENT_MODE(CURRENT_MODE))
inst
 (
	.CURREN(CURREN),
	.IRLEDEN(IRLEDEN),
	.IRPWM(IRPWM),
	.IRLEDEN2(IRLEDEN2),
	.IRPWM2(IRPWM2),
	.IRLED1(IRLED1),
	.IRLED2(IRLED2)
);
`ifdef TIMINGCHECK
specify

   (IRPWM *> IRLED1) = (0.0, 0.0);
   (IRLEDEN *> IRLED1) = (0.0, 0.0);
   (IRPWM2 *> IRLED2) = (0.0, 0.0);
   (IRLEDEN2 *> IRLED2) = (0.0, 0.0);
endspecify
`endif
endmodule


//--------------------------------------------//
// --------SMCCLK (20MHz Internal Clock) -----//
// -------------------------------------------//
`timescale 1 ns / 1 ps

module SMCCLK ( CLK );
output CLK ;

reg CLK ;

always
 begin
	 CLK = 0;
	 forever #25 CLK = ~CLK;
 end
 
endmodule
//----------------------------------------------//
//------ I3C IO ---------------------------------//
//------------------------------------------------// 
/* added such that post routing netlist becomes compliant to Vital port name */
`timescale 10ps/1ps
module ICE_IO_I3C (
	PACKAGEPIN, 
	LATCHINPUTVALUE, 
	CLOCKENABLE, 
	INPUTCLK, 
	OUTPUTCLK, 
	OUTPUTENABLE, 
	DOUT1, 
	DOUT0, 
	DIN1, 
	DIN0,
        PUENB,
	WEAKPUENB
	
 );

parameter PIN_TYPE	= 6'b000000;	   
parameter PULLUP 	= 1'b0;      		// By default PULL Up=0. Set  1'b1 to enable 3p3k/6p8k/10k resistors controlled by PUENB signal.
parameter WEAK_PULLUP  	= 1'b0;			// By default WEAK PULL Up=0. Set  1'b1 to enable 100K weak resistors controlled by WEAKPUENB signal.
parameter NEG_TRIGGER 	= 1'b0; 			
parameter IO_STANDARD 	= "SB_LVCMOS"; 
						 

input DOUT1;  			// Input output 1
input DOUT0;  			// Input output 0

input CLOCKENABLE;    		// Clock enables to in/out clocks

output DIN1;    		// Output input 1
output DIN0;    		// Output input 0

input OUTPUTENABLE;   		// Ouput-Enable 
input LATCHINPUTVALUE;    	// Input control
input INPUTCLK;   		// Input clock
input OUTPUTCLK;  		// Output clock

inout PACKAGEPIN; 		//' User's package pin - 'PAD' output

input PUENB; 			// Active low pullup resistor enable/disable signal.
input WEAKPUENB; 		// Active low weak pullup resistor enable/disable signal.


//------------- Main Body of verilog ----------------------------------------------------
wire inclk_, outclk_;
wire inclk, outclk;
reg INCLKE_sync, OUTCLKE_sync; 

assign (weak0, weak1) CLOCKENABLE =1'b1 ;
assign inclk_ = (INPUTCLK ^ NEG_TRIGGER); // change the input clock phase
assign outclk_ = (OUTPUTCLK ^ NEG_TRIGGER); // change the output clock phase
//assign inclk = (inclk_ & CLOCKENABLE);
//assign outclk = (outclk_ & CLOCKENABLE);


////// CLKEN sync ////// 
always@(inclk_ or CLOCKENABLE)
begin 
    if(~inclk_)
	INCLKE_sync =CLOCKENABLE;
end

always@(outclk_ or CLOCKENABLE)
begin 
	if(~outclk_)
	OUTCLKE_sync =CLOCKENABLE;
end 

assign inclk =(inclk_ & INCLKE_sync); 
assign outclk =(outclk_ & OUTCLKE_sync); 


wire bs_en;   //Boundary scan enable
wire shift;   //Boundary scan shift
wire tclk;    //Boundary scan clock
wire update;  //Boundary scan update
wire sdi;     //Boundary scan serial data in
wire mode;    //Boundary scan mode
wire hiz_b;   //Boundary scan tristate control
wire sdo;     //Boundary scan serial data out

//wire rstio; disabled as this a power on only signal   	//Normal Input reset
assign  bs_en = 1'b0;	//Boundary scan enable
assign  shift = 1'b0;	//Boundary scan shift
assign  tclk = 1'b0;	//Boundary scan clock
assign  update = 1'b0;	//Boundary scan update
assign  sdi = 1'b0;	//Boundary scan serial data in
assign  mode = 1'b0;	//Boundary scan mode
assign  hiz_b = 1'b1;	//Boundary scan Tristate control
  
wire padoen, padout, padin;

assign net200 = !((!PUENB & PULLUP ) || (!WEAKPUENB & WEAK_PULLUP));
assign (weak1, weak0)  PACKAGEPIN = net200 ? 1'bz: 1'b1;
assign PACKAGEPIN = (~padoen) ? padout : 1'bz;
assign padin = PACKAGEPIN ;



wire hold, oepin;							  // The required package pin type must be set when io_macro is instantiated.
assign hold = LATCHINPUTVALUE;
assign oepin = OUTPUTENABLE;
 
 preio_physical preiophysical_i (	//original names unchanged
 	.hold(hold),
	.rstio(1'b0),			//Disabled as this is power on only.
	.bs_en(bs_en),
	.shift(shift),
	.tclk(tclk),
	.inclk(inclk),
	.outclk(outclk),
	.update(update),
	.oepin(oepin),
	.sdi(sdi),
	.mode(mode),
	.hiz_b(hiz_b),
	.sdo(sdo),
	.dout1(DIN1),
	.dout0(DIN0),
	.ddr1(DOUT1),
	.ddr0(DOUT0),
	.padin(padin),
	.padout(padout),
	.padoen(padoen),
	.cbit(PIN_TYPE)
	);

`ifdef TIMINGCHECK
specify
   (PACKAGEPIN *> DIN0) = (1.0, 1.0);
   (PACKAGEPIN *> DIN1) = (1.0, 1.0);
   (INPUTCLK *> DIN0) = (1.0, 1.0);
   (INPUTCLK *> DIN1) = (1.0, 1.0);
   (DOUT0 *> PACKAGEPIN) = (1.0, 1.0);
   (DOUT1 *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTENABLE *> PACKAGEPIN) = (1.0, 1.0);
   // (INPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   (OUTPUTCLK *> PACKAGEPIN) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN0) = (1.0, 1.0);
   (LATCHINPUTVALUE *> DIN1) = (1.0, 1.0);
   $setup(posedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge INPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge INPUTCLK, 1.0);
   $hold(posedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, negedge INPUTCLK, 1.0);
   $hold(negedge INPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(negedge INPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge CLOCKENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge CLOCKENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge CLOCKENABLE, 1.0);
   $setup(posedge PACKAGEPIN, posedge OUTPUTCLK, 1.0);
   $setup(negedge PACKAGEPIN, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge PACKAGEPIN, 1.0);
   $hold(posedge OUTPUTCLK, negedge PACKAGEPIN, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge DOUT1, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT1, negedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT1, negedge OUTPUTCLK, 1.0);
   $hold(negedge OUTPUTCLK, posedge DOUT1, 1.0);
   $hold(negedge OUTPUTCLK, negedge DOUT1, 1.0);
   $setup(posedge DOUT0, posedge OUTPUTCLK, 1.0);
   $setup(negedge DOUT0, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge DOUT0, 1.0);
   $hold(posedge OUTPUTCLK, negedge DOUT0, 1.0);
   $setup(posedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $setup(negedge OUTPUTENABLE, posedge OUTPUTCLK, 1.0);
   $hold(posedge OUTPUTCLK, posedge OUTPUTENABLE, 1.0);
   $hold(posedge OUTPUTCLK, negedge OUTPUTENABLE, 1.0);

endspecify
`endif

endmodule

/*****************************************************************/
/*      		     IO_PAD_I3C				 */ 
/*****************************************************************/
`timescale 10ps/1ps 
module IO_PAD_I3C ( 
	PACKAGEPIN,
 	DOUT,
	DIN,
	OE,
	PUENB,
	WEAKPUENB
 ); 

parameter PULLUP = 1'b0; 		   // By default PULL Up=0. Set  1'b1 to enable 3p3k/6p8k/10k resistors controlled by PUENB signal.
parameter WEAK_PULLUP = 1'b0;             // By default WEAK PULL Up=0. Set  1'b1 to enable 100K resistors controlled by WEAKPUENB signal.
parameter IO_STANDARD = "SB_LVCMOS";      // [ SB_LVCOMS | SB_LVDS_INPUT ]
				
input 	DIN;            // Data from core to PAD  
input 	OE;		// Output Data Enable (tristate) 
input 	PUENB;		// Active low pullup resistor enable/disable signal. 
input 	WEAKPUENB; 	// Active low weak pullup resistor enable/disable signal.
output 	DOUT;           // Data from PAD to core 
inout 	PACKAGEPIN; 	//' User's package pin - 'PAD' output

assign PACKAGEPIN = (~OE)? DIN : 1'bz;
assign DOUT       = PACKAGEPIN ;

`ifdef TIMINGCHECK
specify
   (PACKAGEPIN *> DOUT) = (1.0, 1.0);
   (DIN *> PACKAGEPIN) = (1.0, 1.0);
   (OE  *> PACKAGEPIN) = (1.0,1.0); 	
endspecify
`endif 

endmodule   //IO_PAD_I3C 

