`timescale 1 ns/10 ps  // time-unit/precision
`define BIT_TIME (1000/12)
`define CLK_PER (1000/16)

module tb_uf16soc ( );
`define USB_CDC_INST tb_uf16soc.u_uf16soc.u_usb_cdc

   localparam MAX_BITS = 128;
   localparam MAX_BYTES = (8*1024);
   localparam MAX_STRING = 128;

   reg        dp_force;
   reg        dn_force;
   reg        power_on;
   reg [8*MAX_STRING-1:0] test;

   wire                   dp_sense;
   wire                   dn_sense;

   integer                errors;
   integer                warnings;

   localparam             IN_BULK_MAXPACKETSIZE = 'd8;
   localparam             OUT_BULK_MAXPACKETSIZE = 'd8;
   localparam             VENDORID = 16'h1D50;
   localparam             PRODUCTID = 16'h6130;

`include "usb_tasks.v"
`include "uf16soc_tasks.v"

   `progress_bar(test, 39)

   reg                    clk;

   initial begin
      clk = 0;
   end

   always @(clk or power_on) begin
      if (power_on | clk)
        #(`CLK_PER/2) clk <= ~clk;
   end

   wire led;
   wire usb_p;
   wire usb_n;
   wire usb_pu;

   uf16soc u_uf16soc (.clk(clk),
                      .led(led),
                      .usb_p(usb_p),
                      .usb_n(usb_n),
                      .usb_pu(usb_pu),
                      .pin1());

   assign usb_p = dp_force;
   assign usb_n = dn_force;

   assign (pull1, highz0) usb_p = usb_pu; // 1.5kOhm device pull-up resistor
   //pullup (usb_p); // to speedup simulation don't wait for usb_pu

   //pulldown (weak0) dp_pd (usb_p), dn_pd (usb_n); // 15kOhm host pull-down resistors
   assign (highz1, weak0) usb_p = 1'b0; // to bypass verilator error on above pulldown
   assign (highz1, weak0) usb_n = 1'b0; // to bypass verilator error on above pulldown

   assign dp_sense = usb_p;
   assign dn_sense = usb_n;

   usb_monitor #(.MAX_BITS(MAX_BITS),
                 .MAX_BYTES(MAX_BYTES))
   u_usb_monitor (.usb_dp_i(dp_sense),
                  .usb_dn_i(dn_sense));

   reg [6:0]  address;
   reg [15:0] datain_toggle;
   reg [15:0] dataout_toggle;
   reg [8*MAX_BYTES-1:0] data;
   reg [7:0]             flash_init_data[0:1024*1024-1];
   integer               i;

   initial begin : u_host
      $timeformat(-6, 3, "us", 3);
      $dumpfile("tb.dump");
      $dumpvars;

      power_on = 1'b1;
      dp_force = 1'bZ;
      dn_force = 1'bZ;
      errors = 0;
      warnings = 0;
      address = 'd2;
      dataout_toggle = 'd0;
      datain_toggle = 'd0;
      wait_idle(20000000/83*`BIT_TIME);
      #(100000/83*`BIT_TIME);

      usb_configuration(address);
      #(1000000/83*`BIT_TIME);

      test = "START TERMINAL";
      test_write({8'h00},
                 0, address, datain_toggle, dataout_toggle);
      test_read({"UF16 v0.3", 8'h0D, 8'h0A, 8'h0D, 8'h0A},
                13, OK, address, datain_toggle);
      test_write({"1 2 + ."},
                 7, address, datain_toggle, dataout_toggle);
      #(20000000/83*`BIT_TIME);
      test_read({" 3"},
                2, OK, address, datain_toggle);


      test = "Test END";
      #(100*`BIT_TIME);
      `report_end("All tests correctly executed!")
   end
endmodule
