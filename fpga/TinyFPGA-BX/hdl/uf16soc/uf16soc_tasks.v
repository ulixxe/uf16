
task automatic test_write
  (
   input [8*MAX_BYTES-1:0] data,
   input integer           bytes,
   input [6:0]             address,
   inout [15:0]            datain_toggle,
   inout [15:0]            dataout_toggle
   );
   localparam              DATA_TIMEOUT = 10000000/83*`BIT_TIME;
   integer                 i;
   begin : u_test_write_task
      if (bytes > 0) begin
         for (i = bytes-1; i >= 0; i = i-1) begin
            test_data_out(address, ENDP_BULK,
                          {data[8*i +:8]},
                          1, PID_ACK, OUT_BULK_MAXPACKETSIZE, DATA_TIMEOUT, 0, dataout_toggle);
            // read loopback
            test_data_in(address, ENDP_BULK,
                         {data[8*i +:8]},
                         1, PID_ACK, IN_BULK_MAXPACKETSIZE, DATA_TIMEOUT, 0, datain_toggle, ZLP);
         end
      end
      test_data_out(address, ENDP_BULK,
                    {8'h0D},
                    1, PID_ACK, OUT_BULK_MAXPACKETSIZE, DATA_TIMEOUT, 0, dataout_toggle);
   end
endtask

localparam NO_OK = 0,
           OK = 1;

task automatic test_read
  (
   input [8*MAX_BYTES-1:0] data,
   input integer           bytes,
   input                   req_ok,
   input [6:0]             address,
   inout [15:0]            datain_toggle
   );
   localparam              DATA_TIMEOUT = 10000000/83*`BIT_TIME;
   begin : u_test_read_task
      if (bytes > 0) begin
         if (req_ok) begin
            test_data_in(address, ENDP_BULK,
                         {data, "  ok", 8'h0D, 8'h0A},
                         bytes+6, PID_ACK, IN_BULK_MAXPACKETSIZE, DATA_TIMEOUT, 0, datain_toggle, ZLP);
         end else begin
            test_data_in(address, ENDP_BULK,
                         data,
                         bytes, PID_ACK, IN_BULK_MAXPACKETSIZE, DATA_TIMEOUT, 0, datain_toggle, ZLP);
         end
      end else if (req_ok) begin
         test_data_in(address, ENDP_BULK,
                      {"  ok", 8'h0D, 8'h0A},
                      6, PID_ACK, IN_BULK_MAXPACKETSIZE, DATA_TIMEOUT, 0, datain_toggle, ZLP);
      end
   end
endtask
