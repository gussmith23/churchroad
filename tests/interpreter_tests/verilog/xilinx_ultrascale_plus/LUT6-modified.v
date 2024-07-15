`timescale  1 ps / 1 ps
//
// LUT6 primitive for Xilinx FPGAs
// Compatible with Verilator tool (www.veripool.org)
// Copyright (c) 2019-2020 Frédéric REQUIN
// License : BSD
//
// @ninehusky: Modified so that INIT is a port, not a parameter

module LUT6
(
    input  wire I0, I1, I2, I3, I4, I5,
    input  wire[63:0]  INIT,
    output wire O
);
    wire [5:0] _w_idx = { I5, I4, I3, I2, I1, I0 };
    
    assign O = INIT[_w_idx];

endmodule
