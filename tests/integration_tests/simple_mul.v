// RUN: cargo run -- \
// RUN:   --filepath %s \
// RUN:   --top-module-name mul \
// RUN:   --architecture xilinx-ultrascale-plus \
// RUN: | FileCheck %s

module mul(input [15:0] a, b, output [15:0] out);
  assign out = a * b;
endmodule

// CHECK: module out(a, b, out);