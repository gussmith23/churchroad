// RUN: cargo run -- \
// RUN:   --filepath %s \
// RUN:   --top-module-name mul \
// RUN:   --architecture xilinx-ultrascale-plus \
// RUN:   --simulate \
// RUN:   --simulate-with-verilator-arg="--verilator_include_dir=$LAKEROAD_DIR/lakeroad-private/DSP48E2" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-DXIL_XECLIB" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-Wno-UNOPTFLAT" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-Wno-COMBDLY" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-Wno-LATCH" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-Wno-WIDTH" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-Wno-STMTDLY" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-Wno-CASEX" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-Wno-TIMESCALEMOD" \
// RUN:   --simulate-with-verilator-arg="--verilator_extra_arg=-Wno-PINMISSING" \
// RUN: | FileCheck %s

module mul(input [15:0] a, input [31:0] b, output [31:0] out);
  assign out = a * b;
endmodule

// TODO wrong module name
// CHECK: module top(
// CHECK:   input [16-1:0] a,
// CHECK:   input [32-1:0] b,
// CHECK:   output [32-1:0] out,
// CHECK: );
// CHECK:   DSP48E2 #(
// CHECK:   DSP48E2 #(