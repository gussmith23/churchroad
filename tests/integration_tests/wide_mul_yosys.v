// RUN: [ -n "$CHURCHROAD_DIR" ] || { echo "CHURCHROAD_DIR is not set"; exit 1; }
// RUN: LAKEROAD="$LAKEROAD_DIR/bin/lakeroad-portfolio.py" \
// RUN: CHURCHROAD="cargo run -- " \
// RUN: yosys -m "$CHURCHROAD_DIR/yosys-plugin/churchroad.so" -p " \
// RUN:  read_verilog %s; \
// RUN:  hierarchy -top mul; \
// RUN:  churchroad mul; \
// RUN:  proc; \
// RUN:  write_verilog" \
// RUN: | FileCheck %s

(* solvers = "bitwuzla" *)
(* architecture = "xilinx-ultrascale-plus" *)
module mul(input [15:0] a, input [31:0] b, output [31:0] out);
  assign out = a * b;
endmodule

// TODO wrong module name
// CHECK: module mul(a, b, out);
// CHECK:   DSP48E2 #(
// CHECK:   DSP48E2 #(