// RUN: "$YOSYS" -p 'read_verilog -sv %s; prep -top test; write_lakeroad' \
// RUN:   | FileCheck %s
module test(input a, b, output out);
  assign out = a & b;
endmodule

// CHECK: (let v0 (Wire "v0" 1))
// CHECK: (let v1 (Wire "v1" 1))
// CHECK: (let v2 (Wire "v2" 1))
// CHECK: (union v2 (Op2 (And) v0 v1))
// CHECK: (let a (Var "a" 1))
// CHECK: (union v0 a)
// CHECK: (let b (Var "b" 1))
// CHECK: (union v1 b)
// CHECK: (let out v2)
// CHECK: (delete (Wire "v0" 1))
// CHECK: (delete (Wire "v1" 1))
// CHECK: (delete (Wire "v2" 1))