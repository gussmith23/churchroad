// RUN: $YOSYS -q -m $CHURCHROAD_DIR/yosys-plugin/churchroad.so \
// RUN:   -p 'read_verilog -sv %s; prep -top test; write_lakeroad' \
// RUN:   | FileCheck %s

module some_module(input a, b, output out);
  assign out = a & b;
endmodule

module test(input a, b, output out);
  some_module some_module_instance(.a(a), .b(b), .out(out));
endmodule

// CHECK: (let v0 (Wire "v0" 1))
// CHECK: (let v1 (Wire "v1" 1))
// CHECK: (let v2 (Wire "v2" 1))
// CHECK: (let some_module_instance (ModuleInstance "some_module" (vec-of "a" "b") (vec-of v0 v1)))
// CHECK: (IsPort "" "a" (Input) (Var "a" 1))
// CHECK: (union v0 (Var "a" 1))
// CHECK: (IsPort "" "b" (Input) (Var "b" 1))
// CHECK: (union v1 (Var "b" 1))
// CHECK: (IsPort "" "out" (Output) v2)
// CHECK: (delete (Wire "v0" 1))
// CHECK: (delete (Wire "v1" 1))
// CHECK: (delete (Wire "v2" 1))