// RUN: $YOSYS -q -m $CHURCHROAD_DIR/yosys-plugin/churchroad.so \
// RUN:   -p 'read_verilog -sv %s; prep -top test; write_churchroad' \
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
// CHECK: (let some_module_instance- (ModuleInstance "some_module" (StringNil)(ExprNil)(StringCons "a"(StringCons "b"(StringNil)))(ExprCons v0(ExprCons v1(ExprNil)))))
// CHECK: (union (GetOutput some_module_instance- "out") v2)
// CHECK: (IsPort "" "a" (Input) v0)
// CHECK: (IsPort "" "b" (Input) v1)
// CHECK: (IsPort "" "out" (Output) v2)
// CHECK: (run-schedule (saturate (seq typing misc)))
