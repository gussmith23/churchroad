// RUN: $YOSYS -q -m $CHURCHROAD_DIR/yosys-plugin/churchroad.so \
// RUN:   -p 'read_verilog -sv %s; prep -top top; write_lakeroad' \
// RUN:   | cat

module top
  (
	  input [31:0] a, b, c,
    output [31:0] out
  );
  assign out = (a*b)+c;
endmodule

// CHECK: (let v0 (Wire "v0" 32))
// CHECK: (let v1 (Wire "v1" 32))
// CHECK: (let v2 (Wire "v2" 32))
// CHECK: (let v3 (Wire "v3" 32))
// CHECK: (let v4 (Wire "v4" 32))
// CHECK: (union v4 (Op2 (Add) v0 v3))
// CHECK: (union v0 (Op2 (Mul) v1 v2))
// CHECK: (let a (Var "a" 32))
// CHECK: (IsPort "" "a" (Input) a)
// CHECK: (union v1 a)
// CHECK: (let b (Var "b" 32))
// CHECK: (IsPort "" "b" (Input) b)
// CHECK: (union v2 b)
// CHECK: (let c (Var "c" 32))
// CHECK: (IsPort "" "c" (Input) c)
// CHECK: (union v3 c)
// CHECK: (let out v4)
// CHECK: (IsPort "" "out" (Output) out)
// CHECK: (delete (Wire "v0" 32))
// CHECK: (delete (Wire "v1" 32))
// CHECK: (delete (Wire "v2" 32))
// CHECK: (delete (Wire "v3" 32))
// CHECK: (delete (Wire "v4" 32))
