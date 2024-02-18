// DO NOT MERGE: need to get permission to use this file.
//
// Copyright 2023 Intel Corporation.
//
// This reference design file is subject licensed to you by the terms and
// conditions of the applicable License Terms and Conditions for Hardware
// Reference Designs and/or Design Examples (either as signed by you or
// found at https://www.altera.com/common/legal/leg-license_agreement.html ).
//
// As stated in the license, you agree to only use this reference design
// solely in conjunction with Intel FPGAs or Intel CPLDs.
//
// THE REFERENCE DESIGN IS PROVIDED "AS IS" WITHOUT ANY EXPRESS OR IMPLIED
// WARRANTY OF ANY KIND INCLUDING WARRANTIES OF MERCHANTABILITY,
// NONINFRINGEMENT, OR FITNESS FOR A PARTICULAR PURPOSE. Intel does not
// warrant or assume responsibility for the accuracy or completeness of any
// information, links or other items within the Reference Design and any
// accompanying materials.
//
// In the event that you do not agree with such terms and conditions, do not
// use the reference design file.
/////////////////////////////////////////////////////////////////////////////

// RUN: yosys -m $CHURCHROAD_DIR/yosys-plugin/churchroad.so %s \
// RUN:   -p 'read_verilog -sv %s; prep -top permuter_4x4_sim; pmuxtree; prep; write_lakeroad' \
// RUN:   | FileCheck %s

module permuter_4x4_sim #(
    parameter SIZE = 4
) (
	input clk,
	input [4*SIZE-1:0] din,
	input [1:0] control,
	output reg [4*SIZE-1:0] dout
);


always @(posedge clk) begin
    case (control)
        2'b00: begin
            {dout[(3+1)*SIZE-1:3*SIZE], dout[(2+1)*SIZE-1:2*SIZE], dout[(1+1)*SIZE-1:1*SIZE], dout[(0+1)*SIZE-1:0*SIZE]} <= {din[(3+1)*SIZE-1:3*SIZE], din[(2+1)*SIZE-1:2*SIZE], din[(1+1)*SIZE-1:1*SIZE], din[(0+1)*SIZE-1:0*SIZE]};
        end
        2'b01: begin
            {dout[(3+1)*SIZE-1:3*SIZE], dout[(2+1)*SIZE-1:2*SIZE], dout[(1+1)*SIZE-1:1*SIZE], dout[(0+1)*SIZE-1:0*SIZE]} <= {din[(2+1)*SIZE-1:2*SIZE], din[(3+1)*SIZE-1:3*SIZE], din[(0+1)*SIZE-1:0*SIZE], din[(1+1)*SIZE-1:1*SIZE]};
        end
        2'b10: begin
            {dout[(3+1)*SIZE-1:3*SIZE], dout[(2+1)*SIZE-1:2*SIZE], dout[(1+1)*SIZE-1:1*SIZE], dout[(0+1)*SIZE-1:0*SIZE]} <= {din[(1+1)*SIZE-1:1*SIZE], din[(0+1)*SIZE-1:0*SIZE], din[(3+1)*SIZE-1:3*SIZE], din[(2+1)*SIZE-1:2*SIZE]};
        end
        2'b11: begin
            {dout[(3+1)*SIZE-1:3*SIZE], dout[(2+1)*SIZE-1:2*SIZE], dout[(1+1)*SIZE-1:1*SIZE], dout[(0+1)*SIZE-1:0*SIZE]} <= {din[(0+1)*SIZE-1:0*SIZE], din[(1+1)*SIZE-1:1*SIZE], din[(2+1)*SIZE-1:2*SIZE], din[(3+1)*SIZE-1:3*SIZE]};
        end
    endcase
end

endmodule


// CHECK: ; $0\dout[15:0]
// CHECK: (let v0 (Wire "v0" 16))
// CHECK: ; $auto$rtlil.cc:2443:Or$13
// CHECK: (let v1 (Wire "v1" 1))
// CHECK: ; $auto$rtlil.cc:2509:Mux$11
// CHECK: (let v2 (Wire "v2" 16))
// CHECK: ; $auto$rtlil.cc:2509:Mux$9
// CHECK: (let v3 (Wire "v3" 16))
// CHECK: ; $procmux$3_CMP
// CHECK: (let v4 (Wire "v4" 1))
// CHECK: ; $procmux$4_CMP
// CHECK: (let v5 (Wire "v5" 1))
// CHECK: ; $procmux$5_CMP
// CHECK: (let v6 (Wire "v6" 1))
// CHECK: ; clk
// CHECK: (let v7 (Wire "v7" 1))
// CHECK: ; control
// CHECK: (let v8 (Wire "v8" 2))
// CHECK: ; din
// CHECK: (let v9 (Wire "v9" 16))
// CHECK: ; dout
// CHECK: (let v10 (Wire "v10" 16))
// CHECK: ; cells
// CHECK: (union v1 (Op2 (Or) v5 v4))
// CHECK: ; { \din [11:8] \din [15:12] \din [3:0] \din [7:4] }
// CHECK: (let v11 (Extract 7 4 v9))
// CHECK: (let v12 (Extract 3 0 v9))
// CHECK: (let v13 (Extract 15 12 v9))
// CHECK: (let v14 (Extract 11 8 v9))
// CHECK: (let v15 (Concat v11 v12))
// CHECK: (let v16 (Concat v15 v13))
// CHECK: (let v17 (Concat v16 v14))
// CHECK: (union v2 (Op3 (Mux) v6 v9 v17))
// CHECK: (union v0 (Op3 (Mux) v1 v2 v3))
// CHECK: ; { \din [7:0] \din [15:8] }
// CHECK: (let v18 (Extract 15 8 v9))
// CHECK: (let v19 (Extract 7 0 v9))
// CHECK: (let v20 (Concat v18 v19))
// CHECK: ; { \din [3:0] \din [7:4] \din [11:8] \din [15:12] }
// CHECK: (let v21 (Concat v13 v14))
// CHECK: (let v22 (Concat v21 v11))
// CHECK: (let v23 (Concat v22 v12))
// CHECK: (union v3 (Op3 (Mux) v4 v20 v23))
// CHECK: ; TODO: assuming 0 default for Reg
// CHECK: (union v10 (Reg 0 v7 v0))
// CHECK: ; 2'11
// CHECK: (let v24 (BV 3 2))
// CHECK: (union v4 (Op2 (Eq) v8 v24))
// CHECK: ; 2'10
// CHECK: (let v25 (BV 2 2))
// CHECK: (union v5 (Op2 (Eq) v8 v25))
// CHECK: ; 1'1
// CHECK: (let v26 (BV 1 1))
// CHECK: ; TODO not handling signedness
// CHECK: (let v27 (ZeroExtend v26 2))
// CHECK: (union v6 (Op2 (Eq) v8 v27))
// CHECK: ; inputs
// CHECK: (let clk (Var "clk" 1))
// CHECK: (union v7 clk)
// CHECK: (let control (Var "control" 2))
// CHECK: (union v8 control)
// CHECK: (let din (Var "din" 16))
// CHECK: (union v9 din)
// CHECK: ; outputs
// CHECK: (let dout v10)
// CHECK: ; delete wire expressions
// CHECK: (delete (Wire "v0" 16))
// CHECK: (delete (Wire "v1" 1))
// CHECK: (delete (Wire "v2" 16))
// CHECK: (delete (Wire "v3" 16))
// CHECK: (delete (Wire "v4" 1))
// CHECK: (delete (Wire "v5" 1))
// CHECK: (delete (Wire "v6" 1))
// CHECK: (delete (Wire "v7" 1))
// CHECK: (delete (Wire "v8" 2))
// CHECK: (delete (Wire "v9" 16))
// CHECK: (delete (Wire "v10" 16))
