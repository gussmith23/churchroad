module alu (
  input logic i_a,
  input logic i_b,
  input logic [1:0] i_control, // Control signal to specify the operation
  output logic  o_res
  
);
  always_comb  begin
    case (i_control)
      2'b00: o_res = i_a ^ i_b; // Bitwise additon
      2'b01: o_res = ~(i_a | i_b); 
      2'b10: o_res = i_a | i_b; // Bitwise OR
      2'b11: o_res = i_a & i_b; // Bitwise AND
    endcase
  end
endmodule

