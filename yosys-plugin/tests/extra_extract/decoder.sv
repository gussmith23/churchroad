// The "decoder" is a component that breaks the "input" into smaller stuff
module decoder (
  input logic [3:0] i_instr,
  output logic [1:0] o_op,
  output logic  o_a,
  output logic  o_b
);
        assign o_op = i_instr[1:0];
        assign o_a = i_instr[2];
        assign o_b = i_instr[3];
endmodule
