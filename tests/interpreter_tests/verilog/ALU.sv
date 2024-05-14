module ALU (
    input logic op,
    input logic [7:0] a,
    input logic [7:0] b,
    output logic [7:0] out
);
        assign out = op ? a & b : a | b; 
endmodule
