module Register (
  input wire clk,
  input wire reset,
  input wire logic write_en,
  input wire logic [32-1:0] in,
  output logic [32-1:0] out
);
  always_ff @(posedge clk) begin
    if (reset)
      out <= 0;
    else if (write_en)
      out <= in;
    else
      out <= out;
  end
endmodule

