// timescale  1 ps / 1 ps

module counter(
    input logic clk,
    output logic [3:0] count
);
    always @(posedge clk) begin
        begin
            count <= count + 1;
        end
    end
endmodule