module simple_mux(input a, b, c, output o);
  assign o = a ? b : c;
endmodule