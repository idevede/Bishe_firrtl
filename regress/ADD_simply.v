module ADD_simply(
  input         clock,
  input         reset,
  input  [31:0] io_fn,
  input  [31:0] io_in,
  output [31:0] io_out
);
  assign io_out = io_in + (io_fn > 32'h8 ? 32'h6 : 32'h7); // @[]
endmodule
