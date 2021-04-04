module And_simply(
  input         clock,
  input         reset,
  input  [31:0] io_fn,
  input  [31:0] io_in,
  output [31:0] io_out
);
  wire [31:0] new_DoPrim0 = ~(io_fn | io_in);
  assign io_out = new_DoPrim0;
endmodule
