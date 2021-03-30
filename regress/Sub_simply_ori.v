module Sub_simply(
  input         clock,
  input         reset,
  input  [31:0] io_fn,
  input  [31:0] io_in,
  output [31:0] io_out
);
  wire [32:0] _T_1 = io_in - 32'h7;
  wire [32:0] _T_3 = io_in - 32'h6;
  wire [32:0] _GEN_0 = io_fn > 32'h8 ? _T_1 : _T_3; // @[]
  assign io_out = _GEN_0[31:0];
endmodule
