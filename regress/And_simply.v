module And_simply(
  input         clock,
  input         reset,
  input  [31:0] io_fn,
  input  [31:0] io_in,
  output [31:0] io_out
);
  wire [31:0] _T_1 = ~io_fn;
  wire [31:0] _T_2 = ~io_in;
  wire [31:0] new_DoPrim0 = _T_1 & _T_2;
  wire [31:0] new_DoPrim0 = ~(io_fn | io_in);
  assign io_out = new_DoPrim0;
endmodule
