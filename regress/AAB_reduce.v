module AAB_reduce(
  input         clock,
  input         reset,
  input  [31:0] io_fn,
  input  [31:0] io_in,
  output [31:0] io_out
);
  wire [31:0] _T_1 = ~io_fn;
  wire [63:0] _T_2 = _T_1 * io_in;
  wire [63:0] _GEN_0 = {{32'd0}, io_fn};
  wire [64:0] _GEN_1 = _GEN_0 + _T_2;
  wire [31:0] new_DoPrim0 = _GEN_1[31:0];
  assign io_out = new_DoPrim0;
endmodule
