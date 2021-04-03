module ADD_simply_after(
  input         clock,
  input         reset,
  input  [31:0] io_fn,
  input  [31:0] io_in,
  output [31:0] io_out
);
  wire [2:0] _GEN_0 = io_fn > 32'h8 ? 3'h7 : 3'h6; // @[]
  wire [31:0] _GEN_1 = {{29'd0}, _GEN_0};
  wire [31:0] new_DoPrim0 = io_in + _GEN_1;
  assign io_out = new_DoPrim0;
endmodule
