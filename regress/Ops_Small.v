module Ops_Small(
  input  [3:0] sel,
  input  [7:0] is,
  output [7:0] os
);
  wire [8:0] _Mux_op_0 = is - 8'h2;
  wire [8:0] _Mux_op_1 = is + 8'h3;
  wire [8:0] _GEN_0 = sel == 4'h1 ? _Mux_op_0 : _Mux_op_1;
  wire [8:0] _Mux_op_3 = is + 8'h1;
  wire [8:0] _GEN_1 = sel == 4'h0 ? _Mux_op_3 : _GEN_0;
  assign os = _GEN_1[7:0];
endmodule
