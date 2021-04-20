module Case_Youixanji(
  input         clock,
  input         reset,
  input  [1:0]  io_fn,
  input  [31:0] io_in,
  output [31:0] io_out
);
  wire  _T = 2'h3 == io_fn; // @[Conditional.scala 37:30]
  wire  _T_1 = 2'h2 == io_fn; // @[Conditional.scala 37:30]
  wire  _T_2 = 2'h1 == io_fn; // @[Conditional.scala 37:30]
  wire  _T_3 = 2'h0 == io_fn; // @[Conditional.scala 37:30]
  wire  _GEN_0 = _T_3 ? 1'h0 : 1'h1; // @[Conditional.scala 39:67 cmd6.sc 17:22 cmd6.sc 11:10]
  wire  _GEN_1 = _T_2 | _GEN_0; // @[Conditional.scala 39:67 cmd6.sc 16:22]
  wire [1:0] _Mux_op_0 = {{1'd0}, _GEN_1};
  wire [1:0] _GEN_2 = _T_1 ? 2'h2 : _Mux_op_0;
  wire [1:0] _GEN_3 = _T ? 2'h3 : _GEN_2; // @[Conditional.scala 40:58 cmd6.sc 14:22]
  wire [31:0] result = {{30'd0}, _GEN_3}; // @[cmd6.sc 10:20]
  assign io_out = result + io_in; // @[cmd6.sc 21:19]
endmodule
