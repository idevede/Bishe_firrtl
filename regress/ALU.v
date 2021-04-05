module ALU(
  input         clock,
  input         reset,
  input  [1:0]  io_fn,
  input  [31:0] io_in,
  output [31:0] io_out
);
  wire  _T = 2'h2 == io_fn; // @[Conditional.scala 37:30]
  wire [31:0] _T_2 = io_in + 32'h7; // @[cmd48.sc 27:27]
  wire  _T_3 = 2'h1 == io_fn; // @[Conditional.scala 37:30]
  wire [31:0] _T_5 = io_in + 32'h6; // @[cmd48.sc 28:27]
  wire  _T_6 = 2'h0 == io_fn; // @[Conditional.scala 37:30]
  wire [31:0] _T_8 = io_in + 32'h2; // @[cmd48.sc 29:27]
  wire [31:0] _GEN_0 = _T_6 ? _T_8 : 32'h1; // @[Conditional.scala 39:67 cmd48.sc 29:22 cmd48.sc 25:10]
  wire [31:0] _GEN_1 = _T_3 ? _T_5 : _GEN_0; // @[Conditional.scala 39:67 cmd48.sc 28:22]
  wire [31:0] result = _T ? _T_2 : _GEN_1; // @[Conditional.scala 40:58 cmd48.sc 27:22]
  assign io_out = result; // @[Conditional.scala 40:58 cmd48.sc 27:22]
endmodule
