package scala

import firrtl._
import firrtl.passes._
import firrtl.transforms._
import tutorial.lesson3.ReportArea

object RunPass extends App {
  // Example firrtl
  val input =
  """
   |circuit ADD_simply :
   |  module ADD_simply :
   |    input clock : Clock
   |    input reset : UInt<1>
   |    input io_fn : UInt<32>
   |    input io_in : UInt<32>
   |    output io_out : UInt<32>
   |
   |    node _T = gt(io_fn, UInt<32>("h8"))
   |    node _T_1 = add(io_in, UInt<32>("h7"))
   |    node _T_2 = tail(_T_1, 1)
   |    node _T_3 = add(io_in, UInt<32>("h6"))
   |    node _T_4 = tail(_T_3, 1)
   |    node _GEN_1 = tail(add(io_in, mux(_T, UInt<32>("h6"), UInt<32>("h7"))), 1)
   |    io_out <= _GEN_1
  """.stripMargin

  // Parse the input
  val state = CircuitState(firrtl.Parser.parse(input), UnknownForm)

  // Designate a series of transforms to be run in this order
  val transforms = Seq(
    ToWorkingIR,
    CheckHighForm,
    ResolveKinds,
    InferTypes,
    CheckTypes,
    ResolveKinds,
    InferTypes,
    ResolveFlows,
    CheckFlows,
    new InferWidths,
    CheckWidths,
    PullMuxes,
    ExpandConnects,
    RemoveAccesses,
    ExpandWhens,

    RemoveValidIf,
    new ConstantPropagation,
    PadWidths,
    new ConstantPropagation,
    Legalize,
    new ConstantPropagation,
    SplitExpressions,
    new CombineCats,
    CommonSubexpressionElimination,
    new DeadCodeElimination,
    //new DeadCodeElimination,

    new ReportArea
  )

  // Run transforms and capture final state
  val finalState = transforms.foldLeft(state) {
    (c: CircuitState, t: Transform) => t.runTransform(c)
  }

  // Emit output
  println(finalState.circuit.serialize)
}
