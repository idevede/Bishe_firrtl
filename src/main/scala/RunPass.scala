package scala

import firrtl._
import firrtl.passes._
import firrtl.transforms._
import tutorial.lesson3._

object RunPass extends App {
  // Example firrtl
  // circuit And_simply : 
  // module And_simply : 
  //   input clock : Clock
  //   input reset : UInt<1>
  //   output io : {flip fn : UInt<32>, flip in : UInt<32>, out : UInt<32>}
  //   node _T_1 = not(io.fn)
  //   node _T_2 = not(io.in)
  //   io.out <= and(_T_1,_T_2)
  // |circuit ADD_simply :
  //  |  module ADD_simply :
  //  |    input clock : Clock
  //  |    input reset : UInt<1>
  //  |    input io_fn : UInt<32>
  //  |    input io_in : UInt<32>
  //  |    output io_out : UInt<32>
  //  |
  //  |    node _T = or(io_fn,io_in)
  //  |    node _T_1 = not(_T)   
  //  |    io_out <= _T_1
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
   |    node _T = not(or(io_fn,io_in))
   |    io_out <= _T
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

    new ReportArea,
    new ReportTimingFull

  )

  // Run transforms and capture final state
  val finalState = transforms.foldLeft(state) {
    (c: CircuitState, t: Transform) => t.runTransform(c)
  }

  // Emit output
  println(finalState.circuit.serialize)
}
