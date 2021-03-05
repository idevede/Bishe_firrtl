// SPDX-License-Identifier: Apache-2.0

package firrtlTests

import firrtl._
import firrtl.passes._
import firrtl.transforms._
import firrtl.annotations._
import firrtl.passes.memlib.SimpleTransform
import firrtl.testutils._

import java.io.File
import java.nio.file.Paths



class DCE_small_Test extends FirrtlFlatSpec {
  // Not using executeTest because it is for positive testing, we need to check that stuff got
  // deleted
  private val customTransforms = Seq(
    new LowFirrtlOptimization,
    RemoveEmpty
  )
  private def exec(input: String, check: String, annos: Seq[Annotation] = List.empty): Unit = {
    val state = CircuitState(parse(input), ChirrtlForm, annos)
    val finalState = (new LowFirrtlCompiler).compileAndEmit(state, customTransforms)
    val res = finalState.getEmittedCircuit.value
    // Convert to sets for comparison
    val resSet = Set(parse(res).serialize.split("\n"): _*) 
    println(resSet)
    val checkSet = Set(parse(check).serialize.split("\n"): _*)
    resSet should be(checkSet)
  }

  // "Unread wire" should "be deleted" in {
  //   val input =
  //     """circuit AddNot:
  //       |  module AddNot:
  //       |    input a: UInt<8>
  //       |    input b: UInt<8>
  //       |    output o: UInt<9>
  //       |    node c = add(a, not(b))
  //       |    node d = add(a, not(b))
  //       |    o <= add(c,d)""".stripMargin
  //   val check =
  //     """circuit Top :
  //       |  module Top : 
  //       |    input x : UInt<1>
  //       |    output z : UInt<1>
  //       |    z <= x""".stripMargin
  //   exec(input, check)
  // }

"Unread wire" should "be deleted" in {
    val input =
      """circuit AddNot:
        |  module AddNot:
        |    input a: UInt<8>
        |    input b: UInt<8>
        |    output o: UInt<9>
        |    node c = add(a, not(b))
        |    node d = add(a, not(b))
        |    o <= add(c,d)""".stripMargin
    val check =
      """circuit Top :
        |  module Top : 
        |    input x : UInt<1>
        |    output z : UInt<1>
        |    z <= x""".stripMargin
    exec(input, check)
  }
 
}

class DCE_small_CommandLineSpec extends FirrtlFlatSpec {

  val testDir = createTestDirectory("dce")
  val inputFile = Paths.get(getClass.getResource("/features/HasDeadCode.fir").toURI()).toFile()
  val outFile = new File(testDir, "HasDeadCode.v")
  val args = Array("-i", inputFile.getAbsolutePath, "-o", outFile.getAbsolutePath, "-X", "verilog")

  "Dead Code Elimination" should "run by default" in {
    firrtl.Driver.execute(args) match {
      case FirrtlExecutionSuccess(_, verilog) =>
        (verilog should not).include(regex("wire +a"))
      case _ => fail("Unexpected compilation failure")
    }
  }

  it should "not run when given --no-dce option" in {
    firrtl.Driver.execute(args :+ "--no-dce") match {
      case FirrtlExecutionSuccess(_, verilog) =>
        (verilog should include).regex("wire +a")
      case _ => fail("Unexpected compilation failure")
    }
  }
}
