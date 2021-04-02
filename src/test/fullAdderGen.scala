// fullAdderGen.scala
package test
 
object FullAdderGen extends App {
  chisel3.Driver.execute(args, () => new FullAdder)
}