// SPDX-License-Identifier: Apache-2.0

package firrtl
package passes

import firrtl.{SystemVerilogEmitter, Transform, VerilogEmitter}
import firrtl.ir._
import firrtl.options.Dependency
import firrtl.Mappers._
import firrtl.Utils.{flow, get_info, kind}

// Datastructures
import scala.collection.mutable

// Splits compound expressions into simple expressions
//  and named intermediate nodes
object SplitExpressions extends Pass {

  override def prerequisites = firrtl.stage.Forms.LowForm ++
    Seq(Dependency(firrtl.passes.RemoveValidIf), Dependency(firrtl.passes.memlib.VerilogMemDelays))

  override def optionalPrerequisiteOf =
    Seq(Dependency[SystemVerilogEmitter], Dependency[VerilogEmitter])

  override def invalidates(a: Transform) = a match {
    case ResolveKinds => true
    case _            => false
  }

  private def onModule(m: Module): Module = {
    val namespace = Namespace(m)
    def onStmt(s: Statement): Statement = {
      val v = mutable.ArrayBuffer[Statement]()
      // Splits current expression if needed
      // Adds named temporaries to v
      def split(e: Expression): Expression = e match {
        case e: DoPrim =>
          val name = namespace.newTemp
          v += DefNode(get_info(s), name, e)
          WRef(name, e.tpe, kind(e), flow(e))
        case e: Mux =>
          val name = namespace.newTemp
          v += DefNode(get_info(s), name, e)
          WRef(name, e.tpe, kind(e), flow(e))
        case e: ValidIf =>
          val name = namespace.newTemp
          v += DefNode(get_info(s), name, e)
          WRef(name, e.tpe, kind(e), flow(e))
        case _ => e
      }

      // Recursive. Splits compound nodes
      def onExp(e: Expression): Expression =
        e.map(onExp) match {
          case ex: DoPrim => ex.map(split)
          case ex => ex
        }

      s.map(onExp) match {
        case x: Block => x.map(onStmt)
        case EmptyStmt => EmptyStmt
        case x =>
          v += x
          v.size match {
            case 1 => v.head
            case _ => Block(v.toSeq)
          }
      }
    }
    val Statement1 = onStmt(m.body)

   
    var Stmts_node = collection.mutable.HashMap[String, Statement]()
    var Stmts_node_array = collection.mutable.ArrayBuffer[(String, Statement)]()
    
    var new_add1 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    var new_add2 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    var new_conn2 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    //var new_block = s
    var new_op_Nodes = collection.mutable.ArrayBuffer[DefNode]()
    var new_Stat = collection.mutable.ArrayBuffer[DefNode]()
    var stmts = new collection.mutable.ArrayBuffer[Statement]
    var new_Tail_Node = DefNode(NoInfo,"_GEN_0",new_add1)
    var new_add_Node = DefNode(NoInfo,"_T_8",new_add1)
    var new_Mux_Node = DefNode(NoInfo,"_T_7",new_add1)
    var temp_count = 0
    val Nots = collection.mutable.HashMap[String, (DoPrim,String)]()
    val Ands = collection.mutable.ArrayBuffer[(String,DoPrim,String,String,firrtl.ir.PrimOp)]()
    
    Statement1.foreachStmt{
            state =>
              println("state",state,state.getClass)
              //stmts+= state
              state match{
                  case DefNode(info,name,oper)=>
                    //println("name",name)
                    Stmts_node(name) = state
                    Stmts_node_array += ((name,state))
                    //处理(~A & ~B == ~(A|B))
                    oper match{
                      case DoPrim(op,args,consts,tpe)=>
                        println(op,op.getClass)
                        op match {
                          case PrimOps.Not =>
                            var not_name = ""
                            args(0) match {
                              case WRef(name, _, _, _)=>
                                not_name = name
                              case UIntLiteral(name, _)=>
                                not_name = name.toString
                              case _ => 
                            }//)
                            Nots(name) = (DoPrim(op,args,consts,tpe),not_name)
                          case PrimOps.And =>
                            println("and-here")
                            var add1_name = ""
                            var add2_name = ""
                            args(0) match {
                              case WRef(name, _, _, _)=>
                                add1_name = name
                              case UIntLiteral(name, _)=>
                                add1_name = name.toString
                              case _ => 
                            }//)
                            args(1) match {
                              case WRef(name, _, _, _)=>
                                add2_name = name
                              case UIntLiteral(name, _)=>
                                add2_name = name.toString
                              case _ => 
                            }//)
                            Ands += ((name,DoPrim(op,args,consts,tpe),add1_name,add2_name,op))
                          case _ =>

                        }

                      case _ =>
                    }
                  case Connect(info,outputPortRef, expr) =>
                    //println("Catch Connect", expr)
                    expr match {
                      case DoPrim(op,args,consts,tpe)=>
                          var new_name = "new_DoPrim"+temp_count.toString
                          var new_Node = firrtl.ir.DefNode(NoInfo,new_name,expr)
                          Stmts_node("new_DoPrim"+temp_count.toString) = new_Node
                          Stmts_node_array += (("new_DoPrim"+temp_count.toString,new_Node))
                          Stmts_node("Connect"+temp_count.toString) = ir.Connect(info,outputPortRef,WRef(new_name,tpe,NodeKind,SinkFlow))
                          Stmts_node_array += (("Connect"+temp_count.toString,ir.Connect(info,outputPortRef,WRef(new_name,tpe,NodeKind,SinkFlow))))
                          temp_count +=1
                      case _ =>
                        Stmts_node("Connect"+temp_count.toString) = state
                        Stmts_node_array += (("Connect"+temp_count.toString,state))
                        temp_count +=1
                    }
                    
                    //println("Connect!!",state)
                  case _ =>
                    Stmts_node("Other"+temp_count.toString) = state
                    Stmts_node_array += (("Other"+temp_count.toString,state))//存储了所有的Stmts
              }
    }

        //处理(~A & ~B == ~(A|B))
        //val Nots = collection.mutable.HashMap[String, (Expression,String)]()
        //val Ands = collection.mutable.HashMap[(String, DoPrim,String,String,firrtl.ir.PrimOp)]()
        Stmts_node_array.foreach{
          state => state._2 match{
            case DefNode(info,name,value) =>
              value match{
                case DoPrim(op,args,consts,tpe)=>               
                  op match {
                    case PrimOps.And =>
                      println(args.getClass)
                      var add1_name = ""
                      var add2_name = ""
                      args(0) match {
                        case WRef(name, _, _, _)=>
                          add1_name = name
                        case UIntLiteral(name, _)=>
                          add1_name = name.toString
                        case _ => args
                      }//)
                      args(1) match {
                        case WRef(name, _, _, _)=>
                          add2_name = name
                        case UIntLiteral(name, _)=>
                          add2_name = name.toString
                        case _ => args
                      }//)
                      Ands += ((name,DoPrim(op,args,consts,tpe),add1_name,add2_name,op))
                    case _ =>
                  }
                case _ =>
              } 
            case _ =>
          }
        }
        Ands.foreach{
          node=>
            var flag_1 = 0
            //println("node!!", node)
            Nots.get(node._3) match {
                  case Some(tuple) => //DoPrim(not,ArrayBuffer(Reference(io_in,UIntType(IntWidth(32)),PortKind,SourceFlow)),ArrayBuffer(),UIntType(IntWidth(32))))
                    flag_1 +=1
                    //println("tuple!!",tuple)
                    new_add1 = tuple._1
                  case _ => node
              }
            Nots.get(node._4) match {
                  case Some(tuple) => //DoPrim(not,ArrayBuffer(Reference(io_in,UIntType(IntWidth(32)),PortKind,SourceFlow)),ArrayBuffer(),UIntType(IntWidth(32))))
                    flag_1 +=1
                    new_add2 = tuple._1
                  case _ => node
              }
            flag_1 match{
              case 2 =>
                //DoPrim(not,ArrayBuffer(),ArrayBuffer(),UIntType(IntWidth(32))))
                var new_or = DoPrim(PrimOps.Or, collection.mutable.ArrayBuffer(new_add1.args(0), new_add2.args(0)),collection.mutable.ArrayBuffer.empty,new_add1.tpe)
                var new_not = DoPrim(PrimOps.Not,collection.mutable.ArrayBuffer(new_or),collection.mutable.ArrayBuffer.empty,new_add1.tpe)
                var new_not_Node = firrtl.ir.DefNode(NoInfo,node._1,new_not)
                //println("new_not_Node",new_not_Node)
                var index = 0
                var dd=0
                Stmts_node_array.foreach{
                  state => state._2 match{
                    case DefNode(info,name,value) =>
                      name match{
                        case node._1 => dd =index
                        case _ => index+=1
                      }
                    case _ =>
                  }
                }
                

                //println("ddddddddd",dd)
                new_Stat += new_not_Node
                new_op_Nodes += new_not_Node
                //这个是修改ROcketCore之后需要的
                //Stmts_node_array.remove(dd)
              case _ =>
            }
      }

      var conn_name = ""
      var conn_tpe = UIntType(IntWidth(33))//new ir.Type
      Stmts_node_array.foreach{
          // state =>
          //   println("state",state,state.getClass)
          //   stmts+= state
          state => state._2 match{
            case ir.Connect(_,WRef(name,tpe,_,_),_)=>
              conn_name = name
              var new_width = BigInt(0)
              tpe match{
                    case UIntType(IntWidth(w))=>
                    new_width = w
                    case _ =>
                  }
              conn_tpe = UIntType(IntWidth(new_width))
              //println("state",state,state.getClass)
            case DefNode(info,name,value)=>
              name match{
                case "_T" =>
                  //println("_T")
                  stmts+= state._2
                case "_GEN_0" =>
                  //println("_GEN_0")
                  stmts+= state._2
                case _ =>
                  stmts+= state._2
              }
              //println("state",state,state.getClass)
            case _ =>
              stmts+= state._2
          }
      }

      //stmts += new_Tail_Node  
      new_op_Nodes.foreach{
        node =>
          stmts += node
      }
      Stmts_node_array.foreach{
          state => state._2 match{
            case ir.Connect(_,WRef(name,tpe,_,_),_)=>
              stmts+= state._2
            case _ =>
              
          }
      }


    val final_stmts = Block(stmts)
    
    Module(m.info, m.name, m.ports, final_stmts)//onStmt(m.body))
  }
  def run(c: Circuit): Circuit = {
    val modulesx = c.modules.map {
      case m: Module    => onModule(m)
      case m: ExtModule => m
    }
    Circuit(c.info, modulesx, c.main)
  }
}
