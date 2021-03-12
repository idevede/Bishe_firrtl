// SPDX-License-Identifier: Apache-2.0

package firrtl.passes
//package firrtl.backends.experimental.smt
//package firrtlTests.execution

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.options.Dependency
import firrtl.PrimOps
import scala.util.control.Breaks._

object CommonSubexpressionElimination extends Pass {

  override def prerequisites = firrtl.stage.Forms.LowForm ++
    Seq(
      Dependency(firrtl.passes.RemoveValidIf),
      Dependency[firrtl.transforms.ConstantPropagation],
      Dependency(firrtl.passes.memlib.VerilogMemDelays),
      Dependency(firrtl.passes.SplitExpressions),
      Dependency[firrtl.transforms.CombineCats]
    )

  override def optionalPrerequisiteOf =
    Seq(Dependency[SystemVerilogEmitter], Dependency[VerilogEmitter])

  override def invalidates(a: Transform) = false

  private def cse(s: Statement): Statement = {
    val expressions = collection.mutable.HashMap[MemoizedHash[Expression], String]()
    val nodes = collection.mutable.HashMap[String, Expression]()
    val Tails = collection.mutable.HashMap[String, (Expression,String)]()
    val ADDs = collection.mutable.HashMap[String, (DoPrim,String,String)]()
    //val MUXs = collection.mutable.HashMap[String, (Expression,Expression,String,String,String, Expression)]()//ArrayBuffer
    val MUXs = collection.mutable.ArrayBuffer[(Expression,Expression,String,String,String, Expression,String)]()//ArrayBuffer
    

    def eliminateNodeRef(e: Expression): Expression = e match {

      

      case WRef(name, tpe, kind, flow) =>
          
        nodes.get(name) match {
          case Some(expression) =>
            expressions.get(expression) match {
              case Some(cseName) if cseName != name =>
                WRef(cseName, tpe, kind, flow)
              case _ => e
            }
          case _ => e
        }
      case _ => 
        e.map(eliminateNodeRef)
    }


    def checkExpr(expr: ir.Expression): Unit = expr match {
        case  DoPrim(op,args,consts,tpe)=>
          //println(op)
          //println(op.getClass) //println(op.getclass)
          op match{

            case PrimOps.Add =>
              // println("Catch ADD")
              // println(expr)
              // println(args)
              expr.foreachExpr(checkExpr)
            // case _ =>
            //   println("Catch Other")
            case PrimOps.Tail =>
              //println("Catch Tail")
              expr.foreachExpr(checkExpr)
            case PrimOps.Eq =>
              println("Eq")
            
            case _ =>
              println("Catch Other")
          }
            
        case WRef(name, _, _, _)=>
          //println(expr)
          println("WRef",name, name.getClass)
        case Mux(cond, tval, fval, tpe) =>
          //println("Mux")
          expr.foreachExpr(checkExpr)
        case _ =>
          //println("other") 
          expr
          //println("other"+expr)
      }

    def updateExpType(e: Expression): Expression = e match {
        //case DoPrim(Mul, args, consts, tpe)                        => e.map(updateExpType)
    
        case Mux(cond, tval, fval, tpe) =>
          val newExp = Mux(cond, fval, tval, UnknownType)
          newExp.map(updateExpType)
        case _ => 
          //println("update")
          //println(e)
          e
        }



    def eliminateNodeRefs(s: Statement): Statement = {
      s.map(eliminateNodeRef) match {
        case x: DefNode=> //(info,name,value) =>
          val new_exp = x.value//updateExpType(x.value)
          new_exp match{
            case  DoPrim(op,args,consts,tpe)=>

              op match{

                case PrimOps.Add =>
                  var add1_name : String =""
                  var add2_name : String =""
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
                  ADDs(x.name) = (DoPrim(op,args,consts,tpe),add1_name,add2_name)
                case PrimOps.Tail =>
                  var tail_name : String =""
                    args(0) match {
                      case WRef(name, _, _, _)=>
                        tail_name = name
                      case _ => args
                    }//)
                    Tails(x.name) = (new_exp,tail_name)
                case PrimOps.Eq =>
                    println("Eq")
                case _ =>
                    op
              }
            case WRef(name, _, _, _)=>
              println("New name")
              println(name)
              //println(new_exp)
            case Mux(cond, tval, fval, tpe) =>
              var cond_name : String =""
              var tval_name : String =""
              var fval_name : String =""
              cond match {
                case WRef(name, _, _, _)=>
                  cond_name = name
                case  DoPrim(op,args,consts,tpe) =>
                  cond_name = op.toString
                case _ =>
                
              }
              tval match {
                case WRef(name, _, _, _)=>
                  tval_name = name
                case  DoPrim(op,args,consts,tpe) =>
                  tval_name = op.toString
                case _ =>

              }
              fval match {
                case WRef(name, _, _, _)=>
                  fval_name = name
                case  DoPrim(op,args,consts,tpe) =>
                  fval_name = op.toString
                case _ =>

              }

              MUXs += ((tval, fval,cond_name,tval_name,fval_name,cond,x.name))//new_ex
                     //Expression,Expression,String,String,String, Expression,String
              println(x.name)
        
            case _ => x.value
                  //println("New Type")
                  //println(x.value)
              }
          
          nodes(x.name) = new_exp//x.value
          x.name match{
            case "io_out" =>
              println("io_out")
            case _ =>
              expressions.getOrElseUpdate(new_exp, x.name)
          }
        
          x
        case other => other.map(eliminateNodeRefs)
      }

      
    }
   

    val Statement1 = eliminateNodeRefs(s)    
    var stmts = new collection.mutable.ArrayBuffer[Statement]
    var Stmts_node = collection.mutable.LinkedHashMap[String, Statement]()
    var Stmts_node_array = collection.mutable.ArrayBuffer[(String, Statement)]()
    var new_block = Block(stmts)
    
    var temp_count = 0
    Statement1.foreachStmt{
         state =>
           //println("state",state,state.getClass)
           //stmts+= state
           state match{
             case DefNode(_,name,_)=>
                //println("name",name)
                Stmts_node(name) = state
                Stmts_node_array += ((name,state))
              case Connect(_,outputPortRef, expr) =>
                outputPortRef match {
                  case WRef(name,_,_,_)=>
                    Stmts_node(name) = state
                    Stmts_node_array += ((name,state))
                  case _ =>
                    Stmts_node("Connect"+temp_count.toString) = state
                    Stmts_node_array += (("Connect"+temp_count.toString,state))
                    temp_count +=1
                }
                //println("Connect!!",outputPortRef)
              case _ =>
                Stmts_node("Other"+temp_count.toString) = state
                Stmts_node_array += (("Other"+temp_count.toString,state))
           }
    }
    println("Stmts_node_array",Stmts_node_array)

    var new_name = "_Mux_op_"
    var index = 0
    //new_Tail_Node = firrtl.ir.DefNode(NoInfo,node._1,new_Tail)
    
    var new_mux_Node = collection.mutable.ArrayBuffer[DefNode]()
    MUXs.foreach{
      node=>
        var new_Node = collection.mutable.ArrayBuffer[DefNode]()
        var flag = 0 
        var Ref = new Array[Expression](3)
        println("node",node)
        node._1 match {
          case DoPrim(op,args,consts,tpe)=>
            //rintln("node",node._2._1)
            //node 直接就加进去
            flag = 1
            var new_Node_1 = firrtl.ir.DefNode(NoInfo,new_name+index.toString,node._1)
            new_Node += new_Node_1
            Ref(0) = WRef(new_name+index.toString,tpe,NodeKind,SourceFlow)
            index = index+1
          case _ =>
            Ref(0) = node._1
        }

        node._2 match {
          case DoPrim(op,args,consts,tpe)=>
            flag = 1
            var new_Node_2 = firrtl.ir.DefNode(NoInfo,new_name+index.toString,node._2)
            new_Node += new_Node_2
            Ref(1) = WRef(new_name+index.toString,tpe,NodeKind,SourceFlow)
            index = index+1
          case _ =>
            Ref(1) = node._2
        }

        node._6 match {
          case DoPrim(op,args,consts,tpe)=>
            flag = 1
            var new_Node_6 = firrtl.ir.DefNode(NoInfo,new_name+index.toString,node._6)
            new_Node += new_Node_6
            Ref(2) = WRef(new_name+index.toString,tpe,NodeKind,SourceFlow)
            index = index+1
          case _ =>
            Ref(2) = node._6
        }
      
      flag match{
        case 1 =>
          var i = 0
          var pos = 0
          Stmts_node_array.foreach{
            stmt => stmt._1 match{
              case node._7 =>
                pos = i
                //break
              case _ =>
                i +=1
            }
          }

          println("pos",pos)

          Stmts_node_array.remove(pos)
          var NN = DefNode(NoInfo,node._7,Mux(Ref(2),Ref(0),Ref(1))) 
          new_Node.length match{
            case 0 =>
              Stmts_node_array.insert(pos,(node._7,NN))
            case 1 =>
              Stmts_node_array.insert(pos,(("a",new_Node(0))),((node._7,NN)))
            case 2 =>
              Stmts_node_array.insert(pos,(("a",new_Node(0))),(("b",new_Node(1))),((node._7,NN)))
            case 3 =>
              Stmts_node_array.insert(pos,(("a",new_Node(0))),(("b",new_Node(1))),(("c",new_Node(2))),((node._7,NN)) ) 
          }

          // Stmts_node.get(node._7) match {
          //     case Some(expr)  =>
          //       println("Catch Expr!", expr)
          //       println(node._7)
          //       //Stmts_node -= node._7
          //       Stmts_node(node._7) =  DefNode(NoInfo,node._7,Mux(Ref(2),Ref(0),Ref(1)))
          //       //new_Node+=expr
          //     case _ => 
          //   }
          // new_Node += DefNode(NoInfo,node._7,Mux(Ref(2),Ref(0),Ref(1)))
          
          
        case 0 =>
      }
    }

    // new_Node.foreach{
    //   node =>
    //     println("new_node",node)
    //     stmts += node
    // }

    Stmts_node_array.foreach{
      node => 
        println("node",node)
        node._2 match{
        case DefNode(_,name,_)=>
            println("node._2",node._2)
            stmts += node._2
        case  _ =>
      }    
    }
    // new_Node.foreach{
    //   node =>
    //     println(node)
    //     stmts += node
    // }

    // new_mux_Node.foreach{
    //   node =>
    //     //println(node)
    //     stmts += node
    // }

    Stmts_node_array.foreach{
      node => node._2 match{
        case DefNode(_,name,_)=>
            
        case  _ =>
          stmts += node._2
      }    
    }

    //stmts + new_Node
    val final_stmts = Block(stmts)
    //Statement1
    stmts.foreach{
      node => println("node",node)
    }
    final_stmts

    //接下来的数据分支进行处理
  /*
    var new_add1 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    var new_add2 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    var new_conn2 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    var new_block = s
    var new_Stat = collection.mutable.ArrayBuffer[DefNode]()
    var stmts = new collection.mutable.ArrayBuffer[Statement]
    var new_Tail_Node = DefNode(NoInfo,"_GEN_0",new_add1)
    var new_add_Node = DefNode(NoInfo,"_T_8",new_add1)
    var new_Mux_Node = DefNode(NoInfo,"_T_7",new_add1)
    MUXs.foreach{
      node=>
        //第一个tail
        //MUXs [String, (Expression,Expression,String,String,String, Expression)]
        //println("node._2._4",node._2._4)
        println(node._1)
        Tails.get(node._2._4) match {
              case Some(tuple) =>
                //println("MUX get", tuple, tuple._2)
                ADDs.get(tuple._2) match {
                  case Some(tuple_add)=>
                    //println("ADD get", tuple_add)
                    new_add1 = tuple_add._1 //DoPrim(add,ArrayBuffer(Reference(io_in,UIntType(IntWidth(32)),PortKind,SourceFlow), UIntLiteral(7,IntWidth(32))),ArrayBuffer(),UIntType(IntWidth(33)))
                    //println("tuple_add",tuple_add._1)
                    node
                  case _ => node
                }
              case _ => node
          }
        Tails.get(node._2._5) match {
              case Some(tuple) =>
                //println("MUX get2", tuple, tuple._2)
                ADDs.get(tuple._2) match {
                  case Some(tuple_add)=>
                    //println("ADD get2", tuple_add)
                    //println(tuple_add)
                    new_add2 = tuple_add._1
                    node
                  case _ => node
                }
              case _ => node
          }

        //println("new_add1", new_add1)
        def buildExpression(expr: Expression): Expression = {
          expr match {
            case _: DoPrim | _: Mux | _: ValidIf | _: Literal =>
              expr.map(buildExpression)
            case _ =>
          }
          expr
        }
            
        var new_Mux_1 = new_add1.args(1)
        var new_Mux_0 = new_add1.args(0)
        //var new_type= UIntType(IntWidth(33))
        var new_width = BigInt(32)
        new_add2.args(1) match {
            case WRef(name,tpe,_,_) =>
                var temp2 = ""
                //new_type = tpe
                tpe match{
                  case UIntType(IntWidth(w))=>
                  new_width = w
                  case _ =>
                }
                println("tpe",tpe)
                new_add1.args(1) match {
                  case WRef(name_add1,_,_,_) =>
                    temp2 = name_add1
                  case _ =>
                    temp2 = ""
                }
                //println("name,temp2", name,temp2)
                var flag = name.equals(temp2)
                flag match {
                  case true =>
                    new_Mux_1 = new_add1.args(1)
                  case false => 
                    println("name,temp2", name,temp2)
                    new_Mux_1 = Mux(node._2._6, new_add2.args(1), new_add1.args(1), UnknownType)
                }
            case UIntLiteral(value, width)  =>
                println(width)
                //new_width = width
                width match{
                  case IntWidth(w)=>
                  new_width = w
                  case _ =>
                }
                var temp2 = BigInt(0)
                new_add1.args(1) match {
                  case UIntLiteral(value_add1, width) =>
                    temp2 = value_add1
                  case _ =>
                    temp2 = BigInt(0)
                }
              
                var flag = value.equals(temp2)
                flag match {
                  case true =>
                    //println("value,temp2", value,temp2)
                    new_Mux_1 = new_add2.args(1)
                  case false => 
                    //println("value,temp2", value,temp2)
                    new_Mux_1 = Mux(node._2._6, new_add2.args(1), new_add1.args(1), UnknownType)
                }
                
            case _ => 
              new_Mux_1 = Mux(node._2._6, new_add2.args(1), new_add1.args(1), UnknownType)
        }
        
        new_add2.args(0) match {
            case WRef(name,tpe,_,_) =>
                var temp2 = ""
                //new_type = tpe
                tpe match{
                  case UIntType(IntWidth(w))=>
                  new_width = w
                  case _ =>
                }
                new_add1.args(0) match {
                  case WRef(name_add0,_,_,_) =>
                    temp2 = name_add0
                  case _ =>
                    temp2 = ""
                }
                var flag = name.equals(temp2)
                flag match {
                  case true =>
                    new_Mux_0 = new_add1.args(0)
                  case false => 
                    new_Mux_0 = Mux(node._2._6, new_add2.args(0), new_add1.args(0), UnknownType)
                }
            case UIntLiteral(value, width)  =>
                println(width)
                //new_width = width
                width match{
                  case IntWidth(w)=>
                  new_width = w
                  case _ =>
                  
                }
                
                var temp2 = BigInt(0)
                new_add1.args(0) match {
                  case UIntLiteral(value_add0, width) =>
                    temp2 = value_add0
                  case _ =>
                    temp2 = BigInt(0)
                }
                var flag = value.equals(temp2)
                flag match {
                  case true =>
                    new_Mux_0 = new_add1.args(0)
                  case false => 
                    new_Mux_0 = Mux(node._2._6, new_add2.args(0), new_add1.args(0), UnknownType)
                }
                
            case _ => 
              new_Mux_0 = Mux(node._2._6, new_add2.args(0), new_add1.args(0), UnknownType)
        }
        
        println("new_Mux_0",new_Mux_0)
        println("new_Mux_1",new_Mux_1)
          
          //Mux(node._2._6, new_add2.args(1), new_add1.args(1), UnknownType)


        //println("MUXs", node._1,node._2,node._2._1)
        var new_Mux = Mux(node._2._6, new_add2.args(1), new_add1.args(1), UnknownType)
        //println("newExp", new_Mux)
        var new_add = DoPrim(PrimOps.Add, collection.mutable.ArrayBuffer(new_Mux_0, new_Mux_1),collection.mutable.ArrayBuffer.empty,UIntType(IntWidth(new_width+1)))
        //println("new_add3", new_add)

        var new_Tail = DoPrim(PrimOps.Tail, collection.mutable.ArrayBuffer(new_add),collection.mutable.ArrayBuffer(1),UIntType(IntWidth(new_width)))
        //println("new_add4", new_Tail)

        new_Tail_Node = firrtl.ir.DefNode(NoInfo,node._1,new_Tail)
        //new_add_Node = firrtl.ir.DefNode(NoInfo,"_T_8",new_add)
        //new_Mux_Node = firrtl.ir.DefNode(NoInfo,"_T_7",buildExpression(new_Mux))
        // expressions.getOrElseUpdate(new_Mux, "_T_7")
        // expressions.getOrElseUpdate(new_add, "_T_8")
        // expressions.getOrElseUpdate(new_Tail, "io_out")
        new_conn2 = new_Tail
        
        new_Stat += new_Tail_Node
        //new_Stat += new_add_Node
        //new_Stat += new_Mux_Node
        
        //var new_Stat2 = collection.mutable.ArrayBuffer[DefNode](new_Tail_Node, new_add_Node, new_Mux_Node)
        //var new_Stat = collection.mutable.ArrayBuffer[DefNode](DefNode(_,"_GEN_0",new_Tail), DefNode(_,"_T_8",new_add), DefNode(_,"_T_7",new_Mux))
        // println("new_Mux_Node",new_Mux_Node.toString,new_Mux_Node.getClass)
        // println("new_Mux",new_Mux,buildExpression(new_Mux).getClass)
        // println("new_Stat",new_Stat)
        // println("new_Stmts",stmts,stmts.getClass)
        //Block(Seq(DefNode(_, _, value))
        //new_block = Block(new_Stat.toSeq)
        // println("new_block",new_block)
        

        // case Mux(cond, tval, fval, tpe) =>
        //   val newExp = Mux(cond, fval, tval, UnknownType)
          //newExp.map(updateExpType)
      
    }
    

    new_block = Block(stmts)
    val Statement2 = eliminateNodeRefs(s)

    Statement2.foreachStmt{
        state => state match{
          case ir.Connect(_,_,_)=>
            println("state",state,state.getClass)
          case DefNode(info,name,value)=>
            name match{
              case "_T" =>
                stmts+= state
              case "_GEN_0" =>
                stmts+= state
              case _ =>
                stmts+= state
            }
            //println("state",state,state.getClass)
          case _ =>
            stmts+= state
        }
    }

    //stmts += new_Mux_Node
    //stmts += new_add_Node
    stmts += new_Tail_Node   
    stmts += ir.Connect(NoInfo,WRef("io_out",UIntType(IntWidth(32)),PortKind,SinkFlow),new_conn2)
    val final_stmts = Block(stmts) // buildStatement(new_Stat)
    // final_stmts.foreach{
    //   final_stmt => println("final_stmts",final_stmt)
    // }
    println("final_stmts",final_stmts)
    final_stmts
    // println("Nodes:")
    // println(nodes)
    */
  }
/*
从mux入手，寻找跟mux相关的节点，然后查找共同操作，寻找相同节点的
*/
  def run(c: Circuit): Circuit = {
    //println(c)
    //println("m.body")
    val modulesx = c.modules.map {
      case m: ExtModule => 
        //println("This is m")
        //println(m)
        m
      case m: Module    => 

        var new_body = cse(m.body)
        Module(m.info, m.name, m.ports, new_body)
    }
    // println("c.main")
    // println(c.main)
    // println("this is moudlex")
    // println(modulesx)
    // println("c.info")
    // println(c.info)
    Circuit(c.info, modulesx, c.main)
  }
}
