// SPDX-License-Identifier: Apache-2.0

package firrtl.passes
//package firrtl.backends.experimental.smt
//package firrtlTests.execution

import firrtl._
import firrtl.ir._
import firrtl.Mappers._
import firrtl.options.Dependency
import firrtl.PrimOps

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
    val ADDs = collection.mutable.HashMap[String, (DoPrim,String,String,firrtl.ir.PrimOp)]()
    //val MUXs = collection.mutable.HashMap[String, (Expression,Expression,String,String,String, Expression)]()
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
        //println("eliminateNodeRef", e)
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
                  println("Op",op.toString)
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
                  ADDs(x.name) = (DoPrim(op,args,consts,tpe),add1_name,add2_name,op)
                case PrimOps.Sub =>
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
                  ADDs(x.name) = (DoPrim(op,args,consts,tpe),add1_name,add2_name,op)
                case PrimOps.Div =>
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
                  ADDs(x.name) = (DoPrim(op,args,consts,tpe),add1_name,add2_name,op)
                case PrimOps.And =>
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
                  ADDs(x.name) = (DoPrim(op,args,consts,tpe),add1_name,add2_name,op)
                case PrimOps.Or =>
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
                  ADDs(x.name) = (DoPrim(op,args,consts,tpe),add1_name,add2_name,op)
                case PrimOps.Xor =>
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
                  ADDs(x.name) = (DoPrim(op,args,consts,tpe),add1_name,add2_name,op)
                case PrimOps.Tail =>
                  var tail_name : String =""
                    args(0) match {
                      case WRef(name, _, _, _)=>
                        tail_name = name
                      case _ => args
                    }//)
                    Tails(x.name) = (new_exp,tail_name)
            
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
              }
              tval match {
                case WRef(name, _, _, _)=>
                  tval_name = name
              }
              fval match {
                case WRef(name, _, _, _)=>
                  fval_name = name
              }

              MUXs += ((tval, fval,cond_name,tval_name,fval_name,cond,x.name))//new_ex
        
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
          
          // println("expressions")
          //println("This is x", x,x.getClass)
          x
        case other => other.map(eliminateNodeRefs)
      }

      
    }


    def eliminateNewExp(e: Expression): Expression = {
        MUXs.foreach{
          node=> 
            println("eliminateNewExp",node)
        }
        e
        
    }

  
    

    val Statement1 = eliminateNodeRefs(s)
    //var stmts = new collection.mutable.ArrayBuffer[Statement]
    var Stmts_node = collection.mutable.HashMap[String, Statement]()
    //var new_block_head = Block(stmts)
    var Stmts_node_array = collection.mutable.ArrayBuffer[(String, Statement)]()
    
    var new_add1 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    var new_add2 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    var new_conn2 = DoPrim(PrimOps.Not, Seq(), Nil,UnknownType)
    var new_block = s
    var new_Stat = collection.mutable.ArrayBuffer[DefNode]()
    var stmts = new collection.mutable.ArrayBuffer[Statement]
    var new_Tail_Node = DefNode(NoInfo,"_GEN_0",new_add1)
    var new_add_Node = DefNode(NoInfo,"_T_8",new_add1)
    var new_Mux_Node = DefNode(NoInfo,"_T_7",new_add1)
    var temp_count = 0
    Statement1.foreachStmt{
         state =>
           println("state",state,state.getClass)
           //stmts+= state
           state match{
              case DefNode(_,name,_)=>
                //println("name",name)
                Stmts_node(name) = state
                Stmts_node_array += ((name,state))
              case Connect(info,outputPortRef, expr) =>
                //println("Catch Connect", expr)
                expr match {
                  case Mux(cond, tval, fval, tpe) =>
                    println("Catch Connect",expr)
                    MUXs.foreach{
                      mux_temp =>
                        println("MUXS",mux_temp)
                        cond match{
                          case mux_temp._6 =>
                            println("cond",mux_temp)
                            tval match{
                              case mux_temp._1 =>
                                fval match 
                                {
                                  case mux_temp._2 =>
                                    println("Catch Connect")
                                    
                                    outputPortRef match {
                                      case WRef(name,tpe,_,_)=>
                                        //Stmts_node(name) = state
                                        println("Catch Connect",ir.Connect(info,outputPortRef,WRef(mux_temp._7,tpe,NodeKind,SinkFlow)))
                                        Stmts_node(name) = ir.Connect(info,outputPortRef,WRef(mux_temp._7,tpe,NodeKind,SinkFlow))
                                        Stmts_node_array += ((name,ir.Connect(info,outputPortRef,WRef(mux_temp._7,tpe,NodeKind,SinkFlow))))
                                      case _ =>
                                        Stmts_node("Connect"+temp_count.toString) = ir.Connect(info,outputPortRef,WRef(mux_temp._7,tpe,NodeKind,SinkFlow))
                                        Stmts_node_array += (("Connect"+temp_count.toString,ir.Connect(info,outputPortRef,WRef(mux_temp._7,tpe,NodeKind,SinkFlow))))
                                        temp_count +=1
                                    }
                                  case _ =>
                                    Stmts_node("Connect"+temp_count.toString) = state
                                    Stmts_node_array += (("Connect"+temp_count.toString,state))
                                    temp_count +=1
                                }
                              case _ =>
                                Stmts_node("Connect"+temp_count.toString) = state
                                Stmts_node_array += (("Connect"+temp_count.toString,state))
                                temp_count +=1
                            }
                          case _ =>
                            Stmts_node("Connect"+temp_count.toString) = state
                            Stmts_node_array += (("Connect"+temp_count.toString,state))
                            temp_count +=1
                        }
                    }
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
    var new_name = "_Mux_op_"
    var index = 0
    //new_Tail_Node = firrtl.ir.DefNode(NoInfo,node._1,new_Tail)
    var new_Node = collection.mutable.ArrayBuffer[DefNode]()
    var new_mux_Node = collection.mutable.ArrayBuffer[DefNode]()
    
    var new_op_Nodes = collection.mutable.ArrayBuffer[DefNode]()
    MUXs.foreach{
      node=>
        var flag_1 = 0
        //第一个tail
        //MUXs [String, (Expression,Expression,String,String,String, Expression)]
        //val ADDs = collection.mutable.HashMap[String, (DoPrim,String,String)]()
        //println("node._2._4",node._2._4)
        println(node._7)
        //提取共同的加法操作
        Tails.get(node._4) match {
              case Some(tuple) =>
                //println("MUX get", tuple, tuple._2)
                ADDs.get(tuple._2) match {
                  case Some(tuple_add)=>
                    flag_1 +=1
                    //println("ADD get", tuple_add)
                    new_add1 = tuple_add._1 //DoPrim(add,ArrayBuffer(Reference(io_in,UIntType(IntWidth(32)),PortKind,SourceFlow), UIntLiteral(7,IntWidth(32))),ArrayBuffer(),UIntType(IntWidth(33)))
                    //println("tuple_add",tuple_add._1)
                    node
                  case _ => node
                }
              case _ => node
          }
        Tails.get(node._5) match {
              case Some(tuple) =>
                println("MUX get2", tuple, tuple._2)
                ADDs.get(tuple._2) match {
                  case Some(tuple_add)=>
                    flag_1 +=1
                    //println("ADD get2", tuple_add)
                    //println(tuple_add)
                    new_add2 = tuple_add._1
                    node
                  case _ => node
                }
              case _ => node
          }
        var eq_op = ""
        ADDs.get(node._4) match {
          case Some(tuple_add)=>
            flag_1 +=2
            //println("ADD get", tuple_add)
            eq_op = tuple_add._4.toString
            new_add1 = tuple_add._1 //DoPrim(add,ArrayBuffer(Reference(io_in,UIntType(IntWidth(32)),PortKind,SourceFlow), UIntLiteral(7,IntWidth(32))),ArrayBuffer(),UIntType(IntWidth(33)))
            println("new_add2",tuple_add._1)
            node
          case _ => node        
        }
        ADDs.get(node._5) match {
          case Some(tuple_add)=>
            tuple_add._4.toString match{
              case eq_op =>
                flag_1 +=2
                //println("ADD get", tuple_add)
                new_add2 = tuple_add._1 //DoPrim(add,ArrayBuffer(Reference(io_in,UIntType(IntWidth(32)),PortKind,SourceFlow), UIntLiteral(7,IntWidth(32))),ArrayBuffer(),UIntType(IntWidth(33)))
                println("new_add2",tuple_add._1)
                node
              case _ =>
            }
            
          case _ => node        
        }
        
        flag_1 match{
          case 2|4 =>
            var new_Mux_1 = new_add1.args(1)//无意义的初始化
            var new_Mux_0 = new_add1.args(0)
            //var new_type= UIntType(IntWidth(33))
            var new_width = BigInt(32)
            new_add2.args(1) match {
                case WRef(name,tpe,_,_) =>
                    var temp2 = ""
                    tpe match{
                      case UIntType(IntWidth(w))=>
                      new_width = w
                      case _ =>
                    }
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
                        //println("name,temp2", name,temp2)
                        new_Mux_1 = Mux(node._6, new_add2.args(1), new_add1.args(1), UnknownType)
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
                        new_Mux_1 = Mux(node._6, new_add2.args(1), new_add1.args(1), UnknownType)
                    }
                    
                case _ => 
                  new_Mux_1 = Mux(node._6, new_add2.args(1), new_add1.args(1), UnknownType)
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
                        new_Mux_0 = Mux(node._6, new_add2.args(0), new_add1.args(0), UnknownType)
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
                        new_Mux_0 = Mux(node._6, new_add2.args(0), new_add1.args(0), UnknownType)
                    }
                    
                case _ => 
                  new_Mux_0 = Mux(node._6, new_add2.args(0), new_add1.args(0), UnknownType)
            }
            flag_1 match{
              case 2 =>
                var new_add = DoPrim(PrimOps.Add, collection.mutable.ArrayBuffer(new_Mux_0, new_Mux_1),collection.mutable.ArrayBuffer.empty,UIntType(IntWidth(new_width+1)))
                var new_Tail = DoPrim(PrimOps.Tail, collection.mutable.ArrayBuffer(new_add),collection.mutable.ArrayBuffer(1),UIntType(IntWidth(new_width)))
                new_Tail_Node = firrtl.ir.DefNode(NoInfo,node._7,new_Tail)
                new_Stat += new_Tail_Node   
                new_op_Nodes += firrtl.ir.DefNode(NoInfo,node._7,new_Tail)
              case 4 =>
                //var opp = 
                println("new_node_gen!",new_add2)
                new_add2 match{
                  case DoPrim(op,_,_,_) =>
                    var new_op = DoPrim(op, collection.mutable.ArrayBuffer(new_Mux_0, new_Mux_1),collection.mutable.ArrayBuffer.empty,UIntType(IntWidth(new_width+1)))
                    //new_Tail_Node = firrtl.ir.DefNode(NoInfo,node._7,new_op)
              
                    new_op_Nodes += firrtl.ir.DefNode(NoInfo,node._7,new_op)
                  case _ =>

                }
                
              case _ =>

            }
            
          case _=>
        }    
    }
    
    

    new_block = Block(stmts)
    //println("new_block",new_block,new_block.getClass)
    //val Statement2 = eliminateNodeRefs(s)
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


    //stmts += new_Mux_Node
    //stmts += new_add_Node
     
    //stmts += ir.Connect(NoInfo,WRef(conn_name,conn_tpe,PortKind,SinkFlow),new_conn2)
   
    val final_stmts = Block(stmts) // buildStatement(new_Stat)
    println("final_stmts",final_stmts)
    final_stmts
    // println("Nodes:")
    // println(nodes)
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

      //println("new_body",new_body)

      Module(m.info, m.name, m.ports, new_body)
    }
    Circuit(c.info, modulesx, c.main)
  }
}
