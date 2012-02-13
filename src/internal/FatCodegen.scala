package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericFatCodegen extends GenericNestedCodegen with FatTraversal {
  val IR: Expressions with Effects with FatExpressions
  import IR._  
  
  
  override def emitBlockFocused(result: Block[Any])(implicit stream: PrintWriter): Unit = {
    var currentScope = fattenAll(innerScope)
    currentScope = getSchedule(currentScope)(result) // clean things up!
    result match {
      case Block(Combine(rs)) => emitFatBlockFocused(currentScope)(rs.map(Block(_)))  // TODO: find another way
      case _ => emitFatBlockFocused(currentScope)(List(result))
    }
  }

  def emitFatBlockFocused(currentScope: List[Stm])(result: List[Block[Any]])(implicit stream: PrintWriter): Unit = {
/*
    val dbg = (result == List(Sym(1729)))
    if (dbg) {
      println("***trigger***")
      println(syms(result))
      println(boundSyms(result))
      println(readSyms(result))
      println(symsFreq(result))
    }
    
    println("-- block for "+result)
    currentScope.foreach(println(_))
*/    
    // do what super does, modulo fat stuff
    focusExactScopeFat(currentScope)(result) { levelScope => 
/*
      println("-- level for "+result)
      levelScope.foreach(println(_))
      println("-- exact for "+result)
      availableDefs.foreach(println(_))
*/
      levelScope foreach {
        case TP(sym, rhs) => emitNode(sym,rhs)
        case TTP(lhs, mhs, rhs) => emitFatNode(lhs, rhs)
      }
    }
  }


  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: java.io.PrintWriter) = rhs match {  // TODO: get rid of. used by SimplifyTranform
    case Forward(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
  
  def emitFatNode(sym: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = rhs match {
    case _ => sys.error("don't know how to generate code for: "+rhs)
  }

  // DELITE SPECIFIC METHOD -- used for kernel activation records
  def emitFatNodeKernelExtra(sym: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = { }


  def emitFatBlock(rhs: List[Block[Any]])(implicit stream: PrintWriter): Unit = {
    emitBlock(Block(Combine(rhs.map(getBlockResultFull)))) // TODO: find another way
  }


}