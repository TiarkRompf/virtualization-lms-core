package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericFatCodegen extends GenericNestedCodegen with FatTraversal {
  val IR: Expressions with Effects with FatExpressions
  import IR._  
  
  //  ------------------- these are needed by loop fusion. they should live elsewhere.
  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = None
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
  def unapplySimpleCollectIf(e: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollect(e).map((_,Nil))

  def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]): Def[Any] = sys.error("not implemented")

  def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true

  // -------------------
  
  
  
  override def emitBlockFocused(result: Exp[Any])(implicit stream: PrintWriter): Unit = {
    var currentScope = fattenAll(innerScope)
    currentScope = getFatSchedule(currentScope)(result) // clean things up!
    result match {
      case Combine(rs) => emitFatBlockFocused(currentScope)(rs)
      case _ => emitFatBlockFocused(currentScope)(List(result))
    }
  }

  def emitFatBlockFocused(currentScope: List[TTP])(result: List[Exp[Any]])(implicit stream: PrintWriter): Unit = {
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
      for (TTP(syms, rhs) <- levelScope) {
        emitFatNode(syms, rhs)
      }
    }
  }


  def emitFatNode(sym: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = rhs match {
    case ThinDef(Reflect(s, u, effects)) => emitFatNode(sym, ThinDef(s)) // call back into emitFatNode, not emitNode
    case ThinDef(a) => emitNode(sym(0), a)
    case _ => sys.error("don't know how to generate code for: "+rhs)
  }

  // DELITE SPECIFIC METHOD -- used for kernel activation records
  def emitFatNodeKernelExtra(sym: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter): Unit = { }


  def emitFatBlock(rhs: List[Exp[Any]])(implicit stream: PrintWriter): Unit = {
    emitBlock(Combine(rhs))
  }


}