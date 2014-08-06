/*TODO DISABLED
package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericFatCodegen extends GenericNestedCodegen with FatBlockTraversal {
  val IR: Expressions with Effects with FatExpressions
  import IR._  
  
  
  override def traverseStm(stm: Stm) = stm match {
    case TTP(lhs, mhs, rhs) => emitFatNode(lhs, rhs)
    case _ => super.traverseStm(stm)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {  // TODO: get rid of. used by SimplifyTranform
    case Forward(x) => emitValDef(sym, quote(x))
    case _ => super.emitNode(sym, rhs)
  }
  
  def emitFatNode(sym: List[Sym[Any]], rhs: FatDef): Unit = rhs match {
    case _ => sys.error("don't know how to generate code for: "+rhs)
  }

  // DELITE SPECIFIC METHOD -- used for kernel activation records
  def emitFatNodeKernelExtra(sym: List[Sym[Any]], rhs: FatDef): Unit = { }
  def emitNodeKernelExtra(sym: List[Sym[Any]], rhs: Def[Any]): Unit = { }

  def emitFatBlock(rhs: List[Block[Any]]): Unit = {
    emitBlock(Block(Combine(rhs.map(getBlockResultFull)))) // TODO: find another way
  }

}*/
