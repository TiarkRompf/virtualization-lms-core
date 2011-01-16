package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericFatCodegen extends GenericNestedCodegen with FatScheduling {
  val IR: Expressions with Effects with FatExpressions
  import IR._  
  
  case class Combine(a: List[Exp[Any]]) extends Exp[Any]


  override def emitBlockFocused(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    var currentScope = innerScope.map(fatten)
    currentScope = getFatSchedule(currentScope)(result) // clean things up!
    emitFatBlockFocused(currentScope)(result)
  }

  def emitFatBlockFocused(currentScope: List[TTP])(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    // do what super does, modulo fat stuff
    focusExactScopeFat(currentScope)(result) { levelScope => 
      for (TTP(syms, rhs) <- levelScope) {
        emitFatNode(syms, rhs)
      }
    }
  }

  def focusExactScopeFat[A](currentScope: List[TTP])(result: Exp[_])(body: List[TTP] => A): A = {
    
    val saveInner = innerScope
    
    val e1 = currentScope
    shallow = true
    val e2 = getFatSchedule(currentScope)(result) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false

    // shallow is 'must outside + should outside' <--- currently shallow == deep for lambdas, meaning everything 'should outside'
    // bound is 'must inside'

    // find transitive dependencies on bound syms, including their defs (in case of effects)
    val bound = e1.flatMap(z => boundSyms(z.rhs))
    val g1 = getFatDependentStuff(currentScope)(bound)
    
    val levelScope = e1.filter(z => (e2 contains z) && !(g1 contains z)) // shallow (but with the ordering of deep!!) and minus bound

/*
    // sanity check to make sure all effects are accounted for
    result match {
      case Def(Reify(x, effects)) =>
        val actual = levelScope.filter(effects contains _.sym)
        assert(effects == actual.map(_.sym), "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)
      case _ =>
    }
*/
    val innerScope2 = e1 diff levelScope // delay everything that remains

    innerScope = innerScope2 flatMap { 
      case TTP(List(sym), ThinDef(rhs)) => List(TP(sym, rhs))
      case e => 
      println("ignore: " + e)
      Nil
    }

    val rval = body(levelScope)
    
    innerScope = saveInner
    rval
  }


  def emitFatNode(sym: List[Sym[_]], rhs: FatDef)(implicit stream: PrintWriter): Unit = rhs match {
    case ThinDef(Reflect(s, effects)) => emitFatNode(sym, ThinDef(s)) // call back into emitFatNode, not emitNode
    case ThinDef(a) => emitNode(sym(0), a)
    case _ => system.error("don't know how to generate code for: "+rhs)
  }

  def emitFatBlock(rhs: List[Exp[_]])(implicit stream: PrintWriter): Unit = {
    emitBlock(Combine(rhs))
  }

  
}