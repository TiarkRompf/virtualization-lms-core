package scala.virtualization.lms
package internal

import java.io.PrintWriter

trait GenericCodegen extends Scheduling {
  val IR: Expressions
  import IR._

  
  def emitBlock(y: Exp[_])(implicit stream: PrintWriter): Unit = {
    val deflist = buildScheduleForResult(y)
    
    for (TP(sym, rhs) <- deflist) {
      emitNode(sym, rhs)
    }
  }

  def getBlockResult[A](s: Exp[A]): Exp[A] = s
  
  def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter): Unit = {
    throw new Exception("don't know how to generate code for: " + rhs)
  }
  
  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit

  def quote(x: Exp[_]) = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(z) => z.toString
    case Sym(n) => "x"+n
  }
}



trait GenericNestedCodegen extends GenericCodegen {
  val IR: Expressions with Effects
  import IR._

  var shallow = false

//  var outerScope: List[TP[_]] = Nil
//  var levelScope: List[TP[_]] = Nil
  var innerScope: List[TP[_]] = null  // no, it's not a typo

  def initialDefs = super.availableDefs

  override def availableDefs = if (innerScope ne null) innerScope else initialDefs


  def focusBlock[A](result: Exp[_])(body: => A): A = {
//    val saveOuter = outerScope
//    val saveLevel = levelScope
    val saveInner = innerScope

//    outerScope = outerScope ::: levelScope
//    levelScope = Nil
    innerScope = buildScheduleForResult(result) // deep list of deps
    
    val rval = body
    
//    outerScope = saveOuter
//    levelScope = saveLevel
    innerScope = saveInner
    
    rval
  }



  def focusExactScope[A](result: Exp[_])(body: List[TP[_]] => A): A = {
    
    val saveInner = innerScope
    
    val e1 = availableDefs
    shallow = true
    val e2 = buildScheduleForResult(result) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false

    // shallow is 'must outside + should outside' <--- currently shallow == deep for lambdas, meaning everything 'should outside'
    // bound is 'must inside'

    // find transitive dependencies on bound syms, including their defs (in case of effects)
    val bound = for (TP(sym, rhs) <- e1; s <- boundSyms(rhs)) yield s
    val g1 = getDependentStuff(bound)
    
    val levelScope = e1.filter(z => (e2 contains z) && !(g1 contains z)) // shallow (but with the ordering of deep!!) and minus bound

    // sanity check to make sure all effects are accounted for
    result match {
      case Def(Reify(x, effects)) =>
        val actual = levelScope.filter(effects contains _.sym)
        assert(effects == actual.map(_.sym), "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)
      case _ =>
    }

    innerScope = e1 diff levelScope // delay everything that remains

    val rval = body(levelScope)
    
    innerScope = saveInner
    rval
  }


  override def emitBlock(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    focusBlock(result) {
      emitBlockFocused(result)
    }
  }
  
  def emitBlockFocused(result: Exp[_])(implicit stream: PrintWriter): Unit = {
    focusExactScope(result) { levelScope =>
      for (TP(sym, rhs) <- levelScope)
        emitNode(sym, rhs)
    }
  }


  override def getBlockResult[A](s: Exp[A]): Exp[A] = s match {
    case Def(Reify(x, _)) => x
    case _ => super.getBlockResult(s)
  }
  

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Reflect(s, effects) =>
      emitNode(sym, s)
    case Reify(s, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }

}