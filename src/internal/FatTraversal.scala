package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}


trait FatTraversal extends NestedTraversal with FatScheduling {
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


  def focusExactScopeFat[A](currentScope: List[TTP])(resultB: List[Block[Any]])(body: List[TTP] => A): A = {
    val result = resultB.map(getBlockResultFull)

    val saveInner = innerScope

    val e1 = currentScope
    //shallow = true
    //val e2 = getFatSchedule(currentScope)(result) // shallow list of deps (exclude stuff only needed by nested blocks)
    //shallow = false

    // shallow is 'must outside + should outside' <--- currently shallow == deep for lambdas, meaning everything 'should outside'
    // bound is 'must inside'

    // find transitive dependencies on bound syms, including their defs (in case of effects)
    val bound = e1.flatMap(z => boundSyms(z.rhs))
    val g1 = getFatDependentStuff(currentScope)(bound)

    // e1 = reachable
    val h1 = e1 filterNot (g1 contains _) // 'may outside'
    val f1 = g1.flatMap { t => syms(t.rhs) } flatMap { s => h1 filter (_.lhs contains s) } // fringe: 1 step from g1

    val e2 = getFatScheduleM(e1)(result, false, true)       // (shallow|hot)*  no cold ref on path

    val e3 = getFatScheduleM(e1)(result, true, false)       // (shallow|cold)* no hot ref on path

    val f2 = f1 filterNot (e3 contains _)                   // fringe restricted to: any* hot any*

    val h2 = getFatScheduleM(e1)(f2.flatMap(_.lhs), false, true)    // anything that depends non-cold on it...

    // things that should live on this level:
    // - not within conditional: no cold ref on path (shallow|hot)*
    // - on the fringe but outside of mustInside, if on a hot path any* hot any*

    val shouldOutside = e1 filter (z => (e2 contains z) || (h2 contains z))

    val levelScope = e1.filter(z => (shouldOutside contains z) && !(g1 contains z)) // shallow (but with the ordering of deep!!) and minus bound

    // stuff needed for 'must inside': this will be hoisted as well!
    //case class Combine(p:List[Exp[Any]]) extends Exp[Any]
    //val g2 = g1.flatMap(z=>syms(z.rhs))//buildScheduleForResult(Combine(g1.map(_.sym)))

    object LocalDef {
      def unapply[A](x: Exp[A]): Option[FatDef] = { // fusion may have rewritten Reify contents so we look at local scope
        currentScope.find(_.lhs contains x).map(_.rhs)
      }
    }    

    // sanity check to make sure all effects are accounted for
    result foreach {
      case LocalDef(ThinDef(Reify(x, u, effects))) =>
        val actual = levelScope.filter(_.lhs exists (effects contains _))
        val isDifferent = effects != actual.flatMap(_.lhs filter (effects contains _))
        val missingSyms = effects filterNot (e => levelScope exists (_.lhs contains e))
        val isMissingSymbols = missingSyms.nonEmpty
        if (isDifferent && !isMissingSymbols) printlog("effect ordering changed: expected " + effects.mkString(",") + " got " + actual.map(_.lhs).mkString(","))
        if (isMissingSymbols || (isDifferent && verbosity >= 2)) { // if no missing symbols, only warn in debug mode          
          val expected = effects.map(d=>fatten(findDefinition(d.asInstanceOf[Sym[Any]]).get))
          val missing = expected filterNot (actual contains _)
          val printfn = if (!isMissingSymbols) printlog _ else printerr _
          printfn("error: violated ordering of effects")
          printfn("  missing symbols: " + missingSyms.mkString(","))
          printfn("  expected:")
          expected.foreach(d => printfn("    "+d))
          printfn("  actual:")
          actual.foreach(d => printfn("    "+d))
          // stuff going missing because of stray dependencies is the most likely cause 
          // so let's print some debugging hints
          printfn("  missing:")
          if (!isMissingSymbols)
            printfn("  note: there is nothing missing so the different order might in fact be ok (artifact of new effect handling? TODO)")
          missing.foreach { d => 
            val inDeep = e1 contains d
            val inShallow = e2 contains d
            val inDep = g1 contains d
            printfn("    "+d+" <-- inDeep: "+inDeep+", inShallow: "+inShallow+", inDep: "+inDep)
            if (inDep) e1 foreach { z =>
              val b = boundSyms(z.rhs)
              if (b.isEmpty) "" else {
                val g2 = getFatDependentStuff(currentScope)(b)
                if (g2 contains d) {
                  printfn("    depends on " + z + " (bound: "+b+")")
                  val path = getFatSchedule(g2)(d)
                  for (p <- path) printfn("      "+p)
                }
              }
            }
          }
        }
      case _ =>
    }
/*
    // sanity check to make sure all effects are accounted for
    result match {
      case Def(Reify(x, u, effects)) =>
        val actual = levelScope.filter(effects contains _.sym)
        assert(effects == actual.map(_.sym), "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)
      case _ =>
    }
*/
    val innerScope2 = e1 diff levelScope // delay everything that remains


    innerScope = innerScope2 flatMap { 
      case TTP(List(sym), ThinDef(rhs)) => List(TP(sym, rhs))
      case e => 
        val z = innerScope.filter(e.lhs contains _.sym)
        if (z.length != e.lhs.length)
          printerr("TROUBLE: couldn't get syms " + e.lhs + ", found only " + z)
        z
    }

    val rval = body(levelScope)

    innerScope = saveInner
    rval
  }


  def focusFatBlock[A](rhs: List[Block[Any]])(body: => A): A = {
    focusBlock(Block(Combine(rhs.map(getBlockResultFull))))(body)
  }

}