package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}


trait CodeMotion extends Scheduling {
  val IR: Expressions with Effects /* effects just for sanity check */
  import IR._

  def getExactScope[A](currentScope: List[Stm])(result: List[Exp[Any]]): List[Stm] = {
    // currentScope must be tight for result and strongly sorted
    val e1 = currentScope

    // shallow is 'must outside + should outside' <--- currently shallow == deep for lambdas, meaning everything 'should outside'
    // bound is 'must inside'

    // find transitive dependencies on bound syms, including their defs (in case of effects)
    val bound = e1.flatMap(z => boundSyms(z.rhs))
    val g1 = getFatDependentStuff(currentScope)(bound)

    // e1 = reachable
    val h1 = e1 filterNot (g1 contains _) // 'may outside'
    val f1 = g1.flatMap { t => syms(t.rhs) } flatMap { s => h1 filter (_.lhs contains s) } // fringe: 1 step from g1

    val e2 = getScheduleM(e1)(result, false, true)       // (shallow|hot)*  no cold ref on path

    val e3 = getScheduleM(e1)(result, true, false)       // (shallow|cold)* no hot ref on path

    val f2 = f1 filterNot (e3 contains _)                   // fringe restricted to: any* hot any*

    val h2 = getScheduleM(e1)(f2.flatMap(_.lhs), false, true)    // anything that depends non-cold on it...

    // things that should live on this level:
    // - not within conditional: no cold ref on path (shallow|hot)*
    // - on the fringe but outside of mustInside, if on a hot path any* hot any*
    
    // TODO: use (shallow|hot)* hot any* instead
    
    // ---- begin FIXME ----
    
    // AKS: temporarily reverted to old code here to patch a bug in OptiML's MiniMSMBuilder application
    // then/else branches were being unsafely hoisted out of a conditional 
    //val shouldOutside = e1 filter (z => (e2 contains z) || (h2 contains z))

    //* 
    //TODO: uncomment after resolving the issue above
    
    val loopsNotInIfs = e2 filterNot (e3 contains _)    // (shallow|hot)* hot (shallow|hot)*   <---- a hot ref on all paths!
    val reachFromTopLoops = getSchedule(e1)(loopsNotInIfs,false)

    val f3 = f1 filter (reachFromTopLoops contains _)    // fringe restricted to: (shallow|hot)* hot any*
    val h3 = getScheduleM(e1)(f3.flatMap(_.lhs), false, true)    // anything that depends non-cold on it...
    
    val shouldOutside = e1 filter (z => (e2 contains z) || (h3 contains z))
    
    //val shouldOutside = e1 filter (z => (e2 contains z) || (h2 contains z))
    //*/

    val levelScope = e1.filter(z => (shouldOutside contains z) && !(g1 contains z)) // shallow (but with the ordering of deep!!) and minus bound
    
    // ---- end FIXME ----

    // sym->sym->hot->sym->cold->sym  hot --> hoist **** iff the cold is actually inside the loop ****
    // sym->sym->cold->sym->hot->sym  cold here, hot later --> push down, then hoist

/*

    loop { i =>                z = *if (x) bla
      if (i > 0)               loop { i =>
        *if (x)                  if (i > 0)
          bla                      z
    }                          }

    loop { i =>                z = *bla
      if (x)                   loop { i =>
        if (i > 0)               if (x)
          *bla                     if (i > 0)
    }                                z
                               }
*/

    
    // TODO: recursion!!!  identify recursive up-pointers
    
    
    
    
    
    

    object LocalDef {
      def unapply[A](x: Exp[A]): Option[Stm] = { // fusion may have rewritten Reify contents so we look at local scope
        currentScope.find(_.lhs contains x)
      }
    }    

    // sanity check to make sure all effects are accounted for
    result foreach {
      case LocalDef(TP(_, Reify(x, u, effects))) =>        
        val observable = if (addControlDeps) effects.filterNot(controlDep) else effects
        val acteffects = levelScope.flatMap(_.lhs) filter (observable contains _)
        if (observable.toSet != acteffects.toSet) {
          val actual = levelScope.filter(_.lhs exists (observable contains _))
          val expected = observable.map(d=>/*fatten*/(findDefinition(d.asInstanceOf[Sym[Any]]).get)) 
          val missing = expected filterNot (actual contains _)
          val printfn = if (missing.isEmpty) printlog _ else printerr _
          printfn("error: violated ordering of effects in " + result + " = Reify(" + x + ", " + u + ", " + effects + ")")
          printfn("(Note: " + strDef(x) + ")")
          printfn("  expected:")
          expected.foreach(d => printfn("    "+d))
          printfn("  actual:")
          actual.foreach(d => printfn("    "+d))
          // stuff going missing because of stray dependencies is the most likely cause 
          // so let's print some debugging hints
          printfn("  missing:")
          if (missing.isEmpty)
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
                  val path = getSchedule(g2)(d)
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


    levelScope
  }
}