package scala.lms
package internal

trait CodeMotion extends Scheduling {
  val IR: BaseExp
  import IR._
  
  // currentScope must be tight for result and strongly sorted
  // shallow is 'must outside + should outside'
  // currently shallow == deep for lambdas, meaning everything 'should outside'
  // bound is 'must inside'
  // sym->sym->hot->sym->cold->sym  hot --> hoist **** iff the cold is actually inside the loop ****
  // sym->sym->cold->sym->hot->sym  cold here, hot later --> push down, then hoist
  def getExactScope[A](currentScope: List[Stm])(result: List[Exp[Any]]): List[Stm] = {
    // Find transitive dependencies on bound syms, including defs (in case of effects)
    val bound = currentScope.flatMap(z => boundSyms(z.rhs))
    val mustInside = getFatDependents(currentScope)(bound)
    
    val mayOutside = currentScope filterNot (mustInside contains _)
    // syms 1 step away from 'must inside'
    val fringe1 = mustInside.flatMap{t => syms(t.rhs) } flatMap {s => mayOutside filter (_.lhs contains s) }
  
    val hotDeps = getScheduleM(currentScope)(result, false, true)
    val coldDeps = getScheduleM(currentScope)(result, true, false)
    
    // Fringe restricted to any* hot any*
    val fringeHot = fringe1 filterNot (coldDeps contains _)
    
    // Anything that depends non-cold on it
    val h2 = getScheduleM(currentScope)(fringeHot.flatMap(_.lhs), false, true)
 
    // val shouldOutside = currentScope filter (z => (hotDeps contains z) || (h2 contains z)) 
    
    val loopsNotInIfs = hotDeps filterNot (coldDeps contains _)
    val reachFromTopLoops = getSchedule(currentScope)(loopsNotInIfs,false)
    
    val f3 = fringe1 filter (reachFromTopLoops contains _) // fringe restricted to: (shallow|hot)* hot any*
    val h3 = getScheduleM(currentScope)(f3.flatMap(_.lhs), false, true) // anything that depends non-cold on it
    
    val shouldOutside = currentScope filter (z => (hotDeps contains z) || (h3 contains z))
    
    // shallow (but with ordering of deep) minus bound
    val levelScope = currentScope.filter(z => (shouldOutside contains z) && !(mustInside contains z))
  
    object LocalDef {
      // Fusion may have rewritten Reify contents so we look at local scope
      def unapply[A](x: Exp[A]): Option[Stm] = currentScope.find(_.lhs contains x)
    }
    
    // Sanity check to make sure all effects are accounted for
    result foreach {
      case LocalDef(TP(_, Reify(x, u, effects))) => 
        val observable = if (addControlDeps) effects.filterNot(controlDep) else effects
        val actEffects = levelScope.flatMap(_.lhs) filter (observable contains _)
        if (observable.toSet != actEffects.toSet) {
          val actual = levelScope.filter(_.lhs exists (observable contains _))
          val expected = observable.map(d => findStm(d.asInstanceOf[Sym[Any]]).get)
          val missing = expected filterNot (actual contains _)
          val printfn = if (missing.isEmpty) clog _ else cerror _
          printfn(s"Violated ordering of effects in $result = Reify($x, $u, $effects)")
          printfn("(Note: " + strDef(x) + ")")
          printfn("  Expected:")
          expected.foreach(d => printfn(s"    $d"))
          printfn("  Actual:")
          actual.foreach(d => printfn(s"    $d"))
          printfn("  Missing:")
          if (missing.isEmpty)
            printfn("  (None. Ordering may be ok)") // TODO
          missing.foreach{d => 
            val inDeep = currentScope contains d
            val inShallow = hotDeps contains d
            val inDep = mustInside contains d
            printfn(s"    $d <-- inDeep: $inDeep, inShallow: $inShallow, inDep: $inDep")
            if (inDep) currentScope foreach {z => 
              val b = boundSyms(z.rhs)
              if (b.isEmpty) "" else {
                val g2 = getFatDependents(currentScope)(b)
                if (g2 contains d) {
                  printfn(s"    depends on $z (bound: $b)")
                  val path = getSchedule(g2)(d)
                  for (p <- path) printfn(s"    $p")
                }
              }
            } /* End if inDep */
          } /* End missing foreach */
        } /* End if observable != actual */
    }
    
    (levelScope)
  }
}
