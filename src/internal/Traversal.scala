package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait Traversal extends Scheduling {
  val IR: Expressions
  import IR._

  type Block[+T]
  
  def reifyBlock[T: Manifest](x: => Exp[T]): Block[T]

  def compactize(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = { throw new Exception("Method compactize should be overriden.") }

  def getFreeVarBlock(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = Nil

  def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = Nil // TODO: Nil or Exception??

  def getBlockResult[A](s: Block[A]): Exp[A] = getBlockResultFull(s) // = s.res
  def getBlockResultFull[A](s: Block[A]): Exp[A] // = s.res
  
  def traverseBlock[A](block: Block[A]): Unit
  def traverseStm(stm: Stm): Unit
  
  def reset: Unit = ()
}


/*
trait CustomTransformer extends NestedTraversal {
  val IR: Expressions with Effects
  import IR._
  
  def traverseBlock[A](block: Block[A]): Unit = {
    focusBlock(block) {
      traverseBlockFocused(block)
    }
  }

  def traverseBlockFocused[A](block: Block[A]): Unit = {
    focusExactScope(block) { levelScope =>
      levelScope foreach traverseStm
    }
  }
  
  def traverseStm(stm: Stm): Unit = {
    blocks(stm.rhs) foreach traverseBlock
  }

}
*/




trait NestedTraversal extends Traversal {
  val IR: Expressions with Effects
  import IR._

  // ----- block definition

  type Block[+T] = IR.Block[T]
  def reifyBlock[T: Manifest](x: => Exp[T]): Block[T] = IR.reifyEffects(x)

  override def getBlockResultFull[A](s: Block[A]): Exp[A] = s.res
  
  override def getBlockResult[A](s: Block[A]): Exp[A] = s match {
    case Block(Def(Reify(x, _, _))) => x
    case Block(x) => x
  }

  
  // ----- stateful focus management
  
  
//  var shallow = false

//  var outerScope: List[TP[Any]] = Nil
//  var levelScope: List[TP[Any]] = Nil
  var innerScope: List[Stm] = null  // no, it's not a typo

  def initialDefs = super.availableDefs

  override def availableDefs = if (innerScope ne null) innerScope else initialDefs


  def focusBlock[A](result: Block[Any])(body: => A): A = 
    focusFatBlock[A](List(result))(body)
    
  def focusFatBlock[A](result: List[Block[Any]])(body: => A): A = {
    val initDef = initialDefs
    val availDef = availableDefs

//    val saveOuter = outerScope
//    val saveLevel = levelScope
    val saveInner = innerScope

//    outerScope = outerScope ::: levelScope
//    levelScope = Nil
    innerScope = getSchedule(availDef)(result.map(getBlockResultFull), false) // deep list of deps (unsorted, possibly recursive)

    var rval = null.asInstanceOf[A]
    try {
      rval = body
    }
    catch {
      case e => throw e
    }
    finally {
      innerScope = saveInner
    }

//    outerScope = saveOuter
//    levelScope = saveLevel
//    innerScope = saveInner

    rval
  }



  def focusExactScope[A](resultB: Block[Any])(body: List[Stm] => A): A = 
    focusExactScopeFat[A](availableDefs)(List(resultB))(body)
  
  def focusExactScopeFat[A](currentScope: List[Stm])(resultB: List[Block[Any]])(body: List[Stm] => A): A = {
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

    val e2 = getScheduleM(e1)(result, false, true)       // (shallow|hot)*  no cold ref on path

    val e3 = getScheduleM(e1)(result, true, false)       // (shallow|cold)* no hot ref on path

    val f2 = f1 filterNot (e3 contains _)                   // fringe restricted to: any* hot any*

    val h2 = getScheduleM(e1)(f2.flatMap(_.lhs), false, true)    // anything that depends non-cold on it...

    // things that should live on this level:
    // - not within conditional: no cold ref on path (shallow|hot)*
    // - on the fringe but outside of mustInside, if on a hot path any* hot any*

    val shouldOutside = e1 filter (z => (e2 contains z) || (h2 contains z))

    val levelScope = e1.filter(z => (shouldOutside contains z) && !(g1 contains z)) // shallow (but with the ordering of deep!!) and minus bound

    // stuff needed for 'must inside': this will be hoisted as well!
    //case class Combine(p:List[Exp[Any]]) extends Exp[Any]
    //val g2 = g1.flatMap(z=>syms(z.rhs))//buildScheduleForResult(Combine(g1.map(_.sym)))

    object LocalDef {
      def unapply[A](x: Exp[A]): Option[Stm] = { // fusion may have rewritten Reify contents so we look at local scope
        currentScope.find(_.lhs contains x)
      }
    }    

    // sanity check to make sure all effects are accounted for
    result foreach {
      case LocalDef(TP(_, Reify(x, u, effects))) =>
        val actual = levelScope.filter(_.lhs exists (effects contains _))
        if (effects != actual.flatMap(_.lhs filter (effects contains _))) {
          val expected = effects.map(d=>/*fatten*/(findDefinition(d.asInstanceOf[Sym[Any]]).get))
          val missing = expected filterNot (actual contains _)
          val printfn = if (missing.isEmpty) printlog _ else printerr _
          printfn("error: violated ordering of effects")
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
    
    innerScope = e1 diff levelScope // delay everything that remains

    val rval = body(levelScope)

    innerScope = saveInner
    rval
  }


/*
  def focusExactScope[A](resultB: Block[Any])(body: List[TP[Any]] => A): A = {
    val result = getBlockResultFull(resultB)

    val saveInner = innerScope

    val e1 = availableDefs
    //shallow = true
    //val e2 = buildScheduleForResult(result) // shallow list of deps (exclude stuff only needed by nested blocks)
    //shallow = false

    // shallow is 'must outside + should outside' <--- currently shallow == deep for lambdas, meaning everything 'should outside'
    // bound is 'must inside'

    // find transitive dependencies on bound syms, including their defs (in case of effects)
    val bound = for (TP(sym, rhs) <- e1; s <- boundSyms(rhs)) yield s
    val g1 = getDependentStuff(bound) // 'must inside'

    // e1 = reachable
    val h1 = e1 filterNot (g1 contains _) // 'may outside'
    val f1 = g1.flatMap { t => syms(t.rhs) } flatMap { s => h1 filter (_.sym == s) } // fringe: 1 step from g1

    val e2 = buildScheduleForResultM(e1)(result, false, true)       // (shallow|hot)*  no cold ref on path

    val e3 = buildScheduleForResultM(e1)(result, true, false)       // (shallow|cold)* no hot ref on path

    val f2 = f1 filterNot (e3 contains _)                           // fringe restricted to: any* hot any*

    val h2 = buildScheduleForResultM(e1)(f2.map(_.sym), false, true)       // anything that depends non-cold on it...

    // things that should live on this level:
    // - not within conditional: no cold ref on path (shallow|hot)*
    // - on the fringe but outside of mustInside, if on a hot path any* hot any*

    val shouldOutside = e1 filter (z => (e2 contains z) || (h2 contains z))

    if (verbosity > 2) {
      println("--- e1")
      e1.foreach(println)
      println("--- e2 (non-cold)")
      e2.foreach(println)
      println("--- g1 (bound)")
      g1.foreach(println)
      println("--- fringe")
      f1.foreach(println)
      println("--- h2 (fringe; any* hot any*; and non-cold inputs)")
      h2.foreach(println)
    }

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

    val levelScope = e1.filter(z => (shouldOutside contains z) && !(g1 contains z)) // shallow (but with the ordering of deep!!) and minus bound

    // sanity check to make sure all effects are accounted for
    result match {
      case Def(Reify(x, u, effects)) =>
        val actual = levelScope.filter(effects contains _.sym)
        if (effects != actual.map(_.sym))
          printerr("violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)
      case _ =>
    }

    innerScope = e1 diff levelScope // delay everything that remains

    val rval = body(levelScope)

    innerScope = saveInner
    rval
  }
*/    
  
  
  // ---- bound and free vars

	def boundInScope(x: List[Exp[Any]]): List[Sym[Any]] = {
		(x.flatMap(syms):::innerScope.flatMap(t => t.lhs:::boundSyms(t.rhs))).distinct
	}
	
	def usedInScope(y: List[Exp[Any]]): List[Sym[Any]] = {
		(y.flatMap(syms):::innerScope.flatMap(t => syms(t.rhs))).distinct
	}
	
	def readInScope(y: List[Exp[Any]]): List[Sym[Any]] = {
		(y.flatMap(syms):::innerScope.flatMap(t => readSyms(t.rhs))).distinct
	}
	
  // bound/used/free variables in current scope, with input vars x (bound!) and result y (used!)
  def boundAndUsedInScope(x: List[Exp[Any]], y: List[Exp[Any]]): (List[Sym[Any]], List[Sym[Any]]) = {
    (boundInScope(x), usedInScope(y))
  }

  def freeInScope(x: List[Exp[Any]], y: List[Exp[Any]]): List[Sym[Any]] = {
    val (bound, used) = boundAndUsedInScope(x,y)
    // aks: freeInScope used to collect effects that are not true input dependencies. TR, any better solution?
    // i would expect read to be a subset of used, but there are cases where read has symbols not in used (TODO: investigate)
    val read = readInScope(y)
    (used intersect read) diff bound
  }

  // TODO: remove
  override def getFreeVarBlock(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = {
    focusBlock(start) {
      freeInScope(local, List(getBlockResultFull(start)))
    }
  }

  override def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = Nil // FIXME: should have generic impl



  // ----- high level api

  def traverseBlock[A](block: Block[A]): Unit = {
    focusBlock(block) {
      traverseBlockFocused(block)
    }
  }

  def traverseBlockFocused[A](block: Block[A]): Unit = {
    focusExactScope(block) { levelScope =>
      levelScope foreach traverseStm
    }
  }

  def traverseStm(stm: Stm): Unit = { // override this to implement custom traversal
    blocks(stm.rhs) foreach traverseBlock
  }

  // ----- reset

  override def reset { // used anywhere?
    innerScope = null
    //shallow = false
    IR.reset
    super.reset
  }
}

