package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericCodegen extends Scheduling {
  val IR: Expressions
  import IR._

  // TODO: should some of the methods be moved into more specific subclasses?
  
  def kernelFileExt = ""
  def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {}
  def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {}
  
  // Initializer
  def initializeGenerator(buildDir:String): Unit = {}
  def finalizeGenerator(): Unit = {}
  def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {}

  def emitDataStructures(path: String): Unit = {}
  
  // exception handler
  def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }

  // optional type remapping (default is identity)
  def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[Any]] ) {
      remap(m.typeArguments.head)
    }
    else m.toString
  }
  def remapImpl[A](m: Manifest[A]) : String = remap(m)
  //def remapVar[A](m: Manifest[Variable[A]]) : String = remap(m.typeArguments.head)

  def getFreeVarBlock(start: Exp[Any], local: List[Sym[Any]]): List[Sym[Any]] = { throw new Exception("Method getFreeVarBlock should be overriden.") }

  def hasMetaData: Boolean = false
  def getMetaData: String = null

  def getDSLHeaders: String = null


  // ----------

  def emitBlock(y: Exp[Any])(implicit stream: PrintWriter): Unit = {
    val deflist = buildScheduleForResult(y)
    
    for (TP(sym, rhs) <- deflist) {
      emitNode(sym, rhs)
    }
  }

  def getBlockResult[A](s: Exp[A]): Exp[A] = s
  
  def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }
  
  def emitExternalLib(rhs: Def[Any]): Unit = {
    throw new GenerationFailedException("don't know how to generate external lib for " + rhs)
  }
  
  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit
    
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit
      
  def quote(x: Exp[Any]) : String = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "null" // why is null getting lifted now? something to do with Equal
    case Const(f: Float) => f.toString + "f"
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case External(s: String, args: List[Exp[Any]]) => s.format(args map (quote(_)) : _*)
    case null => "null"
    case _ => throw new RuntimeException("could not quote " + x)
  }

}



trait GenericNestedCodegen extends GenericCodegen {
  val IR: Expressions with Effects
  import IR._

//  var shallow = false

//  var outerScope: List[TP[Any]] = Nil
//  var levelScope: List[TP[Any]] = Nil
  var innerScope: List[TP[Any]] = null  // no, it's not a typo

  def initialDefs = super.availableDefs

  override def availableDefs = if (innerScope ne null) innerScope else initialDefs


  def focusBlock[A](result: Exp[Any])(body: => A): A = {
    val initDef = initialDefs
    val availDef = availableDefs

//    val saveOuter = outerScope
//    val saveLevel = levelScope
    val saveInner = innerScope

//    outerScope = outerScope ::: levelScope
//    levelScope = Nil
    innerScope = buildScheduleForResult(result) // deep list of deps

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

/* fom delite-develop:
  // a block should only emit a dependency if it truly depends on it (as an input),
  // or if it is an effect that has not been emitted yet by anybody
  var ignoreEffects = false
  var effectScope: List[TP[_]] = Nil // global to all blocks
  var freeVarEffectScope: List[TP[_]] = Nil // global to all blocks

  var scope: List[TP[_]] = Nil
  var nested = 0
  var lastNodeAttempted: TP[_] = _
*/


  def focusExactScope[A](result: Exp[Any])(body: List[TP[Any]] => A): A = {
    
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
        assert(effects == actual.map(_.sym), "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)
      case _ =>
    }

    innerScope = e1 diff levelScope // delay everything that remains

    val rval = body(levelScope)
    
    innerScope = saveInner
    rval
  }


  override def emitBlock(result: Exp[Any])(implicit stream: PrintWriter): Unit = {
    focusBlock(result) {
      emitBlockFocused(result)
    }
  }
  
  def emitBlockFocused(result: Exp[Any])(implicit stream: PrintWriter): Unit = {
    focusExactScope(result) { levelScope =>
      for (TP(sym, rhs) <- levelScope)
        emitNode(sym, rhs)
    }
  }


  override def getBlockResult[A](s: Exp[A]): Exp[A] = s match {
    case Def(Reify(x, _, _)) => x
    case _ => super.getBlockResult(s)
  }
  

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
//    case Read(s) =>
//      emitValDef(sym, quote(s))
    case Reflect(s, u, effects) =>
      emitNode(sym, s)
    case Reify(s, u, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }

	def boundInScope(x: List[Exp[Any]]): List[Sym[Any]] = {
		(x.flatMap(syms):::innerScope.flatMap(t => t.sym::boundSyms(t.rhs))).distinct
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
  override def getFreeVarBlock(start: Exp[Any], local: List[Sym[Any]]): List[Sym[Any]] = {
    focusBlock(start) {
      freeInScope(local, List(start))
    }
  }


  def reset { // used anywhere?
    innerScope = null
    //shallow = false
    IR.reset
  }

}