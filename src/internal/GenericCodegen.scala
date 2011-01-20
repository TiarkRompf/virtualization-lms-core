package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericCodegen extends Scheduling {
  val IR: Expressions
  import IR._

  // TODO: should some of the methods be moved into more specific subclasses?
  
  def kernelFileExt = ""
  def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {}
  def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {}
  
  // Initializer
  def generatorInit(build_dir:String): Unit = {}
  def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {}

  def emitDataStructures(): Unit = {}
  
  // exception handler
  def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }

  // optional type remapping (default is identity)
  def remap[A](m: Manifest[A]) : String = m.toString
  def remapImpl[A](m: Manifest[A]) : String = remap(m)

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

  def getFreeVarBlock(start: Exp[Any], local: List[Sym[Any]]): List[Sym[Any]] = { throw new Exception("Method getFreeVarBlock should be overriden.") }
  def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = syms(rhs) //{ throw new Exception("Method getFreeVarNode should be overriden.") }

  def hasMetaData: Boolean = false
  def getMetaData: String = null

  def getDSLHeaders: String = null
}



trait GenericNestedCodegen extends GenericCodegen {
  val IR: Expressions with Effects
  import IR._

  var shallow = false

//  var outerScope: List[TP[Any]] = Nil
//  var levelScope: List[TP[Any]] = Nil
  var innerScope: List[TP[Any]] = null  // no, it's not a typo

  def initialDefs = super.availableDefs

  override def availableDefs = if (innerScope ne null) innerScope else initialDefs


  def focusBlock[A](result: Exp[Any])(body: => A): A = {
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


  def focusExactScope[A](result: Exp[Any])(body: List[TP[Any]] => A): A = {
    
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
    case Def(Reify(x, _)) => x
    case _ => super.getBlockResult(s)
  }
  

  override def syms(e: Any): List[Sym[Any]] = e match {
//    case Reflect(s, effects) if ignoreEffects => syms(s) // TODO what about ignoreEffects business?
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Read(s) =>
      emitValDef(sym, quote(s))
    case Reflect(s, effects) =>
      emitNode(sym, s)
    case Reify(s, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }


  // bound/used/free variables in current scope, with input vars x (bound!) and result y (used!)
  def boundAndUsedInScope(x: List[Exp[Any]], y: Exp[Any]): (List[Sym[Any]], List[Sym[Any]]) = {
    val used = (syms(y):::innerScope.flatMap(t => syms(t.rhs))).distinct
    val bound = (x.flatMap(syms):::innerScope.flatMap(t => t.sym::boundSyms(t.rhs))).distinct
    (bound, used)
  }
  def freeInScope(x: List[Exp[Any]], y: Exp[Any]): List[Sym[Any]] = {
    val (bound, used) = boundAndUsedInScope(x,y)
    used diff bound
  }

  // TODO: remove
  override def getFreeVarBlock(start: Exp[Any], local: List[Sym[Any]]): List[Sym[Any]] = {
    focusBlock(start) {
      freeInScope(local, start)
    }
  }

  // TODO: remove
  //override def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = { Nil }
  override def getFreeVarNode(rhs: Def[Any]): List[Sym[Any]] = rhs match { // getFreeVarBlock(syms(rhs), boundSyms(rhs))
    case Reflect(s, effects) => getFreeVarNode(s)
    case _ => super.getFreeVarNode(rhs)
  }

  // TODO: necessary? does it actually do the right thing? <-- boundSyms?
  def getEffectsBlock(start: Exp[Any]): List[Sym[Any]] = {
    val save = shallow
    shallow = false
    val stDeep = dep(start)

    // deep list of deps
    val e1 = GraphUtil.stronglyConnectedComponents[TP[Any]](stDeep.flatMap(e => findDefinition(e).toList), { d =>
      dep(d.rhs).flatMap { e =>
        findDefinition(e).toList
      }
    }).flatten.reverse

    // deep on everything except start
    shallow = true
    val stShallow = dep(start)
    shallow = false

    val e2 = GraphUtil.stronglyConnectedComponents[TP[Any]](stShallow.flatMap(e => findDefinition(e).toList), { d =>
      dep(d.rhs).flatMap { e =>
        findDefinition(e).toList
      }
    }).flatten.reverse

    // only the deep dependencies of start
    val e3 = e1 filterNot { e2 contains _ }

    val e4 = e3 flatMap { e =>
      e.sym match {
        case Def(Reflect(x, effects)) => List(e.sym): List[Sym[Any]]
        case _ => Nil
      }
    }

    shallow = save
    e4
  }

  def reset { // used anywhere?
    innerScope = null
    shallow = false
    IR.reset
  }

}