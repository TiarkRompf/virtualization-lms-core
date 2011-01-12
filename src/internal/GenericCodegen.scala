package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericCodegen extends Scheduling {
  val IR: Expressions
  import IR._

  // TODO: should some of the methods be moved into more specific subclasses?
  
  def kernelFileExt = ""
  def emitKernelHeader(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {}
  def emitKernelFooter(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultType: String, resultIsVar: Boolean)(implicit stream: PrintWriter): Unit = {}
  
  // Initializer
  def generatorInit(build_dir:String): Unit = {}
  def kernelInit(sym: Sym[_], vals: List[Sym[_]], vars: List[Sym[_]], resultIsVar: Boolean): Unit = {}

  def emitDataStructures(): Unit = {}
  
  // exception handler
  def exceptionHandler(outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }

  // optional type remapping (default is identity)
  def remap[A](m: Manifest[A]) : String = m.toString
  def remapImpl[A](m: Manifest[A]) : String = remap(m)

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

  //def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit
  
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit
      
  def quote(x: Exp[_]) : String = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "null" // why is null getting lifted now? something to do with Equal
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case External(s: String, args: List[Exp[Any]]) => s.format(args map (quote(_)) : _*)
    case null => "null"
    case _ => throw new RuntimeException("could not quote " + x)
  }

  def getFreeVarBlock(start: Exp[_], local: List[Sym[_]]): List[Sym[_]] = { throw new Exception("Method getFreeVarBlock should be overriden.") }
  def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = syms(rhs) //{ throw new Exception("Method getFreeVarNode should be overriden.") }

  def hasMetaData: Boolean = false
  def getMetaData: String = null
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


  // bound/used/free variables in current scope, with input vars x (bound!) and result y (used!)
  def boundAndUsedInScope(x: List[Exp[_]], y: Exp[_]): (List[Sym[_]], List[Sym[_]]) = {
    val used = (syms(y):::innerScope.flatMap(t => syms(t.rhs))).distinct
    val bound = (x.flatMap(syms):::innerScope.flatMap(t => t.sym::boundSyms(t.rhs))).distinct
    (bound, used)
  }
  def freeInScope(x: List[Exp[_]], y: Exp[_]): List[Sym[_]] = {
    val (bound, used) = boundAndUsedInScope(x,y)
    used diff bound
  }


  override def getFreeVarBlock(start: Exp[_], local: List[Sym[_]]): List[Sym[_]] = {
    focusBlock(start) {
      freeInScope(local, start)
    }
  }

  //override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = { Nil }
  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match { // getFreeVarBlock(syms(rhs), boundSyms(rhs))
    case Reflect(s, effects) => getFreeVarNode(s)
    case _ => super.getFreeVarNode(rhs)
  }

  def getEffectsBlock(start: Exp[_]): List[Sym[_]] = {
    val save = shallow
    shallow = false
    val stDeep = dep(start)

    // deep list of deps
    val e1 = GraphUtil.stronglyConnectedComponents[TP[_]](stDeep.flatMap(e => findDefinition(e).toList), { d =>
      dep(d.rhs).flatMap { e =>
        findDefinition(e).toList
      }
    }).flatten.reverse

    // deep on everything except start
    shallow = true
    val stShallow = dep(start)
    shallow = false

    val e2 = GraphUtil.stronglyConnectedComponents[TP[_]](stShallow.flatMap(e => findDefinition(e).toList), { d =>
      dep(d.rhs).flatMap { e =>
        findDefinition(e).toList
      }
    }).flatten.reverse

    // only the deep dependencies of start
    val e3 = e1 filterNot { e2 contains _ }

    val e4 = e3 flatMap { e =>
      e.sym match {
        case Def(Reflect(x, effects)) => List(e.sym): List[Sym[_]]
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