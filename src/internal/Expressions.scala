package scala.virtualization.lms
package internal

import java.lang.{StackTraceElement,Thread}
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.Queue
import scala.collection.mutable.ListBuffer
import scala.reflect.SourceContext


/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).
 *
 * @since 0.1
 */
trait Expressions extends Utils {

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def tp: Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
    def pos: List[SourceContext] = Nil
  }

  // Exps which will be known by codegen time
  abstract class ConstExp[+T:Manifest] extends Exp[T]

  // Exp which is known at staging time
  case class Const[+T:Manifest](x: T) extends ConstExp[T] {
    // TODO: Equality of Const(1.0) == Const(1) can cause weird type issues in generated code
    // but this check can cause weird issues where, e.g. it appears Const(true) != Const(true)
    /*override def equals(x: Any): Boolean = x match {
      case that@Const(y) =>
        println(s"Comparing $this to $that (${this.tp} to ${that.tp}")
        val eql = this.tp == that.tp && x == y // value and type equality
        println(s"Equal: $eql")
        eql

      case _ => false
    }*/
  }

  var nParams = 0
  case class Param[T:Manifest](private var _x: T) extends ConstExp[T] {
    val id: Int = {nParams += 1; nParams - 1}
    private var fixed = false
    def fix { fixed = true }
    def isFixed = fixed

    def x = _x
    def x_=(v: T) { if (!fixed) _x = v else throw new Exception("Attempted to set fixed param") }
    def setValue(v: T) { if (!fixed) _x = v else throw new Exception("Attempted to set fixed param") }

    override def equals(x: Any): Boolean = x match {
      case that: Param[_] => this.id == that.id // TODO: value equality?
      case _ => false
    }

    var sourceContexts: List[SourceContext] = Nil
    override def pos = sourceContexts
    def withPos(pos: SourceContext) = { sourceContexts ::= pos; this }

    override def toString = "P#" + id + pos.headOption.flatMap{ctx => allContexts(ctx).last.assignedVariable}.map(name => s" ($name)").getOrElse("")
    override def hashCode = id
  }

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    var sourceContexts: List[SourceContext] = Nil
    override def pos = sourceContexts
    def withPos(pos: List[SourceContext]) = { sourceContexts :::= pos; this }
  }

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  var nVars = 0
  def fresh[T:Manifest]: Sym[T] = Sym[T] { nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }

  def fresh[T:Manifest](pos: List[SourceContext]): Sym[T] = fresh[T].withPos(pos)

  def quotePos(e: Exp[Any]): String = e.pos match {
    case Nil => "<unknown>"
    case cs =>
      cs.map(c => allContexts(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
  }
  def quoteTopPos(e: Exp[Any]): String = e.pos match {
    case Nil => "<unknown>"
    case cs => getPathAndLine(cs).map{case (path,line) => path.split("/").last + ":" + line}.mkString(";")
  }

  abstract class Def[+T] { // operations (composite)
    override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
  }

  abstract class Stm // statement (links syms and definitions)

  def infix_lhs(stm: Stm): List[Sym[Any]] = stm match {
    case TP(sym, rhs) => sym::Nil
  }

  def infix_rhs(stm: Stm): Any = stm match { // clients use syms(e.rhs), boundSyms(e.rhs) etc.
    case TP(sym, rhs) => rhs
  }

  def infix_defines[A](stm: Stm, sym: Sym[A]): Option[Def[A]] = stm match {
    case TP(`sym`, rhs: Def[A]) => Some(rhs)
    case _ => None
  }

  def infix_defines[A](stm: Stm, rhs: Def[A]): Option[Sym[A]] = stm match {
    case TP(sym: Sym[A], `rhs`) => Some(sym)
    case _ => None
  }

  case class TP[+T](sym: Sym[T], rhs: Def[T]) extends Stm

  // --- Graph construction state
  var globalDefs: Seq[Stm] = Queue.empty
  var localDefs: Seq[Stm] = Queue.empty
  var globalSymsCache: Map[Sym[Any],Stm] = Map.empty
  var globalDefsCache: Map[Any,Stm] = Map.empty

  /* Create a string representing the IR node definition for the given expression */
  def strDef(e: Exp[Any]): String = e match {
    case Const(z) => z.toString
    case Def(d) => e.toString + " = " + d.toString
    case e: Exp[_] => "(bound " + e.toString + ")"
  }

  /* Create a string representing the original code creating an expression
     if possible or the internal IR node definition otherwise */
  def quoteDef(e: Exp[Any]): String = quoteCode(e.pos).getOrElse(strDef(e))

  /**
   * Remove a symbol from the graph construction state.
   * Symbol should be dead (i.e. after transformer mirroring)
   */
  def scrubSym(sym: Sym[Any]) = {
    def scrubStms(stms: Seq[Stm]) = stms filterNot {
      case TP(lhs,rhs) => (lhs == sym)
      case _ => false
    }
    localDefs = scrubStms(localDefs)
    globalDefs = scrubStms(globalDefs)
    globalDefsCache = globalDefsCache filterNot{case(s,_) => s == sym }
  }

  def reifySubGraph[T](b: =>T): (T, Seq[Stm]) = {
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    val saveGlobalSyms = globalSymsCache
    val saveGlobalDefs = globalDefsCache
    localDefs = Queue.empty
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    globalSymsCache = saveGlobalSyms
    globalDefsCache = saveGlobalDefs
    (r, defs)
  }

  def reflectSubGraph(ds: Seq[Stm]): Unit = {
    val lhs = ds.flatMap(_.lhs)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    // equivalent to: globalDefs filter (_.lhs exists (lhs contains _))
    val existing = lhs flatMap (globalSymsCache get _)
    assert(existing.isEmpty, "already defined: " + existing + " for " + ds)
    for (stm <- ds) {
      localDefs :+= stm
      globalDefs :+= stm
      globalDefsCache += (stm.rhs->stm)
      for (s <- stm.lhs) globalSymsCache += (s->stm)
    }
  }

  def findDefinition[T](s: Sym[T]): Option[Stm] =
    globalSymsCache.get(s)
    //globalDefs.find(x => x.defines(s).nonEmpty)

  def findDefinition[T](d: Def[T]): Option[Stm] =
    globalDefsCache.get(d)
    //globalDefs.find(x => x.defines(d).nonEmpty)

  def findOrCreateDefinition[T:Manifest](d: Def[T], pos: List[SourceContext]): Stm =
    findDefinition[T](d) map { x => x.defines(d).foreach(_.withPos(pos)); x } getOrElse {
      createDefinition(fresh[T](pos), d)
    }

  def findOrCreateDefinitionExp[T:Manifest](d: Def[T], pos: List[SourceContext]): Exp[T] =
    findOrCreateDefinition(d, pos).defines(d).get

  def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    val f = TP(s, d)
    reflectSubGraph(List(f))
    f
  }


  protected implicit def toAtom[T:Manifest](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
    findOrCreateDefinitionExp(d, List(pos)) // TBD: return Const(()) if type is Unit??
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match {
      case s @ Sym(_) =>
        findDefinition(s).flatMap(_.defines(s))
      case _ =>
        None
    }
  }


  // dependencies

  // regular data (and effect) dependencies
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product =>
      // performance hotspot: this is the same as
      // p.productIterator.toList.flatMap(syms(_))
      // but faster
      val iter = p.productIterator
      val out = new ListBuffer[Sym[Any]]
      while (iter.hasNext) {
        val e = iter.next()
        out ++= syms(e)
      }
      out.result
    case _ => Nil
  }

  // symbols which are bound in a definition
  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }

  // symbols which are bound in a definition, but also defined elsewhere
  def tunnelSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(tunnelSyms(_))
    case p: Product => p.productIterator.toList.flatMap(tunnelSyms(_))
    case _ => Nil
  }

  // symbols of effectful components of a definition
  def effectSyms(x: Any): List[Sym[Any]] = x match {
    case ss: Iterable[Any] => ss.toList.flatMap(effectSyms(_))
    case p: Product => p.productIterator.toList.flatMap(effectSyms(_))
    case _ => Nil
  }

  // soft dependencies: they are not required but if they occur,
  // they must be scheduled before
  def softSyms(e: Any): List[Sym[Any]] = e match {
    // empty by default
    //case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(softSyms(_))
    case p: Product => p.productIterator.toList.flatMap(softSyms(_))
    case _ => Nil
  }

  // generic symbol traversal: f is expected to call rsyms again
  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[Any] => f(s)
    case ss: Iterable[Any] => ss.toList.flatMap(f)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }


  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case ss: Iterable[Any] => ss.toList.flatMap(symsFreq(_))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    //case _ => rsyms(e)(symsFreq)
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))


  // bookkeeping

  def reset { // used by delite?
    nVars = 0
    nParams = 0
    globalDefs = Queue.empty
    localDefs = Queue.empty
    globalSymsCache = Map.empty
    globalDefsCache = Map.empty
  }

}
