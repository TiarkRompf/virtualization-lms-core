package scala.virtualization.lms
package internal

import scala.reflect.SourceContext
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ListBuffer
import java.lang.{StackTraceElement,Thread}


/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends UtilsExp {

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def tp: Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
    def pos: List[SourceContext] = Nil
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    var sourceContexts: List[SourceContext] = Nil
    override def pos = sourceContexts
    def withPos(pos: List[SourceContext]) = { sourceContexts :::= pos; this }
  }

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  var nVars = 0
  def fresh[T:Manifest]: Sym[T] = Sym[T] { nVars += 1;  if (nVars%1000 == 0) printlog("nVars="+nVars);  nVars -1 }

  def fresh[T:Manifest](pos: List[SourceContext]): Sym[T] = fresh[T].withPos(pos)

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

  // graph construction state
  
  var globalDefs: List[Stm] = Nil
  var localDefs: List[Stm] = Nil
  var globalDefsCache: Map[Sym[Any],Stm] = Map.empty

  def reifySubGraph[T](b: =>T): (T, List[Stm]) = {
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    val saveGlobalCache = globalDefsCache
    localDefs = Nil
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    globalDefsCache = saveGlobalCache
    (r, defs)
  }

  def reflectSubGraph(ds: List[Stm]): Unit = {
    val lhs = ds.flatMap(_.lhs)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    // equivalent to: globalDefs filter (_.lhs exists (lhs contains _))
    val existing = lhs flatMap (globalDefsCache get _)
    assert(existing.isEmpty, "already defined: " + existing + " for " + ds)
    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
    for (stm <- ds; s <- stm.lhs) {      
      globalDefsCache += (s->stm)
    }
  }

  def findDefinition[T](s: Sym[T]): Option[Stm] =
    globalDefsCache.get(s)
    //globalDefs.find(x => x.defines(s).nonEmpty)

  def findDefinition[T](d: Def[T]): Option[Stm] =
    globalDefs.find(x => x.defines(d).nonEmpty)

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

  def mtype[A,B](m:Manifest[A]): Manifest[B] = m.asInstanceOf[Manifest[B]] // hack: need to pass explicit manifest (e.g. during mirroring)
  def mpos(s: List[SourceContext]): SourceContext = if (s.nonEmpty) s.head else implicitly[SourceContext] // hack: got list of pos but need to pass single pos (e.g. to mirror)

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
    globalDefs = Nil
    localDefs = Nil
    globalDefsCache = Map.empty
  }

}
