package scala.virtualization.lms
package internal

import scala.reflect.SourceContext
import scala.annotation.unchecked.uncheckedVariance
import java.lang.{StackTraceElement,Thread}

/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends Utils {

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def Type : Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
    def pos: List[SourceContext] = Nil
  }

  // TODO: The fact that we label Consts crashes all the dot tests in the testsuite. In case you need those tests and
  // don't care about typing MDArrays, simply revert this Const labeling :)
  case class Const[+T:Manifest](x: T) extends Exp[T] {
//    var id: Int = {nVars += 1; nVars - 1}
//    override def toString = "Const(" + id + ": " + x.toString+ ")"
//    override def equals(other: Any): Boolean = other match {
//      case that: Const[_] => that.canEqual(this) && this.x == that.x && this.typeManifest == that.typeManifest && this.id == that.id
//      case _ => false
//    }
  }

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    var sourceInfo = Thread.currentThread.getStackTrace // TODO: make use of SourceContext instead
    var sourceContexts: List[SourceContext] = Nil
    override def pos = sourceContexts
    def withPos(pos: List[SourceContext]) = { sourceContexts :::= pos; this }
    var lastRead: Sym[T @uncheckedVariance] = this // TODO
    var version = id
  }

  case class Variable[+T](val e: Exp[Variable[T]]) // TODO: decide whether it should stay here ... FIXME: should be invariant

  var nVars = 0
  def fresh[T:Manifest]: Sym[T] = Sym[T] { nVars += 1; nVars -1 }

  def fresh[T:Manifest](pos: List[SourceContext]): Sym[T] = fresh[T].withPos(pos)

  def quotePos(e: Exp[Any]): String = e.pos match {
    case Nil => "<unknown>"
    case cs => 
      def all(cs: SourceContext): List[SourceContext] = cs.parent match {
        case None => List(cs)
        case Some(p) => cs::all(p)
      }
    cs.map(c => all(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
  }

/*
  def fresh[T:Manifest] = {
    val (name, id, nameId) = nextName("x")
    val sym = Sym[T](id)
    sym.name = name
    sym.nameId = nameId
    sym
  }

  def fresh[T:Manifest](d: Def[T], ctx: Option[SourceContext]) = {
    def enclosingNamedContext(sc: SourceContext): Option[SourceContext] = sc.bindings match {
      case (null, _) :: _ =>
        if (!sc.parent.isEmpty) enclosingNamedContext(sc.parent.get)
        else None
      case (name, line) :: _ =>
        Some(sc)
    }

    // create base name from source context
    val (basename, line, srcCtx) = if (!ctx.isEmpty) {
      enclosingNamedContext(ctx.get) match {
        case None =>
          // no enclosing context has variable assignment
          var outermost = ctx.get
          while (!outermost.parent.isEmpty) {
            outermost = outermost.parent.get
          }
          ("x", 0, Some(outermost))
        case Some(sc) => sc.bindings match {
          case (n, l) :: _ =>
            (n, l, Some(sc))
        }
      }
    } else ("x", 0, None)
    val (name, id, nameId) = nextName(basename)
    val sym = Sym[T](id)
    sym.name = name
    sym.nameId = nameId
    sym.sourceContext = srcCtx
    sym
  }
*/

  abstract class Def[+T] // operations (composite)

  //abstract class Stm // statement (links syms and definitions)
  
  case class TP[+T](sym: Sym[T], rhs: Def[T]) //extends Stm

  var globalDefs: List[TP[Any]] = Nil

  def findDefinition[T](s: Sym[T]): Option[TP[T]] =
    globalDefs.find(_.sym == s).asInstanceOf[Option[TP[T]]]

  def findDefinition[T](d: Def[T]): Option[TP[T]] =
    globalDefs.find(_.rhs == d).asInstanceOf[Option[TP[T]]]

  def findOrCreateDefinition[T:Manifest](d: Def[T], pos: List[SourceContext]): TP[T] =
    findDefinition[T](d) map { f => f.sym.withPos(pos); f } getOrElse {
      createDefinition(fresh[T](pos), d)
    }

  def createDefinition[T](s: Sym[T], d: Def[T]): TP[T] = {
    val f = TP(s, d)
    globalDefs = globalDefs:::List(f)
    f
  }

  protected implicit def toAtom[T:Manifest](d: Def[T])(implicit ctx: SourceContext): Exp[T] = {
    findOrCreateDefinition(d, List(ctx)).sym // TODO: return Const(()) if type is Unit??
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).map(_.rhs)
      case _ =>
        None
    }
  }


  // dependencies

  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))



    case _ => Nil
  }

  def effectSyms(x: Any): List[Sym[Any]] = x match {
    case p: Product => p.productIterator.toList.flatMap(effectSyms(_))
    case _ => Nil
  }



  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[Any] => f(s)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }

  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
//    case _ => rsyms(e)(symsFreq)
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))



/*
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    case _ => Nil
  }
*/

/*
  def symsShare(e: Any): List[(Sym[Any], Int)] = {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(symsShare(_))
    case _ => Nil
  }
*/



  // bookkeeping

  def reset { // used by delite?
    nVars = 0
    globalDefs = Nil
  }

}
