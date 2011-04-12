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
trait Expressions {

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    def Type : Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
  }

  case class Const[+T:Manifest](x: T) extends Exp[T]

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    //var sourceInfo = Thread.currentThread.getStackTrace // until we can get useful info out of the manifest
    var name: String = "x" + (if (id == 0) "" else id)
    var nameId: Int = id
  }

  case class Variable[+T:Manifest](val e: Exp[T]) // TODO: decide whether it should stay here ...

  case class External[A:Manifest](s: String, fmt_args: List[Exp[Any]] = List()) extends Exp[A]

  var nVars = 0
  var idMap = Map[String, Int]() // next id for variable name

  // returns (name, global id, per-name id)
  def nextName(basename: String): (String, Int, Int) = {
    nVars += 1
    val id = nVars - 1
    idMap.get(basename) match {
      case None =>
        idMap += (basename -> 1)
        (basename + id, id, 0)
      case Some(varnum) =>
        idMap += (basename -> (varnum + 1))
        (basename + id, id, varnum)
    }
  }

  def fresh[T:Manifest] = {
    val (name, id, nameId) = nextName("x")
    val sym = Sym[T](id)
    sym.name = name
    sym.nameId = nameId
    sym
  }

  def fresh[T:Manifest](d: Def[T], ctx: Option[SourceContext]) = {
    def enclosingVarName(ctxs: List[List[(String, Int)]]): (String, Int) = ctxs match {
      case first :: rest => (first: @unchecked) match {
        case (null, _) :: _ => enclosingVarName(rest)
        case (name, line) :: _ => (name, line)
      }
      case List() => ("x", 0)
    }

    // create base name from source context of Def
    val (basename, line) = if (!ctx.isEmpty) {
      enclosingVarName(ctx.get.allContexts)
    } else ("x", 0)
    val (name, id, nameId) = nextName(basename)
    val sym = Sym[T](id)
    sym.name = name + (if (line != 0) "_" + line else "")
    sym.nameId = nameId
    sym
  }

  abstract class Def[+T] // operations (composite)

  case class TP[+T](sym: Sym[T], rhs: Def[T]) 

  var globalDefs: List[TP[Any]] = Nil

  def findDefinition[T](s: Sym[T]): Option[TP[T]] =
    globalDefs.find(_.sym == s).asInstanceOf[Option[TP[T]]]

  def findDefinition[T](d: Def[T]): Option[TP[T]] =
    globalDefs.find(_.rhs == d).asInstanceOf[Option[TP[T]]]

  def findOrCreateDefinition[T:Manifest](d: Def[T]): TP[T] =
    findDefinition[T](d).getOrElse {
      createDefinition(fresh[T](d, None), d)
    }

  def findOrCreateDefinition[T:Manifest](d: Def[T], ctx: SourceContext): TP[T] =
    findDefinition[T](d).getOrElse {
      createDefinition(fresh[T](d, Some(ctx)), d)
    }

  def createDefinition[T](s: Sym[T], d: Def[T]): TP[T] = {
    val f = TP(s, d)
    globalDefs = globalDefs:::List(f)
    f
  }

  protected implicit def toAtom[T:Manifest](d: Def[T])(implicit ctx: SourceContext): Exp[T] = {
    findOrCreateDefinition(d, ctx).sym
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).map(_.rhs)
      case _ =>
        None
    }
  }


  def reset { // used anywhere?
    nVars = 0
    globalDefs = Nil
  }

/*
  // dependencies
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }

  def dep(e: Exp[Any]): List[Sym[Any]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }
*/
}
