package scala.virtualization.lms
package internal

trait Expressions {
  
  // expressions
  abstract class Exp[+T]

  case class Const[T](x: T) extends Exp[T]
  case class Sym[T](n: Int) extends Exp[T]

  var nVars = 0
  def fresh[T] = Sym[T] { nVars += 1; nVars -1 }

  // definitions
  abstract class Def[T]

  case class TP[T](sym: Sym[T], rhs: Def[T]) {
    override def toString() = sym + " = " + rhs
  }
  
  var globalDefs: List[TP[_]] = Nil

  def findDefinition[T](s: Sym[T]): Option[TP[T]] = 
    globalDefs.find(_.sym == s).asInstanceOf[Option[TP[T]]]
    
  def findDefinition[T](d: Def[T]): Option[TP[T]] =
    globalDefs.find(_.rhs == d).asInstanceOf[Option[TP[T]]]
  
  def findOrCreateDefinition[T](d: Def[T]): TP[T] =
    findDefinition[T](d).getOrElse {
      createDefinition(fresh[T], d)
    }

  def createDefinition[T](s: Sym[T], d: Def[T]): TP[T] = {
    val f = TP(s, d)
    globalDefs = globalDefs:::List(f)
    f
  }

  implicit def toAtom[T](d: Def[T]): Exp[T] = {
    findOrCreateDefinition(d).sym
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
  
/*
  def syms(d: Product): List[Sym[Any]] = { // TODO: cleanup
    for {
      e <- d.productIterator.toList
      f <- (e match {
        case s: Sym[Any] => List(s)
        case p: Product => syms(p)
        case _ => Nil
      })
    } yield f
  }
*/

  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }
  
  def dep(e: Exp[Any]): List[Sym[Any]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }

}
