/*TODO DISABLED
package scala.virtualization.lms
package epfl
package test4

// unused code -- just a sanity check to make sure it compiles

trait FooA {
  type Rep[T] = Option[T]

  def decons[A](x: Rep[scala.::[A]]): Some[(Rep[A], Rep[List[A]])]

  object Foo {
    def unapply[A](x: Rep[scala.::[A]]): Some[(Rep[A], Rep[List[A]])] = decons[A](x)
  }

  def foo(xs: Rep[List[Int]]): Unit = xs match {
    case Foo(a,b) => foo(b)
    case _ => 
  }
}


trait FooB {

  sealed abstract class Exp[+T]

  case class Sym[T]() extends Exp[T]
  case class Const[T]() extends Exp[T]
  
  def lookup[A](x: Sym[A]): Option[Const[A]]

  object Lookup {
    def unapply[U](x: Sym[U]): Option[Const[U]] = lookup(x)
  }

  object IsSym {
    def unapply[U](x: Exp[U]): Option[Sym[U]] = x match {
//2.10 M%      case s @ Sym() => Some(s)
      case s: Sym[U] => Some(s)
      case _ => None
    }
  }

  def eval[A](term: Exp[A]): Const[A] = term match {
    case IsSym(Lookup(c)) => c
//    case Lookup(c) => c
//2.10 M5    case c @ Const() => c
      case c: Const[A] => c
  }

}*/
