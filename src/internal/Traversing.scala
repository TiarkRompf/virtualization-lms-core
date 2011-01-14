package scala.virtualization.lms
package internal

trait Traversing extends Expressions {
  
  abstract class Traverser { // a polymorphic function, basically...
    def apply[A](x: Exp[A]): Exp[A]
  }

  def mirror[A:Manifest](e: Def[A], f: Traverser): Exp[A] = system.error("don't know how to mirror " + e)
  
}