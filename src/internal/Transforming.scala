package scala.virtualization.lms
package internal

import scala.collection.mutable.HashMap

trait Transforming extends Traversing {
  
  class SubstTransformer extends Traverser {
    val subst = new HashMap[Exp[Any], Exp[Any]]
    
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
      case Some(y) => apply(y.asInstanceOf[Exp[A]]) case None => x 
    }
    
    def transform[A](s: Sym[A], x: Def[A]): Exp[A] = {
      if (subst.contains(s)) return apply(s)
      implicit val m: Manifest[A] = s.Type.asInstanceOf[Manifest[A]]
      
      val y = mirror(x, this)
      if (s != y) {
/*
        if (y.isInstanceOf[Sym[_]] && findDefinition(y.asInstanceOf[Sym[_]]).nonEmpty)
          println("--> replace " + s+"="+x + " by " + y+"="+findDefinition(y.asInstanceOf[Sym[_]]).get.rhs)
        else
          println("--> replace " + s+"="+x + " by " + y)
*/
        subst(s) = y // TODO: move out of conditional
      }
      y
    }
  }
  
  
  
}