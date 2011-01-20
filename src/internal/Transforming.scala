package scala.virtualization.lms
package internal

import scala.collection.mutable.HashMap

trait Transforming extends Expressions {
  
  abstract class Transformer { // a polymorphic function, basically...
    def apply[A](x: Exp[A]): Exp[A]
  }

  def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = system.error("don't know how to mirror " + e)



  class SubstTransformer extends Transformer {
    val subst = new HashMap[Exp[Any], Exp[Any]]
    
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
      case Some(y) => apply(y.asInstanceOf[Exp[A]]) case None => x 
    }

/*    
    def transform[A](s: Sym[A], x: Def[A]): Exp[A] = {
      if (subst.contains(s)) return apply(s)
      implicit val m: Manifest[A] = s.Type.asInstanceOf[Manifest[A]]
      
      val y = mirror(x, this)
      if (y != s) {
/*
        if (y.isInstanceOf[Sym[Any]] && findDefinition(y.asInstanceOf[Sym[Any]]).nonEmpty)
          println("--> replace " + s+"="+x + " by " + y+"="+findDefinition(y.asInstanceOf[Sym[Any]]).get.rhs)
        else
          println("--> replace " + s+"="+x + " by " + y)
*/
        subst(s) = y // TODO: move out of conditional
      }
      y
    }
*/    
  }
  
}


trait FatTransforming extends Transforming with FatExpressions {

  //def mirror[A:Manifest](e: FatDef, f: Transformer): Exp[A] = system.error("don't know how to mirror " + e)  
  
}
