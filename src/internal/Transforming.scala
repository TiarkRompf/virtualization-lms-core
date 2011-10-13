package scala.virtualization.lms
package internal

import scala.collection.mutable.HashMap

trait Transforming extends Expressions {
  
  abstract class Transformer { // a polymorphic function, basically...
    def apply[A](x: Exp[A]): Exp[A]
    def apply[A](xs: List[Exp[A]]): List[Exp[A]] = xs map (e => apply(e))
    def apply[A](xs: Seq[Exp[A]]): Seq[Exp[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Exp[A]): X=>Exp[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Exp[A]): (X,Y)=>Exp[A] = (z1:X,z2:Y) => apply(f(z1,z2))
    //def apply[A](xs: Summary): Summary = xs //TODO
    def onlySyms[A](xs: List[Sym[A]]): List[Sym[A]] = xs map (e => apply(e)) collect { case e: Sym[A] => e }
  }

	object IdentityTransformer extends Transformer {
		def apply[A](x: Exp[A]) = x
	}

  // FIXME: mirroring for effects!

  def mtype[A,B](m:Manifest[A]): Manifest[B] = m.asInstanceOf[Manifest[B]] // hack: need to pass explicit manifest during mirroring
  
  def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = sys.error("don't know how to mirror " + e)

  def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer): Def[A] = sys.error("don't know how to mirror " + e) //hm...


  class SubstTransformer extends Transformer {
    val subst = new HashMap[Exp[Any], Exp[Any]]
    
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
      case Some(y) if y != x => apply(y.asInstanceOf[Exp[A]]) case _ => x 
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

  //def mirror[A:Manifest](e: FatDef, f: Transformer): Exp[A] = sys.error("don't know how to mirror " + e)  
  
}
