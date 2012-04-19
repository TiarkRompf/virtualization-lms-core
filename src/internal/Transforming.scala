package scala.virtualization.lms
package internal

import util.OverloadHack
import scala.collection.mutable.HashMap
import scala.reflect.SourceContext

trait Transforming extends Expressions with OverloadHack { // FIXME: effects only needed for block
  this: scala.virtualization.lms.common.BaseExp => // probably shouldn't be here...
  
  abstract class CanTransform[C[_]] {
    def transform[A](x:C[A], t: Transformer): C[A]
  }
  
  abstract class Transformer { // a polymorphic function, basically...
    def apply[A](x: Exp[A]): Exp[A]
    def apply[A,C[_]:CanTransform](x: C[A]): C[A] = implicitly[CanTransform[C]].transform(x, this)
    def apply[A](x: Interface[A]): Interface[A] = x.ops.wrap(apply[x.ops.Self](x.ops.elem))
    def apply[A](xs: List[Exp[A]]): List[Exp[A]] = xs map (e => apply(e))
    def apply[A](xs: Seq[Exp[A]]): Seq[Exp[A]] = xs map (e => apply(e))
    def apply[X,A](f: X=>Exp[A]): X=>Exp[A] = (z:X) => apply(f(z))
    def apply[X,A](f: X=>Interface[A])(implicit o: Overloaded1): X=>Interface[A] = (z:X) => apply(f(z))
    def apply[X,Y,A](f: (X,Y)=>Exp[A]): (X,Y)=>Exp[A] = (z1:X,z2:Y) => apply(f(z1,z2))
    //def apply[A](xs: Summary): Summary = xs //TODO
    def onlySyms[A](xs: List[Sym[A]]): List[Sym[A]] = xs map (e => apply(e)) collect { case e: Sym[A] => e }
  }

	object IdentityTransformer extends Transformer {
		def apply[A](x: Exp[A]) = x
	}

  // FIXME: mirroring for effects!

  def mtype[A,B](m:Manifest[A]): Manifest[B] = m.asInstanceOf[Manifest[B]] // hack: need to pass explicit manifest during mirroring
  
  def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = sys.error("don't know how to mirror " + e)

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
  this: scala.virtualization.lms.common.BaseExp => // probably shouldn't be here...
  
  //def mirror[A:Manifest](e: FatDef, f: Transformer): Exp[A] = sys.error("don't know how to mirror " + e)  
  
}
