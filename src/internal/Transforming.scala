package scala.virtualization.lms
package internal

import util.OverloadHack
import scala.collection.{immutable,mutable}
import scala.reflect.SourceContext

trait AbstractTransformer {
  val IR: Expressions with Blocks with OverloadHack
  import IR._
  
  def hasContext = false
  def reflectBlock[A](xs: Block[A]): Exp[A] = sys.error("reflectBlock not supported by context-free transformers")
  
  def apply[A](x: Exp[A]): Exp[A]
  def apply[A](xs: Block[A]): Block[A] = Block(apply(xs.res))
  //def apply[A](x: Interface[A]): Interface[A] = x.ops.wrap(apply[x.ops.Self](x.ops.elem)) //FIXME: find another way
  def apply[A](xs: List[Exp[A]]): List[Exp[A]] = xs map (e => apply(e))
  def apply[A](xs: Seq[Exp[A]]): Seq[Exp[A]] = xs map (e => apply(e))
  def apply[X,A](f: X=>Exp[A]): X=>Exp[A] = (z:X) => apply(f(z))
  //def apply[X,A](f: X=>Interface[A])(implicit o: Overloaded1): X=>Interface[A] = (z:X) => apply(f(z))
  def apply[X,Y,A](f: (X,Y)=>Exp[A]): (X,Y)=>Exp[A] = (z1:X,z2:Y) => apply(f(z1,z2))
  //def apply[A](xs: Summary): Summary = xs //TODO
  def onlySyms[A](xs: List[Sym[A]]): List[Sym[A]] = xs map (e => apply(e)) collect { case e: Sym[A] => e }
  
}

trait AbstractSubstTransformer extends AbstractTransformer {
  import IR._
  var subst = immutable.Map.empty[Exp[Any], Exp[Any]]
  
  def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
    case Some(y) if y != x => apply(y.asInstanceOf[Exp[A]]) case _ => x 
  }
}


trait Transforming extends Expressions with Blocks with OverloadHack {
  self: scala.virtualization.lms.common.BaseExp => // probably shouldn't be here...
  
  /*abstract class Transformer extends AbstractTransformer { // a polymorphic function, basically...
    val IR: self.type = self    
  }*/

  type Transformer = AbstractTransformer { val IR: self.type }

  class SubstTransformer extends /*AbstractSubstTransformer*/ AbstractTransformer { val IR: self.type = self 
    val subst = new mutable.HashMap[Exp[Any], Exp[Any]]
    def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
      case Some(y) if y != x => apply(y.asInstanceOf[Exp[A]]) case _ => x 
    }
  }

  /*object IdentityTransformer extends Transformer {
    def apply[A](x: Exp[A]) = x
  }*/

  // FIXME: mirroring for effects!

  def mtype[A,B](m:Manifest[A]): Manifest[B] = m.asInstanceOf[Manifest[B]] // hack: need to pass explicit manifest during mirroring
  
  def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = mirrorDef(e,f)

  def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = sys.error("don't know how to mirror " + e)

  def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = sys.error("don't know how to mirror " + e) //hm...
  
}


trait FatTransforming extends Transforming with FatExpressions {
  this: scala.virtualization.lms.common.BaseExp => // probably shouldn't be here...
  
  //def mirror[A:Manifest](e: FatDef, f: Transformer): Exp[A] = sys.error("don't know how to mirror " + e)  
  
}
