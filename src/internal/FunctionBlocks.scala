package scala.lms
package internal

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.lms.util.OverloadHack

/*
 * This trait allows functions to be stored as case class parameters in a way that works correctly
 * with transformers. Pre-transformation, they are converted from lambdas to BlockN classes,
 * and then post transformation they are converted from BlockN classes back to lambdas (with the
 * correctly transformed free and bound vars).
 */
trait FunctionBlocksExp extends BaseExp with OverloadHack {
  /*
   * BlockN definitions
   */
  implicit def lambdaToBlock0[R:Typ](f: () => Exp[R]) = Block0(reifyEffects(f()))
  case class Block0[R:Typ](blockRes: Block[R])
     
  implicit def lambdaToBlock1[T1:Typ,R:Typ](f: Exp[T1] => Exp[R]) = {
    val x1 = bound[T1]
    Block1(x1,reifyEffects(f(x1)))
  }    
  case class Block1[T1:Typ,R:Typ](blockArg1: Sym[T1], blockRes: Block[R]) 
  
  implicit def lambdaToBlock2[T1:Typ,T2:Typ,R:Typ](f: (Exp[T1],Exp[T2]) => Exp[R]) = {
    val (x1,x2) = (bound[T1],bound[T2])
    Block2(x1,x2,reifyEffects(f(x1,x2)))
  }
  case class Block2[T1:Typ,T2:Typ,R:Typ](blockArg1: Sym[T1], blockArg2: Sym[T2], blockRes: Block[R]) 
 
 
  /*
   * boundSyms override required for all BlockNs
   */
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case Block0(blk) => effectSyms(blk)
    case Block1(b1,blk) => scala.List(b1) ::: effectSyms(blk)
    case Block2(b1,b2,blk) => scala.List(b1,b2) ::: effectSyms(blk)
    case _ => super.boundSyms(e)
  }
 
  
  /*
   * Enable transformation of BlockN types back to lambdas
   */
   
   def transformBlockWithBound[A](t: SubstitutionTransformer{val IR: FunctionBlocksExp.this.type}, f: Block[A], boundVars: List[(Exp[Any],Exp[Any])]) = {
     t.withSubstScope(boundVars: _*) {
       t.reflectBlock(f)
     }
   }
   
  implicit def transformerToBlockTransformer(t: SubstitutionTransformer{val IR: FunctionBlocksExp.this.type}) = new {
    def apply[R](x: Block0[R]): (() => Exp[R]) =  { () => t.reflectBlock(x.blockRes) }
    def apply[T1,R](x: Block1[T1,R]): Exp[T1] => Exp[R] = { a => transformBlockWithBound(t, x.blockRes, List(x.blockArg1 -> a)) }
    def apply[T1,T2,R](x: Block2[T1,T2,R]): (Exp[T1],Exp[T2]) => Exp[R] = { (a,b) => transformBlockWithBound(t, x.blockRes, List(x.blockArg1 -> a, x.blockArg2 -> b)) }
  }  
  
  /*
   * For mirroring of BlockN types without conversion to lambdas
   */
   def copyBlock0[R:Typ](b: Block0[R], t: Transformer) = Block0(t(b.blockRes))
   def copyBlock1[T1:Typ,R:Typ](b: Block1[T1,R], t: Transformer) = Block1(t(b.blockArg1).asInstanceOf[Sym[T1]], t(b.blockRes))
   def copyBlock2[T1:Typ,T2:Typ,R:Typ](b: Block2[T1,T2,R], t: Transformer) = Block2(t(b.blockArg1).asInstanceOf[Sym[T1]], t(b.blockArg2).asInstanceOf[Sym[T2]], t(b.blockRes))  
}
  
