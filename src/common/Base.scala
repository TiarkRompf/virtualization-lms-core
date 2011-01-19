package scala.virtualization.lms
package common

import internal.{Expressions, Effects, Transforming, FatExpressions, FatTransforming}
import internal.{ScalaCodegen, ScalaNestedCodegen, ScalaFatCodegen, 
  CudaCodegen, CudaNestedCodegen, CudaFatCodegen, 
  CCodegen, CNestedCodegen, CFatCodegen,
  CLikeCodegen}

/**
 * The Base trait defines the type constructor Rep, which is the higher-kinded type that allows for other DSL types to be
 * polymorphically embedded.
 *
 * @since 0.1 
 */
trait Base extends EmbeddedControls {

  type Rep[+T]

  implicit def unit[T:Manifest](x: T): Rep[T]
}

/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with Expressions with Transforming {
  type Rep[+T] = Exp[T]

  implicit def unit[T:Manifest](x: T) = Const(x)
}

trait EffectExp extends BaseExp with Effects {

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = e match {
//    case Reflect(Print(x), es) => Reflect(Print(f(x)), es map (e => f(e)))
    case Reify(x, es) => Reify(f(x), es map (e => f(e)))
    case _ => super.mirror(e,f)
  }
    
}

trait BaseFatExp extends BaseExp with FatExpressions with FatTransforming


// TODO: what is the point of these, I suggest to remove them 
// Answer: provide an interface to codegen without depending on internal._

trait ScalaGenBase extends ScalaCodegen

trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase

trait ScalaGenFat extends ScalaFatCodegen with ScalaGenBase


trait CLikeGenBase extends CLikeCodegen


trait CudaGenBase extends CudaCodegen

trait CudaGenEffect extends CudaNestedCodegen with CudaGenBase

trait CudaGenFat extends CudaFatCodegen with CudaGenBase



trait CGenBase extends CCodegen

trait CGenEffect extends CNestedCodegen with CGenBase

trait CGenFat extends CFatCodegen with CGenBase
