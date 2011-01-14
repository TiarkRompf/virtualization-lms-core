package scala.virtualization.lms
package common

import internal.{Expressions, Effects, Traversing, FatExpressions}
import internal.{ScalaCodegen, ScalaNestedCodegen, ScalaFatCodegen, 
  CudaCodegen, CudaNestedCodegen, CudaFatCodegen, 
  CCodegen, CNestedCodegen, CFatCodegen}

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
trait BaseExp extends Base with Expressions with Traversing {
  type Rep[+T] = Exp[T]

  implicit def unit[T:Manifest](x: T) = Const(x)
}

trait EffectExp extends BaseExp with Effects

trait BaseFatExp extends BaseExp with FatExpressions


// TODO: what is the point of these, I suggest to remove them 
// Answer: provide an interface to codegen without depending on internal._

trait ScalaGenBase extends ScalaCodegen

trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase

trait ScalaGenFat extends ScalaFatCodegen with ScalaGenBase




trait CudaGenBase extends CudaCodegen

trait CudaGenEffect extends CudaNestedCodegen with CudaGenBase

trait CudaGenFat extends CudaFatCodegen with CudaGenBase



trait CGenBase extends CCodegen

trait CGenEffect extends CNestedCodegen with CGenBase

trait CGenFat extends CFatCodegen with CGenBase
