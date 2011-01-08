package scala.virtualization.lms
package common

import internal.{Expressions, Effects}
import internal.{ScalaCodegen, ScalaNestedCodegen, CudaCodegen, CudaNestedCodegen, CCodegen, CNestedCodegen}

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
trait BaseExp extends Base with Expressions {
  type Rep[+T] = Exp[T]

  implicit def unit[T:Manifest](x: T) = Const(x)
}

trait EffectExp extends BaseExp with Effects {

}


// TODO: what is the point of these, I suggest to remove them 
// Answer: provide an interface to codegen without depending on internal._

trait ScalaGenBase extends ScalaCodegen {
  import IR._

}

trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase {

}


trait CudaGenBase extends CudaCodegen {
  import IR._

}

trait CudaGenEffect extends CudaNestedCodegen with CudaGenBase {

}

trait CGenBase extends CCodegen {
  import IR._

}

trait CGenEffect extends CNestedCodegen with CGenBase {

}