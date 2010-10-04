package scala.virtualization.lms
package common

import internal.{Expressions, Effects, ScalaCodegen, ScalaNestedCodegen }

/**
 * The Base trait defines the type constructor Rep, which is the higher-kinded type that allows for other DSL types to be
 * polymorphically embedded.
 *
 * @since 0.1 
 */
trait Base extends EmbeddedControls {

  type Rep[+T]
  implicit def unit[T](x: T): Rep[T]

}

/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with Expressions {

  type Rep[+T] = Exp[T]
  implicit def unit[T](x: T) = Const(x)
  
}

trait ScalaGenBase extends ScalaCodegen with BaseExp {

}


trait EffectExp extends BaseExp with Effects {
  
}

trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase with EffectExp {
  
}