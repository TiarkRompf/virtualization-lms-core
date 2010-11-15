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