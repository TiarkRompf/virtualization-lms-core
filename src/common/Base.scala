package scala.virtualization.lms
package common

import internal.{Expressions, Effects, ScalaCodegen, ScalaNestedCodegen }

/**
 * The Base trait defines the type Rep, which is the higher-ordered kind that allows for other DSL types to be
 * polymorphically embedded.
 *
 *  
 */
trait Base extends EmbeddedControls {

  type Rep[+T]

}

trait BaseExp extends Base with Expressions {

  type Rep[+T] = Exp[T]
  
}

trait ScalaGenBase extends ScalaCodegen with BaseExp {

}



trait EffectExp extends BaseExp with Effects {
  
}

trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase with EffectExp {
  
}