package scala.virtualization.lms
package common

import internal.{Expressions, Effects, ScalaCodegen, ScalaNestedCodegen }

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