package scala.virtualization.lms
package common

import internal.{Expressions, Effects, ScalaCodegen, ScalaNestedCodegen }

/**
 * The Base trait defines the type Rep, which is the higher-ordered kind that allows for other DSL types to be
 * polymorphically embedded.
 *
 * @since 0.1 
 */
trait Base extends EmbeddedControls {

  type Rep[+T]
  type Var[T]
  
  implicit def unit[T](x: T): Rep[T]
}

/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with Expressions {
  type Var[+T] = Variable[T]
  // TODO: make a design decision here.
  // defining Var[T] as Sym[T] is dangerous. If someone forgets to define a more-specific implicit conversion from
  // Var[T] to Ops, e.g. implicit def varToRepStrOps(s: Var[String]) = new RepStrOpsCls(varToRep(s))
  // then the existing implicit from Rep to Ops will be used, and the ReadVar operation will be lost.
  // Defining Vars as separate from Exps will always cause a compile-time error if the implicit is missing.
  //type Var[T] = Sym[T]
  type Rep[+T] = Exp[T]

  implicit def unit[T](x: T) = Const(x)  
}

trait ScalaGenBase extends ScalaCodegen with BaseExp {

}


trait EffectExp extends BaseExp with Effects {
  
}

trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase with EffectExp {
  
}