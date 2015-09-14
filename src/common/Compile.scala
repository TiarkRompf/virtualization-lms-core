package scala.lms
package common

import internal.ScalaCompile

trait Compile extends Base {
  
  def compile[A,B](f: Rep[A] => Rep[B])(implicit mA: Typ[A], mB: Typ[B]): A=>B
  
}

trait CompileScala extends Compile with BaseExp with ScalaCompile {
  
}
