package scala.virtualization.lms
package common

import internal.ScalaCompile

trait Compile extends Base {
  
  def compile[A,B](f: Rep[A] => Rep[B], dynamicClass: Class[_] = null)(implicit mA: Manifest[A], mB: Manifest[B]): A=>B
  
}

trait CompileScala extends Compile with BaseExp with ScalaCompile
