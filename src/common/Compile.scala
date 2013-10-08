package scala.virtualization.lms
package common

import internal.ScalaCompile

trait Compile extends Base {
  
  def compile1[A,B](f: Rep[A] => Rep[B])(implicit mA: Manifest[A], mB: Manifest[B]): A=>B
  // Needed for backwards compatibility of tests after renaming compile to
  // compile1 for a unified naming scheme
  def compile[A,B](f: Rep[A] => Rep[B])(implicit mA: Manifest[A], mB: Manifest[B]) = compile1[A,B](f)
  
}

trait CompileScala extends Compile with BaseExp with ScalaCompile
