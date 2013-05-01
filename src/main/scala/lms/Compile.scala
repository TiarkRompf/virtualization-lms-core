package scala.lms

import internal.ScalaCompile

trait Compile extends Base {

  def compile[A,B](f: Rep[A] => Rep[B])(implicit mA: TypeRep[A], mB: TypeRep[B]): A=>B

}

trait CompileScala extends Compile with BaseExp with ScalaCompile {

}
