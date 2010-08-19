package scala.virtualization.lms
package common

import java.io.PrintWriter

trait Equal extends Base {
  def __equal(a: Rep[Any], b: Rep[Any]): Rep[Boolean]
}

trait EqualExp extends Equal with BaseExp {
  case class Equal(a: Exp[Any], b: Exp[Any]) extends Def[Boolean]
  def __equal(a: Rep[Any], b: Rep[Any]): Rep[Boolean] = Equal(a,b)
}

trait ScalaGenEqual extends ScalaGenBase with EqualExp {
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Equal(a,b) =>  emitValDef(sym, quote(a) + "==" + quote(b))
    case _ => super.emitNode(sym, rhs)
  }
}
