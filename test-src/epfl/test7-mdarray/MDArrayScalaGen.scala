package scala.virtualization.lms
package epfl
package test7

import common._
import java.io.PrintWriter

trait MDArrayScalaGen extends ScalaGenBase with MDArrayBaseExp {
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // TODO: Fill in basic operation generation here
    case _ => throw new Exception("Not implemented!")
  }

}