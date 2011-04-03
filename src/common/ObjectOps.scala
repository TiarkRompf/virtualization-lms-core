package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenerationFailedException}

trait ObjectOps extends Variables with OverloadHack {
  def infix_toString(lhs: Rep[Any]) = object_tostring(lhs)
  def infix_toStringL(lhs: Rep[Any]) = object_tostring(lhs)

  def object_tostring(lhs: Rep[Any]): Rep[String]
}

trait ObjectOpsExp extends ObjectOps with VariablesExp {
  case class ObjectToString(o: Exp[Any]) extends Def[String]

  def object_tostring(lhs: Exp[Any]) = ObjectToString(lhs)
}

trait ScalaGenObjectOps extends ScalaGenBase {
  val IR: ObjectOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case ObjectToString(lhs) => emitValDef(sym, "(" + quote(lhs) + ").toString()")
    case _ => super.emitNode(sym, rhs)
  }
}