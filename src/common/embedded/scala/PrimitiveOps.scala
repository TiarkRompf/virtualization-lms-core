package scala.virtualization.lms
package common.embedded.scala

import java.io.PrintWriter
import scala.virtualization.lms.common.{BaseExp, Base}
import scala.virtualization.lms.internal.ScalaGenBase

trait PrimitiveOps extends Base {
  object Double {
    def parseDouble(s: Rep[String]) = obj_double_parse_double(s)
  }

  def obj_double_parse_double(s: Rep[String]) : Rep[Double]
}

trait PrimitiveOpsExp extends PrimitiveOps with BaseExp
{
  case class ObjDoubleParseDouble(s: Exp[String]) extends Def[Double]

  def obj_double_parse_double(s: Exp[String]) = ObjDoubleParseDouble(s)
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, "java.lang.Double.parseDouble(" + quote(s) + ")")
    case _ => super.emitNode(sym, rhs)    
  }
}
