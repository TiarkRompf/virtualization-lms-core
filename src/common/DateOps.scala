package scala.virtualization.lms
package common

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import java.util.Date

/**
 * Lifter Classes for Date
 */
trait DateOps extends Base {

  class DateOpsCls(x: Rep[Date]) {
    def getTime(): Rep[Long] =  dtGetTime(x)
  }

  implicit def date2DateOpsCls(x: Rep[Date]): DateOpsCls = new DateOpsCls(x)
  def dtGetTime(x: Rep[Date]): Rep[Long]
}

trait DateExp extends DateOps with BaseExp {

  case class DtGetTime(x: Exp[Date]) extends Def[Long]
  case class NewDate() extends Def[Date]

  def newDate(): Exp[Date] = NewDate()
  def dtGetTime(x: Exp[Date]): Exp[Long] = DtGetTime(x)
 
  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case NewDate() => newDate()
    case DtGetTime(x) => dtGetTime(f(x))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait DateExpOpt extends DateExp {
  override def dtGetTime(x: Exp[Date]): Exp[Long] = x match {
    case Const(x) => unit(x.getTime)
    case _ => super.dtGetTime(x)
  }
}

trait ScalaGenDate extends ScalaGenBase {
  val IR: DateExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewDate() => emitValDef(sym, "new Date()")
    case DtGetTime(x) => emitValDef(sym, quote(x) + ".getTime()");
    case _ => super.emitNode(sym, rhs)
  }
}