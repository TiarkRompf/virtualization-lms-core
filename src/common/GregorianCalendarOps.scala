package scala.lms
package common

import scala.lms.common._
import scala.reflect.SourceContext
import java.util.Date
import java.util.GregorianCalendar

/**
 * Lifter Classes for GregorianCalendar
 */
trait GregorianCalendarOps extends Base {

  class GregorianCalendarOpsCls(x: Rep[GregorianCalendar]) {
    def getTime(): Rep[Date] =  gcGetTime(x)
  }

  implicit def date2GregorianCalendarOpsCls(x: Rep[GregorianCalendar]): GregorianCalendarOpsCls = new GregorianCalendarOpsCls(x)
  def newGregorianCalendar(y: Rep[Int], m: Rep[Int], d: Rep[Int]): Rep[GregorianCalendar]
  def gcGetTime(x: Rep[GregorianCalendar]): Rep[Date]
}

trait GregorianCalendarExp extends GregorianCalendarOps with BaseExp {

  case class NewGregorianCalendar(y: Exp[Int], m: Exp[Int], d: Exp[Int]) extends Def[GregorianCalendar]
  case class GcGetTime(x: Exp[GregorianCalendar]) extends Def[Date]

  def newGregorianCalendar(y: Exp[Int], m: Exp[Int], d: Exp[Int]): Exp[GregorianCalendar] = NewGregorianCalendar(y, m, d)
  def gcGetTime(x: Exp[GregorianCalendar]): Exp[Date] = GcGetTime(x)
 
  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case NewGregorianCalendar(y,m,d) => newGregorianCalendar(f(y),f(m),f(d))
    case GcGetTime(x) => gcGetTime(f(x))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait GregorianCalendarExpOpt extends GregorianCalendarExp {
  override def gcGetTime(x: Exp[GregorianCalendar]): Exp[Date] = x match {
    case Const(x) => unit(x.getTime)
    case _ => super.gcGetTime(x)
  }
}

trait ScalaGenGregorianCalendar extends ScalaGenBase {
  val IR: GregorianCalendarExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewGregorianCalendar(y, m, d) => emitValDef(sym, "new GregorianCalendar(" + quote(y) + ", " + quote(m) + ", " + quote(d) + ")")
    case GcGetTime(x) => emitValDef(sym, quote(x) + ".getTime()");
    case _ => super.emitNode(sym, rhs)
  }
}