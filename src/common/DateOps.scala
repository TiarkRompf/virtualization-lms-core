package scala.virtualization.lms
package common

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.CNestedCodegen
import scala.reflect.SourceContext
import java.util.Date

/**
 * Lifter Classes for Date
 */
trait DateOps extends Base {

  class DateOpsCls(x: Rep[Date]) {
    def getTime(): Rep[Long] = dtGetTime(x)
	def getYear(): Rep[Long] = dtGetYear(x)
  }

  implicit def date2DateOpsCls(x: Rep[Date]): DateOpsCls = new DateOpsCls(x)
  def newDate(): Rep[Date]
  def newDate(x: Rep[Long]): Rep[Date]
  def dtGetTime(x: Rep[Date]): Rep[Long]
  def dtGetYear(x: Rep[Date]): Rep[Long]
  def getDateAsString(x: Rep[Long]): Rep[String]
}

trait DateExp extends DateOps with BaseExp {

  case class DtGetTime(x: Exp[Date]) extends Def[Long]
  case class DtGetYear(x: Exp[Date]) extends Def[Long]
  case class NewDate(x: Exp[Long]) extends Def[Date]
  case class GetDateAsString(x: Exp[Long]) extends Def[String]

  def newDate(): Exp[Date] = NewDate(null)
  def newDate(x: Rep[Long]): Exp[Date] = NewDate(x)
  def dtGetTime(x: Exp[Date]): Exp[Long] = DtGetTime(x)
  def dtGetYear(x: Exp[Date]): Exp[Long] = DtGetYear(x)
  def getDateAsString(x: Rep[Long]) = GetDateAsString(x)
 
  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case NewDate(x) => newDate(x)
    case DtGetTime(x) => dtGetTime(f(x))
	case DtGetYear(x) => dtGetYear(f(x))
    case GetDateAsString(x) => getDateAsString(f(x))
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
    case NewDate(x) => emitValDef(sym, "new java.util.Date(" + { if (x!=null) quote(x) } + ")")
    case DtGetTime(x) => emitValDef(sym, quote(x) + ".getTime()");
    case DtGetYear(x) => emitValDef(sym, quote(x) + ".getYear() + 1900");
    case GetDateAsString(x) =>  emitValDef(sym, "new java.util.Date(" + { if (x!=null) quote(x) } + ").toString")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenDate extends CGenBase with CNestedCodegen {
  val IR: DateExp
  import IR._

  override def lowerNode[T:Manifest](sym: Sym[T], rhs: Def[T]) = rhs match {
	case NewDate(x) => sym.atPhase(LIRLowering) { LIRLowering(x).asInstanceOf[Exp[T]] }
	case _ => super.lowerNode(sym,rhs)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewDate(x) => stream.println("long " + quote(sym) + " = " + quote(x) + "; // date")
  	case DtGetYear(x) =>
		emitValDef(sym, quote(x) + "/10000")
    case gd@GetDateAsString(x) =>  
		emitValDef(sym, getMemoryAllocString("9", "char"))
		stream.println("snprintf(" + quote(sym) + ", 9, \"%lu\", " + quote(x) + ");")
    case _ => super.emitNode(sym, rhs)
  }
}
