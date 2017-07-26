package scala.lms
package common

import scala.lms.common._
import scala.lms.internal.CNestedCodegen
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
  def dtGetMonth(x: Rep[Date]): Rep[Long]
  def epochGetYear(x: Rep[Date]): Rep[Long]
  def getDateAsString(x: Rep[Long]): Rep[String]
}

trait DateExp extends DateOps with BaseExp {

  case class DtGetTime(x: Exp[Date]) extends Def[Long]
  case class DtGetYear(x: Exp[Date]) extends Def[Long]
  case class DtGetMonth(x: Exp[Date]) extends Def[Long]
  case class EpochGetYear(x: Exp[Date]) extends Def[Long] {
    val z   = fresh[Int]
    val era = fresh[Int]
    val doe = fresh[Int]
    val yoe = fresh[Int]
    val y   = fresh[Int]
    val doy = fresh[Int]
    val mp  = fresh[Int]
    val d   = fresh[Int]
    val m   = fresh[Int]
  }
  case class NewDate(x: Exp[Long]) extends Def[Date]
  case class GetDateAsString(x: Exp[Long]) extends Def[String]

  def newDate(): Exp[Date] = NewDate(null)
  def newDate(x: Rep[Long]): Exp[Date] = NewDate(x)
  def dtGetTime(x: Exp[Date]): Exp[Long] = DtGetTime(x)
  def dtGetYear(x: Exp[Date]): Exp[Long] = DtGetYear(x)
  def dtGetMonth(x: Exp[Date]): Exp[Long] = DtGetMonth(x)
  def epochGetYear(x: Exp[Date]): Exp[Long] = EpochGetYear(x)
  def getDateAsString(x: Rep[Long]) = GetDateAsString(x)

  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case NewDate(x) => newDate(x)
    case DtGetTime(x) => dtGetTime(f(x))
    case DtGetYear(x) => dtGetYear(f(x))
    case DtGetMonth(x) => dtGetMonth(f(x))
    case EpochGetYear(x) => epochGetYear(f(x))
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
    case DtGetYear(x) => emitValDef(sym, src"$x / 10000L")
    case DtGetMonth(x) => emitValDef(sym, src"($x / 100L) % 100L")
    case dgy@EpochGetYear(x) =>
      emitValDef(dgy.z, src"$x + 719468")
      emitValDef(dgy.era, src"(${dgy.z} >= 0 ? ${dgy.z} : ${dgy.z} - 146096) / 146097")
      stream.println(src"unsigned int ${dgy.doe} = (unsigned)(${dgy.z} - ${dgy.era} * 146097);")
      stream.println(src"unsigned int ${dgy.yoe} = (${dgy.doe} - ${dgy.doe} / 1460 + ${dgy.doe} / 36524 - ${dgy.doe} / 146096) / 365;")
      emitValDef(dgy.y, src"(int)(${dgy.yoe}) + ${dgy.era} * 400")
      stream.println(src"unsigned int ${dgy.doy} = ${dgy.doe} - (365 * ${dgy.yoe} + ${dgy.yoe} / 4 - ${dgy.yoe} / 100);")
      stream.println(src"unsigned int ${dgy.mp} = (5 * ${dgy.doy} + 2) / 153;")
      stream.println(src"unsigned int ${dgy.d} = ${dgy.doy} - (153 * ${dgy.mp} + 2) / 5 + 1;")
      stream.println(src"unsigned int ${dgy.m} = ${dgy.mp} + (${dgy.mp} < 10 ? 3 : -9);")

      emitValDef(sym, src"${dgy.y} + (${dgy.m} <= 2)")
    case gd@GetDateAsString(x) =>
		emitValDef(sym, getMemoryAllocString("9", "char"))
		stream.println("snprintf(" + quote(sym) + ", 9, \"%lu\", " + quote(x) + ");")
    case _ => super.emitNode(sym, rhs)
  }
}
