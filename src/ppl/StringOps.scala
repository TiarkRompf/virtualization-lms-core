package scala.virtualization.lms
package common

import scala.virtualization.lms.util.OverloadHack
import java.io.PrintWriter

trait StringOps extends Base with Variables with OverloadHack {
  implicit def varToRepStrOps(s: Var[String]) : RepStrOpsCls
  implicit def repStrToRepStrOps(s: Rep[String]) = new RepStrOpsCls(s)
  implicit def strToRepStrOps(s: String) = new RepStrOpsCls(s)

  // NOTE: with infix methods, if something doesn't get lifted, this won't give you a compile time error,
  //       since string concat is defined on all objects
  def __ext__+(s1: String, s2: Rep[Any]) = string_plus(s1,s2)
  def __ext__+(s1: Rep[Any], s2: String)(implicit o: Overloaded1) = string_plus(s1,s2)
  def __ext__+(s1: String, s2: Var[Any])(implicit o: Overloaded4) = string_plus(s1,varToRep(s2))
  def __ext__+(s1: Var[Any], s2: String)(implicit o: Overloaded5) = string_plus(varToRep(s1),s2)

  class RepStrOpsCls(s: Rep[String]) {
    def trim() = string_trim(s);
    def split(separators: String) = string_split(s, separators);
  }

  def string_plus(s: Rep[Any], o: Rep[Any]): Rep[String]
  def string_trim(s: Rep[String]) : Rep[String]
  def string_split(s: Rep[String], separators: Rep[String]) : Rep[Array[String]]
}

trait StringOpsExp extends StringOps with BaseExp with VariablesExp {
  implicit def varToRepStrOps(s: Var[String]) = new RepStrOpsCls(varToRep(s))

  case class StringPlus(s: Exp[Any], o: Exp[Any]) extends Def[String]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String]) extends Def[Array[String]]

  def string_plus(s: Exp[Any], o: Exp[Any]): Rep[String] = StringPlus(s,o)
  def string_trim(s: Exp[String]) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String]) : Rep[Array[String]] = StringSplit(s, separators)
}

trait ScalaGenString extends ScalaGenBase with StringOpsExp {
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep) => emitValDef(sym, "%s.split(%s)".format(quote(s), quote(sep)))
    case _ => super.emitNode(sym, rhs)
  }
}