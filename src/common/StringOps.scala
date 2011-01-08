package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenerationFailedException, CGenBase, CudaGenBase, ScalaGenBase}

trait StringOps extends Variables with OverloadHack {
  // NOTE: if something doesn't get lifted, this won't give you a compile time error,
  //       since string concat is defined on all objects
  def infix_+(s1: String, s2: Rep[Any]) = string_plus(s1,s2)
  def infix_+(s1: Rep[Any], s2: String)(implicit o: Overloaded1) = string_plus(s1,s2)
  def infix_+(s1: String, s2: Var[Any])(implicit o: Overloaded4) = string_plus(s1,s2)
  def infix_+(s1: Var[Any], s2: String)(implicit o: Overloaded5) = string_plus(s1,s2)

  def infix_trim(s: Rep[String]) = string_trim(s)
  def infix_split(s: Rep[String], separators: Rep[String]) = string_split(s, separators)
  def infix_valueOf(s: Rep[String], d: Rep[Double]) = string_valueof(d)

  def string_plus(s: Rep[Any], o: Rep[Any]): Rep[String]
  def string_trim(s: Rep[String]): Rep[String]
  def string_split(s: Rep[String], separators: Rep[String]): Rep[Array[String]]
  def string_valueof(d: Rep[Double]): Rep[String]
}

trait StringOpsExp extends StringOps with VariablesExp {
  case class StringPlus(s: Exp[Any], o: Exp[Any]) extends Def[String]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String]) extends Def[Array[String]]
  case class StringValueOf(d: Exp[Double]) extends Def[String]

  def string_plus(s: Exp[Any], o: Exp[Any]): Rep[String] = StringPlus(s,o)
  def string_trim(s: Exp[String]) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String]) : Rep[Array[String]] = StringSplit(s, separators)
  def string_valueof(d: Exp[Double]) = StringValueOf(d)
}

trait ScalaGenStringOps extends ScalaGenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep) => emitValDef(sym, "%s.split(%s)".format(quote(s), quote(sep)))
    case StringValueOf(d) => emitValDef(sym, "java.lang.String.valueOf(%s)".format(quote(d)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenStringOps extends CudaGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringSplit(s, sep) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenStringOps extends CGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym,"strcat(%s,%s);".format(quote(s1),quote(s2)))
    case StringTrim(s) => throw new GenerationFailedException("CGenStringOps: StringTrim not implemented yet")
    case StringSplit(s, sep) => throw new GenerationFailedException("CGenStringOps: StringSplit not implemented yet")
    case _ => super.emitNode(sym, rhs)
  }
}