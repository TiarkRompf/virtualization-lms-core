package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{GenerationFailedException}
import scala.reflect.SourceContext

trait LiftString {
  this: Base =>

  implicit def strToRepStr(s: String) = unit(s)
}

trait StringOps extends Variables with OverloadHack {
  // NOTE: if something doesn't get lifted, this won't give you a compile time error,
  //       since string concat is defined on all objects
  
  def infix_+(s1: String, s2: Rep[Any])(implicit o: Overloaded1, pos: SourceContext) = string_plus(unit(s1), s2)
  def infix_+[T:Manifest](s1: String, s2: Var[T])(implicit o: Overloaded2, pos: SourceContext) = string_plus(unit(s1), readVar(s2))
  def infix_+(s1: Rep[String], s2: Rep[Any])(implicit o: Overloaded1, pos: SourceContext) = string_plus(s1, s2)
  def infix_+[T:Manifest](s1: Rep[String], s2: Var[T])(implicit o: Overloaded2, pos: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[String], s2: Rep[String])(implicit o: Overloaded3, pos: SourceContext) = string_plus(s1, s2)
  def infix_+(s1: Rep[String], s2: Var[String])(implicit o: Overloaded4, pos: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[Any], s2: Rep[String])(implicit o: Overloaded5, pos: SourceContext) = string_plus(s1, s2)
  def infix_+(s1: Rep[Any], s2: Var[String])(implicit o: Overloaded6, pos: SourceContext) = string_plus(s1, readVar(s2))
  def infix_+(s1: Rep[Any], s2: String)(implicit o: Overloaded7, pos: SourceContext) = string_plus(s1, unit(s2))
  
  def infix_+(s1: Var[String], s2: Rep[Any])(implicit o: Overloaded8, pos: SourceContext) = string_plus(readVar(s1), s2)  
  def infix_+[T:Manifest](s1: Var[String], s2: Var[T])(implicit o: Overloaded9, pos: SourceContext) = string_plus(readVar(s1), readVar(s2))
  def infix_+(s1: Var[String], s2: Rep[String])(implicit o: Overloaded10, pos: SourceContext) = string_plus(readVar(s1), s2)    
  def infix_+(s1: Var[String], s2: Var[String])(implicit o: Overloaded11, pos: SourceContext) = string_plus(readVar(s1), readVar(s2))    
  def infix_+[T:Manifest](s1: Var[T], s2: Rep[String])(implicit o: Overloaded12, pos: SourceContext) = string_plus(readVar(s1), s2)
  def infix_+[T:Manifest](s1: Var[T], s2: Var[String])(implicit o: Overloaded13, pos: SourceContext) = string_plus(readVar(s1), readVar(s2))
  def infix_+[T:Manifest](s1: Var[T], s2: String)(implicit o: Overloaded14, pos: SourceContext) = string_plus(readVar(s1), unit(s2))
  
  // these are necessary to be more specific than arithmetic/numeric +. is there a more generic form of this that will work?
  //def infix_+[R:Manifest](s1: Rep[String], s2: R)(implicit c: R => Rep[Any], o: Overloaded15, pos: SourceContext) = string_plus(s1, c(s2))  
  def infix_+(s1: Rep[String], s2: Double)(implicit o: Overloaded15, pos: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Float)(implicit o: Overloaded16, pos: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Int)(implicit o: Overloaded17, pos: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Long)(implicit o: Overloaded18, pos: SourceContext) = string_plus(s1, unit(s2))
  def infix_+(s1: Rep[String], s2: Short)(implicit o: Overloaded19, pos: SourceContext) = string_plus(s1, unit(s2))  
  
  def infix_startsWith(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext) = string_startswith(s1,s2)
  def infix_trim(s: Rep[String])(implicit pos: SourceContext) = string_trim(s)
  def infix_split(s: Rep[String], separators: Rep[String])(implicit pos: SourceContext) = string_split(s, separators)
  def infix_toDouble(s: Rep[String])(implicit pos: SourceContext) = string_todouble(s)
  def infix_toFloat(s: Rep[String])(implicit pos: SourceContext) = string_tofloat(s)
  def infix_toInt(s: Rep[String])(implicit pos: SourceContext) = string_toint(s)
  def infix_charAt(s: Rep[String], i: Rep[Int])(implicit pos: SourceContext) = string_charAt(s,i)
  def infix_endsWith(s: Rep[String], e: Rep[String])(implicit pos: SourceContext) = string_endsWith(s,e)
  def infix_contains(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext) = string_contains(s1,s2)

  object String {
    def valueOf(a: Rep[Any])(implicit pos: SourceContext) = string_valueof(a)
  }

  def string_plus(s: Rep[Any], o: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def string_startswith(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext): Rep[Boolean]
  def string_trim(s: Rep[String])(implicit pos: SourceContext): Rep[String]
  def string_split(s: Rep[String], separators: Rep[String])(implicit pos: SourceContext): Rep[Array[String]]
  def string_valueof(d: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def string_todouble(s: Rep[String])(implicit pos: SourceContext): Rep[Double]
  def string_tofloat(s: Rep[String])(implicit pos: SourceContext): Rep[Float]
  def string_toint(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
  def string_charAt(s: Rep[String], i: Rep[Int])(implicit pos: SourceContext): Rep[Char]
  def string_endsWith(s: Rep[String], e: Rep[String])(implicit pos: SourceContext): Rep[Boolean]
  def string_contains(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext): Rep[Boolean]
}

trait StringOpsExp extends StringOps with VariablesExp {
  case class StringPlus(s: Exp[Any], o: Exp[Any]) extends Def[String]
  case class StringStartsWith(s1: Exp[String], s2: Exp[String]) extends Def[Boolean]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String]) extends Def[Array[String]]
  case class StringEndsWith(s: Exp[String], e: Exp[String]) extends Def[Boolean]  
  case class StringCharAt(s: Exp[String], i: Exp[Int]) extends Def[Char]
  case class StringValueOf(a: Exp[Any]) extends Def[String]
  case class StringToDouble(s: Exp[String]) extends Def[Double]
  case class StringToFloat(s: Exp[String]) extends Def[Float]
  case class StringToInt(s: Exp[String]) extends Def[Int]
  case class StringContains(s1: Exp[String], s2: Exp[String]) extends Def[Boolean]

  def string_plus(s: Exp[Any], o: Exp[Any])(implicit pos: SourceContext): Rep[String] = StringPlus(s,o)
  def string_startswith(s1: Exp[String], s2: Exp[String])(implicit pos: SourceContext) = StringStartsWith(s1,s2)
  def string_trim(s: Exp[String])(implicit pos: SourceContext) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String])(implicit pos: SourceContext) : Rep[Array[String]] = StringSplit(s, separators)
  def string_valueof(a: Exp[Any])(implicit pos: SourceContext) = StringValueOf(a)
  def string_todouble(s: Exp[String])(implicit pos: SourceContext) = StringToDouble(s)
  def string_tofloat(s: Exp[String])(implicit pos: SourceContext) = StringToFloat(s)
  def string_toint(s: Exp[String])(implicit pos: SourceContext) = StringToInt(s)
  def string_charAt(s: Exp[String], i: Exp[Int])(implicit pos: SourceContext) = StringCharAt(s,i)
  def string_endsWith(s: Exp[String], e: Exp[String])(implicit pos: SourceContext) = StringEndsWith(s,e)
  def string_contains(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext) = StringContains(s1,s2)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case StringPlus(a,b) => string_plus(f(a),f(b))
    case StringStartsWith(s1, s2) => string_startswith(f(s1), f(s2))
    case StringTrim(s) => string_trim(f(s))
    case StringSplit(s,sep) => string_split(f(s),f(sep))
    case StringToDouble(s) => string_todouble(f(s))
    case StringToFloat(s) => string_tofloat(f(s))
    case StringToInt(s) => string_toint(f(s))
    case StringEndsWith(s, e) => string_endsWith(f(s),f(e))
    case StringCharAt(s,i) => string_charAt(f(s),f(i))
    case StringValueOf(a) => string_valueof(f(a))
    case StringContains(s1,s2) => string_contains(f(s1),f(s2))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenStringOps extends ScalaGenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => emitValDef(sym, "%s+%s".format(quote(s1), quote(s2)))
    case StringStartsWith(s1,s2) => emitValDef(sym, "%s.startsWith(%s)".format(quote(s1),quote(s2)))
    case StringTrim(s) => emitValDef(sym, "%s.trim()".format(quote(s)))
    case StringSplit(s, sep) => emitValDef(sym, "%s.split(%s)".format(quote(s), quote(sep)))
    case StringEndsWith(s, e) => emitValDef(sym, "%s.endsWith(%s)".format(quote(s), quote(e)))    
    case StringCharAt(s,i) => emitValDef(sym, "%s.charAt(%s)".format(quote(s), quote(i)))
    case StringValueOf(a) => emitValDef(sym, "java.lang.String.valueOf(%s)".format(quote(a)))
    case StringToDouble(s) => emitValDef(sym, "%s.toDouble".format(quote(s)))
    case StringToFloat(s) => emitValDef(sym, "%s.toFloat".format(quote(s)))
    case StringToInt(s) => emitValDef(sym, "%s.toInt".format(quote(s)))
    case StringContains(s1,s2) => emitValDef(sym, "%s.contains(%s)".format(quote(s1),quote(s2)))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenStringOps extends CudaGenBase {
  val IR: StringOpsExp
  import IR._
}

trait OpenCLGenStringOps extends OpenCLGenBase {
  val IR: StringOpsExp
  import IR._
}

trait CGenStringOps extends CGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    //case StringPlus(s1,s2) => emitValDef(sym,"strcat(%s,%s)".format(quote(s1),quote(s2)))
    case StringStartsWith(s1,s2) => emitValDef(sym, "(strlen(%s)>=strlen(%s)) && strncmp(%s,%s,strlen(%s))".format(quote(s1),quote(s2),quote(s1),quote(s2),quote(s2)))
    //case StringTrim(s) => 
    //case StringSplit(s, sep) => 
    case StringEndsWith(s, e) => emitValDef(sym, "(strlen(%s)>=strlen(%s)) && strncmp(%s+strlen(%s)-strlen(%s),%s,strlen(%s))".format(quote(s),quote(e),quote(s),quote(e),quote(s),quote(e),quote(e)))    
    case StringCharAt(s,i) => emitValDef(sym, "%s[%s]".format(quote(s), quote(i)))
    //case StringValueOf(a) => 
    case StringToDouble(s) => emitValDef(sym, "strtod(%s,NULL)".format(quote(s)))
    case StringToFloat(s) => emitValDef(sym, "strtodf(%s,NULL)".format(quote(s)))
    case StringToInt(s) => emitValDef(sym, "atoi(%s)".format(quote(s)))
    case StringContains(s1,s2) => emitValDef(sym, "(strstr(%s,%s)!=NULL)".format(quote(s1),quote(s2)))
    case _ => super.emitNode(sym, rhs)
  }
}
