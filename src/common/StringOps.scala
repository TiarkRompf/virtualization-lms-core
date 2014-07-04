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
  def infix_+[T:Manifest](s1: Rep[String], s2: Rep[T])(implicit o: Overloaded1, pos: SourceContext): Rep[String] = {
    if (manifest[T] == classManifest[Array[Byte]])
        string_plus(s1, string_new(s2))
    else string_plus(s1, s2)
  }
  def infix_+[T:Manifest](s1: Rep[String], s2: Var[T])(implicit o: Overloaded2, pos: SourceContext): Rep[String] = string_plus(s1, readVar(s2))
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
  def infix_toLong(s: Rep[String])(implicit pos: SourceContext) = string_tolong(s)
  def infix_substring(s: Rep[String], beginIndex: Rep[Int])(implicit pos: SourceContext) = string_substring(s, beginIndex)
  def infix_substring(s: Rep[String], beginIndex: Rep[Int], endIndex: Rep[Int])(implicit pos: SourceContext) = string_substring(s, beginIndex, endIndex)
  def infix_length(s: Rep[String])(implicit pos: SourceContext) = string_length(s)

  object String {
    def valueOf(a: Rep[Any])(implicit pos: SourceContext) = string_valueof(a)
  }

  def string_new(s: Rep[Any]): Rep[String]
  def string_plus(s: Rep[Any], o: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def string_startswith(s1: Rep[String], s2: Rep[String])(implicit pos: SourceContext): Rep[Boolean]
  def string_trim(s: Rep[String])(implicit pos: SourceContext): Rep[String]
  def string_split(s: Rep[String], separators: Rep[String])(implicit pos: SourceContext): Rep[Array[String]]
  def string_valueof(d: Rep[Any])(implicit pos: SourceContext): Rep[String]
  def string_todouble(s: Rep[String])(implicit pos: SourceContext): Rep[Double]
  def string_tofloat(s: Rep[String])(implicit pos: SourceContext): Rep[Float]
  def string_toint(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
  def string_tolong(s: Rep[String])(implicit pos: SourceContext): Rep[Long]
  def string_substring(s: Rep[String], beginIndex: Rep[Int])(implicit pos: SourceContext): Rep[String]
  def string_substring(s: Rep[String], beginIndex: Rep[Int], endIndex: Rep[Int])(implicit pos: SourceContext): Rep[String]
  def string_length(s: Rep[String])(implicit pos: SourceContext): Rep[Int]
}

trait StringOpsExp extends StringOps with VariablesExp {
  case class StringNew(s: Rep[Any]) extends Def[String]
  case class StringPlus(s: Exp[Any], o: Exp[Any]) extends Def[String]
  case class StringStartsWith(s1: Exp[String], s2: Exp[String]) extends Def[Boolean]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String]) extends Def[Array[String]]
  case class StringValueOf(a: Exp[Any]) extends Def[String]
  case class StringToDouble(s: Exp[String]) extends Def[Double]
  case class StringToFloat(s: Exp[String]) extends Def[Float]
  case class StringToInt(s: Exp[String]) extends Def[Int]
  case class StringToLong(s: Exp[String]) extends Def[Long]
  case class StringSubstring(s: Exp[String], beginIndex: Exp[Int]) extends Def[String]
  case class StringSubstringWithEndIndex(s: Exp[String], beginIndex: Exp[Int], endIndex: Exp[Int]) extends Def[String]
  case class StringLength(s: Exp[String]) extends Def[Int]

  def string_new(s: Rep[Any]) = StringNew(s)
  def string_plus(s: Exp[Any], o: Exp[Any])(implicit pos: SourceContext): Rep[String] = StringPlus(s,o)
  def string_startswith(s1: Exp[String], s2: Exp[String])(implicit pos: SourceContext) = StringStartsWith(s1,s2)
  def string_trim(s: Exp[String])(implicit pos: SourceContext) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String])(implicit pos: SourceContext) : Rep[Array[String]] = StringSplit(s, separators)
  def string_valueof(a: Exp[Any])(implicit pos: SourceContext) = StringValueOf(a)
  def string_todouble(s: Exp[String])(implicit pos: SourceContext) = StringToDouble(s)
  def string_tofloat(s: Exp[String])(implicit pos: SourceContext) = StringToFloat(s)
  def string_toint(s: Exp[String])(implicit pos: SourceContext) = StringToInt(s)
  def string_tolong(s: Exp[String])(implicit pos: SourceContext) = StringToLong(s)
  def string_substring(s: Exp[String], beginIndex: Exp[Int])(implicit pos: SourceContext) = StringSubstring(s, beginIndex)
  def string_substring(s: Exp[String], beginIndex: Exp[Int], endIndex: Exp[Int])(implicit pos: SourceContext) = StringSubstringWithEndIndex(s, beginIndex, endIndex)
  def string_length(s: Rep[String])(implicit pos: SourceContext) = StringLength(s)

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case StringPlus(a,b) => string_plus(f(a),f(b))
    case StringTrim(s) => string_trim(f(s))
    case StringSplit(s,sep) => string_split(f(s),f(sep))
    case StringToDouble(s) => string_todouble(f(s))
    case StringToFloat(s) => string_tofloat(f(s))
    case StringToInt(s) => string_toint(f(s))
    case StringToLong(s) => string_tolong(f(s))
    case StringSubstring(s, beginIndex) => string_substring(f(s), f(beginIndex))
    case StringSubstringWithEndIndex(s, beginIndex, endIndex) => string_substring(f(s), f(beginIndex), f(endIndex))
    case StringLength(s) => string_length(f(s))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenStringOps extends ScalaGenBase {
  val IR: StringOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringNew(s1) => emitValDef(sym, "new String(" + quote(s1) + ")")
    case StringPlus(s1,s2) => emitValDef(sym, src"$s1+$s2")
    case StringStartsWith(s1,s2) => emitValDef(sym, src"$s1.startsWith($s2)")
    case StringTrim(s) => emitValDef(sym, src"$s.trim()")
    case StringSplit(s, sep) => emitValDef(sym, src"$s.split($sep)")
    case StringValueOf(a) => emitValDef(sym, src"java.lang.String.valueOf($a)")
    case StringToDouble(s) => emitValDef(sym, src"$s.toDouble")
    case StringToFloat(s) => emitValDef(sym, src"$s.toFloat")
    case StringToInt(s) => emitValDef(sym, src"$s.toInt")
    case StringToLong(s) => emitValDef(sym, src"$s.toLong")
    case StringSubstring(s,b) => emitValDef(sym, src"$s.substring($b)")
    case StringSubstringWithEndIndex(s,b,e) => emitValDef(sym, "$s.substring($b,$e)")
    case StringLength(s) => emitValDef(sym, src"$s.length")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenStringOps extends CudaGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case StringSplit(s, sep) => throw new GenerationFailedException("CudaGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenStringOps extends OpenCLGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringPlus(s1,s2) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringTrim(s) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case StringSplit(s, sep) => throw new GenerationFailedException("OpenCLGen: Not GPUable")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CGenStringOps extends CGenBase {
  val IR: StringOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StringToInt(s) => emitValDef(sym,src"atoi($s)")
    case StringToLong(s) => emitValDef(sym,src"atol($s)")
    case StringToFloat(s) => emitValDef(sym,src"atof($s)")
    case StringToDouble(s) => emitValDef(sym,src"atof($s)")
    case StringSubstring(s,b) => emitValDef(sym, src"strdup($s+$b)")
    case StringSubstringWithEndIndex(s,a,b) => emitValDef(sym, src"({ int l=$b-$a; char* r=(char*)malloc(l); memcpy(r,((char*)$s)+$a,l); r[l]=0; r; })")
    case StringLength(s) => emitValDef(sym, src"strlen($s)")
    case StringPlus(s1,s2) => s2.tp.toString match {
      // Warning: memory leaks. We need a global mechanism like reference counting, possibly release pool(*) wrapping functions.
      // (*) See https://developer.apple.com/library/mac/documentation/Cocoa/Reference/Foundation/Classes/NSAutoreleasePool_Class/Reference/Reference.html
      case "java.lang.String" => emitValDef(sym,src"({ int l1=strlen($s1),l2=strlen($s2); char* r=(char*)malloc(l1+l2+1); memcpy(r,$s1,l1); memcpy(r+l1,$s2,l2); r[l1+l2]=0; r; })")
      case "Char" => emitValDef(sym,src"({ int l1=strlen($s1); char* r=(char*)malloc(l1+2); memcpy(r,$s1,l1); r[l1]=$s2; r[l1+2]=0; r; })")
    }
    case StringTrim(s) => throw new GenerationFailedException("CGenStringOps: StringTrim not implemented yet")
    case StringSplit(s, sep) => throw new GenerationFailedException("CGenStringOps: StringSplit not implemented yet")
    case _ => super.emitNode(sym, rhs)
  }
}
