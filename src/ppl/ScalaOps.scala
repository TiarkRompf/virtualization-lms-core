package scala.virtualization.lms
package ppl

import java.io.{BufferedReader, FileReader, PrintWriter}

import common._
import util.OverloadHack

/**
 * Embedding for Int, Range, etc...
 */
trait ScalaOps extends Base with OverloadHack {
  
  implicit def unit[T](x:T): Rep[T]
  
  /**
   Int operations
   **/
  implicit def repIntToRepIntOps(i: Rep[Int]) = new RepIntOpsCls(i)
  implicit def intToRepIntOps(i: Int) = new RepIntOpsCls(i)
  
  class RepIntOpsCls(a: Rep[Int]) {
    def until(b: Rep[Int]): Rep[Range] = int_until(a,b)
    def < (b: Rep[Int]): Rep[Boolean]  = int_lt(a,b) 
  }
  // abstract methods for int operations
  def int_until(a: Rep[Int], b: Rep[Int]): Rep[Range]
  def int_lt   (a: Rep[Int], b: Rep[Int]): Rep[Boolean]
  
  /**
   Double Operations
   **/ 
  implicit def repDoubleToRepDoubleOps(d: Rep[Double]) = new RepDoubleOpsCls(d)
  implicit def doubleToRepDoubleOps(d: Double) = new RepDoubleOpsCls(d)
  
  object Double {
    def parseDouble(s: Rep[String]) = obj_parsedouble(s)
  }
  
  class RepDoubleOpsCls(a: Rep[Double]) {
    def + (b: Rep[Double]) = double_plus(a,b)
    def * (b: Rep[Double]) = double_mul(a,b)
    def / (b: Rep[Double]) = double_div(a,b)
    def / (b: Rep[Int])(implicit o: Overloaded1)  = double_div(a,implicitConvert(b)) 
    def <= (b: Rep[Double]) = double_lte(a,b)
  }
  
  def obj_parsedouble(s: Rep[String]) : Rep[Double]
  def double_plus(a: Rep[Double], b: Rep[Double]): Rep[Double]
  def double_mul(a: Rep[Double], b: Rep[Double]): Rep[Double]
  def double_div(a: Rep[Double], b: Rep[Double]): Rep[Double]
  def double_lte(a: Rep[Double], b: Rep[Double]): Rep[Boolean]
  
  /*
   * Range operations
   */
  implicit def repRangeToRepRangeOps(r: Rep[Range]) = new RepRangeOpsCls(r)
  implicit def rangeToRepRangeOps(r: Range) = new RepRangeOpsCls(r)
  class RepRangeOpsCls(r: Rep[Range]) {
    def start : Rep[Int] = range_start(r)
    def step : Rep[Int] = range_step(r)
    def end : Rep[Int] = range_end(r)
    def foreach(f: (Rep[Int])=>Rep[Unit]):Rep[Unit] = range_foreach(r,f)      
  }
  def range_start(r: Rep[Range]) : Rep[Int]
  def range_step(r: Rep[Range]) : Rep[Int]
  def range_end(r: Rep[Range]) : Rep[Int]
  def range_foreach(r: Rep[Range], f: (Rep[Int]) => Rep[Unit]): Rep[Unit]
  
  /**
   * String
   */
  implicit def repStrToRepStrOps(s: Rep[String]) = new RepStrOpsCls(s)
  implicit def strToRepStrOps(s: String) = new RepStrOpsCls(s)
  
  class RepStrOpsCls(s: Rep[String]) {
    def +(o: Rep[Any]) = string_plus(s,o)
    def trim() = string_trim(s);
    def split(separators: String) = string_split(s, separators);
  }
  def string_plus(s: Rep[String], o: Rep[Any]): Rep[String]
  def string_trim(s: Rep[String]) : Rep[String]
  def string_split(s: Rep[String], separators: Rep[String]) : Rep[Array[String]]
    
  /**
   * BufferedReader
   */
  object BufferedReader {
    def apply(f: Rep[FileReader]) = obj_br_apply(f)
  }
  implicit def repBrToRepBrOps(b: Rep[BufferedReader]) = new RepBrOpsCls(b)
  implicit def brToRepBrOps(b: BufferedReader) = new RepBrOpsCls(b)
  
  class RepBrOpsCls(b: Rep[BufferedReader]) {
    def readLine() = br_readline(b)
    def close() = br_close(b)
  }
  def obj_br_apply(f: Rep[FileReader]) : Rep[BufferedReader]
  def br_readline(b: Rep[BufferedReader]) : Rep[String]
  def br_close(b: Rep[BufferedReader]) : Rep[Unit]
  
  /**
   * FileReader
   */
  object FileReader {
    def apply(s: Rep[String]) = obj_fr_apply(s)
  }
  def obj_fr_apply(s: Rep[String]) : Rep[FileReader]
  
  /**
   * Array
   */    
  implicit def repArrayToRepArrayOps[T](a: Rep[Array[T]]) = new RepArrayOpsCls(a)
  implicit def arrayToRepArrayOps[T](a: Array[T]) = new RepArrayOpsCls(a)
  class RepArrayOpsCls[T](a: Rep[Array[T]]){
    def apply(n: Rep[Int]) = array_apply(a, n)
    def length = array_length(a)
  }
  def array_apply[T](x: Rep[Array[T]], n: Rep[Int]): Rep[T]
  def array_length[T](a: Rep[Array[T]]) : Rep[Int]
  
    
  /**
   *   Other implicit converstions
   **/
  // Rep[X] to Rep[Y] Implicit converstions
  implicit def implicitConvert[X,Y](x : Rep[X])(implicit c : X => Y) = convert[X,Y](x)
  def convert[X,Y](x : Rep[X])(implicit c : X => Y): Rep[Y]
  

  /**
   * Other things that need to get lifted like exit, there should be
   * a better way to do this
   */

  def print(x: Rep[Any]): Rep[Unit]
  def println(x: Rep[Any]): Rep[Unit]

  // TODO: there is no way to override this behavior
  def exit(status: Int): Rep[Nothing] = exit(unit(status))
  def exit(): Rep[Nothing] = exit(0)
  def exit(status: Rep[Int]): Rep[Nothing]  
}



trait ScalaOpsExp extends ScalaOps with BaseExp { this: FunctionsExp with EffectExp =>
  implicit def unit[T](x:T): Exp[T] = Const(x)
  
  case class Convert[X,Y](x: Exp[X])(implicit c : X => Y) extends Def[Y]
  case class Until(start: Exp[Int], end: Exp[Int]) extends Def[Range]
  case class IntLT(a: Rep[Int], b: Rep[Int]) extends Def[Boolean]
  case class ObjParseDouble(s: Exp[String]) extends Def[Double]

  case class DoublePlus(a: Exp[Double], b: Exp[Double]) extends Def[Double]
  case class DoubleDivide(n: Exp[Double], x: Exp[Double]) extends Def[Double]
  case class DoubleTimes(n: Exp[Double], x: Exp[Double]) extends Def[Double]
  case class DoubleLTE(a: Rep[Double], b: Rep[Double]) extends Def[Boolean]

  case class RangeStart(r: Exp[Range]) extends Def[Int]
  case class RangeStep(r: Exp[Range]) extends Def[Int]
  case class RangeEnd(r: Exp[Range]) extends Def[Int]
  case class RangeForeach(r: Exp[Range], block: Exp[Int => Unit]) extends Def[Unit]
  
  case class ObjBrApply(f: Exp[FileReader]) extends Def[BufferedReader]
  case class BrReadline(b: Exp[BufferedReader]) extends Def[String]
  case class BrClose(b: Exp[BufferedReader]) extends Def[Unit]
  case class ObjFrApply(f: Exp[String]) extends Def[FileReader]
  case class StringPlus(s: Exp[String], o: Exp[Any]) extends Def[String]
  case class StringTrim(s: Exp[String]) extends Def[String]
  case class StringSplit(s: Exp[String], separators: Exp[String]) extends Def[Array[String]]

  case class ArrayLength[T](a: Exp[Array[T]]) extends Def[Int]
  case class ArrayApply[T](x: Exp[Array[T]], n: Exp[Int]) extends Def[T]

  case class Print(x: Rep[Any]) extends Def[Unit]
  case class Exit(s: Rep[Int]) extends Def[Nothing]


  // TODO: why are the return types required?
  def convert[X,Y](x: Exp[X])(implicit c : X => Y) : Rep[Y] = Convert[X,Y](x)
  
  def int_until(start: Exp[Int], end: Exp[Int]) : Rep[Range] = Until(start, end)
  def int_lt(a: Rep[Int], b: Rep[Int]): Rep[Boolean] = IntLT(a,b)
  
  def obj_parsedouble(s: Exp[String]) : Rep[Double] = ObjParseDouble(s)
  def double_plus(a: Exp[Double], b: Exp[Double]) : Rep[Double] = DoublePlus(a, b)
  def double_div(n: Exp[Double], x: Exp[Double]) : Rep[Double] = DoubleDivide(n, x)
  def double_mul(n: Exp[Double], x: Exp[Double]) : Rep[Double] = DoubleTimes(n, x)
  def double_lte(a: Rep[Double], b: Rep[Double]) : Rep[Boolean] = DoubleLTE(a,b)
  
  def range_start(r: Exp[Range]) : Rep[Int] = RangeStart(r)
  def range_step(r: Exp[Range]) : Rep[Int] = RangeStep(r)
  def range_end(r: Exp[Range]) : Rep[Int] = RangeEnd(r)
  def range_foreach(x: Exp[Range], block: Exp[Int] => Exp[Unit]) : Rep[Unit] = reflectEffect(RangeForeach(x, doLambda(block)))
  
  def obj_br_apply(f: Exp[FileReader]) : Rep[BufferedReader] = ObjBrApply(f)
  def br_readline(b: Exp[BufferedReader]) : Rep[String] = BrReadline(b)
  def br_close(b: Exp[BufferedReader]) : Rep[Unit] = BrClose(b)
  def obj_fr_apply(s: Exp[String]) : Rep[FileReader] = ObjFrApply(s)

  def string_plus(s: Exp[String], o: Exp[Any]): Rep[String] = {
    println("adding a string plus")
    StringPlus(s,o)
  }
  def string_trim(s: Exp[String]) : Rep[String] = StringTrim(s)
  def string_split(s: Exp[String], separators: Exp[String]) : Rep[Array[String]] = StringSplit(s, separators)
  
  def array_apply[T](x: Exp[Array[T]], n: Exp[Int]): Rep[T] = ArrayApply(x, n)
  def array_length[T](a: Exp[Array[T]]) : Rep[Int] = ArrayLength(a)


  def print(x: Rep[Any]) = reflectEffect(Print(x))
  def println(x: Rep[Any]) = reflectEffect(Print(x))
  def exit(s: Rep[Int]) = reflectEffect(Exit(s))
}

  
/* Codegen module for Scala Ops embedding */
trait ScalaGenScalaOps extends ScalaGenEffect  { this: ScalaOpsExp => 

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {

    case IntLT(a,b) => emitValDef(sym, "" + quote(a) + " < " + quote(b))

    case DoubleLTE(a,b) => emitValDef(sym, quote(a) + " <= " + quote(b))
    case DoublePlus(a,b) => emitValDef(sym, "" + quote(a) + " + " + quote(b))
    
    //TODO: we can just create a range directly
    case Until(start, end) => emitValDef(sym, "" + quote(start) + " until " + quote(end))

    case StringPlus(s,o) => {
      System.out.println("emitting a string plus")
      emitValDef(sym, "" + quote(s) + " + " + quote(o))
    }

    //case RangeStart(x) => emitValDef(sym, "" + quote(x) + ".start")
    case RangeForeach(r,f) => {
      stream.println("val " + quote(sym) + " = " + quote(r) + ".foreach{ ")
      emitBlock(f)
      stream.println("}")
    }

    case ArrayLength(x) => emitValDef(sym, "" + quote(x) + ".length")
    case ArrayApply(x,n) => emitValDef(sym, "" + quote(x) + ".apply(" + n + ")")

    case Print(s) => emitValDef(sym, "println(" + quote(s) + ")")
    case Exit(a) => emitValDef(sym, "exit(" + quote(a) + ")")

    case _ => super.emitNode(sym, rhs)
  }
                                         
}
