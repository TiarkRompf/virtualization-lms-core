package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenBase, ScalaGenBase}
import scala.virtualization.lms.util.OverloadHack

trait PrimitiveOps extends Variables with OverloadHack {
  /**
   * Useful chaining implicits
   */
  implicit def intToRepDouble(i: Int) = unit(i.toDouble)
  

  /**
   * Double
   */
  object Double {
    def parseDouble(s: Rep[String]) = obj_double_parse_double(s)
  }

  def obj_double_parse_double(s: Rep[String]) : Rep[Double]

  /**
   * Int
   */
  implicit def intToIntOpsCls(n: Int) = new IntOpsCls(n)
  implicit def repIntToIntOpsCls(n: Rep[Int]) = new IntOpsCls(n)
  implicit def varIntToIntOpsCls(n: Var[Int]) = new IntOpsCls(readVar(n))

  class IntOpsCls(lhs: Rep[Int]){
    // TODO (tiark): either of these cause scalac to crash        
    //def /[A](rhs: Rep[A])(implicit mA: Manifest[A], f: Fractional[A], o: Overloaded1) = int_divide_frac(lhs, rhs)
    //def /(rhs: Rep[Int]) = int_divide(lhs, rhs)
  }

  def infix_/(lhs: Rep[Int], rhs: Rep[Int]) = int_divide(lhs, rhs)

  def int_divide_frac[A:Manifest:Fractional](lhs: Rep[Int], rhs: Rep[A]) : Rep[A]
  def int_divide(lhs: Rep[Int], rhs: Rep[Int]) : Rep[Int]
}

trait PrimitiveOpsExp extends PrimitiveOps with BaseExp {
  /**
   * Double
   */
  case class ObjDoubleParseDouble(s: Exp[String]) extends Def[Double]

  def obj_double_parse_double(s: Exp[String]) = ObjDoubleParseDouble(s)

  /**
   * Int
   */
  case class IntDivideFrac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) extends Def[A]
  case class IntDivide(lhs: Exp[Int], rhs: Exp[Int]) extends Def[Int]

  def int_divide_frac[A:Manifest:Fractional](lhs: Exp[Int], rhs: Exp[A]) : Exp[A] = IntDivideFrac(lhs, rhs)
  def int_divide(lhs: Exp[Int], rhs: Exp[Int]) : Exp[Int] = IntDivide(lhs, rhs)
}

trait ScalaGenPrimitiveOps extends ScalaGenBase {
  val IR: PrimitiveOpsExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ObjDoubleParseDouble(s) => emitValDef(sym, "java.lang.Double.parseDouble(" + quote(s) + ")")
    case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
    case _ => super.emitNode(sym, rhs)    
  }
}

trait CudaGenPrimitiveOps extends CudaGenBase {
  val IR: PrimitiveOpsExp
  import IR._

  //TODO: stdlib.h needs to be included in the common header file
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = {
      rhs match {
        case ObjDoubleParseDouble(s) =>
          if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
          else emitValDef("double", sym, "atof(" + quote(s) + ")")
        case _ => super.emitNode(sym, rhs)
      }
    }
}
