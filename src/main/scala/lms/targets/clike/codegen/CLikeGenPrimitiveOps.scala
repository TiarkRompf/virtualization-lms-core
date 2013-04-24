package scala.lms
package targets.clike.codegen

import ops.{PrimitiveOpsExp}

import java.io.PrintWriter

trait CLikeGenPrimitiveOps extends CLikeGenBase {
  val IR: PrimitiveOpsExp
  import IR._

  //TODO: stdlib.h needs to be included in the common header file
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      //case ObjDoubleParseDouble(s) => emitValDef(sym, "atof(" + quote(s) + ")")
      case ObjDoublePositiveInfinity() => emitValDef(sym, "DBL_MAX")
      //case ObjDoubleMinValue() => emitValDef(sym, "scala.Double.MinValue")
      case DoubleFloatValue(lhs) => emitValDef(sym, "(float)"+quote(lhs))
      //case ObjIntegerParseInt(s) => emitValDef(sym, "java.lang.Integer.parseInt(" + quote(s) + ")")
      //case ObjIntMaxValue() => emitValDef(sym, "scala.Int.MaxValue")
      case IntPlus(lhs,rhs) => emitValDef(sym, quote(lhs) + " + " + quote(rhs))
      case IntMinus(lhs,rhs) => emitValDef(sym, quote(lhs) + " - " + quote(rhs))
      case IntTimes(lhs,rhs) => emitValDef(sym, quote(lhs) + " * " + quote(rhs))
      case IntDivideFrac(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case IntDivide(lhs,rhs) => emitValDef(sym, quote(lhs) + " / " + quote(rhs))
      case IntMod(lhs,rhs) => emitValDef(sym, quote(lhs) + " % " + quote(rhs))
      case IntBinaryOr(lhs,rhs) => emitValDef(sym, quote(lhs) + " | " + quote(rhs))
      case IntBinaryAnd(lhs,rhs) => emitValDef(sym, quote(lhs) + " & " + quote(rhs))
      case IntDoubleValue(lhs) => emitValDef(sym, "(double)"+quote(lhs))
      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CudaGenPrimitiveOps extends CudaGenBase with CLikeGenPrimitiveOps
trait OpenCLGenPrimitiveOps extends OpenCLGenBase with CLikeGenPrimitiveOps
trait CGenPrimitiveOps extends CGenBase with CLikeGenPrimitiveOps
