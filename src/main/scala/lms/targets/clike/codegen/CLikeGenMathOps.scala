package scala.lms
package targets.clike.codegen

import internal.GenericNestedCodegen
import ops.{BaseGenMathOps, MathOpsExp}

import java.io.PrintWriter

trait CLikeGenMathOps extends BaseGenMathOps with CLikeGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathCeil(x) => emitValDef(sym, "ceil(" + quote(x) + ")")
    case MathFloor(x) => emitValDef(sym, "floor(" + quote(x) + ")")
    case MathExp(x) => emitValDef(sym, "exp(" + quote(x) + ")")
    case MathLog(x) => emitValDef(sym, "log(" + quote(x) + ")")
    case MathSqrt(x) => emitValDef(sym, "sqrt(" + quote(x) + ")")
    case MathSin(x) => emitValDef(sym, "sin(" + quote(x) + ")")
    case MathCos(x) => emitValDef(sym, "cos(" + quote(x) + ")")
    case MathAcos(x) => emitValDef(sym, "acos(" + quote(x) + ")")
    case MathAtan(x) => emitValDef(sym, "atan(" + quote(x) + ")")
    case MathAtan2(x,y) => emitValDef(sym, "atan2(" + quote(x) + ", " + quote(y) + ")")
    case MathPow(x,y) => emitValDef(sym, "pow(" + quote(x) + "," + quote(y) + ")")
    case MathAbs(x) => emitValDef(sym, "abs(" + quote(x) + ")")
    case MathMax(x,y) if(remap(sym.tp)=="float" || remap(sym.tp)=="double") => emitValDef(sym, "fmax(" + quote(x) + ", " + quote(y) + ")")
    case MathMax(x,y) if(remap(sym.tp))=="int" => emitValDef(sym, "(" + quote(x) + ">" + quote(y) + ") ? " + quote(x) + " : " + quote(y))
    case MathMin(x,y) if(remap(sym.tp)=="float" || remap(sym.tp)=="double")  => emitValDef(sym, "fmin(" + quote(x) + ", " + quote(y) + ")")
    case MathMin(x,y) if(remap(sym.tp))=="int" => emitValDef(sym, "(" + quote(x) + "<" + quote(y) + ") ? " + quote(x) + " : " + quote(y))
    case _ => super.emitNode(sym, rhs)
  }
}


trait CudaGenMathOps extends CLikeGenMathOps with CudaGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "CUDART_PI_F")
    case MathE() => emitValDef(sym, "2.7182818284f")
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenMathOps extends CLikeGenMathOps with OpenCLGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "M_PI")
    case MathE() => emitValDef(sym, "M_E")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenMathOps extends CLikeGenMathOps with CGenEffect {
  val IR: MathOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case MathPi() => emitValDef(sym, "M_PI")
    case MathE() => emitValDef(sym, "M_E")
    case _ => super.emitNode(sym, rhs)
  }

}
