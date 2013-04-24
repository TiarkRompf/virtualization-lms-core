package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import ops.{FunctionsExp, BaseGenFunctions, TupledFunctionsExp, GenericGenUnboxedTupleAccess}


trait CudaGenFunctions extends CudaGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case e@Lambda(fun, x, y) =>
        throw new GenerationFailedException("CudaGenFunctions: Lambda is not supported yet")
        // The version for inlined device function
        //stream.println(addTab() + "%s %s = %s;".format(remap(x.tp), quote(x), quote(sym)+"_1"))
        //emitBlock(y)
        //stream.println(addTab() + "%s %s = %s;".format(remap(y.tp), quote(sym), quote(getBlockResult(y))))

        // The version for separate device function
        /*
        //TODO: If function parameter was originally tuple, then each element should be renamed?
        val freeVars = buildScheduleForResult(y).filter(scope.contains(_)).map(_.sym)
        stream.println("__device__ %s %s(%s %s) {".format(e.mB, quote(sym), e.mA, quote(x)))
        emitBlock(y)
        stream.println("%s %s = %s;".format(e.mB, quote(sym), quote(getBlockResult(y))))
        stream.println("return %s;".format(quote(getBlockResult(y))))
        stream.println("}")
        */
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait OpenCLGenFunctions extends OpenCLGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case e@Lambda(fun, x, y) =>
        throw new GenerationFailedException("OpenCLGenFunctions: Lambda is not supported yet")
      case Apply(fun, arg) =>
        emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")

      case _ => super.emitNode(sym, rhs)
    }
  }
}

trait CGenFunctions extends CGenEffect with BaseGenFunctions {
  val IR: FunctionsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@Lambda(fun, x, y) =>
      stream.println(remap(y.tp)+" "+quote(sym)+"("+remap(x.tp)+" "+quote(x)+") {")
      emitBlock(y)
      val z = getBlockResult(y)
      if (remap(z.tp) != "void")
        stream.println("return " + quote(z) + ";")
      stream.println("}")
    case Apply(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenTupledFunctions extends CGenFunctions with GenericGenUnboxedTupleAccess {
  val IR: TupledFunctionsExp
  import IR._

  /*override def quote(x: Exp[Any]) : String = x match {
    case UnboxedTuple(t) => t.map(quote).mkString("((", ",", "))")
    case _ => super.quote(x)
  }*/

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Lambda(fun, UnboxedTuple(xs), y) =>
      stream.println(remap(y.tp)+" "+quote(sym)+"("+xs.map(s=>remap(s.tp)+" "+quote(s)).mkString(",")+") {")
      emitBlock(y)
      val z = getBlockResult(y)
      if (remap(z.tp) != "void")
        stream.println("return " + quote(z) + ";")
      stream.println("}")
    case Apply(fun, UnboxedTuple(args)) =>
      emitValDef(sym, quote(fun) + args.map(quote).mkString("(", ",", ")"))
    case _ => super.emitNode(sym,rhs)
  }

  /*def unwrapTupleStr(s: String): Array[String] = {
    if (s.startsWith("scala.Tuple")) s.slice(s.indexOf("[")+1,s.length-1).filter(c => c != ' ').split(",")
    else Array(s)
  }*/

  /*override def remap[A](m: Manifest[A]): String = m.toString match {
    case f if f.startsWith("scala.Function") =>
      val targs = m.typeArguments.dropRight(1)
      val res = remap(m.typeArguments.last)
      val targsUnboxed = targs.flatMap(t => unwrapTupleStr(remap(t)))
      val sep = if (targsUnboxed.length > 0) "," else ""
      "scala.Function" + (targsUnboxed.length) + "[" + targsUnboxed.mkString(",") + sep + res + "]"

    case _ => super.remap(m)
  }*/
}
