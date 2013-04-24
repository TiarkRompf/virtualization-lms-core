package scala.lms
package targets.clike.codegen

import internal.{GenericNestedCodegen, GenerationFailedException}
import ops.{BaseGenRangeOps, RangeOpsExp}

import java.io.PrintWriter

trait CudaGenRangeOps extends CudaGenEffect with BaseGenRangeOps {
  val IR: RangeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) =>
        stream.println(addTab()+"int %s_start = %s;".format(quote(sym), quote(start)))
        stream.println(addTab()+"int %s_end = %s;".format(quote(sym), quote(end)))
        // Do nothing: will be handled by RangeForeach

    // TODO: What if the range is not continuous integer set?
    case RangeForeach(start, end, i, body) => {
      /*
        //var freeVars = buildScheduleForResult(body).filter(scope.contains(_)).map(_.sym)
        val freeVars = getFreeVarBlock(body,List(i.asInstanceOf[Sym[Any]]))
        // Add the variables of range to the free variable list if necessary
        var paramList = freeVars
        //val Until(startIdx,endIdx) = findDefinition(r.asInstanceOf[Sym[Range]]).map(_.rhs).get.asInstanceOf[Until]
        if(start.isInstanceOf[Sym[Any]]) paramList = start.asInstanceOf[Sym[Any]] :: paramList
        if(end.isInstanceOf[Sym[Any]]) paramList = end.asInstanceOf[Sym[Any]] :: paramList
        paramList = paramList.distinct
        val paramListStr = paramList.map(ele=>remap(ele.tp) + " " + quote(ele)).mkString(", ")
        */
        stream.println(addTab()+"for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
        tabWidth += 1
        emitBlock(body)
        tabWidth -= 1
        stream.println(addTab() + "}")
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenRangeOps extends OpenCLGenEffect with BaseGenRangeOps {
  val IR: RangeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) =>
      throw new GenerationFailedException("OpenCLGenRangeOps: Range vector is not supported")
    case RangeForeach(start, end, i, body) =>
      stream.println("for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      emitBlock(body)
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenRangeOps extends CGenEffect with BaseGenRangeOps {
  val IR: RangeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) =>
      throw new GenerationFailedException("CGenRangeOps: Range vector is not supported")
    case RangeForeach(start, end, i, body) =>
      stream.println("for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      emitBlock(body)
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}
