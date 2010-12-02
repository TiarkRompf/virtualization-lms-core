package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{CudaGenEffect, GenericNestedCodegen, ScalaGenEffect}

trait RangeOps extends Base {
  // workaround for infix not working with manifests
  implicit def repRangeToRangeOpsCls(r: Rep[Range]) = new rangeOpsCls(r)
  class rangeOpsCls(r: Rep[Range]){
    def foreach(f: Rep[Int] => Rep[Unit]) = range_foreach(r, f)
  }

  def infix_until(start: Rep[Int], end: Rep[Int]) = range_until(start,end)
  def infix_start(r: Rep[Range]) = range_start(r)
  def infix_step(r: Rep[Range]) = range_step(r)
  def infix_end(r: Rep[Range]) = range_end(r)
  //def infix_foreach(r: Rep[Range], f: Rep[Int] => Rep[Unit]) = range_foreach(r, f)

  def range_until(start: Rep[Int], end: Rep[Int]): Rep[Range]
  def range_start(r: Rep[Range]) : Rep[Int]
  def range_step(r: Rep[Range]) : Rep[Int]
  def range_end(r: Rep[Range]) : Rep[Int]
  def range_foreach(r: Rep[Range], f: (Rep[Int]) => Rep[Unit]): Rep[Unit]
}

trait RangeOpsExp extends RangeOps with FunctionsExp {
  case class Until(start: Exp[Int], end: Exp[Int]) extends Def[Range]
  case class RangeStart(r: Exp[Range]) extends Def[Int]
  case class RangeStep(r: Exp[Range]) extends Def[Int]
  case class RangeEnd(r: Exp[Range]) extends Def[Int]
  case class RangeForeach(r: Exp[Range], i: Exp[Int], body: Exp[Unit]) extends Def[Unit]

  def range_until(start: Exp[Int], end: Exp[Int]) : Exp[Range] = Until(start, end)
  def range_start(r: Exp[Range]) : Exp[Int] = RangeStart(r)
  def range_step(r: Exp[Range]) : Exp[Int] = RangeStep(r)
  def range_end(r: Exp[Range]) : Exp[Int] = RangeEnd(r)
  def range_foreach(r: Exp[Range], block: Exp[Int] => Exp[Unit]) : Exp[Unit] = {
    val i = fresh[Int]
    reflectEffect(RangeForeach(r, i, reifyEffects(block(i))))
  }
}

trait BaseGenRangeOps extends GenericNestedCodegen {
  val IR: RangeOpsExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case RangeForeach(r, i, body) if shallow => syms(r) // in shallow mode, don't count deps from nested blocks
    case _ => super.syms(e)
  }

  override def getFreeVarNode(rhs: Def[_]): List[Sym[_]] = rhs match {
    case RangeForeach(r, i, body) => getFreeVarBlock(body,List(i.asInstanceOf[Sym[_]]))
    case _ => super.getFreeVarNode(rhs)
  }
}

trait ScalaGenRangeOps extends ScalaGenEffect with BaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Until(start, end) => emitValDef(sym, "" + quote(start) + " until " + quote(end))
    
    // could generate as a while loop instead
    case RangeForeach(r, i, body) => {
      stream.println("val " + quote(sym) + " = " + quote(r) + ".foreach{ " + quote(i) + ": Int =>")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println("}")
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenRangeOps extends CudaGenEffect with BaseGenRangeOps {
  val IR: RangeOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Until(start, end) =>
      if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
      else {
        stream.println(addTab()+"int %s_start = %s;".format(quote(sym), quote(start)))
        stream.println(addTab()+"int %s_end = %s;".format(quote(sym), quote(end)))
      }
      // Do nothing: will be handled by RangeForeach

    // TODO: What if the range is not continuous integer set?
    case RangeForeach(r, i, body) => {
      if(!isGPUable) throw new RuntimeException("CudaGen: Not GPUable")
      else {
        //var freeVars = buildScheduleForResult(body).filter(scope.contains(_)).map(_.sym)
        val freeVars = getFreeVarBlock(body,List(i.asInstanceOf[Sym[_]]))

        // Add the variables of range to the free variable list if necessary
        var paramList = freeVars
        val Until(startIdx,endIdx) = findDefinition(r.asInstanceOf[Sym[Range]]).map(_.rhs).get.asInstanceOf[Until]
        if(startIdx.isInstanceOf[Sym[_]]) paramList = startIdx.asInstanceOf[Sym[_]] :: paramList
        if(endIdx.isInstanceOf[Sym[_]]) paramList = endIdx.asInstanceOf[Sym[_]] :: paramList
        paramList = paramList.distinct
        val paramListStr = paramList.map(ele=>CudaType(ele.Type.toString) + " " + quote(ele)).mkString(", ")

        if(parallelFor) {
          //stream.println("__global__ gpuKernel_%s(%s) {".format(quote(sym),paramListStr))
          tabWidth += 1
          stream.println(addTab()+"int %s = blockIdx.x*blockDim.x + threadIdx.x;".format(quote(i)))
          //stream.println(addTab() + "%s = %s + %s;".format(quote(i), quote(i), quote(startIdx)))
          //stream.println(addTab()+"if(%s < %s) {".format(quote(i), quote(endIdx)))
          stream.println(addTab() + "%s = %s + %s;".format(quote(i), quote(i), quote(r)+"_start"))
          stream.println(addTab()+"if(%s < %s) {".format(quote(i), quote(r)+"_end"))
          tabWidth += 1
          // No parallelism in the inner block
          parallelFor = true
          emitBlock(body)
          parallelFor = false
          tabWidth -= 1
          stream.println(addTab()+"}")
          tabWidth -= 1
          stream.println(addTab()+"}")
        }
        else {
          stream.println(addTab()+"for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(startIdx),quote(i),quote(endIdx),quote(i)))
          tabWidth += 1
          emitBlock(body)
          tabWidth -= 1
          stream.println(addTab() + "}")
        }
      }
    }
    case _ => super.emitNode(sym, rhs)
  }
}
