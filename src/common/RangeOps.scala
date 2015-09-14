package scala.lms
package common

import java.io.PrintWriter

import scala.lms.internal._
import scala.lms.codegen.GenericCodegen
import scala.reflect.SourceContext

trait RangeOps extends Base {
  implicit def rangeTyp: Typ[Range]

  // workaround for infix not working with manifests
  implicit def repRangeToRangeOps(r: Rep[Range]) = new rangeOpsCls(r)
  class rangeOpsCls(r: Rep[Range]){
    def foreach(f: Rep[Int] => Rep[Unit])(implicit pos: SourceContext) = range_foreach(r, f)
  }

  def infix_until(start: Rep[Int], end: Rep[Int])(implicit pos: SourceContext) = range_until(start,end)
  def infix_start(r: Rep[Range])(implicit pos: SourceContext) = range_start(r)
  def infix_step(r: Rep[Range])(implicit pos: SourceContext) = range_step(r)
  def infix_end(r: Rep[Range])(implicit pos: SourceContext) = range_end(r)
  //def infix_foreach(r: Rep[Range], f: Rep[Int] => Rep[Unit]) = range_foreach(r, f)

  def range_until(start: Rep[Int], end: Rep[Int])(implicit pos: SourceContext): Rep[Range]
  def range_start(r: Rep[Range])(implicit pos: SourceContext) : Rep[Int]
  def range_step(r: Rep[Range])(implicit pos: SourceContext) : Rep[Int]
  def range_end(r: Rep[Range])(implicit pos: SourceContext) : Rep[Int]
  def range_foreach(r: Rep[Range], f: (Rep[Int]) => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
}

trait RangeOpsExp extends RangeOps with PrimitiveOps with BaseExp {
  implicit def rangeTyp: Typ[Range] = manifestTyp

  case class Until(start: Exp[Int], end: Exp[Int]) extends Def[Range]
  case class RangeStart(r: Exp[Range]) extends Def[Int]
  case class RangeStep(r: Exp[Range]) extends Def[Int]
  case class RangeEnd(r: Exp[Range]) extends Def[Int]
  //case class RangeForeach(r: Exp[Range], i: Exp[Int], body: Exp[Unit]) extends Def[Unit]
  case class RangeForeach(start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Block[Unit]) extends Def[Unit]

  def range_until(start: Exp[Int], end: Exp[Int])(implicit pos: SourceContext) : Exp[Range] = toAtom(Until(start, end))
  def range_start(r: Exp[Range])(implicit pos: SourceContext) : Exp[Int] = r match {
    case Def(Until(start, end)) => start
    case Def(Reflect(Until(start, end), u, es)) => start
    case _ => toAtom(RangeStart(r))
  }
  def range_step(r: Exp[Range])(implicit pos: SourceContext) : Exp[Int] = toAtom(RangeStep(r))
  def range_end(r: Exp[Range])(implicit pos: SourceContext) : Exp[Int] = r match {
    case Def(Until(start, end)) => end
    case Def(Reflect(Until(start, end), u, es)) => end
    case _ => toAtom(RangeEnd(r))
  }
  def range_foreach(r: Exp[Range], block: Exp[Int] => Exp[Unit])(implicit pos: SourceContext) : Exp[Unit] = {
    val i = fresh[Int]
    val a = reifyEffects(block(i))
    reflectEffect(RangeForeach(range_start(r), range_end(r), i, a), summarizeEffects(a).star)
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(RangeForeach(s,e,i,b), u, es) => reflectMirrored(Reflect(RangeForeach(f(s),f(e),f(i).asInstanceOf[Sym[Int]],f(b)), mapOver(f,u), f(es)))(mtype(typ[A]), pos)
    case Reflect(RangeStart(r), u, es) => reflectMirrored(Reflect(RangeStart(f(r)), mapOver(f,u), f(es)))(mtype(typ[A]), pos)
    case Reflect(RangeStep(r), u, es) => reflectMirrored(Reflect(RangeStep(f(r)), mapOver(f,u), f(es)))(mtype(typ[A]), pos)
    case Reflect(RangeEnd(r), u, es) => reflectMirrored(Reflect(RangeEnd(f(r)), mapOver(f,u), f(es)))(mtype(typ[A]), pos)
    case Reflect(Until(s,e), u, es) => reflectMirrored(Reflect(Until(f(s),f(e)), mapOver(f,u), f(es)))(mtype(typ[A]), pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  override def syms(e: Any): List[Sym[Any]] = e match {
    case RangeForeach(start, end, i, body) => syms(start):::syms(end):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case RangeForeach(start, end, i, y) => i :: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case RangeForeach(start, end, i, body) => freqNormal(start):::freqNormal(end):::freqHot(body)
    case _ => super.symsFreq(e)
  }


}

trait BaseGenRangeOps extends GenericCodegen {
  val IR: RangeOpsExp
  import IR._

}

trait ScalaGenRangeOps extends ScalaGenBase with BaseGenRangeOps {
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) => emitValDef(sym, src"$start until $end")

    /*
    case RangeForeach(r, i, body) => {
      stream.println("val " + quote(sym) + " = " + quote(r) + ".foreach{ " + quote(i) + ": Int =>")
      emitBlock(body)
      stream.println(quote(getBlockResult(body)))
      stream.println("}")
    }
    */

    case RangeForeach(start, end, i, body) => {
      // do not need to print unit result
      //stream.println(quote(getBlockResult(body)))
      gen"""var $i : Int = $start
           |val $sym = while ($i < $end) {
           |${nestedBlock(body)}
           |$i = $i + 1
           |}"""
    }

    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenRangeOps extends CudaGenBase with BaseGenRangeOps {
  val IR: RangeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) =>
        gen"""${addTab()}int ${sym}_start = $start;
             |${addTab()}int ${sym}_end = $end;"""
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
        gen"${addTab()}for(int $i=$start; $i < $end; $i++) {"
        tabWidth += 1
        emitBlock(body)
        tabWidth -= 1
        gen"${addTab()}}"
    }
    case _ => super.emitNode(sym, rhs)
  }
}

trait OpenCLGenRangeOps extends OpenCLGenBase with BaseGenRangeOps {
  val IR: RangeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) =>
      throw new GenerationFailedException("OpenCLGenRangeOps: Range vector is not supported")
    case RangeForeach(start, end, i, body) =>
      gen"""for(int $i=$start; $i < $end; $i++) {
           |${nestedBlock(body)}
           |}"""

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenRangeOps extends CGenBase with BaseGenRangeOps {
  val IR: RangeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) =>
      throw new GenerationFailedException("CGenRangeOps: Range vector is not supported")
    case RangeForeach(start, end, i, body) =>
      gen"""for(int $i=$start; $i < $end; $i++) {
           |${nestedBlock(body)}
           |}"""

    case _ => super.emitNode(sym, rhs)
  }
}
