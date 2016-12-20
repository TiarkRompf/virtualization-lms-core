package scala.lms
package common

import java.io.PrintWriter

import scala.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.reflect.SourceContext

trait RangeOps extends Base {
  trait LongRange

  implicit class rangeOpsCls(r: Rep[Range]) {
    def foreach(f: Rep[Int] => Rep[Unit])(implicit pos: SourceContext) = range_foreach(r, f)
  }
  implicit class lrangeOpsCls(r: Rep[LongRange]) {
    def foreach(f: Rep[Long] => Rep[Unit])(implicit pos: SourceContext) = lrange_foreach(r, f)
    def parforeach(f: Rep[Long] => Rep[Unit])(implicit pos: SourceContext) = lrange_par_foreach(r, f)
  }

  def infix_until(start: Rep[Int], end: Rep[Int])(implicit pos: SourceContext) = range_until(start,end)
  def infix_until(start: Rep[Long], end: Rep[Long])(implicit pos: SourceContext, o: Overloaded1) = lrange_until(start,end)
  def infix_start(r: Rep[Range])(implicit pos: SourceContext) = range_start(r)
  def infix_start(r: Rep[LongRange])(implicit pos: SourceContext, o: Overloaded1) = lrange_start(r)
  def infix_step(r: Rep[Range])(implicit pos: SourceContext) = range_step(r)
  def infix_step(r: Rep[LongRange])(implicit pos: SourceContext, o: Overloaded1) = lrange_step(r)
  def infix_end(r: Rep[Range])(implicit pos: SourceContext) = range_end(r)
  def infix_end(r: Rep[LongRange])(implicit pos: SourceContext, o: Overloaded1) = lrange_end(r)

  def range_until(start: Rep[Int], end: Rep[Int])(implicit pos: SourceContext): Rep[Range]
  def range_start(r: Rep[Range])(implicit pos: SourceContext) : Rep[Int]
  def range_step(r: Rep[Range])(implicit pos: SourceContext) : Rep[Int]
  def range_end(r: Rep[Range])(implicit pos: SourceContext) : Rep[Int]
  def range_foreach(r: Rep[Range], f: (Rep[Int]) => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]

  def lrange_until(start: Rep[Long], end: Rep[Long])(implicit pos: SourceContext): Rep[LongRange]
  def lrange_start(r: Rep[LongRange])(implicit pos: SourceContext) : Rep[Long]
  def lrange_step(r: Rep[LongRange])(implicit pos: SourceContext) : Rep[Long]
  def lrange_end(r: Rep[LongRange])(implicit pos: SourceContext) : Rep[Long]
  def lrange_foreach(r: Rep[LongRange], f: (Rep[Long]) => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]

  def lrange_par_until(start: Rep[Long], end: Rep[Long])(implicit pos: SourceContext): Rep[LongRange]
  def lrange_par_start(r: Rep[LongRange])(implicit pos: SourceContext) : Rep[Long]
  def lrange_par_step(r: Rep[LongRange])(implicit pos: SourceContext) : Rep[Long]
  def lrange_par_end(r: Rep[LongRange])(implicit pos: SourceContext) : Rep[Long]
  def lrange_par_foreach(r: Rep[LongRange], f: (Rep[Long]) => Rep[Unit])(implicit pos: SourceContext): Rep[Unit]
}

trait RangeOpsExp extends RangeOps with FunctionsExp {
  case class Until(start: Exp[Int], end: Exp[Int]) extends Def[Range]
  case class RangeStart(r: Exp[Range]) extends Def[Int]
  case class RangeStep(r: Exp[Range]) extends Def[Int]
  case class RangeEnd(r: Exp[Range]) extends Def[Int]
  case class RangeForeach(start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Block[Unit]) extends Def[Unit]

  case class LongUntil(start: Exp[Long], end: Exp[Long]) extends Def[LongRange]
  case class LongRangeStart(r: Exp[LongRange]) extends Def[Long]
  case class LongRangeStep(r: Exp[LongRange]) extends Def[Long]
  case class LongRangeEnd(r: Exp[LongRange]) extends Def[Long]
  case class LongRangeForeach(start: Exp[Long], end: Exp[Long], i: Sym[Long], body: Block[Unit]) extends Def[Unit]

  case class LongParUntil(start: Exp[Long], end: Exp[Long]) extends Def[LongRange]
  case class LongRangeParStart(r: Exp[LongRange]) extends Def[Long]
  case class LongRangeParStep(r: Exp[LongRange]) extends Def[Long]
  case class LongRangeParEnd(r: Exp[LongRange]) extends Def[Long]
  case class LongRangeParForeach(start: Exp[Long], end: Exp[Long], i: Sym[Long], body: Block[Unit]) extends Def[Unit]

  def lrange_par_until(start: Exp[Long], end: Exp[Long])(implicit pos: SourceContext) : Exp[LongRange] = LongParUntil(start, end)
  def lrange_par_start(r: Exp[LongRange])(implicit pos: SourceContext) : Exp[Long] = r match {
    case Def(LongParUntil(start, end)) => start
    case _ => LongRangeParStart(r)
  }
  def lrange_par_step(r: Exp[LongRange])(implicit pos: SourceContext) : Exp[Long] = LongRangeParStep(r)
  def lrange_par_end(r: Exp[LongRange])(implicit pos: SourceContext) : Exp[Long] = r match {
    case Def(LongParUntil(start, end)) => end
    case _ => LongRangeParEnd(r)
  }
  def lrange_par_foreach(r: Exp[LongRange], block: Exp[Long] => Exp[Unit])(implicit pos: SourceContext): Exp[Unit] = {
    val i = fresh[Long]
    val a = reifyEffects(block(i))
    reflectEffect(LongRangeParForeach(r.start, r.end, i, a), summarizeEffects(a).star)
  }

  def range_until(start: Exp[Int], end: Exp[Int])(implicit pos: SourceContext) : Exp[Range] = Until(start, end)
  def range_start(r: Exp[Range])(implicit pos: SourceContext) : Exp[Int] = r match {
    case Def(Until(start, end)) => start
    case _ => RangeStart(r)
  }
  def range_step(r: Exp[Range])(implicit pos: SourceContext) : Exp[Int] = RangeStep(r)
  def range_end(r: Exp[Range])(implicit pos: SourceContext) : Exp[Int] = r match {
    case Def(Until(start, end)) => end
    case _ => RangeEnd(r)
  }
  def range_foreach(r: Exp[Range], block: Exp[Int] => Exp[Unit])(implicit pos: SourceContext) : Exp[Unit] = {
    val i = fresh[Int]
    val a = reifyEffects(block(i))
    reflectEffect(RangeForeach(r.start, r.end, i, a), summarizeEffects(a).star)
  }

  def lrange_until(start: Exp[Long], end: Exp[Long])(implicit pos: SourceContext) : Exp[LongRange] = LongUntil(start, end)
  def lrange_start(r: Exp[LongRange])(implicit pos: SourceContext) : Exp[Long] = r match {
    case Def(LongUntil(start, end)) => start
    case _ => LongRangeStart(r)
  }
  def lrange_step(r: Exp[LongRange])(implicit pos: SourceContext) : Exp[Long] = LongRangeStep(r)
  def lrange_end(r: Exp[LongRange])(implicit pos: SourceContext) : Exp[Long] = r match {
    case Def(LongUntil(start, end)) => end
    case _ => LongRangeEnd(r)
  }
  def lrange_foreach(r: Exp[LongRange], block: Exp[Long] => Exp[Unit])(implicit pos: SourceContext) : Exp[Unit] = {
    val i = fresh[Long]
    val a = reifyEffects(block(i))
    reflectEffect(LongRangeForeach(r.start, r.end, i, a), summarizeEffects(a).star)
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case Reflect(RangeForeach(s,e,i,b), u, es) => reflectMirrored(Reflect(RangeForeach(f(s),f(e),f(i).asInstanceOf[Sym[Int]],f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(LongRangeForeach(s,e,i,b), u, es) => reflectMirrored(Reflect(LongRangeForeach(f(s),f(e),f(i).asInstanceOf[Sym[Long]],f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(LongRangeParForeach(s,e,i,b), u, es) => reflectMirrored(Reflect(LongRangeParForeach(f(s),f(e),f(i).asInstanceOf[Sym[Long]],f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  override def syms(e: Any): List[Sym[Any]] = e match {
    case RangeForeach(start, end, i, body) => syms(start):::syms(end):::syms(body)
    case LongRangeForeach(start, end, i, body) => syms(start):::syms(end):::syms(body)
    case LongRangeParForeach(start, end, i, body) => syms(start):::syms(end):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case RangeForeach(start, end, i, y) => i :: effectSyms(y)
    case LongRangeForeach(start, end, i, y) => i :: effectSyms(y)
    case LongRangeParForeach(start, end, i, y) => i :: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case RangeForeach(start, end, i, body) => freqNormal(start):::freqNormal(end):::freqHot(body)
    case LongRangeForeach(start, end, i, body) => freqNormal(start):::freqNormal(end):::freqHot(body)
    case LongRangeParForeach(start, end, i, body) => freqNormal(start):::freqNormal(end):::freqHot(body)
    case _ => super.symsFreq(e)
  }

}

trait BaseGenRangeOps extends GenericNestedCodegen {
  val IR: RangeOpsExp
  import IR._

}

trait ScalaGenRangeOps extends ScalaGenEffect with BaseGenRangeOps {
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
      gen"var $i : Int = $start"
      emitValDef(sym, src"while ($i < $end) {")
      gen"""${nestedBlock(body)}
      |$i = $i + 1
      |}"""
    }

    case _ => super.emitNode(sym, rhs)
  }
}

trait CudaGenRangeOps extends CudaGenEffect with BaseGenRangeOps {
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

trait OpenCLGenRangeOps extends OpenCLGenEffect with BaseGenRangeOps {
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

trait CGenRangeOps extends CGenEffect with BaseGenRangeOps {
  val IR: RangeOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Until(start, end) =>
      throw new GenerationFailedException("CGenRangeOps: Range vector is not supported")
    case RangeForeach(start, end, i, body) =>
      // Some compilers don't like the initialization inside for
      stream.println(remap(i.tp) + " " + quote(i) + ";")
      gen"""for($i=$start; $i < $end; $i++) {
        |${nestedBlock(body)}
      |}"""
    case LongRangeForeach(start, end, i, body) =>
      // Some compilers don't like the initialization inside for
      stream.println(remap(i.tp) + " " + quote(i) + ";")
      gen"""for($i=$start; $i < $end; $i++) {
        |${nestedBlock(body)}
      |}"""
    case LongRangeParForeach(start, end, i, body) =>
      // Some compilers don't like the initialization inside for
      stream.println(remap(i.tp) + " " + quote(i) + ";")
      stream.println("#pragma omp parallel for private(" + quote(i) + ")")
      gen"""for($i=$start; $i < $end; $i++) {
        |${nestedBlock(body)}
      |}"""
    case _ => super.emitNode(sym, rhs)
  }
}
