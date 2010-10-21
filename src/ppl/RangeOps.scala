package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import java.io.PrintWriter


trait RangeOps extends Base {
  implicit def repRangeToRepRangeOps(r: Rep[Range]) = new RepRangeOpsCls(r)
  implicit def rangeToRepRangeOps(r: Range) = new RepRangeOpsCls(r)

  def infix_until(start: Rep[Int], end: Rep[Int]) = range_until(start,end)

  class RepRangeOpsCls(r: Rep[Range]) {
    def start : Rep[Int] = range_start(r)
    def step : Rep[Int] = range_step(r)
    def end : Rep[Int] = range_end(r)
    def foreach(f: (Rep[Int])=>Rep[Unit]):Rep[Unit] = range_foreach(r,f)
  }

  def range_until(start: Rep[Int], end: Rep[Int]): Rep[Range]
  def range_start(r: Rep[Range]) : Rep[Int]
  def range_step(r: Rep[Range]) : Rep[Int]
  def range_end(r: Rep[Range]) : Rep[Int]
  def range_foreach(r: Rep[Range], f: (Rep[Int]) => Rep[Unit]): Rep[Unit]
}

trait RangeOpsExp extends RangeOps with BaseExp { this: FunctionsExp =>  // TODO: don't need functions, can do first-order
  case class Until(start: Exp[Int], end: Exp[Int]) extends Def[Range]
  case class RangeStart(r: Exp[Range]) extends Def[Int]
  case class RangeStep(r: Exp[Range]) extends Def[Int]
  case class RangeEnd(r: Exp[Range]) extends Def[Int]
  case class RangeForeach(r: Exp[Range], block: Exp[Int => Unit]) extends Def[Unit]

  def range_until(start: Exp[Int], end: Exp[Int]) : Rep[Range] = Until(start, end)
  def range_start(r: Exp[Range]) : Rep[Int] = RangeStart(r)
  def range_step(r: Exp[Range]) : Rep[Int] = RangeStep(r)
  def range_end(r: Exp[Range]) : Rep[Int] = RangeEnd(r)
  def range_foreach(x: Exp[Range], block: Exp[Int] => Exp[Unit]) : Rep[Unit] = reflectEffect(RangeForeach(x, doLambda(block)))

}

trait ScalaGenRange extends ScalaGenBase {
  val IR: RangeOpsExp
  import IR._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case Until(start, end) => emitValDef(sym, "" + quote(start) + " until " + quote(end))
    case RangeForeach(r,f) => {
      stream.println("val " + quote(sym) + " = " + quote(r) + ".foreach{ ")
      emitBlock(f)
      stream.println(quote(getBlockResult(f)))
      stream.println("}")
    }
    case _ => super.emitNode(sym, rhs)
  }
}
