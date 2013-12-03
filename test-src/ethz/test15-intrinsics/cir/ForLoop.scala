/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Alen Stojanov  (astojanov@inf.ethz.ch)
 *                      Georg Ofenbeck (ofenbeck@inf.ethz.ch)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see http://www.gnu.org/licenses/.
 */

package etgz.test15.cir

import scala.virtualization.lms.common._
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.GenericNestedCodegen

trait ForLoopOps extends FunctionsExp {

  implicit def rangeCons(start: Int) = new RangeOps(unit(start))
  implicit def rangeCons(start: Rep[Int]) = new RangeOps(start)
  class RangeOps(start: Rep[Int]) {
    def genuntil(end: Rep[Int]) = new {
      def foreach(f: Rep[Int] => Rep[Unit])(implicit pos: SourceContext) = for_loop(start, end, fresh[Int], f, true)
    }
    def until(end: Rep[Int]) = new {
      def foreach(f: Rep[Int] => Rep[Unit])(implicit pos: SourceContext) = for_loop(start, end, fresh[Int], f, true)
    }
  }


  def forloop (end: Rep[Int], f: Rep[Int] => Rep[Unit], staged: Boolean = true) : Rep[Unit] = for_loop(Const(0), end, fresh[Int], f, staged)
  def for_loop(start: Rep[Int], end: Rep[Int], i: Sym[Int], f: Rep[Int] => Rep[Unit], staged: Boolean = true): Rep[Unit]
}

trait ForLoopExp extends ForLoopOps {
  //case class ForLoop(body: Block[Unit], iterator: Sym[Int], range: Exp[Int]) (implicit ctx: SourceContext) extends Def[Int]
  case class ForLoop(start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Block[Unit]) extends Def[Unit]

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ForLoop(start, end, i, y) => i :: effectSyms(y)
    case _ => super.boundSyms(e)
  }

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case e@ForLoop(start, end, i, y) => ForLoop(f(start), f(end), f(i).asInstanceOf[Sym[Int]], f(y))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]] // why??


  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@ForLoop(start, end, i, y) => infix_for_loop(f(start), f(end), f(i).asInstanceOf[Sym[Int]], f(y))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  def for_loop (start: Rep[Int], end: Rep[Int], i: Sym[Int], block: Rep[Int] => Rep[Unit], staged: Boolean) : Exp[Unit] = {
    if ( staged ) (start, end) match {
      case (Const(s:Int), Const(e:Int)) if (e - s == 1) => block(start)
      case _ => {
        val a = reifyEffects(block(i))
        reflectEffect(ForLoop(start, end, i, a), summarizeEffects(a).star)
      }
    } else
    {
      (start, end) match {
      case (Const(s:Int), Const(e:Int)) => for (i <- s until e) block(Const(i))
      case _ => assert(false, "Loop can not be automatically unrolled")
    }
    }
  }
  def infix_for_loop(start: Exp[Int], end: Exp[Int], i: Sym[Int], body: Block[Unit]) = ForLoop(start, end, i, body)
}


trait CGenForOps extends CGenEffect with GenericNestedCodegen {

  val IR: ForLoopExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ForLoop(start, end, i, body) =>
      // stream.println("#pragma ivdep")
      stream.println("for(int %s=%s; %s < %s; %s++) {".format(quote(i),quote(start),quote(i),quote(end),quote(i)))
      emitBlock(body)
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}

trait ForLoopFatExp extends ForLoopExp with LoopsFatExp

trait GenForLoopFat extends BaseGenLoopsFat {
  val IR: ForLoopFatExp
  import IR._
}
