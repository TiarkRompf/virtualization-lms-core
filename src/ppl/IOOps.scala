package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._
import java.io.{BufferedReader, FileReader, PrintWriter}

trait IOOps extends Base {

  /**
   * BufferedReader
   */
  implicit def repBrToRepBrOps(b: Rep[BufferedReader]) = new RepBrOpsCls(b)
  implicit def brToRepBrOps(b: BufferedReader) = new RepBrOpsCls(b)

  object BufferedReader {
    def apply(f: Rep[FileReader]) = obj_br_apply(f)
  }

  class RepBrOpsCls(b: Rep[BufferedReader]) {
    def readLine() = br_readline(b)
    def close() = br_close(b)
  }
  def obj_br_apply(f: Rep[FileReader]) : Rep[BufferedReader]
  def br_readline(b: Rep[BufferedReader]) : Rep[String]
  def br_close(b: Rep[BufferedReader]) : Rep[Unit]

  /**
   * FileReader
   */
  object FileReader {
    def apply(s: Rep[String]) = obj_fr_apply(s)
  }
  def obj_fr_apply(s: Rep[String]) : Rep[FileReader]
}

trait IOOpsExp extends IOOps with EffectExp with DSLOpsExp {
  case class ObjBrApply(f: Exp[FileReader])
    extends DSLOp[FileReader,BufferedReader](f => External[BufferedReader]("new java.io.BufferedReader(%s)", List(f)), f)

  case class ObjFrApply(s: Exp[String])
    extends DSLOp[String,FileReader](s => External[FileReader]("new java.io.FileReader(%s)", List(s)), s)

  case class BrReadline(b: Exp[BufferedReader]) extends Def[String]
  case class BrClose(b: Exp[BufferedReader]) extends Def[Unit]

  def obj_br_apply(f: Exp[FileReader]) : Rep[BufferedReader] = reflectEffect(ObjBrApply(f))
  def obj_fr_apply(s: Exp[String]) : Rep[FileReader] = reflectEffect(ObjFrApply(s))
  def br_readline(b: Exp[BufferedReader]) : Rep[String] = reflectEffect(BrReadline(b))
  def br_close(b: Exp[BufferedReader]) : Rep[Unit] = reflectEffect(BrClose(b))
}

trait ScalaGenIO extends ScalaGenEffect with IOOpsExp {

  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case BrReadline(b) => emitValDef(sym, quote(b) + ".readLine()")
    case BrClose(b) => emitValDef(sym, quote(b) + ".close()")
    case _ => super.emitNode(sym, rhs)
  }
}
