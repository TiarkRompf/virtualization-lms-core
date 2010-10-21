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

trait IOOpsExp extends IOOps with EffectExp {
  case class ObjBrApply(f: Exp[FileReader]) extends Def[BufferedReader]
  case class BrReadline(b: Exp[BufferedReader]) extends Def[String]
  case class BrClose(b: Exp[BufferedReader]) extends Def[Unit]
  case class ObjFrApply(f: Exp[String]) extends Def[FileReader]

  // FIXME: this will need to reifyEffect

  def obj_br_apply(f: Exp[FileReader]) : Rep[BufferedReader] = ObjBrApply(f)
  def br_readline(b: Exp[BufferedReader]) : Rep[String] = BrReadline(b)
  def br_close(b: Exp[BufferedReader]) : Rep[Unit] = BrClose(b)
  def obj_fr_apply(s: Exp[String]) : Rep[FileReader] = ObjFrApply(s)

}

trait ScalaGenIO extends ScalaGenEffect {
  val IR: IOOpsExp
  import IR._
  
  abstract override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    // TODO: fill out
    case _ => super.emitNode(sym, rhs)
  }
}
