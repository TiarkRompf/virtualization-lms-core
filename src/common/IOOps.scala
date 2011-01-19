package scala.virtualization.lms
package common

import java.io.{FileReader, FileWriter, BufferedReader, BufferedWriter, PrintWriter}
import scala.virtualization.lms.internal.{GenerationFailedException}
import util.OverloadHack

trait IOOps extends Variables with OverloadHack {

  /**
   * BufferedReader
   */
  object BufferedReader {
    def apply(f: Rep[FileReader]) = obj_br_apply(f)
  }
  def infix_readLine(b: Rep[BufferedReader]) = br_readline(b)
  def infix_close(b: Rep[BufferedReader]) = br_close(b)

  def obj_br_apply(f: Rep[FileReader]): Rep[BufferedReader]
  def br_readline(b: Rep[BufferedReader]): Rep[String]
  def br_close(b: Rep[BufferedReader]): Rep[Unit]

  /**
   * BufferedWriter
   */
  object BufferedWriter {
    def apply(f: Rep[FileWriter]) = obj_bw_apply(f)    
  }

  def infix_write(b: Rep[BufferedWriter], s: Rep[String]) = bw_write(b,s)
  def infix_close(b: Rep[BufferedWriter])(implicit o: Overloaded1) = bw_close(b)

  /*
  implicit def varToRepBufferedWriterOps(x: Var[BufferedWriter]) = new RepBufferedWriterOpsCls(readVar(x))
  implicit def repBufferedWriterToRepBufferedWriterOps(a: Rep[BufferedWriter]) = new RepBufferedWriterOpsCls(a)
  implicit def BufferedWriterToRepBufferedWriterOps(a: BufferedWriter) = new RepBufferedWriterOpsCls(a)
  
  class RepBufferedWriterOpsCls(b: Rep[BufferedWriter]){
    def write(s: Rep[String]) = bw_write(b,s)
  }
  */

  def obj_bw_apply(f: Rep[FileWriter]): Rep[BufferedWriter]
  def bw_write(b: Rep[BufferedWriter], s: Rep[String]): Rep[Unit]
  def bw_close(b: Rep[BufferedWriter]): Rep[Unit]

  /**
   * FileReader
   */
  object FileReader {
    def apply(s: Rep[String]) = obj_fr_apply(s)
  }
  def obj_fr_apply(s: Rep[String]): Rep[FileReader]

  /**
   * FileWriter
   */
  object FileWriter {
    def apply(s: Rep[String]) = obj_fw_apply(s)
  }
  def obj_fw_apply(s: Rep[String]): Rep[FileWriter]
}

trait IOOpsExp extends IOOps with DSLOpsExp {
  case class ObjBrApply(f: Exp[FileReader]) extends Def[BufferedReader]
  case class ObjBwApply(f: Exp[FileWriter]) extends Def[BufferedWriter]
  case class ObjFrApply(s: Exp[String]) extends Def[FileReader]
  case class ObjFwApply(s: Exp[String]) extends Def[FileWriter]

  case class BwWrite(b: Exp[BufferedWriter], s: Rep[String]) extends Def[Unit]
  case class BwClose(b: Exp[BufferedWriter]) extends Def[Unit]
  case class BrReadline(b: Exp[BufferedReader]) extends Def[String]
  case class BrClose(b: Exp[BufferedReader]) extends Def[Unit]

  def obj_br_apply(f: Exp[FileReader]): Rep[BufferedReader] = reflectEffect(ObjBrApply(f))
  def obj_bw_apply(f: Exp[FileWriter]): Rep[BufferedWriter] = reflectEffect(ObjBwApply(f))
  def obj_fr_apply(s: Exp[String]): Rep[FileReader] = reflectEffect(ObjFrApply(s))
  def obj_fw_apply(s: Exp[String]): Rep[FileWriter] = reflectEffect(ObjFwApply(s))

  def bw_write(b: Exp[BufferedWriter], s: Exp[String]) = reflectEffect(BwWrite(b,s))
  def bw_close(b: Exp[BufferedWriter]) = reflectEffect(BwClose(b))
  def br_readline(b: Exp[BufferedReader]) : Rep[String] = reflectEffect(BrReadline(b))
  def br_close(b: Exp[BufferedReader]) : Rep[Unit] = reflectEffect(BrClose(b))
}

trait ScalaGenIOOps extends ScalaGenBase {
  val IR: IOOpsExp
  import IR._
  
  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ObjBrApply(f) => emitValDef(sym, "new java.io.BufferedReader(" + quote(f) + ")")
    case ObjBwApply(f) => emitValDef(sym, "new java.io.BufferedWriter(" + quote(f) + ")")
    case ObjFrApply(s) => emitValDef(sym, "new java.io.FileReader(" + quote(s) + ")")
    case ObjFwApply(s) => emitValDef(sym, "new java.io.FileWriter(" + quote(s) + ")")
    case BwWrite(b,s) => emitValDef(sym, quote(b) + ".write(" + quote(s) + ")")
    case BwClose(b) => emitValDef(sym, quote(b) + ".close()")
    case BrReadline(b) => emitValDef(sym, quote(b) + ".readLine()")
    case BrClose(b) => emitValDef(sym, quote(b) + ".close()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenIOOps extends CLikeGenBase {
  val IR: IOOpsExp
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case ObjBrApply(f) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case ObjFrApply(s) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case BrReadline(b) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case BrClose(b) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CudaGenIOOps extends CudaGenBase with CLikeGenIOOps
trait CGenIOOps extends CGenBase with CLikeGenIOOps


