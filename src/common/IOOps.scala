package scala.lms
package common

import java.io.{File, FileReader, FileWriter, BufferedReader, BufferedWriter, PrintWriter}
import scala.lms.internal.{GenerationFailedException}
import util.OverloadHack
import scala.reflect.SourceContext

// TODO: fine grained effects

trait IOOps extends Variables with OverloadHack with StringOps {

  /**
   * File
   */
  implicit def fileTyp: Typ[File]
  object File {
    def apply(dir: Rep[String])(implicit pos: SourceContext) = obj_file_apply(dir)
  }
  def infix_getCanonicalFile(f: Rep[File])(implicit pos: SourceContext) = file_getcanonicalfile(f)
  def infix_getPath(f: Rep[File])(implicit pos: SourceContext) = file_getpath(f)
  def infix_listFiles(f: Rep[File])(implicit pos: SourceContext) = file_listfiles(f)

  def obj_file_apply(dir: Rep[String])(implicit pos: SourceContext): Rep[File]
  def file_getcanonicalfile(f: Rep[File])(implicit pos: SourceContext): Rep[File]
  def file_getpath(f: Rep[File])(implicit pos: SourceContext): Rep[String]
  def file_listfiles(f: Rep[File])(implicit pos: SourceContext): Rep[Array[File]]
  
  /**
   * BufferedReader
   */
  implicit def bufferedReaderTyp: Typ[BufferedReader]
  object BufferedReader {
    def apply(f: Rep[FileReader])(implicit pos: SourceContext) = obj_br_apply(f)
  }
  def infix_readLine(b: Rep[BufferedReader])(implicit pos: SourceContext) = br_readline(b)
  def infix_close(b: Rep[BufferedReader])(implicit pos: SourceContext) = br_close(b)

  def obj_br_apply(f: Rep[FileReader])(implicit pos: SourceContext): Rep[BufferedReader]
  def br_readline(b: Rep[BufferedReader])(implicit pos: SourceContext): Rep[String]
  def br_close(b: Rep[BufferedReader])(implicit pos: SourceContext): Rep[Unit]

  /**
   * BufferedWriter
   */
  implicit def bufferedWriterTyp: Typ[BufferedWriter]
  object BufferedWriter {
    def apply(f: Rep[FileWriter])(implicit pos: SourceContext) = obj_bw_apply(f)    
  }

  def infix_write(b: Rep[BufferedWriter], s: Rep[String])(implicit pos: SourceContext) = bw_write(b,s)
  def infix_close(b: Rep[BufferedWriter])(implicit o: Overloaded1, pos: SourceContext) = bw_close(b)

  def obj_bw_apply(f: Rep[FileWriter])(implicit pos: SourceContext): Rep[BufferedWriter]
  def bw_write(b: Rep[BufferedWriter], s: Rep[String])(implicit pos: SourceContext): Rep[Unit]
  def bw_close(b: Rep[BufferedWriter])(implicit pos: SourceContext): Rep[Unit]

  /**
   * FileReader
   */
  implicit def fileReaderTyp: Typ[FileReader]
  object FileReader {
    def apply(s: Rep[String])(implicit pos: SourceContext) = obj_fr_apply(s)
  }
  def obj_fr_apply(s: Rep[String])(implicit pos: SourceContext): Rep[FileReader]

  /**
   * FileWriter
   */
  implicit def fileWriterTyp: Typ[FileWriter]
  object FileWriter {
    def apply(s: Rep[String])(implicit pos: SourceContext) = obj_fw_apply(s)
  }
  def obj_fw_apply(s: Rep[String])(implicit pos: SourceContext): Rep[FileWriter]
}

trait IOOpsExp extends IOOps with DSLOpsExp with ArrayOpsExp {
  implicit def fileTyp: Typ[File] = manifestTyp
  implicit def bufferedReaderTyp: Typ[BufferedReader] = manifestTyp
  implicit def bufferedWriterTyp: Typ[BufferedWriter] = manifestTyp
  implicit def fileReaderTyp: Typ[FileReader] = manifestTyp
  implicit def fileWriterTyp: Typ[FileWriter] = manifestTyp

  case class ObjFileApply(dir: Exp[String]) extends Def[File]
  case class FileGetCanonicalFile(f: Exp[File]) extends Def[File]
  case class FileGetPath(f: Exp[File]) extends Def[String]
  case class FileListFiles(f: Exp[File]) extends Def[Array[File]]

  case class ObjBrApply(f: Exp[FileReader]) extends Def[BufferedReader]
  case class ObjBwApply(f: Exp[FileWriter]) extends Def[BufferedWriter]
  case class ObjFrApply(s: Exp[String]) extends Def[FileReader]
  case class ObjFwApply(s: Exp[String]) extends Def[FileWriter]

  case class BwWrite(b: Exp[BufferedWriter], s: Rep[String]) extends Def[Unit]
  case class BwClose(b: Exp[BufferedWriter]) extends Def[Unit]
  case class BrReadline(b: Exp[BufferedReader]) extends Def[String]
  case class BrClose(b: Exp[BufferedReader]) extends Def[Unit]

  def obj_file_apply(dir: Exp[String])(implicit pos: SourceContext): Exp[File] = reflectEffect(ObjFileApply(dir))
  def file_getcanonicalfile(f: Exp[File])(implicit pos: SourceContext) = FileGetCanonicalFile(f)
  def file_getpath(f: Exp[File])(implicit pos: SourceContext) = FileGetPath(f)
  def file_listfiles(f: Exp[File])(implicit pos: SourceContext) = FileListFiles(f)
  
  def obj_br_apply(f: Exp[FileReader])(implicit pos: SourceContext): Exp[BufferedReader] = reflectEffect(ObjBrApply(f))
  def obj_bw_apply(f: Exp[FileWriter])(implicit pos: SourceContext): Exp[BufferedWriter] = reflectEffect(ObjBwApply(f))
  def obj_fr_apply(s: Exp[String])(implicit pos: SourceContext): Exp[FileReader] = reflectEffect(ObjFrApply(s))
  def obj_fw_apply(s: Exp[String])(implicit pos: SourceContext): Exp[FileWriter] = reflectEffect(ObjFwApply(s))

  def bw_write(b: Exp[BufferedWriter], s: Exp[String])(implicit pos: SourceContext) = reflectEffect(BwWrite(b,s))
  def bw_close(b: Exp[BufferedWriter])(implicit pos: SourceContext) = reflectEffect(BwClose(b))
  def br_readline(b: Exp[BufferedReader])(implicit pos: SourceContext) : Exp[String] = reflectEffect(BrReadline(b))
  def br_close(b: Exp[BufferedReader])(implicit pos: SourceContext) : Exp[Unit] = reflectEffect(BrClose(b))
  
  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = ({
    e match {
      case Reflect(ObjFrApply(s), u, es) => reflectMirrored(Reflect(ObjFrApply(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjBrApply(x), u, es) => reflectMirrored(Reflect(ObjBrApply(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjFwApply(s), u, es) => reflectMirrored(Reflect(ObjFwApply(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(ObjBwApply(x), u, es) => reflectMirrored(Reflect(ObjBwApply(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(BrReadline(b), u, es) => reflectMirrored(Reflect(BrReadline(f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(BwWrite(b,s), u, es) => reflectMirrored(Reflect(BwWrite(f(b),f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(BrClose(b), u, es) => reflectMirrored(Reflect(BrClose(f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case Reflect(BwClose(b), u, es) => reflectMirrored(Reflect(BwClose(f(b)), mapOver(f,u), f(es)))(mtype(manifest[A]), pos)
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]  
}

trait ScalaGenIOOps extends ScalaGenBase {
  val IR: IOOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjFileApply(dir) => emitValDef(sym, src"new java.io.File($dir)")
    case FileGetCanonicalFile(f) => emitValDef(sym, src"$f.getCanonicalFile()")
    case FileGetPath(f) => emitValDef(sym, src"$f.getPath()")
    case FileListFiles(f) => emitValDef(sym, src"$f.listFiles()")
    case ObjBrApply(f) => emitValDef(sym, src"new java.io.BufferedReader($f)")
    case ObjBwApply(f) => emitValDef(sym, src"new java.io.BufferedWriter($f)")
    case ObjFrApply(s) => emitValDef(sym, src"new java.io.FileReader($s)")
    case ObjFwApply(s) => emitValDef(sym, src"new java.io.FileWriter($s)")
    case BwWrite(b,s) => emitValDef(sym, src"$b.write($s)")
    case BwClose(b) => emitValDef(sym, src"$b.close()")
    case BrReadline(b) => emitValDef(sym, src"$b.readLine()")
    case BrClose(b) => emitValDef(sym, src"$b.close()")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CLikeGenIOOps extends CLikeGenBase {
  val IR: IOOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjBrApply(f) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case ObjFrApply(s) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case BrReadline(b) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case BrClose(b) => throw new GenerationFailedException("CLikeGenIOOps: Java IO operations are not supported")
    case _ => super.emitNode(sym, rhs)
  }
}
trait CudaGenIOOps extends CudaGenBase with CLikeGenIOOps
trait OpenCLGenIOOps extends OpenCLGenBase with CLikeGenIOOps
trait CGenIOOps extends CGenBase with CLikeGenIOOps


