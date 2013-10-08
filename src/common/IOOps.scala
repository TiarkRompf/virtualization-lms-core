package scala.virtualization.lms
package common

import java.io.{File, FileReader, FileWriter, BufferedReader, BufferedWriter, PrintWriter, FileOutputStream, ObjectOutputStream, FileInputStream, ObjectInputStream}
import scala.virtualization.lms.internal.{GenerationFailedException}
import util.OverloadHack
import scala.reflect.SourceContext

// TODO: fine grained effects

trait IOOps extends Variables with OverloadHack {

  /**
   * File
   */
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
  object FileReader {
    def apply(s: Rep[String])(implicit pos: SourceContext) = obj_fr_apply(s)
  }
  def obj_fr_apply(s: Rep[String])(implicit pos: SourceContext): Rep[FileReader]

  /**
   * FileWriter
   */
  object FileWriter {
    def apply(s: Rep[String])(implicit pos: SourceContext) = obj_fw_apply(s)
  }
  def obj_fw_apply(s: Rep[String])(implicit pos: SourceContext): Rep[FileWriter]

  /**
   * ObjectOutputStream
   */
  object FileInputStream {
    def apply(s: Rep[String])(implicit pos: SourceContext) = obj_fis_apply(s)
  }
  class FileInputStreamOps(x: Rep[FileInputStream]) {
    def available()(implicit pos: SourceContext) = obj_fis_available(x)
  }
  implicit def fisToFisOps(x: Rep[FileInputStream]) = new FileInputStreamOps(x)
  def obj_fis_apply(s: Rep[String]): Rep[FileInputStream]
  def obj_fis_available(s: Rep[FileInputStream]): Rep[Int]

  object ObjectInputStream {
    def apply(s: Rep[FileInputStream])(implicit pos: SourceContext) = obj_ois_apply(s)
  }
  class ObjectInputStreamOps(x: Rep[ObjectInputStream]) {
    def readObject(dynamicType: String = null)(implicit pos: SourceContext) = obj_ois_readObject(x, dynamicType)
    def close()(implicit pos: SourceContext) = obj_ois_close(x)
  }
  implicit def oisTooisOps(x: Rep[ObjectInputStream]) = new ObjectInputStreamOps(x)
  def obj_ois_apply(s: Rep[FileInputStream]): Rep[ObjectInputStream]
  def obj_ois_close(s: Rep[ObjectInputStream]): Rep[Unit]
  def obj_ois_readObject(x: Rep[ObjectInputStream], dynamicType: String = null): Rep[Object]

  object ObjectOutputStream {
    def apply(s: Rep[FileOutputStream])(implicit pos: SourceContext) = obj_oos_apply(s, unit(false))
    def apply(s: Rep[FileOutputStream], x: Rep[Boolean])(implicit pos: SourceContext) = obj_oos_apply(s, x)
  }
  class ObjectOutputStreamOps(x: Rep[ObjectOutputStream]) {
    def writeObject(elem: Rep[Any])(implicit pos: SourceContext) = obj_oos_writeObject(x,elem)
    def close()(implicit pos: SourceContext) = obj_oos_close(x)
  }
  implicit def oosToOoosOps(x: Rep[ObjectOutputStream]) = new ObjectOutputStreamOps(x)
  def obj_oos_apply(s: Rep[FileOutputStream], x: Rep[Boolean])(implicit pos: SourceContext): Rep[ObjectOutputStream]
  def obj_oos_writeObject(s: Rep[ObjectOutputStream], elem: Rep[Any])(implicit pos: SourceContext): Rep[Unit]
  def obj_oos_close(s: Rep[ObjectOutputStream])(implicit pos: SourceContext): Rep[Unit]

  /**
   * FileWriter
   */
  object FileOutputStream {
    def apply(s: Rep[File])(implicit pos: SourceContext) = obj_fos_apply(s)
  }
  def obj_fos_apply(s: Rep[File])(implicit pos: SourceContext): Rep[FileOutputStream]

}

trait IOOpsExp extends IOOps with DSLOpsExp {
  case class ObjFileApply(dir: Exp[String]) extends Def[File]
  case class FileGetCanonicalFile(f: Exp[File]) extends Def[File]
  case class FileGetPath(f: Exp[File]) extends Def[String]
  case class FileListFiles(f: Exp[File]) extends Def[Array[File]]

  case class ObjBrApply(f: Exp[FileReader]) extends Def[BufferedReader]
  case class ObjBwApply(f: Exp[FileWriter]) extends Def[BufferedWriter]
  case class ObjFrApply(s: Exp[String]) extends Def[FileReader]
  case class ObjFwApply(s: Exp[String]) extends Def[FileWriter]
  case class ObjOosApply(s: Exp[FileOutputStream], x: Rep[Boolean]) extends Def[ObjectOutputStream]
  case class ObjOosWriteObject(s: Exp[ObjectOutputStream], elem: Exp[Any]) extends Def[Unit]
  case class ObjOosClose(s: Exp[ObjectOutputStream]) extends Def[Unit]
  case class ObjFosApply(s: Exp[File]) extends Def[FileOutputStream]
  case class ObjFisApply(s: Exp[String]) extends Def[FileInputStream]
  case class ObjOisApply(s: Exp[FileInputStream]) extends Def[ObjectInputStream]
  case class ObjOisClose(s: Exp[ObjectInputStream]) extends Def[Unit]
  case class ObjOisAvailable(s: Exp[FileInputStream]) extends Def[Int]
  case class ObjOisReadObject(s: Exp[ObjectInputStream], dynamicType: String = null) extends Def[Object]

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
  def obj_oos_apply(s: Exp[FileOutputStream], x: Rep[Boolean])(implicit pos: SourceContext): Exp[ObjectOutputStream] = reflectEffect(ObjOosApply(s,x))
  def obj_oos_writeObject(s: Exp[ObjectOutputStream], elem: Exp[Any])(implicit pos: SourceContext): Exp[Unit] = reflectEffect(ObjOosWriteObject(s, elem))
  def obj_oos_close(s: Exp[ObjectOutputStream])(implicit pos: SourceContext): Exp[Unit] = reflectEffect(ObjOosClose(s))
  def obj_fos_apply(s: Exp[File])(implicit pos: SourceContext): Exp[FileOutputStream] = reflectEffect(ObjFosApply(s))
  def obj_fis_apply(s: Rep[String]) = reflectEffect(ObjFisApply(s))
  def obj_ois_apply(s: Rep[FileInputStream]) = reflectEffect(ObjOisApply(s))
  def obj_ois_close(s: Rep[ObjectInputStream]) = reflectEffect(ObjOisClose(s))
  def obj_fis_available(s: Rep[FileInputStream]) = reflectEffect(ObjOisAvailable(s))
  def obj_ois_readObject(x: Rep[ObjectInputStream], dynamicType: String = null) = reflectEffect(ObjOisReadObject(x, dynamicType))

  def bw_write(b: Exp[BufferedWriter], s: Exp[String])(implicit pos: SourceContext) = reflectEffect(BwWrite(b,s))
  def bw_close(b: Exp[BufferedWriter])(implicit pos: SourceContext) = reflectEffect(BwClose(b))
  def br_readline(b: Exp[BufferedReader])(implicit pos: SourceContext) : Exp[String] = reflectEffect(BrReadline(b))
  def br_close(b: Exp[BufferedReader])(implicit pos: SourceContext) : Exp[Unit] = reflectEffect(BrClose(b))
  
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = ({
    e match {
      case Reflect(ObjFrApply(s), u, es) => obj_fr_apply(f(s))
      case Reflect(ObjBrApply(x), u, es) => obj_br_apply(f(x))
      case Reflect(ObjFwApply(s), u, es) => obj_fw_apply(f(s))
      case Reflect(ObjBwApply(x), u, es) => obj_bw_apply(f(x))      
      case Reflect(BrReadline(b), u, es) => br_readline(f(b))
      case Reflect(BwWrite(b,s), u, es) => bw_write(f(b),f(s))
      case Reflect(BrClose(b), u, es) => br_close(f(b))
      case Reflect(BwClose(b), u, es) => bw_close(f(b))
      case _ => super.mirror(e,f)
    }
  }).asInstanceOf[Exp[A]]  
}

trait ScalaGenIOOps extends ScalaGenBase {
  val IR: IOOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case ObjFileApply(dir) => emitValDef(sym, "new java.io.File(" + quote(dir) + ")")
    case FileGetCanonicalFile(f) => emitValDef(sym, quote(f) + ".getCanonicalFile()")
    case FileGetPath(f) => emitValDef(sym, quote(f) + ".getPath()")
    case FileListFiles(f) => emitValDef(sym, quote(f) + ".listFiles()")
    case ObjBrApply(f) => emitValDef(sym, "new java.io.BufferedReader(" + quote(f) + ")")
    case ObjBwApply(f) => emitValDef(sym, "new java.io.BufferedWriter(" + quote(f) + ")")
    case ObjFrApply(s) => emitValDef(sym, "new java.io.FileReader(" + quote(s) + ")")
    case ObjFwApply(s) => emitValDef(sym, "new java.io.FileWriter(" + quote(s) + ")")
    case ObjOosApply(s,x) => 
        if (x == Const(true)) {
            emitValDef(sym, "new java.io.ObjectOutputStream(" + quote(s) + "){")
            stream.println("override protected def writeStreamHeader() {")
            stream.println("reset();")
            stream.println("}")
            stream.println("}")
        } else emitValDef(sym, "new java.io.ObjectOutputStream(" + quote(s) + ")")
    case ObjOosWriteObject(s, elem) => stream.println(quote(s) + ".writeObject(" + quote(elem) + ")")
    case ObjOosClose(s) => stream.println(quote(s) + ".close")
    case ObjFosApply(s) => emitValDef(sym, "new java.io.FileOutputStream(" + quote(s) + ",true)")
    case ObjFisApply(s) => emitValDef(sym, "new java.io.FileInputStream(" + quote(s) + ")")
    case ObjOisApply(s) => emitValDef(sym, "new java.io.ObjectInputStream(" + quote(s) + ")")
    case ObjOisClose(s) => emitValDef(sym, quote(s) + ".close")
    case ObjOisAvailable(s) => emitValDef(sym, quote(s) + ".available")
    case ObjOisReadObject(s, dtype) => {
        if (dtype == null) emitValDef(sym, quote(s) + ".readObject")
        else emitValDef(sym, quote(s) + ".readObject.asInstanceOf[" + dtype + "]") 
    }
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


