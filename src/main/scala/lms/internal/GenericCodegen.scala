package scala.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter, StringWriter}
import scala.reflect.RefinedManifest
import scala.collection.mutable.{Map => MMap}

trait GenericCodegen extends BlockTraversal {
  val IR: Expressions
  import IR._

  // TODO: should some of the methods be moved into more specific subclasses?

  def kernelFileExt = ""
  def emitFileHeader(): Unit = {}
  def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {}
  def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {}

  var analysisResults: MMap[String,Any] = null.asInstanceOf[MMap[String,Any]]

  // Initializer
  def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = { analysisResults = _analysisResults }
  def finalizeGenerator(): Unit = {}
  def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {}

  def emitDataStructures(stream: PrintWriter): Unit = {}
  def emitDataStructures(path: String): Unit = {}
  def getDataStructureHeaders(): String = ""
  def emitTransferFunctions(): Unit = {}

  def dataPath = {
    "data" + java.io.File.separator
  }

  def symDataPath(sym: Sym[Any]) = {
    dataPath + sym.id
  }

  def emitData(sym: Sym[Any], data: Seq[Any]) {
    val outDir = new File(dataPath)
    outDir.mkdirs()
    val outFile = new File(symDataPath(sym))
    val stream = new PrintWriter(outFile)

    for(v <- data) {
      stream.println(v)
    }

    stream.close()
  }

  // exception handler
  def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }

  // optional type remapping (default is identity)
  def remap(s: String): String = s
  def remap[A](s: String, method: String, t:TypeRep[A]) : String = remap(s, method, t.toString)
  def remap(s: String, method: String, t: String) : String = s + method + "[" + remap(t) + "]"
  def remap[A](m:TypeRep[A]): String = m match {
    case rm: RefinedManifest[A] =>  "AnyRef{" + rm.fields.foldLeft(""){(acc, f) => {val (n,mnf) = f; acc + "val " + n + ": " + remap(mnf) + ";"}} + "}"
    case _ if m.erasure == classOf[Variable[Any]] =>
        remap(m.typeArguments.head)
    case _ =>
      // call remap on all type arguments
      val targs = m.typeArguments
      if (targs.length > 0) {
        val ms = m.toString
        ms.take(ms.indexOf("[")+1) + targs.map(tp => remap(tp)).mkString(", ") + "]"
      }
      else m.toString
  }
  def remapImpl[A](m:TypeRep[A]): String = remap(m)
  //def remapVar[A](m:TypeRep[Variable[A]]) : String = remap(m.typeArguments.head)

  def hasMetaData: Boolean = false
  def getMetaData: String = null

  // ---------

  var stream: PrintWriter = _

  def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    val sw = new StringWriter
    stream = new PrintWriter(sw)
    try { body } finally {
      stream.flush
      //printIndented(sw.toString)(out)
      out.print(sw.toString)
      out.flush
      stream = save
    }
  }

  def printIndented(str: String)(out: PrintWriter): Unit = {
    val lines = str.split("[\n\r]")
    var indent = 0
    for (l0 <- lines) {
      val l = l0.trim
      if (l.length > 0) {
        var open = 0
        var close = 0
        var initClose = 0
        var nonWsChar = false
        l foreach {
          case '{' | '(' | '[' => {
            open += 1
            if (!nonWsChar) {
              nonWsChar = true
              initClose = close
            }
          }
          case '}' | ')' | ']' => close += 1
          case x => if (!nonWsChar && !x.isWhitespace) {
            nonWsChar = true
            initClose = close
          }
        }
        if (!nonWsChar) initClose = close
        out.println("  " * (indent - initClose) + l)
        indent += (open - close)
      }
    }
  }

  // ----------

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) => emitNode(sym,rhs)
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }

  def emitBlock(y: Block[Any]): Unit = traverseBlock(y)

  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit

  def emitSource[T:TypeRep, R:TypeRep](f: Exp[T] => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s = fresh[T]
    val body = reifyBlock(f(s))
    emitSource(List(s), body, className, stream)
  }

  def emitSource2[T1:TypeRep, T2:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val body = reifyBlock(f(s1, s2))
    emitSource(List(s1, s2), body, className, stream)
  }

  def emitSource3[T1:TypeRep, T2:TypeRep, T3:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val body = reifyBlock(f(s1, s2, s3))
    emitSource(List(s1, s2, s3), body, className, stream)
  }

  def emitSource4[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val body = reifyBlock(f(s1, s2, s3, s4))
    emitSource(List(s1, s2, s3, s4), body, className, stream)
  }

  def emitSource5[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val body = reifyBlock(f(s1, s2, s3, s4, s5))
    emitSource(List(s1, s2, s3, s4, s5), body, className, stream)
  }

  def emitSource6[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6))
    emitSource(List(s1, s2, s3, s4, s5, s6), body, className, stream)
  }

  def emitSource7[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, T7:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val s7 = fresh[T7]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7), body, className, stream)
  }

  def emitSource8[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, T7:TypeRep, T8:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val s7 = fresh[T7]
    val s8 = fresh[T8]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8), body, className, stream)
  }

  def emitSource9[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, T7:TypeRep, T8:TypeRep, T9:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val s7 = fresh[T7]
    val s8 = fresh[T8]
    val s9 = fresh[T9]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9), body, className, stream)
  }

  def emitSource10[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, T7:TypeRep, T8:TypeRep, T9:TypeRep, T10:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val s7 = fresh[T7]
    val s8 = fresh[T8]
    val s9 = fresh[T9]
    val s10 = fresh[T10]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10), body, className, stream)
  }

  def emitSource11[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, T7:TypeRep, T8:TypeRep, T9:TypeRep, T10:TypeRep, T11:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val s7 = fresh[T7]
    val s8 = fresh[T8]
    val s9 = fresh[T9]
    val s10 = fresh[T10]
    val s11 = fresh[T11]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11), body, className, stream)
  }

  def emitSource12[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, T7:TypeRep, T8:TypeRep, T9:TypeRep, T10:TypeRep, T11:TypeRep, T12:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val s7 = fresh[T7]
    val s8 = fresh[T8]
    val s9 = fresh[T9]
    val s10 = fresh[T10]
    val s11 = fresh[T11]
    val s12 = fresh[T12]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12), body, className, stream)
  }

  def emitSource13[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, T7:TypeRep, T8:TypeRep, T9:TypeRep, T10:TypeRep, T11:TypeRep, T12:TypeRep, T13:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val s7 = fresh[T7]
    val s8 = fresh[T8]
    val s9 = fresh[T9]
    val s10 = fresh[T10]
    val s11 = fresh[T11]
    val s12 = fresh[T12]
    val s13 = fresh[T13]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13), body, className, stream)
  }

  def emitSource14[T1:TypeRep, T2:TypeRep, T3:TypeRep, T4:TypeRep, T5:TypeRep, T6:TypeRep, T7:TypeRep, T8:TypeRep, T9:TypeRep, T10:TypeRep, T11:TypeRep, T12:TypeRep, T13:TypeRep, T14:TypeRep, R:TypeRep](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13], Exp[T14]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val s7 = fresh[T7]
    val s8 = fresh[T8]
    val s9 = fresh[T9]
    val s10 = fresh[T10]
    val s11 = fresh[T11]
    val s12 = fresh[T12]
    val s13 = fresh[T13]
    val s14 = fresh[T14]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14), body, className, stream)
  }

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emitSource[A:TypeRep](args: List[Sym[_]], body: Block[A], className: String, stream: PrintWriter): List[(Sym[Any], Any)] // return free static data in block

  def quote(x: Exp[Any]) : String = x match {
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
    case Const(c: Char) => "'"+c+"'"
    case Const(f: Float) => "%1.10f".format(f) + "f"
    case Const(l: Long) => l.toString + "L"
    case Const(null) => "null"
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case _ => throw new RuntimeException("could not quote " + x)
  }

  // ----------

  override def reset {
    stream = null
    super.reset
  }

  def isPrimitiveType[A](m:TypeRep[A]) : Boolean = {
    m.toString match {
      case "Boolean" | "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" => true
      case _ => false
    }
  }

  def isVoidType[A](m:TypeRep[A]) : Boolean = {
    m.toString match {
      case "Unit" => true
      case _ => false
    }
  }

  def isVariableType[A](m:TypeRep[A]) : Boolean = {
    if(m.erasure == classOf[Variable[AnyVal]]) true
    else false
  }
}



trait GenericNestedCodegen extends NestedBlockTraversal with GenericCodegen {
  val IR: Expressions with Effects
  import IR._

  override def traverseStm(stm: Stm) = super[GenericCodegen].traverseStm(stm)

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//    case Read(s) =>
//      emitValDef(sym, quote(s))
    case Reflect(s, u, effects) =>
      emitNode(sym, s)
    case Reify(s, u, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }

}
