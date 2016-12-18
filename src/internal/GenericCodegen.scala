package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}
import scala.reflect.RefinedManifest
import scala.collection.mutable.{Map => MMap}

trait GenericCodegen extends BlockTraversal {
  val IR: Expressions
  import IR._

  // TODO: should some of the methods be moved into more specific subclasses?
  
  def kernelFileExt = ""
  def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {}
  def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean): Unit = {}
  
  var analysisResults: MMap[String,Any] = null.asInstanceOf[MMap[String,Any]]

  /**
   * List of transformers that should be applied before code generation
   */
  var transformers: List[AbstractTransformer] = List[AbstractTransformer]()
  
  def performTransformations[A:Manifest](body: Block[A]): Block[A] = {
    var transformedBody = body
    transformers foreach { trans =>
      transformedBody = trans.apply[A](body.asInstanceOf[trans.IR.Block[A]]).asInstanceOf[this.Block[A]]
    }
    transformedBody
  }

  def emitFileHeader(): Unit = {}
  def emitFunctions(): Unit = {}
  
  // Initializer
  def initializeGenerator(buildDir:String, args: Array[String], _analysisResults: MMap[String,Any]): Unit = { analysisResults = _analysisResults }
  def finalizeGenerator(): Unit = {}
  def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {}

  def emitDataStructures(out: PrintWriter): Unit = {}
  def emitDataStructures(path: String): Unit = {}
 
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

  def runTransformations[A:Manifest](body: Block[A]): Block[A] = body
 
  // exception handler
  def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }
  
  /**
   * optional type remapping (default is identity)
   * except that we should replace all '$' by '.'
   * because inner class names might contain $ sign
   */
  def remap(s: String): String = s match {
	case "java.lang.Character" => "Character"
	case _ => s.replace('$', '.')
  }
  def remap[A](s: String, method: String, t: Manifest[A]) : String = remap(s, method, t.toString)
  def remap(s: String, method: String, t: String) : String = s + method + "[" + remap(t) + "]"    
  def remap[A](m: Manifest[A]): String = m match {
    case rm: RefinedManifest[A] =>  "AnyRef{" + rm.fields.foldLeft(""){(acc, f) => {val (n,mnf) = f; acc + "val " + n + ": " + remap(mnf) + ";"}} + "}"
    case _ if m.erasure == classOf[Variable[Any]] =>
        remap(m.typeArguments.head)
    case _ =>
      // call remap on all type arguments
      val targs = m.typeArguments
      if (targs.length > 0) {
        val ms = m.toString
        remap(ms.take(ms.indexOf("["))) + "[" + targs.map(tp => remap(tp)).mkString(", ") + "]"
      }
      else remap(m.toString)
  }
  def remapImpl[A](m: Manifest[A]): String = remap(m)
  //def remapVar[A](m: Manifest[Variable[A]]) : String = remap(m.typeArguments.head)

  def hasMetaData: Boolean = false
  def getMetaData: String = null

  def getDSLHeaders: String = null

  // ---------

  var stream: PrintWriter = _

  def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    stream = out
    try { body } finally { stream.flush; stream = save }
  }

  // ----------

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) => emitNode(sym,rhs)
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }
    
  def emitBlock(y: Block[Any]): Unit = traverseBlock(y)

  def emitBlockResult[A: Manifest](b: Block[A]) {	
      if (remap(manifest[A]) != "Unit") stream.println(quote(getBlockResult(b)))
  }
    
  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit
  
  def emitSource0[R : Manifest](f: () => Exp[R], className: String, stream: PrintWriter, dynamicReturnType: String = null): List[(Sym[Any], Any)] = {
    val body = reifyBlock(f())
    emitSource(List(), body, className, stream, dynamicReturnType)
  }

  def emitSource1[T1: Manifest, R : Manifest](f: (Exp[T1]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val body = reifyBlock(f(s1))
    emitSource(List(s1), body, className, stream)
  }

  def emitSource2[T1: Manifest, T2: Manifest, R : Manifest](f: (Exp[T1], Exp[T2]) => Exp[R], className: String, stream: PrintWriter, dynamicReturnType: String = null): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val body = reifyBlock(f(s1, s2))
    emitSource(List(s1, s2), body, className, stream, dynamicReturnType)
  }

  def emitSource3[T1: Manifest, T2: Manifest, T3: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val body = reifyBlock(f(s1, s2, s3))
    emitSource(List(s1, s2, s3), body, className, stream)
  }

  def emitSource4[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val body = reifyBlock(f(s1, s2, s3, s4))
    emitSource(List(s1, s2, s3, s4), body, className, stream)
  }
  
  def emitSource5[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val body = reifyBlock(f(s1, s2, s3, s4, s5))
    emitSource(List(s1, s2, s3, s4, s5), body, className, stream)
  }
  
  def emitSource6[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val s6 = fresh[T6]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6))
    emitSource(List(s1, s2, s3, s4, s5, s6), body, className, stream)
  }
  def emitSource7[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
  def emitSource8[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
  def emitSource9[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
  def emitSource10[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
  def emitSource11[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
  def emitSource12[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, T12: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
  def emitSource13[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, T12: Manifest, T13: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
  def emitSource14[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, T12: Manifest, T13: Manifest, T14: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13], Exp[T14]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
  def emitSource15[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, T12: Manifest, T13: Manifest, T14: Manifest, T15: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13], Exp[T14], Exp[T15]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
    val s15 = fresh[T15]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15), body, className, stream)
  }
  def emitSource16[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, T12: Manifest, T13: Manifest, T14: Manifest, T15: Manifest, T16: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13], Exp[T14], Exp[T15], Exp[T16]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
    val s15 = fresh[T15]
    val s16 = fresh[T16]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16), body, className, stream)
  }
  def emitSource17[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, T12: Manifest, T13: Manifest, T14: Manifest, T15: Manifest, T16: Manifest, T17: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13], Exp[T14], Exp[T15], Exp[T16], Exp[T17]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
    val s15 = fresh[T15]
    val s16 = fresh[T16]
    val s17 = fresh[T17]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17), body, className, stream)
  }
  def emitSource18[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, T12: Manifest, T13: Manifest, T14: Manifest, T15: Manifest, T16: Manifest, T17: Manifest, T18: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13], Exp[T14], Exp[T15], Exp[T16], Exp[T17], Exp[T18]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
    val s15 = fresh[T15]
    val s16 = fresh[T16]
    val s17 = fresh[T17]
    val s18 = fresh[T18]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18), body, className, stream)
  }
  def emitSource19[T1: Manifest, T2: Manifest, T3: Manifest, T4: Manifest, T5: Manifest, T6: Manifest, T7: Manifest, T8: Manifest, T9: Manifest, T10: Manifest, T11: Manifest, T12: Manifest, T13: Manifest, T14: Manifest, T15: Manifest, T16: Manifest, T17: Manifest, T18: Manifest, T19: Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5], Exp[T6], Exp[T7], Exp[T8], Exp[T9], Exp[T10], Exp[T11], Exp[T12], Exp[T13], Exp[T14], Exp[T15], Exp[T16], Exp[T17], Exp[T18], Exp[T19]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
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
    val s15 = fresh[T15]
    val s16 = fresh[T16]
    val s17 = fresh[T17]
    val s18 = fresh[T18]
    val s19 = fresh[T19]
    val body = reifyBlock(f(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19))
    emitSource(List(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12, s13, s14, s15, s16, s17, s18, s19), body, className, stream)
  }

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: PrintWriter, dynamicReturnType: String = null, serializable: Boolean = false): List[(Sym[Any], Any)] // return free static data in block

  def quote(x: Exp[Any]) : String = quote(x, false)

  def quote(x: Exp[Any], forcePrintSymbol: Boolean = false) : String = x match {
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
    case Const(c: Char) => "'"+c+"'"
    case Const(f: Float) => "%1.10f".format(f) + "f"
    case Const(l: Long) => l.toString + "L"
    case Const(null) => "null"
    case Const(z) => z.toString
    case s@Sym(n) => {
	if (forcePrintSymbol) "x" + n
        // Avoid printing symbols that are of type null
	else if (s.tp.toString == "Unit") ""
        else "x"+n
    }
    case x@_ if x == Const(null) => "null"
    case null => "null" 
    case _ => throw new RuntimeException("could not quote " + x)
  }
  
  // ----------
  
  override def reset {
    stream = null
    super.reset
  }

  def isVoidType[A](m: Manifest[A]) : Boolean = {
    m.toString match {
      case "Unit" => true
      case _ => false
    }
  }
  
  // Provides automatic quoting and remapping in the gen string interpolater
  implicit class CodegenHelper(sc: StringContext) {
    def printToStream(arg: Any): Unit = {
      stream.print(quoteOrRemap(arg))
    }

    def quoteOrRemap(arg: Any): String = arg match {
      case xs: Seq[_] => xs.map(quoteOrRemap).mkString(",")
      case e: Exp[_] => quote(e)
      case m: Manifest[_] => remap(m)
      case s: String => s
	  case null => "null"
      case _ => throw new RuntimeException(s"Could not quote or remap $arg")
    }

    // First line of a part of the context may contain
    // a | and therefore should not be stripped
    def stripContextPart(part: String): String = {
      val lines = part.linesWithSeparators
      if (!lines.hasNext) part
      else lines.next + (lines.foldLeft("")(_+_).stripMargin)
    }

    def src(args: Any*): String = {
      sc.raw(args.map(quoteOrRemap): _*).stripMargin
    }

    def gen(args: Any*): Unit = {
      sc.checkLengths(args)
      val start :: contextStrings = sc.parts.iterator.toList
      printToStream(start.stripMargin)
      for ((arg, contextString) <- args zip contextStrings) {
        printToStream(arg)
        printToStream(stripContextPart(contextString))
      }
      stream.println()
    }
  }
}



trait GenericNestedCodegen extends NestedBlockTraversal with GenericCodegen { self =>
  val IR: Expressions with Effects with LoweringTransform
  import IR._

  /* Lowering stuff */
  def lowerNode[T:Manifest](sym: Sym[T], rhs: Def[T]): Unit = {
//	println("Lowering " + sym  + " with def " + rhs )
    rhs match {
      case Reflect(s, u, effects) => lowerNode(sym, s)
      case dflt@_ => { 
        //System.out.println("Don't know how to lower symbol " + dflt + ".")
		()
      }
    }
  }

  object HIRLowering extends LoweringTransformer
  object LIRLowering extends LoweringTransformer
  object LIRTraversal extends NestedBlockTraversal {
    val IR: self.IR.type = self.IR
    def apply[A: Manifest](b: Block[A]) = traverseBlock(b)
    override def traverseStm(stm: Stm): Unit = stm match {
        case TP(sym, rhs) => lowerNode(sym, rhs)(sym.tp)
        case _ => throw new GenerationFailedException(s"don't know how to generate code for statement: $stm during LIRTraversal")
	}
  }

  def remapManifest[A:Manifest](m: Sym[A]): Manifest[_] = manifest[A]

  override def runTransformations[A:Manifest](body: Block[A]): Block[A] = {
    val b = HIRLowering.run(body)
	LIRTraversal(b)
    LIRLowering.run(b)
  }

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

  case class NestedBlock(b: Block[Any])
  def nestedBlock(b: Block[Any]) = NestedBlock(b)

  // Allows the gen string interpolator to perform emitBlock when passed a Block
  implicit class NestedCodegenHelper(sc: StringContext) extends CodegenHelper(sc) {

  override def printToStream(arg: Any): Unit = arg match {
      case NestedBlock(b) => emitBlock(b)
      case b: Block[_] => stream.print(quoteOrRemap(getBlockResult(b)))
      case _ => stream.print(quoteOrRemap(arg))
    }
  }
}
