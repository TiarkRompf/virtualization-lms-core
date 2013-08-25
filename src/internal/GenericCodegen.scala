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
  
  def emitFileHeader(): Unit = {}
  
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
  
  // exception handler
  def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }
  
  // optional type remapping (default is identity)
  def remap(s: String): String = s
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
        ms.take(ms.indexOf("[")+1) + targs.map(tp => remap(tp)).mkString(", ") + "]"
      }
      else m.toString    
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
    
  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit

  private def generateDynamicClassesList(dynamicClasses: String*) = {
     val notnull = dynamicClasses.filter (c => c!=null)
     /* Either all of them are not null or all of them are null */
     //if ((notnull.length != 0) && (notnull.length != dynamicClasses.length))
       // throw new RuntimeException("invalid number of dynamicClasses encountered when compiling DynamicRecords")
     if (notnull.length == 0) null
     else {
        val x = dynamicClasses.toList
        (x.dropRight(1), x.last)
     }
  }  

  def emitSource0[T : Manifest, R : Manifest](f: () => Exp[R], className: String, stream: PrintWriter, dynamicReturn: String = null): List[(Sym[Any], Any)] = {
    val body = reifyBlock(f())
    emitSource(List(), body, className, stream, generateDynamicClassesList(dynamicReturn))
  }

  def emitSource1[T : Manifest, R : Manifest](f: Exp[T] => Exp[R], className: String, stream: PrintWriter, dynamicClass: String = null, dynamicReturn: String = null): List[(Sym[Any], Any)] = {
    val s = fresh[T]
    val body = reifyBlock(f(s))
    emitSource(List(s), body, className, stream, generateDynamicClassesList(dynamicClass, dynamicReturn))
  }

  def emitSource2[T1 : Manifest, T2 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2]) => Exp[R], className: String, stream: PrintWriter, dynamicClass: String = null, dynamicClass2: String = null, dynamicReturn: String = null): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val body = reifyBlock(f(s1, s2))
    emitSource(List(s1, s2), body, className, stream, generateDynamicClassesList(dynamicClass, dynamicClass2, dynamicReturn))
  }

  def emitSource3[T1 : Manifest, T2 : Manifest, T3 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3]) => Exp[R], className: String, stream: PrintWriter, dynamicClass: String = null, dynamicClass2: String = null, dynamicClass3: String = null, dynamicReturn: String = null): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val body = reifyBlock(f(s1, s2, s3))
    emitSource(List(s1, s2, s3), body, className, stream, generateDynamicClassesList(dynamicClass, dynamicClass2, dynamicClass3, dynamicReturn))
  }

  def emitSource4[T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[R], className: String, stream: PrintWriter, dynamicClass: String = null, dynamicClass2: String = null, dynamicClass3: String = null, dynamicClass4: String = null, dynamicReturn: String = null): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val body = reifyBlock(f(s1, s2, s3, s4))
    emitSource(List(s1, s2, s3, s4), body, className, stream, generateDynamicClassesList(dynamicClass, dynamicClass2, dynamicClass3, dynamicClass4, dynamicReturn))
  }

  def emitSource5[T1 : Manifest, T2 : Manifest, T3 : Manifest, T4 : Manifest, T5 : Manifest, R : Manifest](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5]) => Exp[R], className: String, stream: PrintWriter, dynamicClass: String = null, dynamicClass2: String = null, dynamicClass3: String = null, dynamicClass4: String = null, dynamicClass5: String = null, dynamicReturn: String = null): List[(Sym[Any], Any)] = {
    val s1 = fresh[T1]
    val s2 = fresh[T2]
    val s3 = fresh[T3]
    val s4 = fresh[T4]
    val s5 = fresh[T5]
    val body = reifyBlock(f(s1, s2, s3, s4, s5))
    emitSource(List(s1, s2, s3, s4, s5), body, className, stream, generateDynamicClassesList(dynamicClass, dynamicClass2, dynamicClass3, dynamicClass4, dynamicClass5, dynamicReturn))
  }

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, stream: PrintWriter, dynamicTypes: Tuple2[List[String],String] = null): List[(Sym[Any], Any)] // return free static data in block

  def quote(x: Exp[Any]) : String = x match {
    case Const(s: String) => "\""+s.replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
    case Const(c: Char) => "'"+c+"'"
    case Const(f: Float) => "%1.10f".format(f) + "f"
    case Const(l: Long) => l.toString + "L"
    case Const(null) => "null"
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case x@_ if x == Const(null) => "null"
    case null => "null" 
    case _ => throw new RuntimeException("could not quote " + x)
  }
  
  // ----------
  
  override def reset {
    stream = null
    super.reset
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
