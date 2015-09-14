package scala.lms
package internal

import java.io.{File, PrintWriter}

trait GenericCodegen extends Traversal {
  val IR: BaseExp
  import IR._

  class GenerationFailedException(msg: String) extends Exception(msg)

  /** these methods support a kernel model of execution and are only used by Delite, should be moved into Delite only? **/
  def deviceTarget: Targets.Value = throw new Exception("Device target is not defined for this codegen.")
  def hostTarget: Targets.Value = Targets.getHostTarget(deviceTarget)
  def isAcceleratorTarget: Boolean = hostTarget != deviceTarget

  def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {}
  def emitKernel(syms: List[Sym[Any]], rhs: Any): Unit = { }
  def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {}
  def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {}

  def emitTransferFunctions(): Unit = {}

  def resourceInfoType = ""
  def resourceInfoSym = ""

  /******/

  def fileExtension = ""
  def emitFileHeader(): Unit = {}

  def initializeGenerator(buildDir:String): Unit = { }
  def finalizeGenerator(): Unit = {}

  def emitDataStructures(stream: PrintWriter): Unit = {}
  def emitDataStructures(path: String): Unit = {}
  def getDataStructureHeaders(): String = ""

  def dataPath = "data" + java.io.File.separator
  def symDataPath(sym: Sym[Any]) = dataPath + sym.id

  def emitData(sym: Sym[Any], data: Seq[Any]) {
    val outDir = new File(dataPath)
    outDir.mkdirs()
    val outFile = new File(symDataPath(sym))
    val stream = new PrintWriter(outFile)

    for(v <- data) { stream.println(v) }

    stream.close()
  }

  // --- Traversal
  override def preprocess[A:Typ](b: Block[A]): Block[A] = { preCodegenAnalysis(b); b }

  // preCodegenAnalysis: A hook that should be overridden by the concrete code generators
  // so that they can perform pre-codegen analysis and collect some metadata
  // As this is optional, the default just does nothing
  def preCodegenAnalysis(y: Block[Any]): Unit = {}

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) =>
      if (!skipDuplicates || !emittedSyms.contains(sym))
        emitNode(sym,rhs)
      if (trackSyms && !emittedSyms.contains(sym))
        emittedSyms = emittedSyms :+ sym

    case TTP(lhs, _, rhs) =>
      if (!skipDuplicates || !emittedSyms.forall(x => !(lhs contains x)))
        emitFatNode(lhs, rhs)
      if (trackSyms)
        lhs foreach(x => if (!emittedSyms.contains(x)) emittedSyms = emittedSyms :+ x)

    case _ => throw new GenerationFailedException(this.toString + ": don't know how to generate code for statement: " + stm)
  }

  // --- Helper functions
  // optional type remapping (default is identity)
  def remap(s: String): String = s
  def remap[A](s: String, method: String, t: Typ[A]) : String = remap(s, method, t.toString)
  def remap(s: String, method: String, t: String) : String = s + method + "[" + remap(t) + "]"
  def remap[A](m: Typ[A]): String = m match {
    case rm: RefinedTyp[_] =>  "AnyRef{" + rm.fields.foldLeft(""){(acc, f) => {val (n,mnf) = f; acc + "val " + n + ": " + remap(mnf) + ";"}} + "}"
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
  def remapImpl[A](m: Typ[A]): String = remap(m)
  //def remapVar[A](m: Typ[Variable[A]]) : String = remap(m.typeArguments.head)

  def remapHost[A](m: Typ[A]): String = remap(m).replaceAll(deviceTarget.toString,hostTarget.toString)

  def hasMetaData: Boolean = false
  def getMetaData: String = null

  // ---------

  var stream: PrintWriter = _

  def withStream[A](out: PrintWriter)(body: => A): A = {
    val save = stream
    stream = out
    try { body } finally { stream.flush; stream = save }
  }

  // ----------

  // HACK: Avoid duplicating common statements across blocks (doesn't work that well at the moment)
  // Useful only when duplicated statements are generated within the same scope for now
  // TODO: This should either be handled in scheduling or in a much cleaner way during codegen
  var emittedSyms: List[Sym[Any]] = Nil
  var skipDuplicates = false
  var trackSyms = false

  def emitInCommonScope(emit: => Unit) {
    var prevEmitted = emittedSyms
    var prevSkip = skipDuplicates
    var prevTrack = trackSyms

    skipDuplicates = true
    trackSyms = true
    emit
    emittedSyms = prevEmitted
    skipDuplicates = prevSkip
    trackSyms = prevTrack
  }

  // Same as emitInCommonScope, but doesn't pop the stack of emittedSyms
  // (Denotes code being emitted isn't in a different scope but may have duplicated symbols)
  def emitWithoutDuplicates(emit: => Unit) {
    var prevSkip = skipDuplicates
    var prevTrack = trackSyms

    skipDuplicates = true
    trackSyms = true
    emit
    skipDuplicates = prevSkip
    trackSyms = prevTrack
  }

  def emitBlock(y: Block[Any]): Unit = traverseBlock(y)

  def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    //case Read(s) => emitValDef(sym, quote(s))
    case Reflect(s, u, effects) => emitNode(sym, s)
    case Reify(s, u, effects) => // just ignore -- effects are accounted for in emitBlock
    case _ => throw new GenerationFailedException(this.toString + ": don't know how to generate code for: " + rhs)
  }

  def emitValDef(sym: Sym[Any], rhs: String): Unit
  def emitVarDecl(sym: Sym[Any]): Unit = throw new GenerationFailedException(this.toString + ": don't know how to emit variable declaration " + quote(sym))
  def emitAssignment(sym: Sym[Any], rhs: String): Unit = throw new GenerationFailedException(this.toString + ": don't know how to emit variable assignment " + quote(sym))

  // --- Fat generation
  def emitFatNode(sym: List[Sym[Any]], rhs: FatDef): Unit = rhs match {
    case _ => sys.error("don't know how to generate code for: "+rhs)
  }

  // TODO: DELITE SPECIFIC METHOD -- used for kernel activation records
  def emitFatNodeKernelExtra(sym: List[Sym[Any]], rhs: FatDef): Unit = { }
  def emitNodeKernelExtra(sym: List[Sym[Any]], rhs: Def[Any]): Unit = { }

  def emitFatBlock(rhs: List[Block[Any]]): Unit = {
    //emitBlock(Block(Combine(rhs.map(getBlockResultFull)))) // TODO: find another way
    // TODO: This may not have desired behavior..
    emitInCommonScope { rhs foreach emitBlock }
  }

  def emitSource[T:Typ, R:Typ](f: Exp[T] => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s = bound[T]
    val body = reifyBlock(f(s))
    emitSource(List(s), body, className, stream)
  }

  def emitSource2[T1:Typ, T2:Typ, R:Typ](f: (Exp[T1], Exp[T2]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = bound[T1]
    val s2 = bound[T2]
    val body = reifyBlock(f(s1, s2))
    emitSource(List(s1, s2), body, className, stream)
  }

  def emitSource3[T1:Typ, T2:Typ, T3:Typ, R:Typ](f: (Exp[T1], Exp[T2], Exp[T3]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = bound[T1]
    val s2 = bound[T2]
    val s3 = bound[T3]
    val body = reifyBlock(f(s1, s2, s3))
    emitSource(List(s1, s2, s3), body, className, stream)
  }

  def emitSource4[T1:Typ, T2:Typ, T3:Typ, T4:Typ, R:Typ](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = bound[T1]
    val s2 = bound[T2]
    val s3 = bound[T3]
    val s4 = bound[T4]
    val body = reifyBlock(f(s1, s2, s3, s4))
    emitSource(List(s1, s2, s3, s4), body, className, stream)
  }

  def emitSource5[T1:Typ, T2:Typ, T3:Typ, T4:Typ, T5:Typ, R:Typ](f: (Exp[T1], Exp[T2], Exp[T3], Exp[T4], Exp[T5]) => Exp[R], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {
    val s1 = bound[T1]
    val s2 = bound[T2]
    val s3 = bound[T3]
    val s4 = bound[T4]
    val s5 = bound[T5]
    val body = reifyBlock(f(s1, s2, s3, s4, s5))
    emitSource(List(s1, s2, s3, s4, s5), body, className, stream)
  }

  /**
   * @param args List of symbols bound to `body`
   * @param body Block to emit
   * @param className Name of the generated identifier
   * @param stream Output stream
   */
  def emitSource[A : Typ](args: List[Sym[_]], body: Block[A], className: String, stream: PrintWriter): List[(Sym[Any], Any)] // return free static data in block

  def quote(x: Exp[Any]) : String = x match {
    case Const(s: String) => "\""+s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")+"\"" // TODO: more escapes?
    case Const(f: Float) => f+"f"
    case Const(c: Char) => "'"+(""+c).replace("'", "\\'").replace("\n", "\\n")+"'"
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case _ => throw new RuntimeException("could not quote " + x)
  }

  // ----------

  def isPrimitiveType[A](m: Typ[A]) : Boolean = m.toString match {
    case "Boolean" | "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" => true
    case _ => false
  }

  def isVoidType[A](m: Typ[A]) : Boolean = m.toString match {
    case "Unit" => true
    case _ => false
  }

  def isVariableType[A](m: Typ[A]) : Boolean = {
    if(m.erasure == classOf[Variable[AnyVal]]) true
    else false
  }

  // TODO: Where is this used?
  case class NestedBlock(b: Block[Any])
  def nestedBlock(b: Block[Any]) = NestedBlock(b)

  // Provides automatic quoting and remapping in the gen string interpolator
  // Allows the gen string interpolator to perform emitBlock when passed a Block
  implicit class CodegenHelper(val sc: StringContext) {
    def printToStream(arg: Any): Unit = arg match {
      case NestedBlock(b) => emitBlock(b)
      case b: Block[_] => stream.print(quoteOrRemap(getBlockResult(b)))
      case _ => stream.print(quoteOrRemap(arg))
    }

    def quoteOrRemap(arg: Any): String = arg match {
      case xs: Seq[_] => xs.map(quoteOrRemap).mkString(",")
      case e: Exp[_] => quote(e)
      case m: Typ[_] => remap(m)
      case s: String => s
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
