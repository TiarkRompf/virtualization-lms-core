package scala.virtualization.lms
package internal

import util.NullOutputStream
import java.io.{File, FileWriter, PrintWriter}

import scala.reflect.SourceContext
import scala.collection.mutable.Stack

trait ChiselCodegen extends GenericCodegen with Config {
  val IR: Blocks
  import IR._

  var inHwScope = false
  private val _nullstream = new PrintWriter(new NullOutputStream())
  override def stream = if (inHwScope) _stream else _nullstream
  def alwaysGen(x: => Any) {
    val inScope = inHwScope
    inHwScope = true
    x
    inHwScope = inScope
  }

  override def deviceTarget: Targets.Value = Targets.Chisel
  override def fileExtension = "scala"
  override def toString = "chisel"
  override def resourceInfoType = ""
  override def resourceInfoSym = ""

  override def singleFileName = s"TopKernelLib.$fileExtension"

  // Generate all code into one file
  override def emitSingleFile() = true

  val controlNodeStack = Stack[Sym[Any]]()

  var hwblockDeps = List[Sym[Any]]()

  private var _baseStream: PrintWriter = _
  def baseStream = _baseStream
  def baseStream_=(s: PrintWriter) { _baseStream = s }

  private var _baseConnectStream: PrintWriter = _
  def baseConnectStream = _baseConnectStream
  def baseConnectStream_=(s: PrintWriter) { _baseConnectStream = s }


  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val staticData = getFreeDataBlock(body)

      //println(Console.CYAN_B + "HELLO")
    withStream(out) {
      alwaysGen {
        emitFileHeader()
      }

      emitBlock(body)
      alwaysGen {
        emitFileFooter()
      }
    }


    staticData
  }

  override def preProcess[A: Manifest](body: Block[A]) = {
    val fullPath = s"""${bd}/$singleFileName"""
    val (file, stream) = getFileStream(fullPath)
    withStream(stream) {
      alwaysGen {
        emitFileHeader()
      }
    }
    withStream(baseStream) {
      alwaysGen { emitBaseFileHeader() }
    }
    withStream(baseConnectStream) {
      alwaysGen { emitBaseConnectFileHeader() }
    }
  }

  override def postProcess[A: Manifest](body: Block[A]) = {
    val (file, stream) = getFileStream()
    withStream(stream) {
      alwaysGen {
        emitFileFooter()
      }
    }
    withStream(baseStream) {
      alwaysGen { emitBaseFileFooter() }
    }
    withStream(baseConnectStream) {
      alwaysGen { emitBaseConnectFileFooter() }
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]): Unit = {
    Console.println(Console.BLACK + Console.YELLOW_B + this.toString + ": don't know how to generate code for: " + rhs + Console.RESET)
  }

  private var bd = ""

  override def initializeGenerator(buildDir: String) = {
    bd = buildDir
    val sep = java.io.File.separator
    val outDir = new File(buildDir); outDir.mkdirs()

    val baseFilePath = s"""${bd}/BaseModule.$fileExtension"""
    baseStream = new PrintWriter(new File(baseFilePath))

    val baseConnectFilePath = s"""${bd}/BaseConnectModule.$fileExtension"""
    baseConnectStream = new PrintWriter(new File(baseConnectFilePath))
  }

  override def finalizeGenerator() = {
    val (file, stream) = getFileStream()
    stream.flush
    stream.close
    baseStream.flush
    baseStream.close
    baseConnectStream.flush
    baseConnectStream.close

  }

  private def emitBaseConnectFileHeader() = {
      //emit(s"""package engine""")
      //emit("import Chisel._")
      //emit(s"""class BaseConnect extends BaseIO{""")
      //emit(s"""val top = Module(new TopModule)""")
  }

  private def emitBaseConnectFileFooter() = {
    //emit("}")
  }
  private def emitBaseFileHeader() = {
      //emit(s"""package engine""")
      emit("import Chisel._")
      emit(s"""class BaseIO extends Module{""")
      emit("var io = new Bundle {")
	emit("")
	emit(s"""val top_en = Bool(INPUT)""")
	emit(s"""val top_done = Bool(OUTPUT)""")
  }

  private def emitBaseFileFooter() = {
    emit("}")
  }

  override def emitFileHeader() = {
    // empty by default. override to emit package or import declarations.
    //emit("/*****************************************\n"+
         //"  Chisel Header \n"+
         //"*******************************************/")
      //emit(s"""package engine""")
      emit("import Chisel._")
    emit(s"""class TopModule extends BaseIO {""")
  }

    override def emitFileFooter() = {
        emit("}")
    emit("/*****************************************\n"+
         "  End of Chisel BACKEND \n"+
         "*******************************************/")
    }

  override def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {
    val kernelName = syms.map(quote).mkString("")
    stream.print("// kernel_" + kernelName + " (")
    if (resourceInfoType != "") {
      stream.print(resourceInfoSym + ":" + resourceInfoType)
      if ((vals ++ vars).length > 0) stream.print(",")
    }
    // Print all val arguments
    stream.print(vals.map(p => quote(p) + ":" + remap(p.tp)).mkString(","))

    // Print all var arguments
    if (vals.length > 0 && vars.length > 0){
      stream.print(",")
    }
    // TODO: remap Ref instead of explicitly adding generated.scala
    if (vars.length > 0){
      stream.print(vars.map(v => quote(v) + ":" + "generated.scala.Ref[" + remap(v.tp) +"]").mkString(","))
    }

    // Print result type
    if (resultIsVar){
      stream.print("): " + "generated.scala.Ref[" + resultType + "] = {")
    }
    else {
      stream.print("): " + resultType + " = {")
    }

    stream.println("")
  }

  override def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean, isMultiLoop: Boolean): Unit = {
    val kernelName = syms.map(quote).mkString("")
    stream.println(s"// } $kernelName")
  }

  def relativePath(fileName: String): String = {
    val i = fileName.lastIndexOf('/')
    fileName.substring(i + 1)
  }

  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    val extra = if ((sourceinfo < 2) || sym.pos.isEmpty) "" else {
      val context = sym.pos(0)
      "      // " + relativePath(context.fileName) + ":" + context.line
    }
    stream.println("var " + quote(sym) + " = " + rhs + extra)
  }

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = " + rhs)
  }

  // TODO: Unused?
  def emitVarDecl(sym: Sym[Any]): Unit = {
    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = null.asInstanceOf[" + remap(sym.tp) + "]")
  }

  def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
    stream.println(quote(sym) + " = " + rhs)

  }

  override def quote(x: Exp[Any]) = x match {
    case Const(l: Long) => l.toString + "L"
    case Const(null) => "null.asInstanceOf["+x.tp+"]"
    case _ => super.quote(x)
  }

    def emit(str: String):Unit = {
        stream.println(str)
    }

    def emitGlobal(str: String):Unit = {
        baseStream.println(str)
    }

    //def emit(str: String):Unit = {
        //baseConnectStream.println(str)
    //}

  def emitGlobalWire(str: String): Unit = {
    emit(s"""val $str = Bool();""")
  }

    def emitComment(str: String):Unit = {
        stream.println(s"""/* $str */""")
    }


}

trait ChiselNestedCodegen extends GenericNestedCodegen with ChiselCodegen {
  val IR: Expressions with Effects
  import IR._

  // Need this again since we override base behavior in ChiselCodegen
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reflect(s, u, effects) => emitNode(sym, s)
    case Reify(s, u, effects) =>
    case _ => super.emitNode(sym, rhs)
  }

  // emit forward decls for recursive vals
  override def traverseStmsInBlock[A](stms: Seq[Stm]): Unit = {
    recursive foreach emitForwardDef
    super.traverseStmsInBlock(stms)
  }

  def emitForwardDef(sym: Sym[Any]): Unit = {
    stream.println("var " + quote(sym) + /*": " + remap(sym.tp) +*/ " = null.asInstanceOf[" + remap(sym.tp) + "]")
  }

  // special case for recursive vals
  override def emitValDef(sym: Sym[Any], rhs: String): Unit = {
    if (recursive contains sym)
      stream.println(quote(sym) + " = " + rhs) // we have a forward declaration above.
    else
      super.emitValDef(sym,rhs)
  }

}


trait ChiselFatCodegen extends GenericFatCodegen with ChiselCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._

  private def dataDeps(rhs: Any) = {
    val bound = boundSyms(rhs)
    val used = syms(rhs)
    focusFatBlock(used.map(Block(_))) { freeInScope(bound, used) }
  }

  def hasDef(sym: Sym[Any]) = sym match {
    case Def(d) => true
    case _ => false
  }

  def recursiveDeps(rhs: Any): List[Sym[Any]] = {
    val deps: List[Sym[Any]] = dataDeps(rhs).filter { hasDef(_) }
    val deps2: List[Sym[Any]] = deps.map { s =>
      val Def(d) = s
      recursiveDeps(d)
    }.flatten.toList
    (deps2 ++ deps).distinct
  }
}

