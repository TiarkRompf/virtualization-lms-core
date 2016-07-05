package scala.virtualization.lms
package internal

import util.NullOutputStream
import java.io.{File, FileWriter, PrintWriter}

import scala.reflect.SourceContext
import scala.collection.mutable.Stack

trait MaxJCodegen extends GenericCodegen with Config {
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

  override def deviceTarget: Targets.Value = Targets.MaxJ
  override def fileExtension = "maxj"
  override def toString = "maxj"
  override def resourceInfoType = ""
  override def resourceInfoSym = ""

  override def singleFileName = s"TopKernelLib.$fileExtension"

  // Generate all code into one file
  override def emitSingleFile() = true

  val controlNodeStack = Stack[Sym[Any]]()

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {
    val staticData = getFreeDataBlock(body)

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
  }

  override def postProcess[A: Manifest](body: Block[A]) = {
    val (file, stream) = getFileStream()
    withStream(stream) {
      alwaysGen {
        emitFileFooter()
      }
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
  }

  override def finalizeGenerator() = {
    val (file, stream) = getFileStream()
    stream.flush
    stream.close
  }

  override def emitFileHeader() = {
    // empty by default. override to emit package or import declarations.
    emit("/*****************************************\n"+
         "  MaxJ BACKEND: emitSource \n"+
         "*******************************************/")
    emit(s"""package engine;""")
    imports.map(x => emit(s"""import ${importPrefix}.${x};"""))
    emit(s"""class TopKernelLib extends KernelLib {""")
    emit(s"""TopKernelLib(KernelLib owner, DFEVar top_en, DFEVar top_done) {""")
    emit(s"""super(owner);""")
  }

	override def emitFileFooter() = {
		emit("	}")
		emit("}")
    emit("/*****************************************\n"+
         "  End of MaxJ BACKEND \n"+
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
    stream.println("val " + quote(sym) + " = " + rhs + extra)
  }

  def emitVarDef(sym: Sym[Variable[Any]], rhs: String): Unit = {
    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = " + rhs)
  }

  // TODO: Unused?
  def emitVarDecl(sym: Sym[Any]): Unit = {
    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = null.asInstanceOf[" + remap(sym.tp) + "];")
  }

  def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
    stream.println(quote(sym) + " = " + rhs + ";")

  }

  override def quote(x: Exp[Any]) = x match {
    case Const(l: Long) => l.toString + "L"
    case Const(null) => "null.asInstanceOf["+x.tp+"]"
    case _ => super.quote(x)
  }

	def emit(str: String):Unit = {
		stream.println(str)
	}

	def emitComment(str: String):Unit = {
		stream.println(s"""/* $str */ """)
	}

  val importPrefix = "com.maxeler.maxcompiler.v2"

  val imports = List(
    "kernelcompiler.stdlib.core.Count.Counter",
    "kernelcompiler.stdlib.core.CounterChain",
    "kernelcompiler.stdlib.core.Count",
    "kernelcompiler.stdlib.core.Count.Params",
    "kernelcompiler.stdlib.memory.Memory",
    "kernelcompiler.Kernel",
    "kernelcompiler.KernelParameters",
    "kernelcompiler.types.base.DFEVar",
    "utils.MathUtils",
    "utils.Bits",
    "kernelcompiler.KernelLib",
    "kernelcompiler.stdlib.KernelMath",
    "kernelcompiler.types.base.DFEType",
    "kernelcompiler.stdlib.core.Stream.OffsetExpr",
    "kernelcompiler.stdlib.Reductions",
    "kernelcompiler.SMIO",
    "kernelcompiler.stdlib.Accumulator",
    "kernelcompiler.types.base.DFEType",
    "kernelcompiler.types.composite.DFEVector",
    "kernelcompiler.types.composite.DFEVectorType",
    "kernelcompiler.types.base.DFEFix.SignMode"
  )

}

trait MaxJNestedCodegen extends GenericNestedCodegen with MaxJCodegen {
  val IR: Expressions with Effects
  import IR._

  // Need this again since we override base behavior in MaxJCodegen
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


trait MaxJFatCodegen extends GenericFatCodegen with MaxJCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
}

