package scala.virtualization.lms
package internal

import util.NullOutputStream
import java.io.{File, FileWriter, PrintWriter}

import scala.reflect.SourceContext

trait MaxJCodegen extends GenericCodegen with Config {
  val IR: Expressions
  import IR._

  var inHwScope = false
  private val _nullstream = new PrintWriter(new NullOutputStream())
  override def stream = if (inHwScope) super.stream else _nullstream
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

  // Generate all code into one file
  override def emitSingleFile() = true

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter) = {

    val sA = remap(manifest[A])

    val staticData = getFreeDataBlock(body)

    withStream(out) {
      stream.println("/*****************************************\n"+
                     "  MaxJ BACKEND: emitSource \n"+
                     "*******************************************/")
      emitFileHeader()

      stream.println("digraph G {")
      try {
        emitBlock(body)
      } catch {
        case e: GenerationFailedException =>
          stream.println("// Generation failed exception")
          Console.println(Console.BLACK + Console.YELLOW_B + e.getMessage() + Console.RESET)
      }
      stream.println("}")

      stream.println("/*****************************************\n"+
                     "  End of MaxJ BACKEND \n"+
                     "*******************************************/")
    }

    staticData
  }

  private var bd = ""

  override def initializeGenerator(buildDir: String) = {
    bd = buildDir
    val sep = java.io.File.separator
    val outDir = new File(buildDir); outDir.mkdirs()
  }

  override def finalizeGenerator() = {
  }

  override def emitFileHeader() {
    // empty by default. override to emit package or import declarations.
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

  override def emitVarDecl(sym: Sym[Any]): Unit = {
    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = null.asInstanceOf[" + remap(sym.tp) + "];")
  }

  override def emitAssignment(sym: Sym[Any], rhs: String): Unit = {
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

  // emit forward decls for recursive vals
  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = {
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

