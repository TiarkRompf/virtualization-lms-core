package scala.virtualization.lms
package internal

import util.NullOutputStream
import java.io.{File, FileWriter, PrintWriter}

import scala.reflect.SourceContext

trait DotCodegen extends GenericCodegen with Config {
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


  override def deviceTarget: Targets.Value = Targets.Dot
  override def fileExtension = "dot"
  override def toString = "dot"
  override def resourceInfoType = ""
  override def resourceInfoSym = ""

  // Generate all code into one file
  override def emitSingleFile() = true

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
  }

  override def emitFileHeader() {
    // empty by default. override to emit package or import declarations.
    emit("/*****************************************\n"+
         "  DOT BACKEND: emitSource \n"+
         "*******************************************/")
    emit("digraph G {")
  }
	override def emitFileFooter() = {
		emit("}")
    emit("/*****************************************\n"+
         "  End of Dot BACKEND \n"+
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
		if (!isVoidType(sym.tp))
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
    case Const(s: Unit) => ""
    case _ => super.quote(x)
  }

	def emitAlias(x: Exp[Any], y: Exp[Any]):Unit = {
		stream.println(s"""define(`${quote(x)}', `${quote(y)}')""")
	}
	def emitAlias(x: Sym[Any], y: String):Unit = {
		stream.println(s"""define(`${quote(x)}', `${y}')""")
	}
	def emitEdge(x:Sym[Any], y:Exp[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Sym[Any], y:Exp[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ xlabel="${label}" ]""")
	}
	def emitEdge(x:Sym[Any], y:Sym[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Sym[Any], y:Sym[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ xlabel="${label}" ]""")
	}
	def emitEdge(x:Exp[Any], y:Sym[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Exp[Any], y:Sym[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ xlabel="${label}" ]""")
	}
	def emitEdge(x:Exp[Any], y:Exp[Any]):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)}""")
	}
	def emitEdge(x:Exp[Any], y:Exp[Any], label:String):Unit = {
		stream.println(s"""${quote(x)} -> ${quote(y)} [ xlabel="${label}" ]""")
	}

	def emit(str: String):Unit = {
		stream.println(str)
	}

	def emitComment(str: String):Unit = {
		stream.println(s"""/* $str */ """)
	}

	val arrowSize = 0.6
	val edgeThickness = 0.5
	val memColor = s""""#6ce6e1""""
	val regColor = s""""#8bd645""""
	val offChipColor = s""""#1A0000""""
	val dblbufBorderColor = s""""#4fb0b0""""
	val ctrlColor = s""""red""""
	val counterColor = s""""#e8e8e8""""
	val counterInnerColor = s""""gray""""
	val fontsize = 10
	val defaultShape = "square"
	val bgcolor = s""""white""""

	// Pipe Colors
	//val pipeFillColor = "#4FA1DB"
	val pipeFillColor = s""""white""""
	val pipeBorderColor = s""""black""""

	// Block Colors
	val mapFuncColor = s""""#f7f26f""""
	val reduceFuncColor = s""""#f2a2cc""""
	val ldFuncColor = s""""#7be58f""""
	val stFuncColor = s""""#7be58f""""

	// Metapipeline colors
	val mpFillColor = s""""#4FA1DB""""
	val mpBorderColor = s""""#4FA1DB""""
	val mpStageFillColor = s""""#BADDFF""""
	val mpStageBorderColor = s""""none""""

	// Parallel colors
	//val parallelFillColor = "#4FDBC2"
	val parallelFillColor = s""""white""""
	//val parallelBorderColor = s""""#00AB8C""""
	val parallelBorderColor = s""""black""""
	val parallelStageFillColor = s""""#CCFFF6""""
	val parallelStageBorderColor = s""""none""""

	// Tile transfer colors
	val tileTransFillColor = s""""#FFA500""""

}

trait DotNestedCodegen extends GenericNestedCodegen with DotCodegen {
  val IR: Expressions with Effects
  import IR._

  // Need this again since we override base behavior in DotCodegen
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reflect(s, u, effects) => emitNode(sym, s)
    case Reify(s, u, effects) =>
    case _ => super.emitNode(sym, rhs)
  }

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


trait DotFatCodegen extends GenericFatCodegen with DotCodegen {
  val IR: Expressions with Effects with FatExpressions
  import IR._
}
