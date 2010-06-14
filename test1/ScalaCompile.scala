package test1

import java.io._

import util.GraphUtil

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader


trait ScalaCompile extends Expressions with Compile { self: ScalaCodegen =>

  var compiler: Global = _
  var reporter: ConsoleReporter = _
  //var output: ByteArrayOutputStream = _ 

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()

  //      settings.Xcodebase.value = codebasePath.toString()
    settings.classpath.value = ""
    settings.bootclasspath.value = ""
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
  //    settings.verbose.value = true

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  var compileCount = 0

  def compile[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A], mB: Manifest[B]): A=>B = {
    if (this.compiler eq null)
      setupCompiler()
    
    val className = "staged$" + compileCount
    compileCount += 1
    
    val source = new StringWriter()
    emitScalaSource(f, className, new PrintWriter(source))

    val compiler = this.compiler
    val run = new compiler.Run

    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
  //      compiler.genJVM.outputDir = fileSystem

    run.compileSources(List(new util.BatchSourceFile("<stdin>", source.toString)))
    reporter.printSummary()

    if (!reporter.hasErrors)
      println("compilation: ok")
    else
      println("compilation: had errors")

    reporter.reset
    //output.reset

    val parent = this.getClass.getClassLoader
    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)

    val cls: Class[_] = loader.loadClass(className)
    val obj: A=>B = cls.newInstance().asInstanceOf[A=>B]
    obj
  }
}


trait ScalaCodegen extends GenericCodegen {

  def emitScalaSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {

    val x = fresh
    val y = f(x)

    val sA = mA.toString
    val sB = mB.toString

    stream.println("class "+className+" extends (("+sA+")=>("+sB+")) {")
    stream.println("def apply("+quote(x)+":"+sA+"): "+sB+" = {")
    
    emitBlock(y, stream)
    stream.println(quote(getBlockResult(y)))
    
    stream.println("}")
    stream.println("}")
    stream.flush
  }


  def emitValDef(sym: Sym[_], rhs: String, stream: PrintWriter): Unit = {
    stream.println("val " + quote(sym) + " = " + rhs)
  }

}
