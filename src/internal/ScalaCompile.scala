package scala.virtualization.lms
package internal

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader


trait ScalaCompile extends Expressions { self: ScalaCodegen =>

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
    settings.classpath.value = System.getProperty("java.class.path")
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