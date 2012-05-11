package scala.virtualization.lms
package internal

import java.io._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader


trait ScalaCompile extends Expressions {

  val codegen: ScalaCodegen { val IR: ScalaCompile.this.type }

  var compiler: Global = _
  var reporter: ConsoleReporter = _
  //var output: ByteArrayOutputStream = _ 

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(":")
      case _ => System.getProperty("sun.boot.class.path")
    }
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
    val staticData = codegen.emitSource(f, className, new PrintWriter(source))

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
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    
    val obj: A=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[A=>B]
    obj
  }
}