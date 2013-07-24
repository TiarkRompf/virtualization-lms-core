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
  val source = new StringWriter()
  val writer = new PrintWriter(source)

  def setupCompiler() = {
    /*
      output = new ByteArrayOutputStream()
      val writer = new PrintWriter(new OutputStreamWriter(output))
    */
    val settings = new Settings()
	val pathSeparator = System.getProperty("path.separator")

    settings.classpath.value = this.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(pathSeparator)
      case _ => System.getProperty("java.class.path")
    }
    settings.bootclasspath.value = Predef.getClass.getClassLoader match {
      case ctx: java.net.URLClassLoader => ctx.getURLs.map(_.getPath).mkString(pathSeparator)
      case _ => System.getProperty("sun.boot.class.path")
    }
    settings.encoding.value = "UTF-8"
    settings.outdir.value = "."
    settings.extdirs.value = ""
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    compiler = new Global(settings, reporter)
  }

  var compileCount = 0
  
  var dumpGeneratedCode = false

  def initCompile = {
    source.getBuffer().setLength(0) 
    val className = "staged$" + compileCount
    compileCount += 1
    className
  }

  def compileLoadClass(src: StringWriter, className: String) = {
    if (this.compiler eq null)
        setupCompiler()
    if (dumpGeneratedCode) println(src)

    val compiler = this.compiler
    val run = new compiler.Run
    val fileSystem = new VirtualDirectory("<vfs>", None)
    compiler.settings.outputDirs.setSingleOutput(fileSystem)
    run.compileSources(List(new util.BatchSourceFile("<stdin>", src.toString)))
    reporter.printSummary()
    if (reporter.hasErrors) {
      println("compilation of the following code had errors:")
      println(src)
    }
    reporter.reset

    val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)
    val cls: Class[_] = loader.loadClass(className)
    cls
  }

  def compile0[B](f: () => Exp[B], dynamicClass: Class[_] = null)(implicit mB: Manifest[B]): () =>B = {
    val className = initCompile 
    val staticData = codegen.emitSource0(f, className, writer, dynamicClass)
    codegen.emitDataStructures(writer)
    val cls = compileLoadClass(source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    val obj: ()=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[()=>B]
    obj
  }

  def compile[A,B](f: Exp[A] => Exp[B], dynamicClass: Class[_] = null)(implicit mA: Manifest[A], mB: Manifest[B]): A=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource1(f, className, writer, dynamicClass)
    codegen.emitDataStructures(writer)
    val cls = compileLoadClass(source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    val obj: A=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[A=>B]
    obj
  }

  def compile2[A1,A2,B](f: (Exp[A1],Exp[A2]) => Exp[B], dynamicClass: Class[_] = null)(implicit mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]): (A1,A2)=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource2(f, className, writer, dynamicClass)
    codegen.emitDataStructures(writer)
    val cls = compileLoadClass(source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    val obj: (A1,A2)=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[(A1,A2)=>B]
    obj
  }
}
