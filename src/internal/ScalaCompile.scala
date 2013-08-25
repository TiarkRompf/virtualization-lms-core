package scala.virtualization.lms
package internal

import java.io._
import scala.virtualization.lms.util._

import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._

import scala.tools.nsc.interpreter.AbstractFileClassLoader

object ScalaCompile {
  var compileCount = 0
  var dumpGeneratedCode = false
  var compiler: Global = _
  var reporter: ConsoleReporter = _
  var cleanerEnabled: Boolean = true 
  //var output: ByteArrayOutputStream = _ 
  val source = new StringWriter()
  val writer = new PrintWriter(source)
  val fileSystem = new VirtualDirectory("<vfs>", None)
  val loader = new AbstractFileClassLoader(fileSystem, this.getClass.getClassLoader)
  lazy val comp = this.compiler
}

trait ScalaCompile extends Expressions {

  val codegen: ScalaCodegen { val IR: ScalaCompile.this.type }

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

    ScalaCompile.reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    ScalaCompile.compiler = new Global(settings, ScalaCompile.reporter)
  }

  def initCompile = {
    System.out.println("Initializing compiler...")
    ScalaCompile.source.getBuffer().setLength(0) 
    ScalaCompile.compileCount = ScalaCompile.compileCount + 1
    val className = "staged$" + ScalaCompile.compileCount
    className
  }

  def compileLoadClass(src: StringWriter, className: String) = {
    if (ScalaCompile.compiler eq null)
        setupCompiler()
    if (ScalaCompile.dumpGeneratedCode) println(src)

    ScalaCompile.compiler.settings.outputDirs.setSingleOutput(ScalaCompile.fileSystem)
    val run = new ScalaCompile.comp.Run
    var parsedsrc = src.toString

    if (ScalaCompile.cleanerEnabled) {
        println("\n\n------------------------------------------------")
        println("EXPERIMENTAL:: CODE BEFORE RUNNING CODEGEN CLEANER")
        println(src.toString)
        parsedsrc = CodegenCleaner.clean(src.toString)
        println("EXPERIMENTAL:: CODE AFTER RUNNING CODEGEN CLEANER")
        println(parsedsrc)
        println("\n\n------------------------------------------------")
    }

    run.compileSources(List(new util.BatchSourceFile("<stdin>", parsedsrc)))
    ScalaCompile.reporter.printSummary()
    if (ScalaCompile.reporter.hasErrors) {
      println("compilation of the following code had errors:")
      println(src)
    }
    ScalaCompile.reporter.reset

    val cls: Class[_] = ScalaCompile.loader.loadClass(className)
    cls
  }

  // Compile0 should never take dynamicClasses as argument (to rep to handle)
  def compile0[B](f: () => Exp[B],dynamicReturnType: String = null)(implicit mB: Manifest[B]): () =>B = {
    val className = initCompile 
    val staticData = codegen.emitSource0(f, className, ScalaCompile.writer, dynamicReturnType)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    val obj: ()=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[()=>B]
    obj
  }

  def compile1[A,B](f: Exp[A] => Exp[B], dynamicClass: String = null, dynamicReturnType: String = null)(implicit mA: Manifest[A], mB: Manifest[B]): A=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource1(f, className, ScalaCompile.writer, dynamicClass, dynamicReturnType)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    val obj: A=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[A=>B]
    obj
  }

  def compile2[A1,A2,B](f: (Exp[A1],Exp[A2]) => Exp[B], dynamicClass: String = null, dynamicClass2: String = null, dynamicReturnType: String = null)(implicit mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]): (A1,A2)=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource2(f, className, ScalaCompile.writer, dynamicClass, dynamicClass2, dynamicReturnType)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    val obj: (A1,A2)=>B = cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[(A1,A2)=>B]
    obj
  }

    
}
