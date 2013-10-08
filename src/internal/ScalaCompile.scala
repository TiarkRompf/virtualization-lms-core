package scala.virtualization.lms
package internal

import java.io.{StringWriter, PrintWriter}
import scala.virtualization.lms.util._

import scala.sys.process._
import scala.tools.nsc._
import scala.tools.nsc.util._
import scala.tools.nsc.reporters._
import scala.tools.nsc.io._
import scala.tools.nsc.interpreter.AbstractFileClassLoader

import java.lang.management.ManagementFactory;
import javax.management.ObjectName;
import javax.management.openmbean.CompositeData;
import javax.management.openmbean.CompositeDataSupport;

object ScalaCompile {
  var compileCount = 0
  var dumpGeneratedCode = false
  var compiler: Global = _
  var reporter: ConsoleReporter = _
  // From what I understand, this is not currently exported from the JVM, but it used internally.
  // (To check, run java -XX:+PrintFlagsFinal -version | grep Huge) and check for the limit.
  val maximumHugeMethodLimit = 8000 
  // NOTE: Always disable these two flags when running the test suite
  val byteCodeSizeCheckEnabled: Boolean = false
  var cleanerEnabled: Boolean = false 
  val source = new StringWriter()
  var writer = new PrintWriter(source)
  val workingDir = System.getProperty("user.dir") + "/CompiledClasses"
  val loader = new AbstractFileClassLoader(AbstractFile.getDirectory(workingDir), this.getClass.getClassLoader)
  lazy val comp = this.compiler
}

trait ScalaCompile extends Expressions {

  val codegen: ScalaCodegen { val IR: ScalaCompile.this.type }

  def setupCompiler() = {
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

    // Create output directory if it does not exist
    val f = new java.io.File(ScalaCompile.workingDir)
    if (!f.exists)
        f.mkdirs()

    settings.outdir.value = ScalaCompile.workingDir
    settings.extdirs.value = ""
    //settings.verbose.value = true
    // -usejavacp needed on windows?

    ScalaCompile.reporter = new ConsoleReporter(settings, null, new PrintWriter(System.out))//writer
    ScalaCompile.compiler = new Global(settings, ScalaCompile.reporter)
  }

  def initCompile = {
    // System.out.println("Initializing compiler...") // This unfortunately
    // breaks the test suite as well :-(
    ScalaCompile.source.getBuffer().setLength(0) 
    val className = "staged" + ScalaCompile.compileCount
    ScalaCompile.compileCount = ScalaCompile.compileCount + 1
    className
  }

  def checkByteCodeSize(className: String): Int = {
    lazy val runtime: Runtime = Runtime.getRuntime();
    // Is compiling huge methods allowed?
    val mserver = ManagementFactory.getPlatformMBeanServer();
    val name = new ObjectName("com.sun.management:type=HotSpotDiagnostic");
    val operationName = "getVMOption";
    val params = Array[Object]("DontCompileHugeMethods")
    val signature = Array[String](classOf[String].getName())
    val result = mserver.invoke(name,operationName,params,signature).asInstanceOf[CompositeDataSupport].get("value")
    // If yes, then check the size
    if (result == "true") {
        val cmd = Seq("javap","-classpath",ScalaCompile.workingDir,"-c",className) #| Seq("cut","-d:","-f1") #| Seq("sort","-n") #| Seq("tail","-n1")
        val size = cmd.!!.trim.toInt
        if (size > ScalaCompile.maximumHugeMethodLimit) {
            println("\n\n|------------------------------------------------------------------------------------|")
            println("| CATASTROPHIC ERROR ENCOUNTERED!!! YOUR CODE IS TOO BIG TO BE COMPILED BY THE JVM   |")
            println("| AND WILL BE INTERPRETED INSTEAD. THIS WILL CAUSE A DRAMATIC PERFORMANCE DROP.      |")
            println("| THE DEVELOPERS WORRY ABOUT YOUR MENTAL HEALTH, AND CANNOT ALLOW YOU TO EXPERIENCE  |")
            println("| THAT. EXITING NOW!                                                                 |")
            println("|                                                                                    |")
            println("| Note: You have two alternatives:                                                   |")
            println("| \t(a) Refactor your code so that the generated code size is smaller.(advised)  |")
            println("| \t(b) Set JVM Option DontCompileHugeMethods to false and rerun (Not advised).  |")
            println("| -----------------------------------------------------------------------------------|")
            System.exit(0)
        }
        return size;
    }
    -1;
  }

  def compileLoadClass(src: StringWriter, className: String) = {
    if (ScalaCompile.compiler eq null)
        setupCompiler()
    if (ScalaCompile.dumpGeneratedCode) println(src)

    ScalaCompile.compiler.settings.outputDirs.setSingleOutput(AbstractFile.getDirectory(ScalaCompile.workingDir))
    val run = new ScalaCompile.comp.Run
    var parsedsrc = src.toString

    if (ScalaCompile.cleanerEnabled) {
        println("\n\n------------------------------------------------")
        println("EXPERIMENTAL:: CODE BEFORE RUNNING CODEGEN CLEANER.\n" + parsedsrc)
        parsedsrc = CodegenCleaner.clean(src.toString)
        println("\n\n------------------------------------------------")
        println("EXPERIMENTAL:: CODE AFTER RUNNING CODEGEN CLEANER\n" + parsedsrc)
    }

    run.compileSources(List(new util.BatchSourceFile("<stdin>", parsedsrc)))

    if (ScalaCompile.byteCodeSizeCheckEnabled) {
        val size = checkByteCodeSize(className)
        if (size != -1) println("ByteCode size of the compiled code is: " + size)
    }

    ScalaCompile.reporter.printSummary()
    if (ScalaCompile.reporter.hasErrors) {
      println("compilation of the following code had errors:")
      println(src)
    }
    ScalaCompile.reporter.reset

    val cls: Class[_] = ScalaCompile.loader.loadClass(className)
    cls
  }

  def compile0[B](f: () => Exp[B])(implicit mB: Manifest[B]): () =>B = {
    val className = initCompile 
    val staticData = codegen.emitSource0(f, className, ScalaCompile.writer)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[()=>B]
  }

  def compile1[A,B](f: Exp[A] => Exp[B])(implicit mA: Manifest[A], mB: Manifest[B]): A=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource1(f, className, ScalaCompile.writer)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[A=>B]
  }

  def compile2[A1,A2,B](f: (Exp[A1],Exp[A2]) => Exp[B])(implicit mA1: Manifest[A1], mA2: Manifest[A2], mB: Manifest[B]): (A1,A2)=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource2(f, className, ScalaCompile.writer)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[(A1,A2)=>B]
  }

  def compile3[A1,A2,A3,B](f: (Exp[A1], Exp[A2], Exp[A3]) => Exp[B])(implicit mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mB: Manifest[B]): (A1,A2,A3)=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource3(f, className, ScalaCompile.writer)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[(A1,A2,A3)=>B]
  }

  def compile4[A1,A2,A3,A4,B](f: (Exp[A1], Exp[A2], Exp[A3], Exp[A4]) => Exp[B])(implicit mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mB: Manifest[B]): (A1,A2,A3,A4)=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource4(f, className, ScalaCompile.writer)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[(A1,A2,A3,A4)=>B]
  }
 
  def compile5[A1,A2,A3,A4,A5,B](f: (Exp[A1], Exp[A2], Exp[A3], Exp[A4], Exp[A5]) => Exp[B])(implicit mA1: Manifest[A1], mA2: Manifest[A2], mA3: Manifest[A3], mA4: Manifest[A4], mA5: Manifest[A5], mB: Manifest[B]): (A1,A2,A3,A4,A5)=>B = {
    val className = initCompile 
    val staticData = codegen.emitSource5(f, className, ScalaCompile.writer)
    codegen.emitDataStructures(ScalaCompile.writer)
    val cls = compileLoadClass(ScalaCompile.source, className)
    val cons = cls.getConstructor(staticData.map(_._1.tp.erasure):_*)
    cons.newInstance(staticData.map(_._2.asInstanceOf[AnyRef]):_*).asInstanceOf[(A1,A2,A3,A4,A5)=>B]
  }
}
