package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}

trait GenericCodegen extends Traversal {
  val IR: Expressions
  import IR._

  // TODO: should some of the methods be moved into more specific subclasses?
  
  def kernelFileExt = ""
  def emitKernelHeader(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {}
  def emitKernelFooter(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultType: String, resultIsVar: Boolean, external: Boolean)(implicit stream: PrintWriter): Unit = {}
  
  // Initializer
  def initializeGenerator(buildDir:String): Unit = {}
  def finalizeGenerator(): Unit = {}
  def kernelInit(syms: List[Sym[Any]], vals: List[Sym[Any]], vars: List[Sym[Any]], resultIsVar: Boolean): Unit = {}

  def emitDataStructures(path: String): Unit = {}
  
  // exception handler
  def exceptionHandler(e: Exception, outFile:File, kstream:PrintWriter): Unit = {
      kstream.close()
      outFile.delete
  }

  // optional type remapping (default is identity)
  def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[Any]] ) {
      remap(m.typeArguments.head)
    }
    else m.toString
  }
  def remapImpl[A](m: Manifest[A]) : String = remap(m)
  //def remapVar[A](m: Manifest[Variable[A]]) : String = remap(m.typeArguments.head)

  def hasMetaData: Boolean = false
  def getMetaData: String = null

  def getDSLHeaders: String = null


  // ----------

  def emitBlock(y: Block[Any])(implicit stream: PrintWriter): Unit = {
    val deflist = buildScheduleForResult(getBlockResult(y))
    
    for (TP(sym, rhs) <- deflist) {
      emitNode(sym, rhs)
    }
  }

  def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
    throw new GenerationFailedException("don't know how to generate code for: " + rhs)
  }
  
  def emitExternalLib(rhs: Def[Any]): Unit = {
    throw new GenerationFailedException("don't know how to generate external lib for " + rhs)
  }
  
  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit
    
  def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): List[(Sym[Any], Any)] // return free static data in block
      
  def quote(x: Exp[Any]) : String = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(null) => "null" // why is null getting lifted now? something to do with Equal
    case Const(f: Float) => f.toString + "f"
    case Const(z) => z.toString
    case Sym(n) => "x"+n
    case null => "null"
    case _ => throw new RuntimeException("could not quote " + x)
  }

}



trait GenericNestedCodegen extends NestedTraversal with GenericCodegen {
  val IR: Expressions with Effects
  import IR._

  override def emitBlock(result: Block[Any])(implicit stream: PrintWriter): Unit = {
    focusBlock(result) {
      emitBlockFocused(result)
    }
  }
  
  def emitBlockFocused(result: Block[Any])(implicit stream: PrintWriter): Unit = {
    focusExactScope(result) { levelScope =>
      for (TP(sym, rhs) <- levelScope)
        emitNode(sym, rhs)
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
//    case Read(s) =>
//      emitValDef(sym, quote(s))
    case Reflect(s, u, effects) =>
      emitNode(sym, s)
    case Reify(s, u, effects) =>
      // just ignore -- effects are accounted for in emitBlock
    case _ => super.emitNode(sym, rhs)
  }




}