/*package scala.virtualization.lms
package common

import java.io.PrintWriter
import java.io.StringWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenerationFailedException}
import scala.virtualization.lms.util.ClosureCompare

import scala.reflect.SourceContext

trait UninlinedFunctions extends Base { 
  implicit def toUninlinedFuncOps0[B:Manifest](fun: Rep[()=>B]) = new UninlinedFuncOps0(fun)
  implicit def toUninlinedFuncOps1[A:Manifest,B:Manifest](fun: Rep[A=>B]) = new UninlinedFuncOps1(fun)
  implicit def toUninlinedFuncOps2[A1:Manifest,A2:Manifest,B:Manifest](fun: Rep[(A1,A2)=>B]) = new UninlinedFuncOps2(fun)
  
  class UninlinedFuncOps0[B:Manifest](f: Rep[()=>B]) {
    def apply()(implicit pos: SourceContext): Rep[B] = uninlinedFuncApply(f)
  }
  class UninlinedFuncOps1[A:Manifest,B:Manifest](f: Rep[A=>B]) {
    def apply(x: Rep[A])(implicit pos: SourceContext): Rep[B] = uninlinedFuncApply(f,x)
  }
  class UninlinedFuncOps2[A1:Manifest,A2:Manifest,B:Manifest](f: Rep[(A1,A2)=>B]) {
    def apply(x: Rep[A1], y: Rep[A2])(implicit pos: SourceContext): Rep[B] = uninlinedFuncApply(f,x,y)
  }

  def createUninlinedFunc0[B:Manifest](fun: () => Rep[B], dRet: String = "", si: () => Rep[Unit] = null)(implicit pos: SourceContext): Rep[()=>B]
  def createUninlinedFunc1[A:Manifest,B:Manifest](fun: Rep[A] => Rep[B], argType: String = "", dRet: String = "", si: () => Rep[Unit] = null)(implicit pos: SourceContext): Rep[A=>B]
  def createUninlinedFunc2[A1:Manifest,A2:Manifest,B:Manifest](fun: (Rep[A1],Rep[A2]) => Rep[B], argType: String = "", argType2: String = "", dRet: String = "", si: () => Rep[Unit] = null)(implicit pos: SourceContext): Rep[(A1,A2)=>B]
  def uninlinedFuncApply[B:Manifest](fun: Rep[() => B])(implicit pos: SourceContext): Rep[B]
  def uninlinedFuncApply[A:Manifest,B:Manifest](fun: Rep[A => B], arg: Rep[A])(implicit pos: SourceContext): Rep[B]
  def uninlinedFuncApply[A1:Manifest,A2:Manifest,B:Manifest](fun: Rep[(A1,A2)=>B], arg: Rep[A1], arg2: Rep[A2])(implicit pos: SourceContext): Rep[B]
}

trait UninlinedFunctionsExp extends UninlinedFunctions with BaseExp with EffectExp { 
  val functionList0 = new scala.collection.mutable.ListBuffer[UninlinedFunc0[_]]()
  val functionList1 = new scala.collection.mutable.ListBuffer[UninlinedFunc1[_,_]]()
  val functionList2 = new scala.collection.mutable.ListBuffer[UninlinedFunc2[_,_,_]]()

  // UninlinedFunc generates nothing, but is saved in the list and is later printed in the preamble 
  // of the file of the generated code
  case class UninlinedFunc0[B:Manifest](f: () => Exp[B], y:Sym[B], dRet:String = "", si: () => Exp[Unit]) extends Def[()=>B] 
  case class UninlinedFunc1[A:Manifest,B:Manifest](f: Exp[A] => Exp[B], x: Sym[A], y:Sym[B], dynamicType: String, dRet:String = "", si: () => Exp[Unit]) extends Def[A=>B] 
  case class UninlinedFunc2[A1:Manifest,A2:Manifest,B:Manifest](f: (Exp[A1],Exp[A2]) => Exp[B], x: Sym[A1], y: Sym[A2], z: Sym[B], argType: String = "", argType2: String = "", dRet:String = "", si: () => Exp[Unit]) extends Def[(A1,A2)=>B]
  case class UninlinedFuncApply0[B:Manifest](f: Exp[() => B]) extends Def[B]
  case class UninlinedFuncApply1[A:Manifest,B:Manifest](f: Exp[A => B], arg: Exp[A]) extends Def[B]
  case class UninlinedFuncApply2[A1:Manifest,A2:Manifest,B:Manifest](f: Exp[(A1,A2) => B], arg: Exp[A1], arg2: Exp[A2]) extends Def[B]

  // si stands for State Initializer
  override def createUninlinedFunc0[B:Manifest](f: () => Exp[B], dRet: String = "", si: () => Exp[Unit] = null)(implicit pos: SourceContext): Exp[()=>B] = {
    val res = fresh[B] // overcomes the fact that the list has B = Any
    val l = UninlinedFunc0(f, res, dRet, si)
    functionList0 += l 
    l
  }
  override def createUninlinedFunc1[A:Manifest,B:Manifest](f: Exp[A] => Exp[B], argType: String = "", dRet: String = "", si: () => Exp[Unit] = null)(implicit pos: SourceContext): Exp[A=>B] = {
    val e = fresh[A]
    val res = fresh[B] // overcomes the fact that the list has B = Any
    val l = UninlinedFunc1(f, e, res, argType, dRet, si)
    functionList1 += l 
    l
  }
  override def createUninlinedFunc2[A1:Manifest,A2:Manifest,B:Manifest](f: (Exp[A1],Exp[A2]) => Exp[B], argType: String = "", argType2: String = "", dRet: String = "", si: () => Exp[Unit] = null)(implicit pos: SourceContext): Exp[(A1,A2)=>B] = {
    val e1 = fresh[A1]
    val e2 = fresh[A2]
    val res = fresh[B] // overcomes the fact that the list has B = Any
    val l = UninlinedFunc2(f, e1, e2, res, argType, argType2, dRet, si) 
    functionList2 += l 
    l
  }

  def uninlinedFuncApply[B:Manifest](f: Exp[()=>B])(implicit pos: SourceContext): Exp[B] = {
    reflectEffect(UninlinedFuncApply0(f))
  }
  def uninlinedFuncApply[A:Manifest,B:Manifest](f: Exp[A=>B], x: Exp[A])(implicit pos: SourceContext): Exp[B] = {
    reflectEffect(UninlinedFuncApply1(f, x))
  }
  def uninlinedFuncApply[A1:Manifest,A2:Manifest,B:Manifest](f: Exp[(A1,A2)=>B], x1: Exp[A1], x2: Exp[A2])(implicit pos: SourceContext): Exp[B] = {
    reflectEffect(UninlinedFuncApply2(f, x1, x2))
  }
}

trait ScalaGenUninlinedFunctions extends ScalaGenEffect {
  val IR: UninlinedFunctionsExp
  import IR._
  
  var emitEnabled: Boolean = false

  private def printState(si: () => Exp[Unit]) {
    // Print state if si is set
    if (si != null) {
        val state = reifyEffects(si())
        emitBlock(state)
    }
  }

  private def argsToStr(e: List[Sym[Any]], argTypes: List[String]) = {
    val z = e zip argTypes
    z.map(x => {
        if (quote(x._1) != "") quote(x._1) + ": " + x._2
        else "x" + x._1.toString.replace("Sym(","").replace(")","") + ": " + x._2
    }).mkString(",")
  }

  private def printUninlinedFuncBody[T:Manifest](b: Block[T]) {
    emitBlock(b)
    stream.println(quote(getBlockResult(b)))
    stream.println("}")
    stream.println("}")
    stream.println()
  }

  private def getArgTypes(syms: List[Sym[Any]], args: String*) = {
    val z = syms zip args.toList
    z.map(x => if (x._2 != "") x._2 else remap(x._1.tp))
  }

  private def getReturnType(res: Sym[Any], dRet: String) = if (dRet != "") dRet else remap(res.tp)

  override def emitFileHeader() = {
    emitEnabled = true
    functionList0.foreach(func => traverseStm(findDefinition(func).get))
    functionList1.foreach(func => traverseStm(findDefinition(func).get))
    functionList2.foreach(func => traverseStm(findDefinition(func).get))
    emitEnabled = false
    functionList0.clear
    functionList1.clear
    functionList2.clear
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case UninlinedFunc0(fun, res, dRet, si) => {
        if (emitEnabled) {
            val retType = getReturnType(res, dRet)
            stream.println("object " + quote(sym)
                           + " extends (()=>" + retType +") {")
            emitEnabled = false
            printState(si)
            // Now print function
			val f = reifyEffects(fun())
            stream.println("def apply(): " + retType + " = {")
            printUninlinedFuncBody(f)
            emitEnabled = true
        }
    }
    case UninlinedFunc1(fun, e, res, argType, dRet, si) => {
        if (emitEnabled) {
            val syms = List(e)
            val argTypes = getArgTypes(syms, argType)
            val retType = getReturnType(res, dRet)
            stream.println("object " + quote(sym)
                           + " extends ((" + argTypes.mkString(",") + ")=>" + retType +") {")
            emitEnabled = false
            printState(si)
            // Now print function
            stream.println("def apply(" + argsToStr(syms, argTypes) + "): " + retType + " = {")
            printUninlinedFuncBody(reifyEffects(fun(e)))
            emitEnabled = true
        }
    }
    case UninlinedFunc2(fun, e1, e2, res, argType, argType2, dRet, si) => {
        if (emitEnabled) {
            val syms = List(e1,e2)
            val argTypes = getArgTypes(syms,argType,argType2)
            val retType = getReturnType(res, dRet)
            stream.println("object " + quote(sym)
                           + " extends ((" + argTypes.mkString(",") + ")=>" + retType +") {")
            emitEnabled = false
            printState(si)
            // Now print function
            stream.println("def apply(" + argsToStr(syms, argTypes) + "): " + retType + " = {")
            printUninlinedFuncBody(reifyEffects(fun(e1,e2)))
            emitEnabled = true
        }
    }
    case UninlinedFuncApply0(fun) =>
      emitValDef(sym, quote(fun) + "()")
    case UninlinedFuncApply1(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case UninlinedFuncApply2(fun, arg, arg2) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ", " + quote(arg2) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenUninlinedFunctions extends CGenEffect {
  val IR: UninlinedFunctionsExp
  import IR._
  
  var emitEnabled: Boolean = false

  private def printState(si: () => Exp[Unit]) {
    // Print state if si is set
    if (si != null) {
        val state = reifyEffects(si())
        emitBlock(state)
    }
  }

  private def argsToStr(e: List[Sym[Any]], argTypes: List[String]) = {
    val z = e zip argTypes
    z.map(x => {
        if (quote(x._1) != "") x._2 + " " + quote(x._1)
        else x._2 + " x" + x._1.toString.replace("Sym(","").replace(")","")
    }).mkString(",")
  }

  private def printUninlinedFuncBody[T:Manifest](b: Block[T]) = {
	val sw = new StringWriter()
	val pw = new PrintWriter(sw)
	withStream(pw) {
    	emitBlock(b)
	    stream.println("return " + quote(getBlockResult(b)) + ";")
    	stream.println("}")
	    stream.println()
	}
	sw.toString
  }

  private def getArgTypes(syms: List[Sym[Any]], args: String*) = {
    val z = syms zip args.toList
    z.map(x => if (x._2 != "") x._2 else remap(x._1.tp))
  }

  private def getReturnType(res: Sym[Any], dRet: String) = if (dRet != "") dRet else remap(res.tp)

  override def emitFileHeader() = {
    emitEnabled = true
    functionList0.foreach(func => traverseStm(findDefinition(func).get))
    functionList1.foreach(func => traverseStm(findDefinition(func).get))
    functionList2.foreach(func => traverseStm(findDefinition(func).get))
    emitEnabled = false
    functionList0.clear
    functionList1.clear
    functionList2.clear
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case UninlinedFunc0(fun, res, dRet, si) => {
        if (emitEnabled) {
            val retType = getReturnType(res, dRet)
            stream.println("object " + quote(sym)
                           + " extends (()=>" + retType +") {")
            emitEnabled = false
            printState(si)
            // Now print function
            stream.println("def apply(): " + retType + " = {")
            printUninlinedFuncBody(reifyEffects(fun()))
            emitEnabled = true
        }
    }
    case UninlinedFunc1(fun, e, res, argType, dRet, si) => {
        if (emitEnabled) {
            val syms = List(e)
            val argTypes = getArgTypes(syms, argType)
            val retType = getReturnType(res, dRet)
            emitEnabled = false
            printState(si)
            // Now print function
			val argStr = argsToStr(syms, argTypes)
            val header = retType + " " + quote(sym) + "(" + argStr + ") {\n"
            var str = header + printUninlinedFuncBody(reifyEffects(fun(e)))
			if (e.tp == manifest[Tuple2[Any,Any]]) {
				str = str.replace(quote(e) + "._1", "key").replace(quote(e) + "._2", "value")
				str = str.replace(argStr,"void* key, void* value")
			}
			stream.println(str)
            emitEnabled = true
        }
    }
    case UninlinedFunc2(fun, e1, e2, res, argType, argType2, dRet, si) => {
        if (emitEnabled) {
            val syms = List(e1,e2)
            val argTypes = getArgTypes(syms,argType,argType2)
            val retType = getReturnType(res, dRet)
            emitEnabled = false
            printState(si)
            // Now print function
            stream.println(retType + " " + quote(sym) + "(" + argsToStr(syms, argTypes) + ") {")
            var str = printUninlinedFuncBody(reifyEffects(fun(e1,e2)))
			// HACK -- STUPID GLIB!
			if (argType.contains("**")) {
				str = str.replace(quote(e1),"(*" + quote(e1) + ")")
				str = str.replace(quote(e2),"(*" + quote(e2) + ")")
			}
			stream.println(str)
            emitEnabled = true
        }
    }
    case UninlinedFuncApply0(fun) =>
      emitValDef(sym, quote(fun) + "()")
    case UninlinedFuncApply1(fun, arg) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ")")
    case UninlinedFuncApply2(fun, arg, arg2) =>
      emitValDef(sym, quote(fun) + "(" + quote(arg) + ", " + quote(arg2) + ")")

    case _ => super.emitNode(sym, rhs)
  }
}*/
