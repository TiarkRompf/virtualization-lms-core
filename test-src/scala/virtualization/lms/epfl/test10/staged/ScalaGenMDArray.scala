package scala.virtualization.lms
package epfl
package test10

import common._
import internal._
import original.MDArray
import java.io.PrintWriter

trait BaseGenMDArray extends GenericNestedCodegen {

  val IR: MDArrayBaseExp
  import IR._

}


trait TypedGenMDArray extends BaseGenMDArray {

  val IR: TY.IR.type
  val TY: MDArrayTypingBubbleUp
  import IR.{Exp, Sym, Def}
  import TY.TypingConstraint

  def emitChecks(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) =
    for (constraint <- TY.getRuntimeChecks(sym))
      emitConstraint(constraint, "RuntimeCheck ")

  def emitConstraint(expr: TypingConstraint, ctrType: String)(implicit stream: PrintWriter) = {
    stream.println("// " + ctrType + ": " + expr.toString)
  }

  def emitShapeValue(sym: Sym[_])(implicit stream: PrintWriter): Unit = {
    stream.println("// Shape: " + TY.getTypingString(sym))
  }
}

trait ScalaGenMDArray extends ScalaGenEffect with TypedGenMDArray {

  import IR._
  import TY.{getShapeLength, getValueLength, getShapeValue, getValueValue}
  var stripped: Boolean = false

  // This function stores the action of the innermost with loop
  var withLoopAction: (String, String)=>Unit = (a, b)=> { sys.error("No with loop action set!") }

  def strip[A](m: Manifest[A]) = {
    stripped = true
    val result = remap(m)
    stripped = false
    result
  }

  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[MDArray[Any]])
      if (stripped)
        remap(m.typeArguments.head)
      else
        "MDArray[" + remap(m.typeArguments.head) + "]"
    else super.remap[A](m)
  }

  def emitSymDecl(sym: Sym[Any], debug: Boolean = false)(implicit stream: PrintWriter) = {

    // emit the debug info: shape, value and others
    debug match {
      case true =>
        stream.println()
        emitShapeValue(sym)
      case _ =>
        ;
    }

    // emit the definition
    stripped = getShapeLength(sym) match {
      case Some(0) => true
      case _ => false
    }
    stream.print("val " + quote(sym) + ": " + remap(sym.Type) + " = ")
    stripped = false
  }

  def emitOperationPrologue(sym: Sym[Any], exp: Exp[Any])(implicit stream: PrintWriter) = {
    stream.println("val result = new Array[" + strip(sym.Type) + "](shape(" + quote(exp) + ").content().foldLeft(1)((a,b) => a*b))")
    stream.println("for(i <- List.range(0, result.length))")
  }

  def emitOperationEpilogue(sym: Sym[Any], exp: Exp[Any], opName: String)(implicit stream: PrintWriter) = {
    stream.println("internalReshape(shape(" + quote(exp) + "), result, \"" + opName + "\")")
  }

  // This makes it easy to get the elements we need
  def findAndCast[T](e: Any): Option[T] = e match {
    case s: Sym[_] => findAndCast[T](findDefinition(s).get.rhs)
    case d: Def[_] => Some(d.asInstanceOf[T]) // hopefully it matches
    case _ => None
  }

  def emitGenArray[A](sym: Sym[_], wl: List[Exp[MDArray[A]]], shp: Exp[MDArray[Int]])(implicit stream: PrintWriter) = {

    val shpSym: Sym[_] = shp.asInstanceOf[Sym[_]]

    def emitGenArrayAction(iv: String, loopResult: String) = {
      stream.println("if (result == null) {")
      stream.println("// create the array and shape")
      stream.println("result = new Array[" + strip(sym.Type) + "](" + quote(shp) + ".content().foldLeft(1)((a,b) => a*b) * " + loopResult + ".content().length)")
      stream.println("rshape = shape(" + loopResult + ").content()")
      stream.println("} else {")
      stream.println("// check shape -- this WILL be redundant due to runtime checks")
      stream.println("if (shape(" + loopResult + ").content().toList != rshape.toList) throw new Exception(opName + \": Incompatible shapes:\" + rshape.toList.toString + \" vs \" + shape(" + loopResult + ").content().toList.toString)")
      stream.println("}")
      stream.println("// copy new content")
      stream.println("val mainIndex: Int = flatten(" + quote(shpSym) + " ::: rshape.toList, " + iv + " ::: zeros(rshape.length), opName)")
      stream.println("for (innerIndex <- List.range(0, rshape.length)) {")
      stream.println("result(mainIndex + innerIndex) = " + loopResult + "(innerIndex)")
      stream.println("}")
    }

    emitSymDecl(sym);
    stream.println("{")
    stream.println("val opName: String = \"genarray\"")
    stream.println("var result: Array[" + strip(sym.Type) + "] = null")
    stream.println("var rshape: Array[Int] = null")

    val savedLoopAction = withLoopAction
    withLoopAction = emitGenArrayAction

    for (withNode <- wl)
      emitBlock(withNode)
      // Let this fail quickly if it's not a WithNode
      //emitWithLoopModifier(withNode.asInstanceOf[Sym[_]], findAndCast[WithNode[_]](withNode).get, emitGenArrayAction)

    stream.println("internalReshape(" + quote(shpSym) + " ::: rshape.toList, result, opName)")
    stream.println("}")

    withLoopAction = savedLoopAction
  }

  def emitModArray[A](sym: Sym[_], wl: List[Exp[MDArray[A]]], array: Exp[MDArray[A]])(implicit stream: PrintWriter) = {

    val arraySym: Sym[_] = array.asInstanceOf[Sym[_]]

    def emitModArrayAction(iv: String, loopResult: String) = {
      stream.println("if (rshape == null) {")
      stream.println("rshape = shape(" + quote(arraySym) + ").drop(" + iv + ".content().length)")
      stream.println("}")
      stream.println("val mainIndex: Int = flatten(shape(" + quote(arraySym) + "), " + iv + " ::: zeros(dim(" + quote(arraySym) + ") - " + iv + ".content().length), opName)")
      stream.println("// check shape -- this WILL be redundant due to runtime checks")
      stream.println("if (shape(" + loopResult + ").content().toList != rshape) throw new Exception(opName + \": Incompatible shapes:\" + rshape.toList.toString + \" vs \" + shape(" + loopResult + ").content().toList.toString)")
      stream.println("// copy new content")
      stream.println("for (innerIndex <- List.range(0, " + loopResult + ".content().length)) {")
      stream.println("result(mainIndex + innerIndex) = " + loopResult + ".content()(innerIndex)")
      stream.println("}")
    }

    emitSymDecl(sym);
    stream.println("{")
    stream.println("val opName: String = \"modarray\"")
    stream.println("var result: Array[" + strip(sym.Type) + "] = new Array[" + strip(sym.Type) + "](shape(" + quote(arraySym) + ").content().foldLeft(1)((a,b) => a*b))")
    stream.println("for (i <- List.range(0, result.length)) {")
    stream.println("result(i) = " + quote(arraySym) + ".content()(i)")
    stream.println("}")
    stream.println("var rshape: List[Int] = null")

    val savedLoopAction = withLoopAction
    withLoopAction = emitModArrayAction

    for (withNode <- wl)
      emitBlock(withNode)
      // Let this fail quickly if it's not a WithNode
      //emitWithLoopModifier(withNode.asInstanceOf[Sym[_]], findAndCast[WithNode[_]](withNode).get, emitModArrayAction)

    withLoopAction = savedLoopAction

    stream.println("internalReshape(shape(" + quote(arraySym) + ") ::: rshape.toList, result, opName)")
    stream.println("}")
  }


  def emitFoldArray(sym: Sym[_], withNode: Exp[MDArray[_]], neutral: Exp[_], foldTerm1: Exp[_], foldTerm2: Exp[_], foldExpr: Exp[_])(implicit stream: PrintWriter) = {

    val neutralSym = neutral.asInstanceOf[Sym[_]]
    val foldTerm1Sym = foldTerm1.asInstanceOf[Sym[_]]
    val foldTerm2Sym = foldTerm2.asInstanceOf[Sym[_]]
    val foldExprSym = foldExpr.asInstanceOf[Sym[_]]

    def emitFoldArrayAction(iv: String, loopElement: String) =
      stream.println("result = foldFunction(result, " + loopElement + ")")

    emitSymDecl(sym);
    stream.println("{")
    stream.println("val opName: String = \"fold\"")
    stream.println("var result: " + remap(neutralSym.Type) + " = " + quote(neutralSym))
    // Emit the fold expression
    stream.println("val foldFunction: (" + remap(neutralSym.Type) + ", " + remap(neutralSym.Type)
      + ") => " + remap(neutralSym.Type) + " = (" + quote(foldTerm1Sym) + ", " + quote(foldTerm2Sym) + ") => {")
    emitBlock(foldExprSym)
    stream.println(quote(getBlockResult(foldExprSym)))
    stream.println("}")

    // Emit the loop action
    val savedLoopAction = withLoopAction
    withLoopAction = emitFoldArrayAction
    emitBlock(withNode)
    withLoopAction = savedLoopAction
    // inside the with loop
    //emitWithLoopModifier(withNode.asInstanceOf[Sym[_]], findAndCast[WithNode[_]](withNode).get, emitFoldArrayAction)

    stream.println("result")
    stream.println("}")
  }

  def emitWithLoopModifier(withNodeSym: Sym[_], withLoop: WithNode[_], emitAction: (String, String) => Unit)(implicit stream: PrintWriter) = {
    // emit existing constraints
    stream.println("// with: " + withLoop.toString)

    // emit actual with loop
    getValueLength(withLoop.lb) match {
      case Some(l) =>
        // emit loop
        for (index <- List.range(0, l)) {
          stream.println("val lb" + index + ": Int = " + quote(withLoop.lb) + ".content()(" + index + ")")
          stream.println("val ub" + index + ": Int = " + quote(withLoop.ub) + ".content()(" + index + ")")
          stream.println("val step" + index + ": Int = " + quote(withLoop.step) + ".content()(" + index + ")")
          stream.println("val width" + index + ": Int = " + quote(withLoop.width) + ".content()(" + index + ")")
          stream.println("val ll" + index + ": Int = if (" + quote(withLoop.lbStrict) + ") lb" + index + " + 1 else lb" + index + "")
          stream.println("val ul" + index + ": Int = if (" + quote(withLoop.ubStrict) + ") ub" + index + " else ub" + index + " + 1")
          stream.println("for (iv" + index + " <- List.range(ll" + index + ", ul" + index + ")) {")
          stream.println("if ((iv" + index + " - lb" + index + ") % step" + index + " <= width" + index + ") {")
        }
        // emit loop content
        stream.print("val " + quote(withLoop.sym) + ": " + remap(withLoop.sym.Type) + " = ")
        stream.println(List.range(0, l).map(i => "iv" + i).mkString("", "::","::Nil"))
        stream.println("val iv: " + remap(withLoop.sym.Type) + " = " + quote(withLoop.sym))
        stream.println("val feval: " + remap(withNodeSym.Type) + " = {")
        emitBlock(withLoop.expr)
        stream.println(quote(getBlockResult(withLoop.expr)))
        stream.println("}")
        // emit loop action
        stream.println("// the action of this loop:")
        emitAction("iv", "feval")
        // emit loop end
        for (index <- List.range(0, l)) {
          stream.println("} // if ((iv" + index + " ...")
          stream.println("} // for (iv" + index + " ...")
        }
      case _ =>
        // emit loop
        stream.println("for (iv <- iterateWithStep(_lb=" + quote(withLoop.lb) + ", lbStrict=" + quote(withLoop.lbStrict) + ", ubStrict=" + quote(withLoop.ubStrict) + ", _ub=" + quote(withLoop.ub) + ", step=" + quote(withLoop.step) + ", width=" + quote(withLoop.width) + ", opName=opName)) {")
        // emit loop content
        stream.println("val " + quote(withLoop.sym) + ": " + remap(withLoop.sym.Type) + " = iv")
        stream.println("val feval: " + remap(withLoop.expr.asInstanceOf[Sym[_]].Type) + " = {")
        emitBlock(withLoop.expr)
        stream.println(quote(getBlockResult(withLoop.expr)))
        stream.println("}")
        // emit loop action
        stream.println("// the action of this loop:")
        emitAction("iv", "feval")
        // emit loop end
        stream.println("}")
    }
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = {
    emitChecks(sym, rhs)
    emitShapeValue(sym)
    rhs match {

      case kc: KnownAtCompileTime[_] =>
        emitSymDecl(sym)
        stream.println("internalReshape(" + (kc.value.shape.content.map(t => t.toString).toList ::: ("Nil"::Nil)).mkString("::") + ", Array(" + kc.value.content.mkString(", ") +"), \"knownAtCompileTime\")")
      case kr: KnownAtRuntime[_] =>
        emitSymDecl(sym)
        stream.println(kr.name + " // this is a function argument")
      case fl: FromList[_] =>
        emitSymDecl(sym)
        stream.println("internalReshape(" + quote(fl.value) + ".length::Nil, " + quote(fl.value) + ", \"fromList\")")
      case fa: FromArray[_] =>
        emitSymDecl(sym)
        stream.println("internalReshape(" + quote(fa.value) + ".length::Nil, " + quote(fa.value) + ", \"fromArray\")")
      case fv: FromValue[_] =>
        emitSymDecl(sym)
        stream.println(quote(fv.value))
      case tl: ToList[_] =>
        emitSymDecl(sym)
        stream.println(quote(tl.value) + ".content().toList // toList")
      case ta: ToArray[_] =>
        emitSymDecl(sym)
        stream.println(quote(ta.value) + ".content() // toArray")
      case tv: ToValue[_] =>
      // This will automatically unbox in case there's boxing done...
        emitSymDecl(sym)
        stream.println(quote(tv.value))
      case td: ToDim[_] =>
        emitSymDecl(sym)
        stream.println("dim(" + quote(td.a) + ")")
      case ts: ToShape[_] =>
        emitSymDecl(sym)
        stream.println("shape(" + quote(ts.a) + ")")
      case rs: Reshape[_] =>
        emitSymDecl(sym)
        stream.println("reshape(" + quote(rs.shp) + ", " + quote(rs.a) + ")")
      case sel: Sel[_] =>
      // Get rid of unnecessary boxing
        getShapeLength(sym) match {
          case Some(0) =>
            emitSymDecl(sym, true)
            stream.println(quote(sel.a) + ".content()(flatten(shape(" + quote(sel.a) + "), " + quote(sel.iv) + ", \"sel\"))")
          case _ =>
            emitSymDecl(sym)
            stream.println("sel(" + quote(sel.iv) + ", " + quote(sel.a) + ")")
        }
      case cat: Cat[_] =>
        emitSymDecl(sym)
        stream.println("cat(" + quote(cat.d) + ", " + quote(cat.a) + ", " + quote(cat.b) + ")")
      case in: InfixOp[_, _] =>
      // helper function
        def emitOperation(scalar: Boolean) = {
          emitOperationPrologue(sym, in.array1)
          scalar match {
            case true => stream.println("result(i) = " + quote(in.array1) + ".content()(i) " + in.opName + "  " + quote(in.array2))
            case false => stream.println("result(i) = " + quote(in.array1) + ".content()(i) " + in.opName + "  " + quote(in.array2) + ".content()(i)")
          }
          emitOperationEpilogue(sym, in.array1, "infixOpAA")
        }
        emitSymDecl(sym)
        stream.println("{")
        getShapeLength(in.array2) match {
          case Some(0) => // we have a scalar element
            emitOperation(true)
          case Some(_) => // we have an array
            emitOperation(false)
          case None => // we don't know what's there
          //TODO: Find out why this is the most common case
            stream.println("// WARNING: Operation not specialized on {arrays|scalars}!")
            stream.println("if (shape(shape(" + quote(in.array2) + ")).content()(0) == 0) {")
            emitOperation(true)
            stream.println("} else {")
            emitOperation(false)
            stream.println("}")
        }
        stream.println("}")
      case un: UnaryOp[_, _] =>
        emitSymDecl(sym)
        stream.println("{")
        emitOperationPrologue(sym, un.array)
        stream.println("result(i) = " + un.opName + quote(un.array) + ".content()(i)")
        emitOperationEpilogue(sym, un.array, "unaryOp")
        stream.println("}")
      case wh: Where[_] =>
        emitSymDecl(sym)
        stream.println("{")
        emitOperationPrologue(sym, wh.array1)
        stream.println("result(i) = if (" + quote(wh.cond) + ".content()(i)) " + quote(wh.array1) + ".content()(i) else " + quote(wh.array2) + ".content()(i)")
        emitOperationEpilogue(sym, wh.array1, "where")
        stream.println("}")
      case va: Values[_] =>
        emitSymDecl(sym); stream.println("{")
        stream.println("val result = new Array[Int](" + quote(va.dim) + ")")
        stream.println("for(i <- List.range(0, result.length))")
        stream.println("result(i) = " + quote(va.value))
        stream.println("internalReshape(" + quote(va.dim) + "::Nil, result, \"values\")")
        stream.println("}")
      case wn: WithNode[_] =>
        emitWithLoopModifier(sym, wn, withLoopAction)
      case ga: GenArrayWith[_] =>
        stream.println
        emitGenArray(sym, ga.lExpr, ga.shp)
        stream.println
      case ma: ModArrayWith[_] =>
        stream.println
        emitModArray(sym, ma.lExpr, ma.a)
        stream.println
      case fa: FoldArrayWith[_] =>
        stream.println
        emitFoldArray(sym, fa.wExpr, fa.neutral, fa.foldTerm1, fa.foldTerm2, fa.foldExpression)
        stream.println
      case soa: ScalarOperatorApplication[_,_,_] =>
        emitSymDecl(sym)
        stream.println("((a: " + soa.getMfA.toString + ", b: " + soa.getMfB.toString + ") => a " + soa.operator + " b)(" + quote(soa.a) + ", " + quote(soa.b) + ")")
      // If must also be translated to account for the scope changes
      // TODO: Is there an architecture where it's not necessary to do this?
      case ite: IfThenElse[_] =>
        emitSymDecl(sym)
        stream.println(" = if (" + quote(ite.cond) + ") {")
        TY.withinDifferentScopes(sym,
          (ite.thenp.asInstanceOf[Sym[_]], () => {emitBlock(ite.thenp); stream.println(quote(getBlockResult(ite.thenp))); stream.println("} else {")})::
          (ite.elsep.asInstanceOf[Sym[_]], () => {emitBlock(ite.elsep); stream.println(quote(getBlockResult(ite.elsep))); stream.println("}")})::
          Nil)
      // let error cases be shown at compile time :)
    }
  }

  // the emitSource in ScalaCodeGen is not exactly what we need - we need to select the parameters on our own
  override def emitSource[A: Manifest, B: Manifest](f: Exp[A] => Exp[B], className: String, stream: PrintWriter): List[(Sym[Any], Any)] = {

    stream.println("package scala.virtualization.lms")
    stream.println("package epfl")
    stream.println("package test10")
    stream.println("package original")
    stream.println()
    stream.println("import test10.original.Conversions._")
    stream.println("import test10.original.Operations._")
    stream.println("import test10.original.SpecificOperations._")
    stream.println()

    super.emitSource(f, className, stream)
  }
}
