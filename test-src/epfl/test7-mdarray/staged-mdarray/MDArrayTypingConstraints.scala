package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

import java.io.{Writer, PrintWriter}
import collection.immutable.HashMap


trait MDArrayTypingConstraints extends BaseGenMDArray with BaseGenIfThenElse with MDArrayTypingUnifier {

  val IR: MDArrayBaseExp with IfThenElseExp
  import IR._

  override type Symbol = Sym[_]
  override type Expression = Exp[_]
  override def getId(s: Symbol): Int = s.id

  protected def addConstraints(tl: List[TypingConstraint]): Unit
  protected def addSymbol(sym: Sym[_]): Unit
  protected def createSubScope(sym: Sym[_])(action: => Unit): Unit

  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
    val nodeConstraints =
      // generic reconstruct value from shape
      ReconstructValueFromShape(ValueVar(sym), ShapeVar(sym), postReq, rhs)::
      // and the specific constraints for each node
      (rhs match {
        // We only analyze the "special" cases here
        case IfThenElse(cond, thenp, elsep) =>
          createSubScope(thenp.asInstanceOf[Sym[_]]) { emitBlock(thenp) }
          createSubScope(elsep.asInstanceOf[Sym[_]]) { emitBlock(elsep) }
          getConstraints(sym, rhs)
        case WithNode(lb, lbStrict, ub, ubStrict, step, width, ivSym, expr) =>
          // emit the expression constraints
          emitBlock(expr)
          // add symbol
          addSymbol(ivSym)
          getConstraints(sym, rhs)
        case GenArrayWith(withLoops, shape) =>
          assert(withLoops.length >= 1)
          // emit with loop constraints
          for (withLoop <- withLoops)
            emitBlock(withLoop)
          getConstraints(sym, rhs)
        case ModArrayWith(withLoops, array) =>
          assert(withLoops.length >= 1)
          // emit with loop constraints
          for (withLoop <- withLoops)
            emitBlock(withLoop)
          getConstraints(sym, rhs)
        case FoldArrayWith(withLoop, neutral, foldTerm1, foldTerm2, foldExpression) =>
          // emit with loop and fold expression constraints
          emitBlock(withLoop)
          // add symbols
          addSymbol(foldTerm1)
          addSymbol(foldTerm2)
          emitBlock(foldExpression)
          getConstraints(sym, rhs)
        case _ =>
          getConstraints(sym, rhs)
      })
    // now what do we do with these constraints?
    addConstraints(nodeConstraints)
    addSymbol(sym)
  }

  def getConstraints(sym: Sym[_], rhs: Def[_]): List[TypingConstraint] = rhs match {
    case KnownAtCompileTime(value) =>
      Equality(ShapeVar(sym), Lst(value.shape.map(i => toValue(i))), postReq, rhs)::
      Equality(ValueVar(sym), Lst(value.content.map((i: Any) => toValue(i)).toList), postReq, rhs)::Nil
    case KnownAtRuntime(name) =>
      Nil
    case FromList(list) =>
      Equality(ShapeVar(sym), Lst(getNewUnknown::Nil), postReq, rhs)::Nil
    case FromArray(array) =>
      Equality(ShapeVar(sym), Lst(getNewUnknown::Nil), postReq, rhs)::Nil
    case FromValue(value) =>
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::Nil
    case ToList(value) =>
      LengthEqualityAeqB(ShapeVar(value), Lst(getNewUnknown::Nil), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(value), postReq, rhs)::Nil
    case ToArray(value) =>
      LengthEqualityAeqB(ShapeVar(value), Lst(getNewUnknown::Nil), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(value), postReq, rhs)::Nil
    case ToValue(value) =>
      LengthEqualityAeqB(ShapeVar(value), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::Nil
    case ToDim(array) =>
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs) ::
      Equality(ValueVar(sym), Lst(LengthOf(ShapeVar(array))::Nil), postReq, rhs)::Nil
    case ToShape(array) =>
      Equality(ValueVar(sym), ShapeVar(array), postReq, rhs)::
      Equality(ShapeVar(sym), Lst(LengthOf(ShapeVar(array))::Nil), postReq, rhs)::Nil
    case Reshape(shape, array) =>
      LengthEqualityAeqB(ShapeVar(shape), Lst(getNewUnknown :: Nil), preReq, rhs)::
      EqualProduct(ValueVar(shape), ShapeVar(array), preReq, rhs)::
      Equality(ShapeVar(sym), ValueVar(shape), postReq, rhs)::Nil
    case Sel(iv, array) =>
      LengthEqualityAeqB(ShapeVar(iv), Lst(getNewUnknown::Nil), preReq, rhs)::
      PrefixLt(ShapeVar(array), ValueVar(iv), ShapeVar(sym), preReq, rhs)::
      SuffixEq(ShapeVar(array), ValueVar(iv), ShapeVar(sym), postReq, rhs)::Nil
    case Cat(d, a, b) =>
      LengthEqualityAeqB(ShapeVar(d), Lst(Nil), preReq, rhs)::
      LengthEqualityAeqB(ShapeVar(a), Lst(getNewUnknown::Nil), preReq, rhs)::
      LengthEqualityAeqB(ShapeVar(a), ShapeVar(b), preReq, rhs)::
      LessThan(ValueVar(d), Lst(LengthOf(ShapeVar(a))::Nil), preReq, rhs)::
      EqualityExceptFor(ValueVar(d), ShapeVar(a), ShapeVar(b), preReq, rhs)::
      LengthEqualityAeqB(ShapeVar(sym), ShapeVar(a), postReq, rhs)::
      EqualityShAeqShBplusShCalongD(ShapeVar(sym), ShapeVar(a), ShapeVar(b), ValueVar(d), postReq, rhs)::Nil
    case Values(dim, value) =>
      Equality(ShapeVar(dim), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(value), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), ValueVar(dim), postReq, rhs)::
      EqualityAeqDimTimesValue(ValueVar(sym), ValueVar(dim), ValueVar(value), postReq, rhs)::Nil
    case InfixOp(array1, array2, op, opName) =>
      EqualityOrScalar(ShapeVar(array1), ShapeVar(array2), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(array1), postReq, rhs)::Nil
    case UnaryOp(array, op, opName) =>
      Equality(ShapeVar(sym), ShapeVar(array), postReq, rhs)::Nil
    case Where(cond, array1, array2) =>
      Equality(ShapeVar(cond), ShapeVar(array1), preReq, rhs)::
      Equality(ShapeVar(cond), ShapeVar(array2), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(array1), postReq, rhs)::Nil
    case WithNode(lb, lbStrict, ub, ubStrict, step, width, ivSym, expr) =>
      LengthEqualityAeqB(ShapeVar(lb), Lst(getNewUnknown::Nil), preReq, rhs)::
      Equality(ShapeVar(lbStrict), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(ubStrict), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(ub), ShapeVar(lb), preReq, rhs)::
      Equality(ShapeVar(step), ShapeVar(lb), preReq, rhs)::
      Equality(ShapeVar(width), ShapeVar(lb), preReq, rhs)::
      Equality(ShapeVar(ivSym), ShapeVar(lb), postReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(expr), postReq, rhs)::Nil
    case GenArrayWith(withLoops, shape) =>
      assert(withLoops.length >= 1)
      withNodeListConstraints(withLoops, rhs):::
      Equality(ShapeVar(shape), Lst(getNewUnknown::Nil), preReq, rhs)::
      Equality(ShapeVar(shape), ShapeVar(recoverWithNode(withLoops.head).lb), preReq, rhs)::
      EqualityAeqBcatC(ShapeVar(sym), ValueVar(shape), ShapeVar(withLoops.head), postReq, rhs)::Nil
    case ModArrayWith(withLoops, array) =>
      assert(withLoops.length >= 1)
      withNodeListConstraints(withLoops, rhs):::
       withLoops.flatMap(wn =>
         PrefixLt(ShapeVar(array), ValueVar(recoverWithNode(wn).lb), ShapeVar(wn), preReq, rhs) ::
         SuffixEq(ShapeVar(array), ValueVar(recoverWithNode(wn).lb), ShapeVar(wn), preReq, rhs) :: Nil
       ) :::
       Equality(ShapeVar(sym), ShapeVar(array), postReq, rhs)::Nil
    case FoldArrayWith(withLoop, neutral, foldTerm1, foldTerm2, foldExpression) =>
      Equality(ShapeVar(neutral), ShapeVar(withLoop), preReq, rhs)::
      Equality(ShapeVar(foldTerm1), ShapeVar(neutral), postReq, rhs)::
      Equality(ShapeVar(foldTerm2), ShapeVar(neutral), postReq, rhs)::
      Equality(ShapeVar(foldExpression), ShapeVar(withLoop), preReq, rhs)::
      Equality(ShapeVar(sym), ShapeVar(neutral), postReq, rhs)::Nil
    case IfThenElse(cond, thenp, elsep) =>
      Equality(ShapeVar(cond), Lst(Nil), preReq, rhs)::
      CommonDenominator(ShapeVar(sym), ShapeVar(thenp), ShapeVar(elsep), postReq, rhs)::Nil
    case ScalarOperatorApplication(function, operator, operand1, operand2) =>
      Equality(ShapeVar(operand1), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(operand2), Lst(Nil), preReq, rhs)::
      Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::Nil
  }

  def toValue(i: Any): TypingElement = i match {
    case i: Int => Value(i)
    case _ => getNewUnknown
  }

  def recoverWithNode(e: Exp[MDArray[_]]): WithNode[_] =
    findDefinition(e.asInstanceOf[Sym[_]]).get.rhs.asInstanceOf[WithNode[_]]

  def withNodeListConstraints(withNodeList: List[Exp[MDArray[_]]], node: Any): List[TypingConstraint] = {
    val f = withNodeList.head // first node
    // map the rest of the nodes
    withNodeList.tail.map(e => Equality(ShapeVar(recoverWithNode(f).lb), ShapeVar(recoverWithNode(e).lb), preReq, node)) :::
    withNodeList.tail.map(e => Equality(ShapeVar(f), ShapeVar(e), preReq, node))
  }

  override def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit = {}
  override def emitSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)(implicit mA: Manifest[A], mB: Manifest[B]): Unit = {}
}