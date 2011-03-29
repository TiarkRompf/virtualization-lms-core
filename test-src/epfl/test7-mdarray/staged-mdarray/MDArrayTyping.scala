package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

import java.io.{Writer, PrintWriter}
import collection.immutable.HashMap


trait MDArrayTyping extends BaseGenMDArray with MDArrayTypingUnifier {

  import IR._

  protected var constraints: List[TypingConstraint] = Nil
  protected var syms: List[Sym[_]] = Nil
  protected var shapes: Map[Sym[_], TypingVariable] = new HashMap[Sym[_], TypingVariable]
  protected var values: Map[Sym[_], TypingVariable] = new HashMap[Sym[_], TypingVariable]

  override type Symbol = Sym[_]
  override type Expression = Exp[_]
  override def getId(s: Symbol): Int = s.id

  /**
   * Gather the constraints in a optimizer & code generator-friendly way
   */
  def doTyping(result: Exp[_], debug: Boolean = false): Unit = {

    (IR eq null) match {
      case true => sys.error("IR: " + IR)
      case false => ;
    }

    // 1. Gather constraints
    constraints = Nil
    syms = Nil
    emitBlock(result)(new PrintWriter(System.err)) // shouldn't output anything
    syms = constraints.flatMap(x => getSymbols(x)).distinct

    // 2. Get the substitution list & the pre-requirement list
    val fullSubstitutions = computeSubstitutions(constraints, debug)
    val pureSubstitutions = computeSubstitutions(constraints.filterNot(constr => constr.prereq), debug)

    // 3. Shapes and values checks
    for(sym <- syms) {
      shapes += new Pair(sym, fullSubstitutions(ShapeVar(sym)))
      values += new Pair(sym, fullSubstitutions(ValueVar(sym)))
    }

    // 4. Runtime check map
    // empty for now, we'll add runtime checks as AST nodes :)
  }


  def getTypingString(sym: Sym[_]): String = {

    val shapeVar: TypingVariable = ShapeVar(sym)
    val valueVar: TypingVariable = ValueVar(sym)
    val shapeVarValue: TypingVariable = shapes(sym)
    val valueVarValue: TypingVariable = values(sym)

    // need to limit the value size so we don't overcrowd the graph
    var valueString = valueVarValue.toString
    (valueString.length > 40) match {
      case true =>
        valueString = valueString.substring(0, 18) + " ... " + valueString.substring(valueString.length - 18)
      case _ =>
        ;
    }

    (valueVar != valueVarValue, shapeVar != shapeVarValue) match {
      case (true, true) => valueVar.toString + "=" + valueString + " and " + shapeVar.toString + "=" + shapeVarValue.toString
      case (true, false) => valueVar.toString + "=" + valueString
      case (false, true) => shapeVar.toString + "=" + shapeVarValue.toString
      case (false, false) => "?!?"
    }
  }

  def getShapeLength(sym: Exp[_]) = getLength(shapes(sym.asInstanceOf[Sym[_]]))
  def getValueLength(sym: Exp[_]) = getLength(values(sym.asInstanceOf[Sym[_]]))
  def getShapeValue(sym: Exp[_]) = getValue(shapes(sym.asInstanceOf[Sym[_]]))
  def getValueValue(sym: Exp[_]) = getValue(values(sym.asInstanceOf[Sym[_]]))


  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter): Unit = {
    val nodeConstraints =
      // generic reconstruct value from shape
      ReconstructValueFromShape(ValueVar(sym), ShapeVar(sym), postReq, rhs)::
      // and the specific constraints for each node
      (rhs match {
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
          // emit the expression constraints
          emitBlock(expr)
          LengthEqualityAeqB(ShapeVar(lb), Lst(getNewUnknown::Nil), preReq, rhs)::
          // TODO: Enable the scalar condition for lbStrict and ubStrict
          // Equality(ShapeVar(lbStrict), Lst(Nil), preReq, rhs)::
          // Equality(ShapeVar(ubStrict), Lst(Nil), preReq, rhs)::
          Equality(ShapeVar(ub), ShapeVar(lb), preReq, rhs)::
          Equality(ShapeVar(step), ShapeVar(lb), preReq, rhs)::
          Equality(ShapeVar(width), ShapeVar(lb), preReq, rhs)::
          Equality(ShapeVar(ivSym), ShapeVar(lb), postReq, rhs)::
          Equality(ShapeVar(sym), ShapeVar(expr), postReq, rhs)::Nil
        case GenArrayWith(withLoops, shape) =>
          assert(withLoops.length >= 1)
          // emit with loop constraints
          for (withLoop <- withLoops)
            emitBlock(withLoop)
          withNodeListConstraints(withLoops, rhs):::
          Equality(ShapeVar(shape), Lst(getNewUnknown::Nil), preReq, rhs)::
          Equality(ShapeVar(shape), ShapeVar(recoverWithNode(withLoops.head).lb), preReq, rhs)::
          EqualityAeqBcatC(ShapeVar(sym), ValueVar(shape), ShapeVar(withLoops.head), postReq, rhs)::Nil
        case ModArrayWith(withLoops, array) =>
          assert(withLoops.length >= 1)
          // emit with loop constraints
          for (withLoop <- withLoops)
            emitBlock(withLoop)
          withNodeListConstraints(withLoops, rhs):::
           withLoops.flatMap(wn =>
             PrefixLt(ShapeVar(array), ValueVar(recoverWithNode(wn).lb), ShapeVar(wn), preReq, rhs) ::
             SuffixEq(ShapeVar(array), ValueVar(recoverWithNode(wn).lb), ShapeVar(wn), preReq, rhs) :: Nil
           ) :::
           Equality(ShapeVar(sym), ShapeVar(array), postReq, rhs)::Nil
        case FoldArrayWith(withLoop, neutral, foldTerm1, foldTerm2, foldExpression) =>
          // emit with loop and fold expression constraints
          emitBlock(withLoop)
          emitBlock(foldExpression)
          Equality(ShapeVar(neutral), ShapeVar(withLoop), preReq, rhs)::
          Equality(ShapeVar(foldTerm1), ShapeVar(neutral), postReq, rhs)::
          Equality(ShapeVar(foldTerm2), ShapeVar(neutral), postReq, rhs)::
          Equality(ShapeVar(foldExpression), ShapeVar(withLoop), preReq, rhs)::
          Equality(ShapeVar(sym), ShapeVar(neutral), postReq, rhs)::Nil
        case IfThenElse(cond, thenp, elsep) =>
          // TODO: Scoping here
          emitBlock(thenp)
          emitBlock(elsep)
          Equality(ShapeVar(cond), Lst(Nil), preReq, rhs)::
          CommonDenominator(ShapeVar(sym), ShapeVar(thenp), ShapeVar(elsep), postReq, rhs)::Nil
        case ScalarOperatorApplication(function, operator, operand1, operand2) =>
          Equality(ShapeVar(operand1), Lst(Nil), preReq, rhs)::
          Equality(ShapeVar(operand2), Lst(Nil), preReq, rhs)::
          Equality(ShapeVar(sym), Lst(Nil), postReq, rhs)::Nil
      })
    // now what do we do with these constraints?
    constraints = nodeConstraints ::: constraints
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