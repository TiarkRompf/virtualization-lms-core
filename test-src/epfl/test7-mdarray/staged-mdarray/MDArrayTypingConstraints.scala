package scala.virtualization.lms
package epfl
package test7

import common._
import internal._
import original._
import original.Conversions._
import collection.mutable.{Queue, HashSet}

trait MDArrayTypingConstraints extends MDArrayTypingPrimitives {

  import IR._

  // XXX: These calls should never fail. If they fail it's better to fail quickly rather than hide the error
  def getSymNumber(e: Any): Int = e match {
    case c: Const[_] => c.id
    case s: Sym[_] => s.id
    case d: Def[_] => findDefinition(d).get.sym.id
  }

  def getDefinition(e: Any): Option[Any] = e match {
    case c: Const[_] => Some(c)
    case s: Sym[_] => findDefinition(s) match {
      case Some(tp) => Some(tp.rhs)
      case _ => None
    }
    case d: Def[_] => Some(d)
  }

  //TODO: This needs to be fixed
  def getElementsInCaseClass(e: Any): List[Exp[_]] = e match {
    case s: Sym[_] => List(e)
    case p: Product => p.productIterator.flatMap(s => getElementsInCaseClass(s)).toList
    case _ => Nil
  }

  // XXX: making the sym function an implicit conversion, I hope Scala's CSE works well enough to eliminate redundant calls
  private implicit def getSymNumberExp(e: Exp[_]): Int = getSymNumber(e)
  private implicit def getSymNumberDef(d: Def[_]): Int = getSymNumber(d)

  def createTypingConstraints(r: Any): Pair[List[TypingConstraint], List[Int]] = {
    val nodes = new HashSet[Any]()
    val nodeQueue: Queue[Any] = new Queue()
    var nodeIds: List[Int] = Nil
    var constraints: List[TypingConstraint] = List()

    nodeQueue.enqueue(r)

    while (nodeQueue.length != 0) {
      val queueNode = nodeQueue.dequeue
      getDefinition(queueNode) match {
        case Some(defNode) =>
          nodes.contains(defNode) match {
            case true =>
              ;
            case false =>
              nodes.add(defNode)
              nodeIds = getSymNumber(defNode)::nodeIds
              val constr = gatherConstraints(defNode)
              constraints = constraints ::: constr._1
              for (elt <- constr._2)
                nodeQueue.enqueue(elt)
          }
        case None =>
          // we still need to add the element to the list, just to know we looked at it
          nodeIds = getSymNumber(queueNode)::nodeIds
      }
    }
    (constraints, nodeIds)
  }

  def gatherConstraints(node: Any): Pair[List[TypingConstraint], List[Exp[_]]] = {
    val result = node match {
      case ct: Const[_] => constConstraints(ct)
      case kc: KnownAtCompileTime[_] => knownAtCompileTimeConstraints(kc)
      case kr: KnownAtRuntime[_] => knownAtRuntimeConstraints(kr)
      case fl: FromList[_] => fromListConstraints(fl)
      case fa: FromArray[_] => fromArrayConstraints(fa)
      case fv: FromValue[_] => fromValueConstraints(fv)
      case tl: ToList[_] => toListConstraints(tl)
      case ta: ToArray[_] => toArrayConstraints(ta)
      case tv: ToValue[_] => toValueConstraints(tv)
      case td: ToDim[_] => toDimConstraints(td)
      case ts: ToShape[_] => toShapeConstraints(ts)
      case rs: Reshape[_] => reshapeConstraints(rs)
      case sel: Sel[_] => selConstraints(sel)
      case cat: Cat[_] => catConstraints(cat)
      case vs: Values[_] => valuesConstraints(vs)
      case io: InfixOp[_, _] => infixOpConstraints(io)
      case uo: UnaryOp[_, _] => unaryOpConstraints(uo)
      case wh: Where[_] => whereConstraints(wh)
      case wn: WithNode[_] => withNodeConstraints(wn)
      case ga: GenArrayWith[_] => genArrayConstraints(ga)
      case ma: ModArrayWith[_] => modArrayConstraints(ma)
      case fa: FoldArrayWith[_] => foldArrayConstraints(fa)
      // TODO: Do something like typing scopes here to account for the fact that both branches cannot be executed at the same time:
      /*
        if (...)
           a + [ 1, 2, 3 ] // shape(a) is [3]
         else
           a + [[ 1, 2, 3 ], [ 4, 5, 6 ]] // shape(a) is [2,3]
       */
      case ite: IfThenElse[_] => ifThenElseConstraint(ite)
      case soa: ScalarOperatorApplication[_, _, _] => scalarOpApplicationConstraints(soa)
      case _ => throw new RuntimeException("Unknown node: " + node.toString)
    }

    //TODO: The Scheduling-like getElementsInCaseClass(.) doesn't work correctly. In order to automate the dependency function we need this fixed
    //println(node.toString + " => " + result._2.mkString(", ") + "   vs   " + getElementsInCaseClass(node).mkString(", "))

    // Adding the default operation of reconstructing the value from shape: if the value
    // is a variable and the shape is known, the value var is replaced by unknowns
    (ReconstructValueFromShape(ValueVar(getSymNumber(node)), ShapeVar(getSymNumber(node)), postReq, node)::result._1, result._2)
  }

  def toValue(i: Any): TypingElement = i match {
    case i: Int => Value(i)
    case _ => getNewUnknown
  }

  def constConstraints(ct: Const[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(ct), Lst(Nil), postReq, ct) ::
     Equality(ValueVar(ct), Lst(toValue(ct.x)::Nil), postReq, ct)::Nil, Nil)

  def knownAtCompileTimeConstraints(kc: KnownAtCompileTime[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(kc), Lst(kc.value.shape.map(i => toValue(i))), postReq, kc) ::
     Equality(ValueVar(kc), Lst(kc.value.content.map((i: Any) => toValue(i)).toList), postReq, kc) :: Nil, Nil)

  def knownAtRuntimeConstraints(kr: KnownAtRuntime[_]) =
    (Nil, Nil) // Unfortunately we know nothing about it...

  def fromListConstraints(fl: FromList[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(fl)), Lst(getNewUnknown::Nil), postReq, fl)::Nil, fl.value::Nil)

  def fromArrayConstraints(fa: FromArray[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(fa)), Lst(getNewUnknown::Nil), postReq, fa)::Nil, fa.value::Nil)

  def fromValueConstraints(fv: FromValue[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(fv)), Lst(Nil), postReq, fv)::Nil, fv.value::Nil)

  def toListConstraints(tl: ToList[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (LengthEqualityAeqB(ShapeVar(tl.value), Lst(getNewUnknown::Nil), preReq, tl)::
     Equality(ShapeVar(tl), ShapeVar(tl.value), postReq, tl)::Nil, tl.value::Nil)

  def toArrayConstraints(ta: ToArray[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (LengthEqualityAeqB(ShapeVar(ta.value), Lst(getNewUnknown::Nil), preReq, ta)::
     Equality(ShapeVar(ta), ShapeVar(ta.value), postReq, ta)::Nil, ta.value::Nil)

  def toValueConstraints(tv: ToValue[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (LengthEqualityAeqB(ShapeVar(tv.value), Lst(Nil), preReq, tv)::
     Equality(ShapeVar(tv), Lst(Nil), postReq, tv)::Nil, tv.value::Nil)

  def toDimConstraints(td: ToDim[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(td), Lst(Nil), postReq, td) ::
     Equality(ValueVar(td), Lst(LengthOf(ShapeVar(td.a))::Nil), postReq, td)::Nil, td.a::Nil)

  def toShapeConstraints(ts: ToShape[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ValueVar(ts), ShapeVar(ts.a), postReq, ts)::
     Equality(ShapeVar(ts), Lst(LengthOf(ShapeVar(ts.a))::Nil), postReq, ts)::Nil,
     ts.a::Nil)

  def reshapeConstraints(rs: Reshape[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (LengthEqualityAeqB(ShapeVar(rs.shp), Lst(getNewUnknown :: Nil), preReq, rs)::
     EqualProduct(ValueVar(rs.shp), ShapeVar(rs.a), preReq, rs)::
     Equality(ShapeVar(rs), ValueVar(rs.shp), postReq, rs)::Nil,
     rs.shp::rs.a::Nil)

  def selConstraints(sel: Sel[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (LengthEqualityAeqB(ShapeVar(sel.iv), Lst(getNewUnknown::Nil), preReq, sel)::
     PrefixLt(ShapeVar(sel.a), ValueVar(sel.iv), ShapeVar(sel), preReq, sel)::
     SuffixEq(ShapeVar(sel.a), ValueVar(sel.iv), ShapeVar(sel), postReq, sel)::Nil,
     sel.iv::sel.a::Nil)

  def catConstraints(cat: Cat[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (LengthEqualityAeqB(ShapeVar(cat.d), Lst(Nil), preReq, cat)::
     LengthEqualityAeqB(ShapeVar(cat.a), Lst(getNewUnknown::Nil), preReq, cat)::
     LengthEqualityAeqB(ShapeVar(cat.a), ShapeVar(cat.b), preReq, cat)::
     LessThan(ValueVar(cat.d), Lst(LengthOf(ShapeVar(cat.a))::Nil), preReq, cat)::
     EqualityExceptFor(ValueVar(cat.d), ShapeVar(cat.a), ShapeVar(cat.b), preReq, cat)::
     LengthEqualityAeqB(ShapeVar(cat), ShapeVar(cat.a), postReq, cat)::
     EqualityShAeqShBplusShCalongD(ShapeVar(cat), ShapeVar(cat.a), ShapeVar(cat.b), ValueVar(cat.d), postReq, cat)::Nil,
     List(cat.d,cat.a,cat.b))

  def valuesConstraints(values: Values[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(values.dim), Lst(Nil), preReq, values)::
     Equality(ShapeVar(values.value), Lst(Nil), preReq, values)::
     Equality(ShapeVar(values), ValueVar(values.dim), postReq, values)::
     EqualityAeqDimTimesValue(ValueVar(values), ValueVar(values.dim), ValueVar(values.value), postReq, values)::Nil,
     values.value::values.dim::Nil)

  def infixOpConstraints(in: InfixOp[_, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (EqualityOrScalar(ShapeVar(in.array1), ShapeVar(in.array2), preReq, in)::
     Equality(ShapeVar(in), ShapeVar(in.array1), postReq, in)::Nil,
     in.array1::in.array2::Nil)

  def unaryOpConstraints(un: UnaryOp[_, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(un), ShapeVar(un.array), postReq, un)::Nil,
     un.array::Nil)

  def whereConstraints(wh: Where[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(wh.cond), ShapeVar(wh.array1), preReq, wh)::
     Equality(ShapeVar(wh.cond), ShapeVar(wh.array2), preReq, wh)::
     Equality(ShapeVar(wh), ShapeVar(wh.array1), postReq, wh)::Nil,
     // TODO: Understand why wh.array1::wh.array2::wh.cond::Nil here produces a type mismatch
     List(wh.array1, wh.array2, wh.cond))

  def withNodeConstraints(w: WithNode[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (LengthEqualityAeqB(ShapeVar(w.lb), Lst(getNewUnknown::Nil), preReq, w)::
     Equality(ShapeVar(w.ub), ShapeVar(w.lb), preReq, w)::
     Equality(ShapeVar(w.step), ShapeVar(w.lb), preReq, w)::
     Equality(ShapeVar(w.width), ShapeVar(w.lb), preReq, w)::
     Equality(ShapeVar(w.sym), ShapeVar(w.lb), postReq, w)::
     Equality(ShapeVar(w), ShapeVar(w.expr), postReq, w)::Nil,
     w.lb::w.lbStrict::w.ub::w.ubStrict::w.step::w.width::w.sym::w.expr::Nil)

  def recoverWithNode(e: Exp[MDArray[_]]): WithNode[_] =
    // this must always hold, else crash :)
    getDefinition(e).get.asInstanceOf[WithNode[_]]

  def withNodeListConstraints(node: Any, withNodeList: List[Exp[MDArray[_]]]): List[TypingConstraint] = {
    val f = withNodeList(0) // first node

    // and map the rest of the nodes
    withNodeList.tail.map(e => Equality(ShapeVar(getSymNumber(recoverWithNode(f).lb)), ShapeVar(getSymNumber(recoverWithNode(e).lb)), preReq, node)) :::
    withNodeList.tail.map(e => Equality(ShapeVar(getSymNumber(f)), ShapeVar(getSymNumber(e)), preReq, node))
  }

  def genArrayConstraints(ga: GenArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (withNodeListConstraints(ga, ga.lExpr):::
     Equality(ShapeVar(ga.shp), Lst(getNewUnknown::Nil), preReq, ga)::
     Equality(ShapeVar(ga.shp), ShapeVar(getSymNumber(recoverWithNode(ga.lExpr.head).lb)), preReq, ga)::
     EqualityAeqBcatC(ShapeVar(ga), ValueVar(ga.shp), ShapeVar(getSymNumber(ga.lExpr.head)), postReq, ga)::Nil,
     ga.shp::ga.lExpr)

  def modArrayConstraints(ma: ModArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (withNodeListConstraints(ma, ma.lExpr) :::
     ma.lExpr.flatMap(wn =>
       PrefixLt(ShapeVar(ma.a), ValueVar(getSymNumber(recoverWithNode(wn).lb)), ShapeVar(getSymNumber(wn)), preReq, ma) ::
       SuffixEq(ShapeVar(ma.a), ValueVar(getSymNumber(recoverWithNode(wn).lb)), ShapeVar(getSymNumber(wn)), preReq, ma) :: Nil
     ) :::
     Equality(ShapeVar(ma), ShapeVar(ma.a), postReq, ma)::Nil,
     ma.a::ma.lExpr)

  def foldArrayConstraints(fa: FoldArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(fa.neutral), ShapeVar(fa.wExpr), preReq, fa)::
     Equality(ShapeVar(fa.foldTerm1), ShapeVar(fa.neutral), postReq, fa)::
     Equality(ShapeVar(fa.foldTerm2), ShapeVar(fa.neutral), postReq, fa)::
     Equality(ShapeVar(fa.foldExpression), ShapeVar(fa.wExpr), preReq, fa)::
     Equality(ShapeVar(fa), ShapeVar(fa.neutral), postReq, fa)::Nil,
     List(fa.neutral, fa.wExpr, fa.foldTerm1, fa.foldTerm2, fa.foldExpression))

  def ifThenElseConstraint(ite: IfThenElse[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(ite.cond), Lst(Nil), preReq, ite)::
     CommonDenominator(ShapeVar(ite), ShapeVar(ite.thenp), ShapeVar(ite.elsep), postReq, ite)::Nil,
     ite.thenp::ite.elsep::ite.cond::Nil)

  def scalarOpApplicationConstraints(soa: ScalarOperatorApplication[_, _, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(soa.a), Lst(Nil), preReq, soa)::
     Equality(ShapeVar(soa.b), Lst(Nil), preReq, soa)::
     Equality(ShapeVar(soa), Lst(Nil), postReq, soa)::Nil,
     soa.a::soa.b::Nil)
}