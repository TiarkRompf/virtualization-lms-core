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

  def printTypingConstraints(r: Any): Unit = {
    val constraints: List[TypingConstraint] = createTypingConstraints(r)
    for (t: TypingConstraint <- constraints)
      println(t)
  }

  // XXX: These calls should never fail. If they fail it's better to fail quickly rather than hide the error
  def getSymNumber(e: Any): Int = e match {
    case c: Const[_] => c.id
    case s: Sym[_] => s.id
    case d: Def[_] => findDefinition(d).get.sym.id
  }

  def getDefinition(e: Any): Any = e match {
    case c: Const[_] => c
    case s: Sym[_] => findDefinition(s).get.rhs
    case d: Def[_] => d
  }

  // XXX: making the sym function an implicit conversion, I hope Scala's CSE works well enough to eliminate redundant calls
  private implicit def getSymNumberExp(e: Exp[_]): Int = getSymNumber(e)
  private implicit def getSymNumberDef(d: Def[_]): Int = getSymNumber(d)

  def createTypingConstraints(r: Any): List[TypingConstraint] = {
    val nodes = new HashSet[Any]()
    val nodeQueue: Queue[Any] = new Queue()
    var constraints: List[TypingConstraint] = List()

    nodeQueue.enqueue(r)

    while (nodeQueue.length != 0) {
      val node = getDefinition(nodeQueue.dequeue)
      nodes.contains(node) match {
        case true =>
          ;
        case false =>
          nodes.add(node)
          val constr = gatherConstraints(node)
          constraints = constraints ::: constr._1
          for (elt <- constr._2)
            nodeQueue.enqueue(elt)
      }
    }
    constraints
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
      case red: Reduce[_,_] => reduceConstraints(red)
      case vs: Values[_] => valuesConstraints(vs)
      case io: InfixOpAA[_, _] => infixOpAAConstraints(io)
      case io: InfixOpAE[_, _] => infixOpAEConstraints(io)
      case uo: UnaryOp[_, _] => unaryOpConstraints(uo)
      case wh: Where[_] => whereConstraints(wh)
      case iv: IndexVector => indexVectorConstraints(iv)
      case wn: WithNode[_] => withNodeConstraints(wn)
      case ga: GenArrayWith[_] => genArrayConstraints(ga)
      case ma: ModArrayWith[_] => modArrayConstraints(ma)
      case fa: FoldArrayWith[_] => foldArrayConstraints(fa)
      case ite: IfThenElse[_] => ifThenElseConstraint(ite)
      case ip: IntPlus => intPlusConstraints(ip)
      case im: IntMinus => intMinusConstraints(im)
      case ie: IntEqual => intEqualConstraints(ie)
      case il: IntLess => intLessConstraints(il)
      case _ => throw new RuntimeException("Unknown node: " + node.toString)
    }
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
    (LengthEqualityAeqB(ShapeVar(cat.a), Lst(getNewUnknown::Nil), preReq, cat)::
     LengthEqualityAeqB(ShapeVar(cat.a), ShapeVar(cat.b), preReq, cat)::
     LessThan(ValueVar(cat.d), Lst(LengthOf(ShapeVar(cat.a))::Nil), preReq, cat)::
     EqualityExceptFor(ValueVar(cat.d), ShapeVar(cat.a), ShapeVar(cat.b), preReq, cat)::
     LengthEqualityAeqB(ShapeVar(cat), ShapeVar(cat.a), postReq, cat)::
     EqualityShAeqShBplusShCalongD(ShapeVar(cat), ShapeVar(cat.a), ShapeVar(cat.b), ValueVar(cat.d), postReq, cat)::Nil,
     cat.d::cat.a::cat.b::Nil)

  def reduceConstraints(red: Reduce[_,_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(red), Lst(Nil), postReq, red)::
     Equality(ValueVar(red), Lst(getNewUnknown::Nil), postReq, red)::Nil,
     red.a::Nil)

  def valuesConstraints(values: Values[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(values.dim), Lst(Nil), preReq, values)::
     Equality(ShapeVar(values.value), Lst(Nil), preReq, values)::
     Equality(ShapeVar(values), ValueVar(values.dim), postReq, values)::
     EqualityAeqDimTimesValue(ValueVar(values), ValueVar(values.dim), ValueVar(values.value), postReq, values)::Nil,
     values.value::values.dim::Nil)

  def infixOpAAConstraints(in: InfixOpAA[_, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(in.array1), ShapeVar(in.array2), preReq, in)::
     Equality(ShapeVar(in), ShapeVar(in.array1), postReq, in)::Nil,
     in.array1::in.array2::Nil)

  def infixOpAEConstraints(in: InfixOpAE[_, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(in.element), Lst(Nil), preReq, in)::
     Equality(ShapeVar(in), ShapeVar(in.array), postReq, in)::Nil,
     in.array::in.element::Nil)

  def unaryOpConstraints(un: UnaryOp[_, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(un), ShapeVar(un.array), postReq, un)::Nil,
     un.array::Nil)

  def whereConstraints(wh: Where[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(wh.cond), ShapeVar(wh.array1), preReq, wh)::
     Equality(ShapeVar(wh.cond), ShapeVar(wh.array2), preReq, wh)::
     Equality(ShapeVar(wh), ShapeVar(wh.array1), postReq, wh)::Nil,
     // TODO: Understand why wh.array1::wh.array2::wh.cond::Nil here produces a type mismatch
     List(wh.array1, wh.array2, wh.cond))

  def indexVectorConstraints(iv: IndexVector): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(iv.lb), Lst(getNewUnknown::Nil), preReq, iv)::
     Equality(ShapeVar(iv.ub), ShapeVar(iv.lb), preReq, iv)::
     Equality(ShapeVar(iv.step), ShapeVar(iv.lb), preReq, iv)::
     Equality(ShapeVar(iv.width), ShapeVar(iv.lb), preReq, iv)::
     Equality(ShapeVar(iv), ShapeVar(iv.ub), postReq, iv)::Nil,
     iv.lb::iv.lbStrict::iv.ub::iv.ubStrict::iv.step::iv.width::Nil)

  def withNodeConstraints(w: WithNode[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(w), ShapeVar(w.expr), postReq, w)::Nil,
     w.expr::w.iv::Nil)

  def recoverWithNode(e: Exp[MDArray[_]]): WithNode[_] =
    getDefinition(e).asInstanceOf[WithNode[_]]

  def withNodeListConstraints(node: Any, withNodeList: List[Exp[MDArray[_]]]): List[TypingConstraint] = {
    val f = withNodeList(0) // first node

    // and map the rest of the nodes
    withNodeList.tail.map(e => Equality(ShapeVar(getSymNumber(recoverWithNode(f).iv)), ShapeVar(getSymNumber(recoverWithNode(e).iv)), preReq, node)) :::
    withNodeList.tail.map(e => Equality(ShapeVar(getSymNumber(f)), ShapeVar(getSymNumber(e)), preReq, node))
  }

  def genArrayConstraints(ga: GenArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (withNodeListConstraints(ga, ga.lExpr):::
     Equality(ShapeVar(ga.shp), Lst(getNewUnknown::Nil), preReq, ga)::
     Equality(ShapeVar(ga.shp), ShapeVar(getSymNumber(recoverWithNode(ga.lExpr.head).iv)), preReq, ga)::
     EqualityAeqBcatC(ShapeVar(ga), ValueVar(ga.shp), ShapeVar(getSymNumber(ga.lExpr.head)), postReq, ga)::Nil,
     ga.shp::ga.lExpr)

  def modArrayConstraints(ma: ModArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (withNodeListConstraints(ma, ma.lExpr) :::
     ma.lExpr.flatMap(wn =>
       PrefixLt(ShapeVar(ma.a), ValueVar(getSymNumber(recoverWithNode(wn).iv)), ShapeVar(getSymNumber(wn)), preReq, ma) ::
       SuffixEq(ShapeVar(ma.a), ValueVar(getSymNumber(recoverWithNode(wn).iv)), ShapeVar(getSymNumber(wn)), preReq, ma) :: Nil
     ) :::
     Equality(ShapeVar(ma), ShapeVar(ma.a), postReq, ma)::Nil,
     ma.a::ma.lExpr)

  def foldArrayConstraints(fa: FoldArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(fa.neutral), ShapeVar(fa.wExpr), preReq, fa)::
     Equality(ShapeVar(fa), ShapeVar(fa.neutral), postReq, fa)::Nil,
     fa.neutral::fa.wExpr::Nil)

  def ifThenElseConstraint(ite: IfThenElse[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(ite.cond), Lst(Nil), preReq, ite)::
     CommonDenominator(ShapeVar(ite), ShapeVar(ite.thenp), ShapeVar(ite.elsep), postReq, ite)::Nil,
     ite.thenp::ite.elsep::ite.cond::Nil)


  // Integer operations for Game of Life, NumericOps is too much
  def intPlusConstraints(ip: IntPlus): Pair[List[TypingConstraint], List[Exp[_]]] = intConstraints(ip, ip.a, ip.b)
  def intMinusConstraints(ip: IntMinus): Pair[List[TypingConstraint], List[Exp[_]]] = intConstraints(ip, ip.a, ip.b)
  def intLessConstraints(ip: IntLess): Pair[List[TypingConstraint], List[Exp[_]]] = intConstraints(ip, ip.a, ip.b)
  def intEqualConstraints(ip: IntEqual): Pair[List[TypingConstraint], List[Exp[_]]] = intConstraints(ip, ip.a, ip.b)

  def intConstraints(op: Def[_], a: Exp[Int], b: Exp[Int]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(a), Lst(Nil), preReq, op) ::
     Equality(ShapeVar(b), Lst(Nil), preReq, op) ::
     Equality(ShapeVar(op), Lst(Nil), postReq, op) :: Nil, a :: b :: Nil)

}