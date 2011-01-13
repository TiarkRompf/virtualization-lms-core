package scala.virtualization.lms
package epfl
package test7

import common._
import internal._
import original._
import original.Conversions._
import collection.mutable.{Queue, HashSet}

trait MDArrayBaseTyping extends MDArrayBaseTypingPrimitives {

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

  def gatherConstraints(node: Any): Pair[List[TypingConstraint], List[Exp[_]]] = node match {
    case ct: Const[_] => constConstraints(ct)
    case kc: KnownAtCompileTime[_] => knownAtCompileTimeConstraints(kc)
    case kr: KnownAtRuntimeListArray[_] => knownAtRuntimeListArrayConstraints(kr)
    case kr: KnownAtRuntimeValue[_] => knownAtRuntimeValueConstraints(kr)
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
    case _ => throw new RuntimeException("Unknown node: " + node.toString)
  }

  def toValue(i: Any): TypingElement = i match {
    case i: Int => Value(i)
    case _ => getNewUnknown
  }

  def constConstraints(ct: Const[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(ct), Lst(Nil), postReq) ::
     Equality(ValueVar(ct), Lst(toValue(ct.x)::Nil), postReq)::Nil, Nil)

  def knownAtCompileTimeConstraints(kc: KnownAtCompileTime[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(kc), Lst(kc.value.shape.map(i => toValue(i))), postReq) ::
     Equality(ValueVar(kc), Lst(kc.value.content.map((i: Any) => toValue(i)).toList), postReq) :: Nil, Nil)

  def knownAtRuntimeListArrayConstraints(kr: KnownAtRuntimeListArray[_]) =
    (Equality(ShapeVar(getSymNumber(kr)), Lst(getNewUnknown::Nil), postReq) :: Nil, Nil)

  def knownAtRuntimeValueConstraints(kr: KnownAtRuntimeValue[_]) =
    (Equality(ShapeVar(getSymNumber(kr)), Lst(Nil), postReq) :: Nil, Nil)

  def fromListConstraints(fl: FromList[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(fl)), Lst(getNewUnknown::Nil), postReq)::Nil, Nil)

  def fromArrayConstraints(fa: FromArray[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(fa)), Lst(getNewUnknown::Nil), postReq)::Nil, Nil)

  def fromValueConstraints(fv: FromValue[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(fv)), Lst(Nil), postReq)::Nil, Nil)

  def toListConstraints(tl: ToList[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(tl)), Lst(getNewUnknown::Nil), preReq)::Nil, tl.value::Nil)

  def toArrayConstraints(ta: ToArray[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(ta)), Lst(getNewUnknown::Nil), preReq)::Nil, ta.value::Nil)

  def toValueConstraints(tv: ToValue[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(getSymNumber(tv)), Lst(Nil), preReq)::Nil, tv.value::Nil)

  def toDimConstraints(td: ToDim[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(td), Lst(Nil), postReq) ::
     Equality(ValueVar(td), Lst(LengthOf(ShapeVar(td.a))::Nil), postReq)::Nil, td.a::Nil)

  def toShapeConstraints(ts: ToShape[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ValueVar(ts), ShapeVar(ts.a), postReq)::
     Equality(ShapeVar(ts), Lst(getNewUnknown::Nil), postReq)::Nil,
     ts.a::Nil)

  def reshapeConstraints(rs: Reshape[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(rs.shp), Lst(getNewUnknown :: Nil), preReq)::
     EqualProduct(ValueVar(rs.shp), ShapeVar(rs.a), preReq)::
     Equality(ShapeVar(rs), ValueVar(rs.shp), postReq)::Nil,
     rs.shp::rs.a::Nil)

  def selConstraints(sel: Sel[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(sel.iv), Lst(getNewUnknown::Nil), preReq)::
     PrefixLt(ShapeVar(sel.a), ValueVar(sel.iv), ShapeVar(sel), preReq)::
     SuffixEq(ShapeVar(sel.a), ValueVar(sel.iv), ShapeVar(sel), postReq)::Nil,
     sel.iv::sel.a::Nil)

  def catConstraints(cat: Cat[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (LengthEqualityAeqB(ShapeVar(cat.a), ShapeVar(cat.a), preReq)::
     LessThan(ValueVar(cat.d), Lst(LengthOf(ShapeVar(cat.a))::Nil), preReq)::
     EqualityExceptFor(ValueVar(cat.d), ShapeVar(cat.a), ShapeVar(cat.b), preReq)::
     // TODO: Replace LengthEqualityAeqB with a node that contains more information - which could infer the exact shape
     // something like EquailtyAeqBcatC would do :)
     LengthEqualityAeqB(ShapeVar(cat), ShapeVar(cat.a), postReq)::Nil,
     cat.d::cat.a::cat.b::Nil)

  def reduceConstraints(red: Reduce[_,_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(red), Lst(Nil), postReq)::
     Equality(ValueVar(red), Lst(getNewUnknown::Nil), postReq)::Nil,
     red.a::Nil)

  def valuesConstraints(values: Values[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(values), ValueVar(values.dim), postReq)::Nil,
     // TODO: We can add more information here for cases where we know dim and value
     values.value::values.dim::Nil)

  def infixOpAAConstraints(in: InfixOpAA[_, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(in.array1), ShapeVar(in.array2), preReq)::
     Equality(ShapeVar(in), ShapeVar(in.array1), postReq)::Nil,
     in.array1::in.array2::Nil)

  def infixOpAEConstraints(in: InfixOpAE[_, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(in), ShapeVar(in.array), postReq)::Nil,
     in.array::in.element::Nil)

  def unaryOpConstraints(un: UnaryOp[_, _]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(un), ShapeVar(un.array), postReq)::Nil,
     un.array::Nil)

  def whereConstraints(wh: Where[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(wh.cond), ShapeVar(wh.array1), preReq)::
     Equality(ShapeVar(wh.cond), ShapeVar(wh.array2), preReq)::
     Equality(ShapeVar(wh), ShapeVar(wh.array1), postReq)::Nil,
     // TODO: Understand why wh.array1::wh.array2::wh.cond::Nil here produces a type mismatch
     List(wh.array1, wh.array2, wh.cond))

  def indexVectorConstraints(iv: IndexVector): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(iv.lb), Lst(getNewUnknown::Nil), preReq)::
     Equality(ShapeVar(iv.ub), ShapeVar(iv.lb), preReq)::
     Equality(ShapeVar(iv.step), ShapeVar(iv.lb), preReq)::
     Equality(ShapeVar(iv.width), ShapeVar(iv.lb), preReq)::
     Equality(ShapeVar(iv), ShapeVar(iv.ub), postReq)::Nil,
     iv.lb::iv.lbStrict::iv.ub::iv.ubStrict::iv.step::iv.width::Nil)

  def withNodeConstraints(w: WithNode[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(w), ShapeVar(w.expr), postReq)::Nil,
     w.expr::w.iv::Nil)

  def recoverWithNode(e: Exp[MDArray[_]]): WithNode[_] =
    getDefinition(e).asInstanceOf[WithNode[_]]

  def withNodeListConstraints(withNodeList: List[Exp[MDArray[_]]]): List[TypingConstraint] = {
    val f = withNodeList(0) // first node

    // and map the rest of the nodes
    withNodeList.tail.map(e => Equality(ShapeVar(getSymNumber(recoverWithNode(f).iv)), ShapeVar(getSymNumber(recoverWithNode(e).iv)), preReq)) :::
    withNodeList.tail.map(e => Equality(ShapeVar(getSymNumber(f)), ShapeVar(getSymNumber(e)), preReq))
  }

  def genArrayConstraints(ga: GenArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (withNodeListConstraints(ga.lExpr):::
     Equality(ValueVar(ga.shp), Lst(getNewUnknown::Nil), preReq)::
     Equality(ValueVar(ga.shp), ShapeVar(getSymNumber(recoverWithNode(ga.lExpr.head).iv)), preReq)::
     EqualityAeqBcatC(ShapeVar(ga), ValueVar(ga.shp), ShapeVar(getSymNumber(ga.lExpr.head)), postReq)::Nil,
     ga.shp::ga.lExpr)

  def modArrayConstraints(ma: ModArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (withNodeListConstraints(ma.lExpr) :::
     ma.lExpr.flatMap(wn =>
       PrefixLt(ShapeVar(ma.a), ValueVar(getSymNumber(recoverWithNode(wn).iv)), ShapeVar(getSymNumber(wn)), preReq) ::
       SuffixEq(ShapeVar(ma.a), ValueVar(getSymNumber(recoverWithNode(wn).iv)), ShapeVar(getSymNumber(wn)), preReq) :: Nil
     ) :::
     Equality(ShapeVar(ma), ShapeVar(ma.a), postReq)::Nil,
     ma.a::ma.lExpr)

  def foldArrayConstraints(fa: FoldArrayWith[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(fa.neutral), ShapeVar(fa.wExpr), preReq)::
     Equality(ShapeVar(fa), ShapeVar(fa.neutral), postReq)::Nil,
     fa.neutral::fa.wExpr::Nil)

  def ifThenElseConstraint(ite: IfThenElse[_]): Pair[List[TypingConstraint], List[Exp[_]]] =
    (Equality(ShapeVar(ite.cond), Lst(Nil), preReq)::
     CommonDenominator(ShapeVar(ite), ShapeVar(ite.thenp), ShapeVar(ite.elsep), postReq)::Nil,
     ite.thenp::ite.elsep::ite.cond::Nil)
}