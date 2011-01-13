package scala.virtualization.lms
package epfl
package test7

import common._
import internal._
import original._
import original.Conversions._
import collection.mutable.HashSet


trait MDArrayBaseTyping extends MDArrayBaseTypingPrimitives {

  import IR._

  def printTypingConstraints(r: Any): Unit = {
    val constraints: List[TypingConstraint] = createTypingConstraints(r)
    for (t: TypingConstraint <- constraints)
      println(t)
  }

  def createTypingConstraints(r: Any): List[TypingConstraint] = {
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

    val result = getDefinition(r)
    val nodes = new HashSet[Any]()

    def toValue(i: Any): TypingElement = i match {
      case i: Int => Value(i)
      case _ => getNewUnknown
    }

    def constConstraints(ct: Const[_]): List[TypingConstraint] = {
      val ctNo = getSymNumber(ct)

      //Equality(ShapeVar(ctNo), Lst(Nil), postReq) ::
      //Equality(ValueVar(ctNo), Lst(toValue(ct.x)::Nil), postReq) :: Nil
      Nil
    }

    def knownAtCompileTimeConstraints(kc: KnownAtCompileTime[_]): List[TypingConstraint] = {
      val symNo: Int = getSymNumber(kc)

      val shapeVar: TypingVariable = ShapeVar(symNo)
      val shapeList: List[TypingElement] = kc.value.shape.map(i => toValue(i))
      val shapeRHS: TypingVariable = Lst(shapeList)
      val valueVar: TypingVariable = ValueVar(symNo)
      val valueList: List[TypingElement] = kc.value.content.map((i: Any) => toValue(i)).toList
      val valueRHS: TypingVariable = Lst(valueList)

      Equality(shapeVar, shapeRHS, postReq) ::
      Equality(valueVar, valueRHS, postReq) :: Nil
    }

    def toDimConstraints(td: ToDim[_]): List[TypingConstraint] = {
      val arrDef = getDefinition(td.a)
      val tdNo = getSymNumber(td)
      val arrNo = getSymNumber(td.a)

      Equality(ShapeVar(tdNo), Lst(Nil), postReq) ::
      Equality(ValueVar(tdNo), Lst(LengthOf(ShapeVar(arrNo))::Nil), postReq) ::
      gatherConstraints(arrDef)
    }

    def toShapeConstraints(ts: ToShape[_]): List[TypingConstraint] = {
      val arrDef = getDefinition(ts.a)
      val tsNo = getSymNumber(ts)
      val arrNo = getSymNumber(ts.a)

      Equality(ValueVar(tsNo), ShapeVar(arrNo), postReq) ::
      Equality(ShapeVar(tsNo), Lst(getNewUnknown::Nil), postReq) ::
      gatherConstraints(arrDef)
    }

    def reshapeConstraints(rs: Reshape[_]): List[TypingConstraint] = {
      val shpDef = getDefinition(rs.shp)
      val arrDef = getDefinition(rs.a)

      val rsNo = getSymNumber(rs)
      val shpNo = getSymNumber(rs.shp)
      val arrNo = getSymNumber(rs.a)

      Equality(ShapeVar(shpNo), Lst(getNewUnknown :: Nil), preReq) ::
      EqualProduct(ValueVar(shpNo), ShapeVar(arrNo), preReq) ::
      Equality(ShapeVar(rsNo), ValueVar(shpNo), postReq) ::
      gatherConstraints(shpDef) :::
      gatherConstraints(arrDef)
    }

    def selConstraints(sel: Sel[_]): List[TypingConstraint] = {
      val ivDef = getDefinition(sel.iv)
      val arrDef = getDefinition(sel.a)

      val selNo = getSymNumber(sel)
      val ivNo = getSymNumber(sel.iv)
      val arrNo = getSymNumber(sel.a)

      Equality(ShapeVar(ivNo), Lst(getNewUnknown::Nil), preReq) ::
      PrefixLt(ShapeVar(arrNo), ValueVar(ivNo), ShapeVar(selNo), preReq) ::
      SuffixEq(ShapeVar(arrNo), ValueVar(ivNo), ShapeVar(selNo), postReq) ::
      gatherConstraints(ivDef) :::
      gatherConstraints(arrDef)
    }

    def catConstraints(cat: Cat[_]): List[TypingConstraint] = {
      val dDef = getDefinition(cat.d)
      val arr1Def = getDefinition(cat.a)
      val arr2Def = getDefinition(cat.b)

      val catNo = getSymNumber(cat)
      val dNo = getSymNumber(cat.d)
      val arr1No = getSymNumber(cat.a)
      val arr2No = getSymNumber(cat.b)

      LengthEqualityAeqB(ShapeVar(arr1No), ShapeVar(arr2No), preReq) ::
      LessThan(ValueVar(dNo), Lst(LengthOf(ShapeVar(arr1No))::Nil), preReq) ::
      EqualityExceptFor(ValueVar(dNo), ShapeVar(arr1No), ShapeVar(arr2No), preReq) ::
      // TODO: Replace LengthEqualityAeqB with a node that contains more information - which could infer the exact shape
      // something like EquailtyAeqBcatC would do :)
      LengthEqualityAeqB(ShapeVar(catNo), ShapeVar(arr1No), postReq) ::
      gatherConstraints(dDef) :::
      gatherConstraints(arr1Def) :::
      gatherConstraints(arr2Def)
    }

    def reduceConstraints(red: Reduce[_,_]): List[TypingConstraint] = {
      val arrDef = getDefinition(red.a)

      val arrNo = getSymNumber(red.a)
      val redNo = getSymNumber(red)

      Equality(ShapeVar(redNo), Lst(Nil), postReq) ::
      Equality(ValueVar(redNo), Lst(getNewUnknown::Nil), postReq) ::
      gatherConstraints(arrDef)
    }

    def valuesConstraints(values: Values[_]): List[TypingConstraint] = {
      val valDef = getDefinition(values.value)
      val dimDef = getDefinition(values.dim)

      val valuesNo = getSymNumber(values)
      val valNo = getSymNumber(values.value)
      val dimNo = getSymNumber(values.dim)

      Equality(ShapeVar(valuesNo), ValueVar(dimNo), postReq) ::
      // TODO: We can add more information here for cases where we know dim and value
      gatherConstraints(valDef) :::
      gatherConstraints(dimDef)
    }

    def infixOpAAConstraints(in: InfixOpAA[_, _]): List[TypingConstraint] = {
      val array1def = getDefinition(in.array1)
      val array2def = getDefinition(in.array2)

      val inOpNo = getSymNumber(in)
      val array1no = getSymNumber(in.array1)
      val array2no = getSymNumber(in.array2)

      Equality(ShapeVar(array1no), ShapeVar(array2no), preReq) ::
      Equality(ShapeVar(inOpNo), ShapeVar(array1no), postReq) ::
      gatherConstraints(array1def) :::
      gatherConstraints(array2def)
    }

    def infixOpAEConstraints(in: InfixOpAE[_, _]): List[TypingConstraint] = {
      val arrayDef = getDefinition(in.array)
      val elemDef = getDefinition(in.element)

      val inOpNo = getSymNumber(in)
      val arrayNo = getSymNumber(in.array)
      val elemNo = getSymNumber(in.element) // Not necessary

      Equality(ShapeVar(inOpNo), ShapeVar(arrayNo), postReq) ::
      gatherConstraints(arrayDef) :::
      gatherConstraints(elemDef)
    }

    def unaryOpConstraints(un: UnaryOp[_, _]): List[TypingConstraint] = {
      val arrayDef = getDefinition(un.array)

      val unOpNo = getSymNumber(un)
      val arrayNo = getSymNumber(un.array)

      Equality(ShapeVar(unOpNo), ShapeVar(arrayNo), postReq) ::
      gatherConstraints(arrayDef)
    }

    def whereConstraints(wh: Where[_]): List[TypingConstraint] = {
      val condDef = getDefinition(wh.cond)
      val arr1def = getDefinition(wh.array1)
      val arr2def = getDefinition(wh.array2)

      val whNo = getSymNumber(wh)
      val condNo = getSymNumber(wh.cond)
      val arr1no = getSymNumber(wh.array1)
      val arr2no = getSymNumber(wh.array2)

      Equality(ShapeVar(condNo), ShapeVar(arr1no), preReq) ::
      Equality(ShapeVar(condNo), ShapeVar(arr2no), preReq) ::
      Equality(ShapeVar(whNo), ShapeVar(arr1no), postReq) ::
      gatherConstraints(condDef) :::
      gatherConstraints(arr1def) :::
      gatherConstraints(arr2def)
    }

    def indexVectorConstraints(iv: IndexVector): List[TypingConstraint] = {
      val lbDef = getDefinition(iv.lb)
      val lbSDef = getDefinition(iv.lbStrict)
      val ubDef = getDefinition(iv.ub)
      val ubSDef = getDefinition(iv.ubStrict)
      val stepDef = getDefinition(iv.step)
      val widthDef = getDefinition(iv.width)

      val ivNo = getSymNumber(iv)
      val lbNo = getSymNumber(iv.lb)
      val lbSNo = getSymNumber(iv.lbStrict)
      val ubNo = getSymNumber(iv.ub)
      val ubSNo = getSymNumber(iv.ubStrict)
      val stepNo = getSymNumber(iv.step)
      val widthNo = getSymNumber(iv.width)

      Equality(ShapeVar(lbNo), Lst(getNewUnknown::Nil), preReq) ::
      Equality(ShapeVar(ubNo), ShapeVar(lbNo), preReq) ::
      Equality(ShapeVar(stepNo), ShapeVar(lbNo), preReq) ::
      Equality(ShapeVar(widthNo), ShapeVar(lbNo), preReq) ::
      Equality(ShapeVar(ivNo), ShapeVar(lbNo), preReq) ::
      gatherConstraints(lbDef) :::
      gatherConstraints(lbSDef) :::
      gatherConstraints(ubDef) :::
      gatherConstraints(ubSDef) :::
      gatherConstraints(stepDef) :::
      gatherConstraints(widthDef)
    }

    def withNodeConstraints(w: WithNode[_]): List[TypingConstraint] = {
      val ivDef = getDefinition(w.iv)
      val exprDef = getDefinition(w.expr)

      val wNo = getSymNumber(w)
      val ivNo = getSymNumber(w.iv)
      val exprNo = getSymNumber(w.expr)

      Equality(ShapeVar(wNo), ShapeVar(exprNo), postReq) ::
      gatherConstraints(ivDef) :::
      gatherConstraints(exprDef)
    }

    def recoverWithNode(e: Exp[MDArray[_]]): WithNode[_] =
      getDefinition(e).asInstanceOf[WithNode[_]]

    def withNodeListConstraints(withNodeList: List[Exp[MDArray[_]]]): List[TypingConstraint] = {
      val f = withNodeList(0) // first node

      // and map the rest of the nodes
      withNodeList.tail.map(e => Equality(ShapeVar(getSymNumber(recoverWithNode(f).iv)), ShapeVar(getSymNumber(recoverWithNode(e).iv)), preReq)) :::
      withNodeList.tail.map(e => Equality(ShapeVar(getSymNumber(f)), ShapeVar(getSymNumber(e)), preReq))
    }

    def genArrayConstraints(ga: GenArrayWith[_]): List[TypingConstraint] = {
      val shpDef = getDefinition(ga.shp)
      val gaNo = getSymNumber(ga)
      val shpNo = getSymNumber(ga.shp)

      Equality(ValueVar(shpNo), Lst(getNewUnknown::Nil), preReq) ::
      Equality(ValueVar(shpNo), ShapeVar(getSymNumber(recoverWithNode(ga.lExpr.head).iv)), preReq) ::
      withNodeListConstraints(ga.lExpr) :::
      EqualityAeqBcatC(ShapeVar(gaNo), ValueVar(shpNo), ShapeVar(getSymNumber(ga.lExpr.head)), postReq) ::
      gatherConstraints(shpDef) :::
      ga.lExpr.flatMap(wn => gatherConstraints(getDefinition(wn)))
    }

    def modArrayConstraints(ma: ModArrayWith[_]): List[TypingConstraint] = {
      val arrDef = getDefinition(ma.a)
      val maNo = getSymNumber(ma)
      val arrNo = getSymNumber(ma.a)

      withNodeListConstraints(ma.lExpr) :::
      ma.lExpr.flatMap(wn =>
        PrefixLt(ShapeVar(arrNo), ValueVar(getSymNumber(recoverWithNode(wn).iv)), ShapeVar(getSymNumber(wn)), preReq) ::
        SuffixEq(ShapeVar(arrNo), ValueVar(getSymNumber(recoverWithNode(wn).iv)), ShapeVar(getSymNumber(wn)), preReq) :: Nil
      ) :::
      Equality(ShapeVar(maNo), ShapeVar(arrNo), postReq) ::
      gatherConstraints(arrDef) :::
      ma.lExpr.flatMap(wn => gatherConstraints(getDefinition(wn)))
    }

    def foldArrayConstraints(fa: FoldArrayWith[_]): List[TypingConstraint] = {
      val neutDef = getDefinition(fa.neutral)
      val withDef = getDefinition(fa.wExpr)

      val faNo = getSymNumber(fa)
      val neutNo = getSymNumber(fa.neutral)
      val withNo = getSymNumber(fa.wExpr)

      Equality(ShapeVar(neutNo), ShapeVar(withNo), preReq) ::
      Equality(ShapeVar(faNo), ShapeVar(neutNo), postReq) ::
      gatherConstraints(neutDef) :::
      gatherConstraints(withDef)
    }

    def ifThenElseConstraint(ite: IfThenElse[_]): List[TypingConstraint] = {
      val thenDef = getDefinition(ite.thenp)
      val elseDef = getDefinition(ite.elsep)
      val condDef = getDefinition(ite.cond)

      val iteNo = getSymNumber(ite)
      val thenNo = getSymNumber(ite.thenp)
      val elseNo = getSymNumber(ite.elsep)
      val condNo = getSymNumber(ite.cond)

      Equality(ShapeVar(condNo), Lst(Nil), preReq) ::
      CommonDenominator(ShapeVar(iteNo), ShapeVar(thenNo), ShapeVar(elseNo), postReq) ::
      gatherConstraints(condDef) :::
      gatherConstraints(thenDef) :::
      gatherConstraints(elseDef)
    }

    def gatherConstraints(rawNode: Any): List[TypingConstraint] = {
      val node = getDefinition(rawNode)
      nodes.contains(node) match {
        case false =>
          nodes.add(node)
          node match {
            case ct: Const[_] => constConstraints(ct)
            case kc: KnownAtCompileTime[_] => knownAtCompileTimeConstraints(kc)
            case kr: KnownAtRuntimeListArray[_] => Equality(ShapeVar(getSymNumber(kr)), Lst(getNewUnknown::Nil), postReq) :: Nil
            case kr: KnownAtRuntimeValue[_] => Equality(ShapeVar(getSymNumber(kr)), Lst(Nil), postReq) :: Nil
            case fl: FromList[_] => Equality(ShapeVar(getSymNumber(fl)), Lst(getNewUnknown::Nil), postReq)::Nil
            case fa: FromArray[_] => Equality(ShapeVar(getSymNumber(fa)), Lst(getNewUnknown::Nil), postReq)::Nil
            case fv: FromValue[_] => Equality(ShapeVar(getSymNumber(fv)), Lst(Nil), postReq)::Nil
            case tl: ToList[_] => Equality(ShapeVar(getSymNumber(tl)), Lst(getNewUnknown::Nil), preReq)::Nil
            case ta: ToArray[_] => Equality(ShapeVar(getSymNumber(ta)), Lst(getNewUnknown::Nil), preReq)::Nil
            case tv: ToValue[_] => Equality(ShapeVar(getSymNumber(tv)), Lst(Nil), preReq)::Nil
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
        case true =>
        // do nothing
          Nil
      }
    }

    gatherConstraints(result)
  }
}