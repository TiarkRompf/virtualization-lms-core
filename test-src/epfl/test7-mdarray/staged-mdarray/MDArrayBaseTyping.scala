package scala.virtualization.lms
package epfl
package test7

import common._
import internal._
import original._
import original.Conversions._
import collection.mutable.HashSet


trait MDArrayBaseTyping {

  var unknownIndex = 0
  val IR: MDArrayBaseExp
  import IR._

  abstract class TypingVariable
  case class Var(name: String) extends TypingVariable { override def toString = name }
  case class Lst(list: List[TypingElement]) extends TypingVariable { override def toString = list.map(elt => elt.toString).mkString("[", "  ", "]") }

  abstract class TypingElement
  case class Value(n: Int) extends TypingElement { override def toString = n.toString }
  case class Unknown(i: Int) extends TypingElement { override def toString = "U" + i.toString}
  case class Dim(v: TypingVariable) extends TypingElement { override def toString = "dim(" + v.toString + ")" }
  def getNewUnknown(): Unknown = Unknown({ unknownIndex += 1; unknownIndex })

  class TypingConstraint(val prereq: Boolean) { override def toString = getConstraintString(this) }
  case class Equality(a: TypingVariable, b: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class LessThan(a: TypingVariable, b: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class LessThanOrEqual(a: TypingVariable, b: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class PrefixLt(a: TypingVariable, b: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class SuffixEq(a: TypingVariable, b: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class EqualityAeqBcatC(a: TypingVariable, b: TypingVariable, c: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class LengthEqualityAeqB(a: TypingVariable, b: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class LengthEqualityAeqBplusC(a: TypingVariable, b: TypingVariable, c: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class CommonDenominator(a: TypingVariable, b: TypingVariable, c: TypingVariable, override val prereq: Boolean) extends TypingConstraint(prereq)
  case class EqualProduct(a: TypingVariable, b: TypingVariable, override  val prereq: Boolean) extends TypingConstraint(prereq)

  private def getConstraintString(constraint: TypingConstraint): String = {
    val body = constraint match {
      case eq: Equality => eq.a.toString + " = " + eq.b.toString
      case lt: LessThan => lt.a.toString + " < " + lt.b.toString
      case le: LessThanOrEqual => le.a.toString + " <= " + le.b.toString
      case pl: PrefixLt => pl.a.toString + "(:length(" + pl.b.toString + ")) < " + pl.b.toString
      case se: SuffixEq => se.a.toString + "(length(" + se.a.toString + ") - length(" + se.b.toString + "):) = " + se.b.toString
      case eq: EqualityAeqBcatC => eq.a.toString + " = " + eq.b.toString + " ::: " + eq.c.toString
      case le: LengthEqualityAeqB => "length(" + le.a.toString + ") = length(" + le.b.toString + ")"
      case le: LengthEqualityAeqBplusC => "length(" + le.a.toString + ") = length(" + le.b.toString + ") + length(" + le.c.toString + ")"
      case cd: CommonDenominator => cd.a.toString + " = common(" + cd.b.toString + ", " + cd.c.toString + ")"
      case ep: EqualProduct => "@runtime => prod(" + ep.a.toString + ") = prod(" + ep.b.toString + ")"
      case _ => "unknown constraint"
    }
    " * " + body + "    (prereq:" + constraint.prereq.toString + ")"
  }

  def printTypingConstraints(r: Exp[_]): Unit = {

    val constraints: List[TypingConstraint] = createTypingConstraints(r)
    for (t: TypingConstraint <- constraints)
      println(t)
  }

  def createTypingConstraints(r: Exp[_]): List[TypingConstraint] = {

    def richDef(e: Exp[_]): RichDef[_] = e match {
      case s: Sym[_] => findDefinition(s).get.rhs.asInstanceOf[RichDef[_]]
      case r: RichDef[_] => r
      case _ => throw new Exception("internal error")
    }

    val result = richDef(r)
    val nodes = new HashSet[RichDef[_]]()

    def knownAtCompileTimeConstraints(kc: KnownAtCompileTime[_]): List[TypingConstraint] = {

      def bind(i: Any): TypingElement = i match {
        case i: Int => Value(i)
        case _ => getNewUnknown
      }

      val shapeVar: TypingVariable = Var("S" + kc.typeVarIndex)
      val shapeList: List[TypingElement] = kc.value.shape.map(i => bind(i))
      val shapeRHS: TypingVariable = Lst(shapeList)
      val valueVar: TypingVariable = Var("V" + kc.typeVarIndex)
      val valueList: List[TypingElement] = kc.value.content.map((i: Any) => bind(i)).toList
      val valueRHS: TypingVariable = Lst(valueList)

      Equality(shapeVar, shapeRHS, false) ::
      Equality(valueVar, valueRHS, false) :: Nil
    }

    def knownAtRuntimeConstraints(kr: KnownAtRuntime[_]): List[TypingConstraint] =
      if (kr.value)
        Equality(Var("S" + kr.typeVarIndex), Lst(Nil), false) :: Nil
      else
        Equality(Var("S" + kr.typeVarIndex), Lst(getNewUnknown::Nil), false) :: Nil

    def infixOpAAConstraints(in: InfixOpAA[_, _]): List[TypingConstraint] = {
      val array1def = richDef(in.array1)
      val array2def = richDef(in.array2)

      Equality(Var("S" + array1def.typeVarIndex), Var("S" + array2def.typeVarIndex), true) ::
      Equality(Var("S" + in.typeVarIndex), Var("S" + array1def.typeVarIndex), false) :: Nil :::
      gatherConstraints(array1def) :::
      gatherConstraints(array2def)
    }

    def reshapeConstraints(rs: Reshape[_]): List[TypingConstraint] = {
      val shpDef = richDef(rs.shp)
      val arrDef = richDef(rs.a)

      Equality(Var("S" + shpDef.typeVarIndex), Lst(getNewUnknown :: Nil), true) ::
      EqualProduct(Var("V" + shpDef.typeVarIndex), Var("S" + arrDef.typeVarIndex), true) ::
      Equality(Var("S" + rs.typeVarIndex), Var("V" + shpDef.typeVarIndex), false) :: Nil :::
      gatherConstraints(shpDef) :::
      gatherConstraints(arrDef)
    }

    def gatherConstraints(node: RichDef[_]): List[TypingConstraint] = nodes.contains(node) match {
      case false =>
        nodes.add(node)
        node match {
          case kc: KnownAtCompileTime[_] => knownAtCompileTimeConstraints(kc)
          case kr: KnownAtRuntime[_] => knownAtRuntimeConstraints(kr)
          case io: InfixOpAA[_, _] => infixOpAAConstraints(io)
          case rs: Reshape[_] => reshapeConstraints(rs)
          // TODO: Add the rest of the nodes here
          case _ => println("Unknown AST node!"); Nil
        }
      case true =>
        // do nothing
        Nil
    }

    gatherConstraints(result)
  }
}