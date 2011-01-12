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

    // XXX: These calls should never fail. If they fail it's better to fail quickly rather than hide the error
    def getSymNumber(e: Any): Int = e match {
      case s: Sym[_] => s.id
      case d: Def[_] => findDefinition(d).get.sym.id
    }
    def getDefinition(e: Any): Def[_] = e match {
      case s: Sym[_] => findDefinition(s).get.rhs
      case d: Def[_] => d
    }

    val result = getDefinition(r)
    val nodes = new HashSet[Def[_]]()

    def knownAtCompileTimeConstraints(kc: KnownAtCompileTime[_]): List[TypingConstraint] = {

      def bind(i: Any): TypingElement = i match {
        case i: Int => Value(i)
        case _ => getNewUnknown
      }

      val symNo: Int = getSymNumber(kc)

      val shapeVar: TypingVariable = Var("S" + symNo)
      val shapeList: List[TypingElement] = kc.value.shape.map(i => bind(i))
      val shapeRHS: TypingVariable = Lst(shapeList)
      val valueVar: TypingVariable = Var("V" + symNo)
      val valueList: List[TypingElement] = kc.value.content.map((i: Any) => bind(i)).toList
      val valueRHS: TypingVariable = Lst(valueList)

      Equality(shapeVar, shapeRHS, false) ::
      Equality(valueVar, valueRHS, false) :: Nil
    }

    def knownAtRuntimeConstraints(kr: KnownAtRuntime[_]): List[TypingConstraint] =
      if (kr.value)
        Equality(Var("S" + getSymNumber(kr)), Lst(Nil), false) :: Nil
      else
        Equality(Var("S" + getSymNumber(kr)), Lst(getNewUnknown::Nil), false) :: Nil

    def infixOpAAConstraints(in: InfixOpAA[_, _]): List[TypingConstraint] = {
      val array1def = getDefinition(in.array1)
      val array2def = getDefinition(in.array2)

      val inOpNo = getSymNumber(in)
      val array1no = getSymNumber(in.array1)
      val array2no = getSymNumber(in.array2)

      Equality(Var("S" + array1no), Var("S" + array2no), true) ::
      Equality(Var("S" + inOpNo), Var("S" + array1no), false) :: Nil :::
      gatherConstraints(array1def) :::
      gatherConstraints(array2def)
    }

    def reshapeConstraints(rs: Reshape[_]): List[TypingConstraint] = {
      val shpDef = getDefinition(rs.shp)
      val arrDef = getDefinition(rs.a)

      val rsNo = getSymNumber(rs)
      val shpNo = getSymNumber(rs.shp)
      val arrNo = getSymNumber(rs.a)

      Equality(Var("S" + shpNo), Lst(getNewUnknown :: Nil), true) ::
      EqualProduct(Var("V" + shpNo), Var("S" + arrNo), true) ::
      Equality(Var("S" + rsNo), Var("V" + shpNo), false) :: Nil :::
      gatherConstraints(shpDef) :::
      gatherConstraints(arrDef)
    }

    def gatherConstraints(node: Def[_]): List[TypingConstraint] = nodes.contains(node) match {
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