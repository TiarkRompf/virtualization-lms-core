package scala.virtualization.lms
package epfl
package test7

import common._
import internal._
import original._
import original.Conversions._
import collection.mutable.HashSet


trait MDArrayBaseTypingPrimitives {
  var unknownIndex = 0
  val IR: MDArrayBaseExp
  // Just to make the code look better :)
  val preReq: Boolean = true
  val postReq: Boolean = false

  abstract class TypingVariable
  abstract class Var(name: String) extends TypingVariable { override def toString = name }
  case class ShapeVar(i: Int) extends Var("S" + i)
  case class ValueVar(i: Int) extends Var("V" + i)
  case class Lst(list: List[TypingElement]) extends TypingVariable { override def toString = list.map(elt => elt.toString).mkString("[", "  ", "]") }

  abstract class TypingElement
  case class Value(n: Int) extends TypingElement { override def toString = n.toString }
  case class Unknown(i: Int) extends TypingElement { override def toString = "U" + i.toString}
  case class LengthOf(v: Var) extends TypingElement { override def toString = "dim(" + v.toString + ")" }
  def getNewUnknown(): Unknown = Unknown({ unknownIndex += 1; unknownIndex })

  abstract class TypingConstraint(val prereq: Boolean, val node: Any) { override def toString = getConstraintString(this) }
  case class Equality(a: TypingVariable, b: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)
  case class EqualityExceptFor(d: TypingVariable, a: TypingVariable, b: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)
  case class LessThan(a: TypingVariable, b: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)
  case class PrefixLt(main: TypingVariable, prefix: TypingVariable, suffix: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)
  case class SuffixEq(main: TypingVariable, prefix: TypingVariable, suffix: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)
  case class EqualityAeqBcatC(a: TypingVariable, b: TypingVariable, c: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)
  case class LengthEqualityAeqB(a: TypingVariable, b: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)
  case class CommonDenominator(a: TypingVariable, b: TypingVariable, c: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)
  case class EqualProduct(a: TypingVariable, b: TypingVariable, _prereq: Boolean, _node: Any) extends TypingConstraint(_prereq, _node)

  private def getConstraintString(constraint: TypingConstraint): String = {
    val body = constraint match {
      case eq: Equality => eq.a.toString + " = " + eq.b.toString
      case eq: EqualityExceptFor => eq.a.toString + "[i] = " + eq.b.toString + "[i] forall i != " + eq.d.toString
      case lt: LessThan => lt.a.toString + " < " + lt.b.toString
      case pl: PrefixLt => pl.main.toString + "(:length(" + pl.prefix.toString + ")) < " + pl.prefix.toString
      case se: SuffixEq => se.main.toString + "(length(" + se.prefix.toString + "):) = " + se.suffix.toString
      case eq: EqualityAeqBcatC => eq.a.toString + " = " + eq.b.toString + " ::: " + eq.c.toString
      case le: LengthEqualityAeqB => "length(" + le.a.toString + ") = length(" + le.b.toString + ")"
      case cd: CommonDenominator => cd.a.toString + " = common(" + cd.b.toString + ", " + cd.c.toString + ")"
      case ep: EqualProduct => "prod(" + ep.a.toString + ") = prod(" + ep.b.toString + ")"
      case _ => "unknown constraint"
    }

    if (constraint.prereq)
      "PRE:    " + String.format("%-30s", body) + "     from " + constraint.node
    else
      "POST:   " + String.format("%-30s", body) + "     from " + constraint.node
  }
}