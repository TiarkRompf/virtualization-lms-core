package scala.virtualization.lms
package epfl
package test7

import common._
import internal._
import original._
import original.Conversions._
import collection.mutable.HashSet


trait MDArrayTypingPrimitives {
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
  case class ReconstructValueFromShape(value: TypingVariable, shape: TypingVariable, _prereq: Boolean, _node:Any) extends TypingConstraint(_prereq, _node)
  case class EqualityAeqDimTimesValue(a: TypingVariable, dim: TypingVariable, value: TypingVariable, _prereq: Boolean, _node:Any) extends TypingConstraint(_prereq, _node)
  case class EqualityShAeqShBplusShCalongD(a: TypingVariable, b: TypingVariable, c: TypingVariable, d: TypingVariable, _prereq: Boolean, _node:Any) extends TypingConstraint(_prereq, _node)

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
      case rv: ReconstructValueFromShape => "reconstruct " + rv.value + " from shape " + rv.shape
      case eq: EqualityAeqDimTimesValue => eq.a.toString + " = " + eq.dim.toString + " x [" + eq.value.toString + "]"
      case eq: EqualityShAeqShBplusShCalongD => eq.a.toString + " = " + eq.b.toString + " <cat> " + eq.c.toString + " along dimension " + eq.d.toString
      case _ => "unknown constraint"
    }

    if (constraint.prereq)
      "PRE:    " + String.format("%-50s", body) + "     from " + constraint.node
    else
      "POST:   " + String.format("%-50s", body) + "     from " + constraint.node
  }

  def makeUnknowns(size: Int): Lst = {
    var list:List[TypingElement] = Nil
    List.range(0, size).map(i => list = getNewUnknown :: list)
    Lst(list)
  }

  def countUnknowns(l: Lst): Int = {
    var unks = 0
    l.list.map(elt => elt match { case v:Value => ; case _ => unks = unks + 1 })
    unks
  }

  abstract class Substitution(name: String) extends (TypingConstraint => TypingConstraint) {
    override def toString = name

    // we only have to override these functions to have a real substitution
    def updateVar(v: Var): TypingVariable
    def updateUnknown(uk: Unknown): TypingElement
    def updateLength(lt: LengthOf): TypingElement

    // the rest is already done :)
    def updateElement(elt: TypingElement): TypingElement = elt match {
      case value: Value => value
      case unknown: Unknown => updateUnknown(unknown)
      case length: LengthOf => updateLength(length)
    }
    def updateVariable(tv: TypingVariable): TypingVariable = tv match {
      case v: Var => updateVar(v)
      case l: Lst => Lst(l.list.map(elt => updateElement(elt)))
    }
    def apply(tcs: List[TypingConstraint]): List[TypingConstraint] = tcs.map(tc => apply(tc))
    def apply(tc: TypingConstraint): TypingConstraint = tc match {
      case eq: Equality => Equality(updateVariable(eq.a), updateVariable(eq.b), eq._prereq, eq._node)
      case ef: EqualityExceptFor => EqualityExceptFor(updateVariable(ef.d), updateVariable(ef.a), updateVariable(ef.b), ef._prereq, ef._node)
      case lt: LessThan => LessThan(updateVariable(lt.a), updateVariable(lt.b), lt._prereq, lt._node)
      case pl: PrefixLt => PrefixLt(updateVariable(pl.main), updateVariable(pl.prefix), updateVariable(pl.suffix), pl._prereq, pl._node)
      case se: SuffixEq => SuffixEq(updateVariable(se.main), updateVariable(se.prefix), updateVariable(se.suffix), se._prereq, se._node)
      case eq: EqualityAeqBcatC => EqualityAeqBcatC(updateVariable(eq.a), updateVariable(eq.b), updateVariable(eq.c), eq._prereq, eq._node)
      case le: LengthEqualityAeqB => LengthEqualityAeqB(updateVariable(le.a), updateVariable(le.b), le._prereq, le._node)
      case cd: CommonDenominator => CommonDenominator(updateVariable(cd.a), updateVariable(cd.b), updateVariable(cd.c), cd._prereq, cd._node)
      case ep: EqualProduct => EqualProduct(updateVariable(ep.a), updateVariable(ep.b), ep._prereq, ep._node)
      case rv: ReconstructValueFromShape => ReconstructValueFromShape(updateVariable(rv.value), updateVariable(rv.shape), rv._prereq, rv._node)
      case eq: EqualityShAeqShBplusShCalongD => EqualityShAeqShBplusShCalongD(updateVariable(eq.a), updateVariable(eq.b), updateVariable(eq.c), updateVariable(eq.d), eq._prereq, eq._node)
      case eq: EqualityAeqDimTimesValue => EqualityAeqDimTimesValue(updateVariable(eq.a), updateVariable(eq.dim), updateVariable(eq.value), eq._prereq, eq._node)
      // XXX: No _ case here, if the substitution doesn't know the constraint it better fail as soon as possible!
    }
  }

  /** substitute a variable by another variable */
  class SubstituteVarToVar(v1: Var, v2: Var) extends Substitution("Substitute " + v1.toString + " by " + v2.toString) {
    override def updateVar(v: Var): Var = if (v == v1) v2 else v
    override def updateUnknown(uk: Unknown): TypingElement = uk
    override def updateLength(lt: LengthOf): TypingElement = LengthOf(updateVar(lt.v))
  }

  /** substitute a variable by a list */
  class SubstituteVarToLst(vv: Var, l: Lst) extends Substitution("Substitute " + vv.toString + " by " + l.toString) {
    override def updateVar(v: Var): TypingVariable = if (v == vv) l else v
    override def updateUnknown(uk: Unknown): TypingElement = uk
    override def updateLength(lt: LengthOf): TypingElement = if (lt.v == vv) Value(l.list.length) else lt
  }

  /** substitute a unknown value by something else */
  class SubstituteUnknown(u: Unknown, elt: TypingElement) extends Substitution("Substitute unknown " + u.toString + " by " + elt.toString) {
    override def updateVar(v: Var): TypingVariable = v
    override def updateUnknown(uk: Unknown): TypingElement = if (uk == u) elt else uk
    override def updateLength(lt: LengthOf): TypingElement = lt
  }

  class SubstitutionList(val substList: List[Substitution]) extends Substitution("SubstitutionList:\n" + substList.mkString(" ", "\n ", "\n")) {

    override def apply(tc: TypingConstraint): TypingConstraint = {
     var res = tc
     for(subst <- substList)
       res = subst.apply(res)
     res
    }

    def apply(tv: TypingVariable): TypingVariable = {
      var res = tv
      for(subst <- substList)
        res = subst.updateVariable(res)
      res
    }

    override def updateVar(v: Var): TypingVariable = v
    override def updateUnknown(uk: Unknown): TypingElement = uk
    override def updateLength(lt: LengthOf): TypingElement = lt

    def length = substList.length
  }

  def getLength(v: TypingVariable): Option[Int] = v match {
    case l: Lst => Some(l.list.length)
    case _ => None
  }

  def getValue(v: TypingVariable): Option[List[Int]] = v match {
    case l: Lst if (countUnknowns(l) == 0) => Some(l.list.map(elt => elt.asInstanceOf[Value].n))
    case _ => None
  }
}