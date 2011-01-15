package scala.virtualization.lms
package epfl
package test7

import common._
import internal._
import original._
import original.Conversions._
import collection.mutable.{Queue, HashSet}

trait MDArrayTypingUnifier extends MDArrayTypingConstraints {

  def getTypingString(index: Int, subst: SubstitutionList): String = {
    val shapeVar = ShapeVar(index)
    val valueVar = ValueVar(index)
    val shapeVarValue: TypingVariable = subst(shapeVar)
    val valueVarValue: TypingVariable = subst(valueVar)

    // need to limit the value size so we don't overcrowd the graph
    var valueString = valueVarValue.toString
    if (valueString.length > 40)
      valueString = valueString.substring(0, 18) + " ... " + valueString.substring(valueString.length - 18)

    (valueVar != valueVarValue, shapeVar != shapeVarValue) match {
      case (true, true) => valueVar.toString + "=" + valueString + " and " + shapeVar.toString + "=" + shapeVarValue.toString
      case (true, false) => valueVar.toString + "=" + valueString
      case (false, true) => shapeVar.toString + "=" + shapeVarValue.toString
      case (false, false) => "?!?"
    }
  }

  def obtainSubstitutions(result: Any, debug: Boolean = false): (SubstitutionList, SubstitutionList) = {
    // 1. Gather constraints
    val constraints: List[TypingConstraint] = createTypingConstraints(result)
    // 2. Get "pure" substitutions
    val pureSubst: List[Substitution] = computeSubstitutions(constraints.filter(const => !const.prereq), debug)
    // 3. Get "full" substitutions
    val fullSubst: List[Substitution] = computeSubstitutions(constraints, debug)

    (new SubstitutionList(pureSubst), new SubstitutionList(fullSubst))
  }

  def computeSubstitutions(inConstraints: List[TypingConstraint], debug: Boolean): List[Substitution] = {

    var finished: Boolean = false
    var substitutions: List[Substitution] = Nil
    var constraints: List[TypingConstraint] = inConstraints

    if (debug)
      println("START computeSubstitutions\n")

    while (!finished) {
      var index: Int = -1
      var indexSuccessful: Int = -1
      var substSuccessful: List[Substitution] = Nil

      if (debug) {
        println()
        for (constraint <- constraints)
          println(" CONSTR: " + constraint)
        println()
      }

      for (constraint <- constraints) {
        index = index + 1
        if (indexSuccessful == -1) {
          val pair = unifyConstraint(constraint)
          if (pair._1) { // it was successful
            if (debug) {
              println(" SOLVED: ")
              println(" CONSTR: " + constraint)
              println(" SUBSTS: " + pair._2)
            }
            indexSuccessful = index
            substSuccessful = pair._2
            substitutions = substitutions ::: substSuccessful
          }
        }
      }

      if (indexSuccessful != -1) {
        // 1. eliminate the constraint
        constraints = constraints.take(indexSuccessful) ::: constraints.drop(indexSuccessful + 1)
        // 2. apply the substitions on the rest of the constraints
        constraints = new SubstitutionList(substSuccessful)(constraints)
      }
      else
        finished = true
    }

    if (debug) {
      println()
      println(" FINAL SOLUTION:")
      for (substitution <- substitutions)
      println(" SUBST: " + substitution)
      println("END computeSubstitutions")
    }

    substitutions
  }

  /*
    TODO: Understand why I get the following match warning (only here...)
    [warn] .../virtualization-lms-core/test-src/epfl/test7-mdarray/staged-mdarray/MDArrayTypingUnifier.scala:102: match is not exhaustive!
    [warn] missing combination            Nil             *             *
    [warn]   def unifyConstraint(tc: TypingConstraint): (Boolean, List[Substitution]) = tc match {
    [warn]       ^
   */
  def unifyConstraint(tc: TypingConstraint): (Boolean, List[Substitution]) = tc match {
    case eq: Equality =>
      (eq.a, eq.b) match {
        case (v1: Var, v2: Var) => (true, if (v1 != v2) (new SubstituteVarToVar(v1, v2))::Nil else Nil)
        case (v: Var, l: Lst) => (true, (new SubstituteVarToLst(v, l))::Nil)
        case (l: Lst, v: Var) => (true, (new SubstituteVarToLst(v, l))::Nil)
        case (l1: Lst, l2: Lst) => unifyLists(l1, l2)
      }
    case ep: EqualProduct =>
      (ep.a, ep.b) match {
        case (l1: Lst, l2: Lst) => {
          def getProduct(lst: Lst): Int = lst.list.foldLeft(1)((oldVal, value) => value match { case v: Value => oldVal * v.n; case _ => oldVal })
          def getUnknowns(lst: Lst): List[TypingElement] = lst.list.filter(value => value match { case v: Value => false; case _ => true })
          val p1 = getProduct(l1)
          val p2 = getProduct(l2)
          (countUnknowns(l1) + countUnknowns(l2)) match {
            case 0 =>
              if (p1 != p2)
                throw new Exception("unification: The product equality cannot be unified due to different products: " + l1.toString + " and " + l2.toString)
              (true, Nil)
            case 1 =>
              if (countUnknowns(l1) == 1) {
                val unknownValue: Int = p2/p1
                if (p1 * unknownValue != p2)
                  throw new Exception("unification: The product equality cannot be unified due to rounding error: " + l1.toString + " and " + l2.toString)
                unifyLists(Lst(getUnknowns(l1)), Lst(Value(unknownValue)::Nil))
              } else {
                val unknownValue: Int = p1/p2
                if (p2 * unknownValue != p1)
                  throw new Exception("unification: The product equality cannot be unified due to rounding error: " + l1.toString + " and " + l2.toString)
                unifyLists(Lst(getUnknowns(l2)), Lst(Value(unknownValue)::Nil))
              }
            case _ =>
              (false, Nil)
          }
        }
        case _ =>
          (false, Nil)
      }
    case pl: PrefixLt =>
      (pl.main, pl.prefix, pl.suffix) match {
        case (main: Lst, prefix: Lst, suffix: Lst) =>
          val mainPref = Lst(main.list.take(prefix.list.length))
          if (countUnknowns(mainPref) + countUnknowns(prefix) == 0) {
            // We can do a check and eliminate the prefix :)
            val greaterOrEqual = List.range(0, prefix.list.length).filter(i => prefix.list(i).asInstanceOf[Value].n >= mainPref.list(i).asInstanceOf[Value].n)
            if (greaterOrEqual.length > 0)
              throw new Exception("unification: Prefix lower than assertion proved false: " + mainPref.toString + " < " + prefix.toString + " main=" + main + ", prefix=" + prefix + ", suffix=" + suffix)
            else
              (true, Nil)
          } else
            (false, Nil)
        case _ =>
          (false, Nil)
      }
    case sf: SuffixEq =>
      (sf.main, sf.prefix, sf.suffix) match {
        case (main: Var, prefix: Lst, suffix: Lst) =>
          (true, new SubstituteVarToLst(main, Lst(makeUnknowns(prefix.list.length).list ::: suffix.list))::Nil)
        case (main: Lst, prefix: Var, suffix: Lst) =>
          if (main.list.length - suffix.list.length < 0)
            throw new Exception("unification: Suffix equal assertion with incorrect sizes: main=" + main.toString + " suffix=" + suffix.toString)
          val mainSuff = Lst(main.list.drop(main.list.length - suffix.list.length))
          val mainSuffUnif = unifyLists(mainSuff, suffix)
          (mainSuffUnif._1, (new SubstituteVarToLst(prefix, makeUnknowns(main.list.length - suffix.list.length))) :: mainSuffUnif._2)
        case (main: Lst, prefix: Lst, suffix: Var) =>
          if (main.list.length - prefix.list.length < 0)
            throw new Exception("unification: Suffix equal assertion with incorrect sizes: main=" + main.toString + " prefix=" + prefix.toString)
          (true, new SubstituteVarToLst(suffix, Lst(main.list.drop(prefix.list.length)))::Nil)
        case (main: Lst, prefix: Lst, suffix: Lst) =>
          if (main.list.length != prefix.list.length + suffix.list.length)
            throw new Exception("unification: Suffix equal assertion with incorrect sizes: main=" + main.toString + " prefix=" + prefix.toString + " suffix=" + suffix.toString)
          unifyLists(Lst(main.list.drop(prefix.list.length)), suffix)
        case _ =>
          (false, Nil)
      }
    case le: LengthEqualityAeqB =>
      (le.a, le.b) match {
        case (l1: Lst, l2: Lst) =>
          if (l1.list.length != l2.list.length)
            throw new Exception("unification: Length equality assertion not satisfiable: " + l1.toString + " " + l2.toString)
          (true, Nil)
        case (l: Lst, v: Var) =>
          (true, new SubstituteVarToLst(v, makeUnknowns(l.list.length))::Nil)
        case (v: Var, l: Lst) =>
          (true, new SubstituteVarToLst(v, makeUnknowns(l.list.length))::Nil)
        case _ =>
          (false, Nil)
      }
    case lt: LessThan =>
      (lt.a, lt.b) match {
        case (l1: Lst, l2:Lst) if (countUnknowns(l1) + countUnknowns(l2) == 0) =>
          if (l1.list.length != l2.list.length)
            throw new Exception("unification: Less than cannot be applied to lists of different length: " + l2.toString + " and " + l1.toString)
          val il1 = l1.list.map(i => i.asInstanceOf[Value].n)
          val il2 = l2.list.map(i => i.asInstanceOf[Value].n)
          for (i <- List.range(0, il1.length))
            if (il1(i) >= il2(i))
              throw new Exception("unification: Less than not satisfied: " + l2.toString + " and " + l1.toString)
          (true, Nil)
        case _ =>
          (false, Nil)
      }
    case eef: EqualityExceptFor =>
      def eliminatePosition(d: Lst, l: Lst, eliminate: Boolean, a: TypingVariable, b: TypingVariable) = {
        if (d.list.length != 1)
          throw new Exception("unification: Equality except for cannot be applied to d of length greater than 1: d=" + a.toString + " a=" + a.toString + " b=" + b.toString)
        if (countUnknowns(d) != 0) // this should not happen
          throw new Exception("unification: Equality except for internal error: d=" + a.toString + " a=" + a.toString + " b=" + b.toString)
        if (d.list(0).asInstanceOf[Value].n >= l.list.length)
          throw new Exception("unification: Equality except for cannot be applied to d of value greater than list length: d=" + a.toString + " a=" + a.toString + " b=" + b.toString)
        else {
          val p = d.list(0).asInstanceOf[Value].n
          if (eliminate)
            Lst(l.list.take(p-1) ::: l.list.drop(p))
          else
            Lst(l.list.take(p-1) ::: getNewUnknown :: l.list.drop(p))
        }
      }
      (eef.d, eef.a, eef.d) match {
        case (d: Lst, a: Lst, b: Var) if (countUnknowns(d) == 0) =>
          val bb = eliminatePosition(d, a, false, a, b)
          (true, new SubstituteVarToLst(b, bb)::Nil)
        case (d: Lst, a: Var, b: Lst) if (countUnknowns(d) == 0) =>
          val aa = eliminatePosition(d, b, false, a, b)
          (true, new SubstituteVarToLst(a, aa)::Nil)
        case (d: Lst, a: Lst, b: Lst) if (countUnknowns(d) == 0) =>
          val aa = eliminatePosition(d, a, true, a, b)
          val bb = eliminatePosition(d, b, true, a, b)
          unifyLists(aa, bb)
        case _ =>
          (false, Nil)
      }
    case eabc: EqualityAeqBcatC =>
      (eabc.a, eabc.b, eabc.c) match {
        case (a: Var, b: Lst, c: Lst) =>
          (true, new SubstituteVarToLst(a, Lst(b.list ::: c.list))::Nil)
        case (a: Lst, b: Var, c: Lst) =>
          if (a.list.length < c.list.length)
            throw new Exception("unification: Equality A = B ::: C cannot be applied to: a=" + a.toString + " b=" + b.toString + " c=" + c.toString)
          (true, new SubstituteVarToLst(b, Lst(a.list.take(a.list.length-c.list.length)))::Nil)
        case (a: Lst, b: Lst, c: Var) =>
          if (a.list.length < b.list.length)
            throw new Exception("unification: Equality A = B ::: C cannot be applied to: a=" + a.toString + " b=" + b.toString + " c=" + c.toString)
          (true, new SubstituteVarToLst(c, Lst(a.list.drop(b.list.length)))::Nil)
        case (a: Lst, b: Lst, c: Lst) =>
          if (a.list.length != b.list.length + c.list.length)
            throw new Exception("unification: Equality A = B ::: C cannot be applied to: a=" + a.toString + " b=" + b.toString + " c=" + c.toString)
          unifyLists(a, Lst(b.list ::: c.list))
        case _ =>
          (false, Nil)
      }
    case cd: CommonDenominator =>
      (cd.a, cd.b, cd.c) match {
        case (a: Var, b: Lst, c: Lst) =>
          if (b.list.length == c.list.length) {
            if (b==c)
              (true, new SubstituteVarToLst(a, b)::Nil)
            else
              (true, new SubstituteVarToLst(a, makeUnknowns(b.list.length))::Nil)
          } else
            (false, Nil)
        case _ =>
          (false, Nil)
      }
    case rv: ReconstructValueFromShape =>
      (rv.shape, rv.value) match {
        case (s: Lst, v:Var) if (countUnknowns(s) == 0) =>
          (true, new SubstituteVarToLst(v, makeUnknowns(s.list.foldLeft(1)((v, elt) => v * elt.asInstanceOf[Value].n)))::Nil)
        case _ =>
          (false, Nil)
      }
    case eq: EqualityAeqDimTimesValue =>
      (eq.dim, eq.value) match {
        case (dim: Lst, value: Lst) if (countUnknowns(dim) == 0) =>
          if ((dim.list.length != 1) || (value.list.length != 1))
            throw new Exception("unification: a=values(dim, value) with incorrect dim/value: a=" + eq.a.toString + " dim=" + dim.toString + " value=" + value.toString)
          val vector: Lst = Lst(List.range(0, dim.list.head.asInstanceOf[Value].n).map(i => value.list.head))
          eq.a match {
            case a: Var =>
              (true, new SubstituteVarToLst(a, vector)::Nil)
            case a: Lst =>
              unifyLists(a, vector)
          }
        case _ =>
          (false, Nil)
      }
    case eq: EqualityShAeqShBplusShCalongD =>
      (eq.b, eq.c, eq.d) match {
        case (b: Lst, c: Lst, d: Lst) if (countUnknowns(d) == 0) =>
          if ((d.list.length != 1) || (b.list.length != c.list.length))
            throw new Exception("unification: a = b <cat> c <along> d failed: a=" + eq.a.toString + " b=" + b.toString + " c=" + c.toString + " d=" + d.toString)
          val dval = d.list.head.asInstanceOf[Value].n
          if ((b.list(dval).isInstanceOf[Value]) && (c.list(dval).isInstanceOf[Value])) {
            val sum = b.list(dval).asInstanceOf[Value].n + c.list(dval).asInstanceOf[Value].n
            val vector = Lst(List.range(0, b.list.length).map(i => if (i==dval) Value(sum) else b.list(i)))
            // prepare the substitution
            eq.a match {
              case a: Var =>
                (true, new SubstituteVarToLst(a, vector)::Nil)
              case a: Lst =>
                unifyLists(a, vector)
            }
          } else
            (false, Nil) // sorry, can't unify that, I'm sound but not complete!
      }
    //XXX: Let the match fail fast if we forgot a condition
    //case _ => (false, Nil)
  }


  def unifyLists(l1: Lst, l2: Lst): (Boolean, List[Substitution]) = {
    var success: Boolean = true
    var substs: List[Substitution] = Nil

    if (l1.list.length != l2.list.length)
      throw new Exception("unification: The following two lists cannot be unified due to different lengths: " + l1.toString + " and " + l2.toString)

    for (i <- Stream.range(0, l1.list.length))
      (l1.list(i), l2.list(i)) match {
        case (u: Unknown, v: Value) => substs = new SubstituteUnknown(u, v) :: substs
        case (u: Unknown, l: LengthOf) => substs = new SubstituteUnknown(u, l) :: substs
        case (u1: Unknown, u2: Unknown) => substs = new SubstituteUnknown(u1, u2) :: substs
        case (v: Value, u: Unknown) => substs = new SubstituteUnknown(u, v) :: substs
        case (v: Value, l: LengthOf) => substs = new SubstituteVarToLst(l.v, makeUnknowns(v.n)) :: substs
        case (v1: Value, v2: Value) =>
          if (v1 != v2) throw new Exception("unification: The following two lists cannot be unified due to different values at position " + i + ": " + l1.toString + " and " + l2.toString)
        case (l: LengthOf, v: Value) => substs = new SubstituteVarToLst(l.v, makeUnknowns(v.n)) :: substs
        case (l: LengthOf, u: Unknown) => substs = new SubstituteUnknown(u, l) :: substs
        case (l1: LengthOf, l2: LengthOf) => if (l1.v != l2.v) success = false // we don't have enough info to do this substitution
      }

    (success, substs.reverse)
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
}