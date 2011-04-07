package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

import java.io.{Writer, PrintWriter}
import collection.immutable.HashMap


trait MDArrayTypingBubbleUp extends MDArrayTypingWithScope {

  import IR.{Sym, Def}
  var remainingConstraints: List[TypingConstraint] = Nil
  var scopeSubsts: SubstitutionList = new SubstitutionList(Nil)

  // Generate only bubble up constraints, for the runtime constraints we need to
  def getBubbleUpConstraints(sym: Sym[_], rhs: Def[_]): List[TypingConstraint] = {

    var nodeConstraints: List[TypingConstraint] = Nil

    // 1. ensure bubbling up
    for (sym <- syms(rhs)) {
      if (scopeSubsts(ShapeVar(sym)) != currentScope.fullSubsts(ShapeVar(sym))) {
        val shapeConstraint = Equality(scopeSubsts(ShapeVar(sym)), currentScope.fullSubsts(ShapeVar(sym)), postReq, "Bubble up for " + sym.toString + " <- " + rhs.toString)
        nodeConstraints = shapeConstraint::nodeConstraints
        assumeConstraint(shapeConstraint)
      }

      if (scopeSubsts(ValueVar(sym)) != currentScope.fullSubsts(ValueVar(sym))) {
        val valueConstraint = Equality(scopeSubsts(ValueVar(sym)), currentScope.fullSubsts(ValueVar(sym)), postReq, "Bubble up for " + sym.toString + " <- " + rhs.toString)
        nodeConstraints = valueConstraint::nodeConstraints
        assumeConstraint(valueConstraint)
      }
    }

    // 2. assume the postconditions
    val constraints = getConstraints(sym, rhs).filterNot(_.prereq)
    for (constraint <- constraints)
      assumeConstraint(constraint)

    nodeConstraints
  }

  // Generate/check runtime prereqs
  def getRuntimeChecks(sym: Sym[_], rhs: Def[_]): List[TypingConstraint] =
    Nil


  protected def assumeConstraint(constraint: TypingConstraint): Unit = {

    remainingConstraints = scopeSubsts(constraint) :: remainingConstraints
    eliminateConstraints()
  }


  protected def eliminateConstraints(): Unit = {

    val (substitutions, constraints) = computeSubstitutions(remainingConstraints, false)
    remainingConstraints = constraints
    scopeSubsts = new SubstitutionList(scopeSubsts.substList ::: substitutions.substList)
  }


  def withinDifferentScopes(pairs: List[Pair[Sym[_], ()=>Unit]]): Unit = {

    var scopes: List[TypingScope] = Nil
    var parentScopeSubsts = scopeSubsts
    var parentRemainingConstraints = remainingConstraints
    var scopeSubstsList: List[SubstitutionList] = Nil

    // prepare scopes for each action
    for (pair <- pairs) {
      val (sym, action) = pair
      val oldScope = currentScope
      val newScopes = currentScope.children.filter(_.sym == sym)
      // TODO: Prove that if there are multiple if-s that return the same value the scoping will be the same
      // example   if (all(A)) 0 else 1; if (all(B)) 1 else 2; // here 1 will add two scopes with the same sym -- is this the same?!?
      // TODO: Test if (all(A)) 0 else <complex fold of genarray>; if (all(B)) 1 else <same complex fold> generates the complex expression twice
      if (newScopes.length < 1) sys.error("There is no scope for the sym. CurrentSym: " + currentScope.sym + " NewSym: " + sym + " Available: " + currentScope.children.map(_.sym).mkString(" "))
      val newScope = newScopes.head

      scopes = newScope :: scopes
      // set everything up for the new scope
      currentScope = newScope

      // emit the code
      action()

      // recover the scope state
      scopeSubstsList = scopeSubsts :: scopeSubstsList
      scopeSubsts = parentScopeSubsts
      remainingConstraints = parentRemainingConstraints
      currentScope = oldScope
    }

    // reconcile scopes
    remainingConstraints = remainingConstraints ::: scopeSubsts(reconcile(scopeSubstsList))
    eliminateConstraints()
  }
}