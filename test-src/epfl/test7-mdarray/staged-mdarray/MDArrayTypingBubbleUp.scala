package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

import java.io.{Writer, PrintWriter}
import collection.immutable.HashMap


trait MDArrayTypingBubbleUp extends MDArrayTypingWithScope {

  import IR.{Sym, Exp, Def, TP, findDefinition}
  var remainingConstraints: List[TypingConstraint] = Nil
  var scopeSubsts: SubstitutionList = new SubstitutionList(Nil)

  var runtimeChecks: Map[Sym[_], List[TypingConstraint]] = Map.empty

  def fillInRuntimeChecks(sym: Sym[_]): Unit = findDefinition(sym) match {
    case None =>
      runtimeChecks = runtimeChecks + (sym -> Nil)
    case Some(tp) =>
      val rhs: Def[_] = tp.rhs
      val argSyms = syms(rhs)
      var checks: List[TypingConstraint] = Nil

      // add the runtime checks for prerequisites
      checks = getConstraints(sym, rhs).filter(_.prereq)

      // add the runtime checks for bubbling up
      for (argSym <- argSyms) {
        if (scopeSubsts(ShapeVar(argSym)) != currentScope.fullSubsts(ShapeVar(argSym)))
          checks = Equality(scopeSubsts(ShapeVar(argSym)), currentScope.fullSubsts(ShapeVar(argSym)), postReq, "Bubble up shape for " + argSym.toString + " <- " + rhs.toString) :: checks

        if (scopeSubsts(ValueVar(argSym)) != currentScope.fullSubsts(ValueVar(argSym)))
          checks = Equality(scopeSubsts(ValueVar(argSym)), currentScope.fullSubsts(ValueVar(argSym)), postReq, "Bubble up value for " + argSym.toString + " <- " + rhs.toString) :: checks
      }

      runtimeChecks = runtimeChecks + (sym -> checks)
      for (argSym <- argSyms)
        fillInRuntimeChecks(argSym)
  }

  override def doTyping(result: Exp[_], debug: Boolean = false): Unit = {
    // Let the bottom layers do their work
    super.doTyping(result, debug)

    // Create runtimeChecks
    fillInRuntimeChecks(result.asInstanceOf[Sym[_]])
  }

  // Gets the exact runtime checks
  def getRuntimeChecks(sym: Sym[_]) = runtimeChecks(sym)

  def emitRuntimeChecks(sym: Sym[_])(implicit stream: PrintWriter) = {
    for (check <- runtimeChecks(sym)) {
      unifyConstraint(scopeSubsts(check)) match {
        case (true, Nil) =>
          // Skip this case, the constraint is already assumed
        case (true, substs) =>
          scopeSubsts = new SubstitutionList(scopeSubsts.substList ::: substs)
          print("// TODO: Generate () check for " + check.toString)
        case (false, _) =>
          print("// TODO: Generate () check for " + check.toString)
      }
    }
  }

  protected def eliminateConstraints(): Unit = {

    val (substitutions, constraints) = computeSubstitutions(remainingConstraints, false)
    remainingConstraints = constraints
    scopeSubsts = new SubstitutionList(scopeSubsts.substList ::: substitutions.substList)
  }


  def withinDifferentScopes(ifSym: Sym[_], pairs: List[Pair[Sym[_], ()=>Unit]]): Unit = {

    var scopes: List[TypingScope] = Nil
    var parentScopeSubsts = scopeSubsts
    var parentRemainingConstraints = remainingConstraints
    var scopeSubstsList: List[SubstitutionList] = Nil

    // prepare scopes for each action
    for (pair <- pairs) {
      val (sym, action) = pair
      val oldScope = currentScope

      if (HAVE_SCOPES) {
        // find the new scope
        val newScopes = currentScope.children.filter(scope => (scope.sym == sym) && (scope.ifSym == ifSym))
        if (newScopes.length < 1) sys.error("There is no scope for the sym. CurrentSym: " + currentScope.sym + " NewSym: " + sym + " Available: " + currentScope.children.map(_.sym).mkString(" "))
        val newScope = newScopes.head

        scopes = newScope :: scopes
        // set everything up for the new scope
        currentScope = newScope
      }

      // emit the code
      action()

      if (HAVE_SCOPES) {
        // recover the scope state
        scopeSubstsList = scopeSubsts :: scopeSubstsList
        scopeSubsts = parentScopeSubsts
        remainingConstraints = parentRemainingConstraints
        currentScope = oldScope
      }
    }

    // if we have scopes, we have to reconcile the scope substitutions and join them into the parent scope
    // if we don't have scoeps, the constraints have been added and eliminated along the way :)
    if (HAVE_SCOPES) {
      // reconcile scopes
      remainingConstraints = remainingConstraints ::: scopeSubsts(reconcile(scopeSubstsList))
      eliminateConstraints()
    }
  }
}