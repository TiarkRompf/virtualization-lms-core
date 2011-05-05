package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

import java.io.{Writer, PrintWriter}
import collection.immutable.HashMap

trait MDArrayTypingWithScope extends MDArrayTypingConstraints {

  final val HAVE_SCOPES: Boolean = true

  import IR.{Sym, Exp, Def, TP}
  protected var currentScope: TypingScope = null

  protected def addConstraints(tl: List[TypingConstraint]): Unit = {
    currentScope.constraints = tl ::: currentScope.constraints
    currentScope.affectedSymbols = currentScope.affectedSymbols ++ getAffectedSymbols(tl)
  }

  protected def addSymbol(sym: Sym[_]): Unit =
    currentScope.constrainedSymbols = currentScope.constrainedSymbols + sym

  protected def createSubScope(ifSym:Sym[_], sym: Sym[_])(action: => Unit): Unit = {

    if (HAVE_SCOPES) {
      // 1. Create new scope
      val oldScope = currentScope
      currentScope = TypingScope(ifSym, sym, oldScope)
      if (oldScope ne null)
        oldScope.children = currentScope :: oldScope.children
      // 2. Perform the action
      action
      // 3. Revert to old scope
      currentScope = oldScope
    }
    else
      // Just perform the action
      action
  }

  case class TypingScope(ifSym: Sym[_], sym: Sym[_], parent: TypingScope) {

    var children: List[TypingScope] = Nil
    var constrainedSymbols: Set[Sym[_]] = Set.empty
    var affectedSymbols: Set[Sym[_]] = Set.empty

    // Different types of constraints...
    var constraints: List[TypingConstraint] = Nil
    var bubbleUpConstraints: List[TypingConstraint] = Nil
    var partialReconciliationConstraints: List[TypingConstraint] = Nil

    var fullSubsts: SubstitutionList = new SubstitutionList(Nil)
    var partSubsts: SubstitutionList = new SubstitutionList(Nil)

    def getLengthFull(v: TypingVariable) = getLength(fullSubsts(v))
    def getValueFull(v: TypingVariable) = getValue(fullSubsts(v))

    override def toString = "Scope (" + sym.toString + ")"
  }

  /**
   * Gather the constraints in a optimizer & code generator-friendly way
   */
  def doTyping(result: Exp[_], debug: Boolean = false): Unit = {

    // 1. Gather constraints
    currentScope = TypingScope(IR.fresh[Any], result.asInstanceOf[Sym[_]], null)
    emitBlock(result)(new PrintWriter(System.err)) // shouldn't output anything

    // 2. Get the substitution list & the pre-requirement list
    solveScope(currentScope, Nil, true)
  }


  def getTypingString(sym: Sym[_]): String = {

    val shapeVar: TypingVariable = ShapeVar(sym)
    val valueVar: TypingVariable = ValueVar(sym)
    val shapeVarValue: TypingVariable = currentScope.fullSubsts(shapeVar)
    val valueVarValue: TypingVariable = currentScope.fullSubsts(valueVar)

    // need to limit the value size so we don't overcrowd the graph
    var valueString = valueVarValue.toString
    (valueString.length > 40) match {
      case true =>
        valueString = valueString.substring(0, 18) + " ... " + valueString.substring(valueString.length - 18)
      case _ =>
        ;
    }

    (valueVar != valueVarValue, shapeVar != shapeVarValue) match {
      case (true, true) => valueVar.toString + "=" + valueString + " and " + shapeVar.toString + "=" + shapeVarValue.toString
      case (true, false) => valueVar.toString + "=" + valueString
      case (false, true) => shapeVar.toString + "=" + shapeVarValue.toString
      case (false, false) => "?!?"
    }
  }


  protected def solveScope(current: TypingScope, parentConstraints: List[TypingConstraint], debug: Boolean): Unit = {

    // Propagate the constraints
    val currentConstraints = current.constraints
    current.constraints = current.constraints ::: parentConstraints

    // Solve the inner children
    for (child <- current.children)
      solveScope(child, current.constraints, true)

    if (debug) {
      println("Solving FULL scope for " + current.sym.toString)
      //println("Incoming constraints:\n" + parentConstraints.mkString("\t", "\n\t", ""))
      //println("Scope additional constraints:\n" + currentConstraints.mkString("\t", "\n\t", ""))
    }

    // Compute the reconciling - only for full substitutions
    val fullReconcileConstraints: List[TypingConstraint] =
      reconcile(current.children.map(_.fullSubsts), "Reconciliation of " + current.children.mkString(" and "))

    // Compute the substitutions
    current.fullSubsts = computeSubstitutions(fullReconcileConstraints ::: current.constraints, false)._1
  }

  protected def reconcile(substitutions: List[SubstitutionList], name: String = "Reconciliation"): List[TypingConstraint] = substitutions.length match {
    case 0 => Nil
    case _ =>
      for (subst <- substitutions)
        println("S: " + subst.substList.mkString(", "))

      val childrenSymbols = substitutions.map(getSymbols(_).toSet)
      var reconcileSymbols = Set.empty[Sym[_]]

      for (childSyms <- childrenSymbols)
        println("R: " + childSyms.mkString(", "))

      if (childrenSymbols.length > 0)
        reconcileSymbols = childrenSymbols.foldLeft(childrenSymbols.head)((a, b) => b intersect a)

    println("Reconciling symbols: " + reconcileSymbols.mkString(", "))

    reconcileSymbols.toList.flatMap((sym: Sym[_]) => reconcileSymbol(ShapeVar(sym), substitutions.map(subst => subst(ShapeVar(sym))), name)) :::
    reconcileSymbols.toList.flatMap((sym: Sym[_]) => reconcileSymbol(ValueVar(sym), substitutions.map(subst => subst(ValueVar(sym))), name))
  }


  protected def reconcileSymbol(v: TypingVariable, values: List[TypingVariable], name: String = "Reconciliation"): List[TypingConstraint] = {

    assert(values != 0) // should be already satisfied

    // Reconciliation strategy:
    //  - first:  check if everyone agrees on the length
    //  - second: check each position and see if everyone agrees on the value
    val lenghts = values.map(getLength(_))
    val length: Int = lenghts.foldLeft(-2)(
      (length, newLen) => newLen match {
        case Some(l) if ((length == -2) || (l == length)) => l
        case _ => -1
      }
    )

    if (length != -1) {
      var elements: List[TypingElement] = Nil

      for (n <- List.range(0, length)) {
        var element: TypingElement = null
        for (value <- values) {
          val newElt: TypingElement = value.asInstanceOf[Lst].list(n)
          element = reconcileElement(element, newElt)
        }
        elements = element :: elements
      }
      Equality(v, Lst(elements.reverse), postReq, name)::Nil
    } else
      Nil
  }

  private def reconcileElement(t1: TypingElement, t2: TypingElement) = (t1, t2) match {
    case (null, _) => t2
    case (Value(v1), Value(v2)) if (v1 == v2) => Value(v1)
    case (Unknown(u1), Unknown(u2)) if (u1 == u2) => Unknown(u1)
    case (LengthOf(v1), LengthOf(v2)) if (v1 == v2) => LengthOf(v1)
    case _ => getNewUnknown
  }

  def getShapeLength(sym: Exp[_]): Option[Int] = currentScope.getLengthFull(ShapeVar(sym.asInstanceOf[Sym[_]]))
  def getValueLength(sym: Exp[_]): Option[Int] = currentScope.getLengthFull(ValueVar(sym.asInstanceOf[Sym[_]]))
  def getShapeValue(sym: Exp[_]): Option[List[Int]] = currentScope.getValueFull(ShapeVar(sym.asInstanceOf[Sym[_]]))
  def getValueValue(sym: Exp[_]): Option[List[Int]] = currentScope.getValueFull(ValueVar(sym.asInstanceOf[Sym[_]]))

  protected def getAffectedSymbols(a: Any): List[Sym[_]] = a match {
    case ShapeVar(s) => s::Nil
    case ValueVar(s) => s::Nil
    case p: Product => p.productIterator.toList.flatMap(getAffectedSymbols(_))
    case _ => Nil
  }
}