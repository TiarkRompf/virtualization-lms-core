package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

import java.io.{Writer, PrintWriter}
import collection.immutable.HashMap


trait MDArrayTypingWithScope extends MDArrayTypingConstraints {

  import IR.{Sym, Exp, Def, TP}
  protected var currentScope: TypingScope = null


  protected def addConstraints(tl: List[TypingConstraint]): Unit = {
    currentScope.constraints = tl ::: currentScope.constraints
    currentScope.affectedSymbols = currentScope.affectedSymbols ++ getAffectedSymbols(tl)
  }

  protected def addSymbol(sym: Sym[_]): Unit =
    currentScope.constrainedSymbols = currentScope.constrainedSymbols + sym

  protected def createSubScope(sym: Sym[_])(action: => Unit): Unit = {
    // 1. Create new scope
    val oldScope = currentScope
    currentScope = TypingScope(sym, oldScope)
    if (oldScope ne null)
      oldScope.children = currentScope :: oldScope.children
    // 2. Perform the action
    action
    // 3. Revert to old scope
    currentScope = oldScope
  }

  case class TypingScope(sym: Sym[_],
                       parent: TypingScope,
                       var children: List[TypingScope] = Nil,
                       var constraints: List[TypingConstraint] = Nil,
                       var constrainedSymbols: Set[Sym[_]] = Set.empty,
                       var affectedSymbols: Set[Sym[_]] = Set.empty) {
    var fullSubsts: SubstitutionList = new SubstitutionList(Nil)
    var partSubsts: SubstitutionList = new SubstitutionList(Nil)

    def getLengthFull(v: TypingVariable) = getLength(fullSubsts(v))
    def getLengthPart(v: TypingVariable) = getLength(partSubsts(v))
    def getValueFull(v: TypingVariable) = getValue(fullSubsts(v))
    def getValuePart(v: TypingVariable) = getValue(partSubsts(v))

    override def toString = "Scope (" + sym.toString + ")"
  }


  /**
   * Gather the constraints in a optimizer & code generator-friendly way
   */
  def doTyping(result: Exp[_], debug: Boolean = false): Unit = {

    // 1. Gather constraints
    currentScope = TypingScope(result.asInstanceOf[Sym[_]], null)
    emitBlock(result)(new PrintWriter(System.err)) // shouldn't output anything

    // 2. Get the substitution list & the pre-requirement list
    solveScope(currentScope, Nil, true)

    // 3. Runtime check map
    // empty for now, we'll add runtime checks as AST nodes :)
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


  def solveScope(current: TypingScope, constraints: List[TypingConstraint], debug: Boolean): Unit = {
    for (child <- current.children)
      solveScope(child, current.constraints ::: constraints, debug)

    if (debug) {
      println("Scope solve for " + current.sym.toString)
      println("Incoming constraints:\n" + constraints.mkString("\t", "\n\t", ""))
      println("Scope additional constraints:\n" + current.constraints.mkString("\t", "\n\t", ""))
    }

    // Compute the symbols we need to reconcile
    val childrenSymbols = current.children.map(child => child.affectedSymbols)
    var reconcileSymbols = Set.empty[Sym[_]]

    if (childrenSymbols.length > 0)
      reconcileSymbols = childrenSymbols.foldLeft(childrenSymbols.head)((a, b) => b intersect a)

    if (debug)
      println("Need to reconcile the following symbols: " + reconcileSymbols.mkString(", ") + "\n\n")

    // Compute the reconciling
    var fullReconcileConstraints: List[TypingConstraint] = Nil
    var partReconcileConstraints: List[TypingConstraint] = Nil

    for (sym <- reconcileSymbols) {
      fullReconcileConstraints = reconcileSymbol(ShapeVar(sym), current.children, true) :::
                                 reconcileSymbol(ValueVar(sym), current.children, true) :::
                                 fullReconcileConstraints
      partReconcileConstraints = reconcileSymbol(ShapeVar(sym), current.children, false) :::
                                 reconcileSymbol(ValueVar(sym), current.children, false) :::
                                 partReconcileConstraints
    }

    // Compute the substitutions
    current.fullSubsts = computeSubstitutions(fullReconcileConstraints ::: current.constraints, debug)
    current.partSubsts = computeSubstitutions(partReconcileConstraints ::: current.constraints.filterNot(c => c.prereq), debug)
  }

  protected def reconcileSymbol(v: TypingVariable, children: List[TypingScope], full: Boolean): List[TypingConstraint] = {

    assert(children != 0) // should be already satisfied

    // Reconciliation strategy:
    //  - first:  check if everyone agrees on the length
    //  - second: check each position and see if everyone agrees on the value
    val lenghts = children.map(child => if (full) child.getLengthFull(v) else child.getLengthPart(v))
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
        for (child <- children) {
          val value: TypingVariable = if (full) child.fullSubsts(v) else child.partSubsts(v)
          val newElt: TypingElement = value.asInstanceOf[Lst].list(n)
          element = reconcileElement(element, newElt)
        }
        elements = element :: elements
      }
      Equality(v, Lst(elements.reverse), postReq, "Reconciliation of " + children.mkString("(", " ", ")"))::Nil
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