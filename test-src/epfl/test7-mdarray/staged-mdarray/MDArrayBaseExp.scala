package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

trait MDArrayBaseExp extends MDArrayBase with BaseExp with IfThenElseExp {
  // needed so that foldTerms are not collected by the CSE
  var foldTermIndex = 0

  /*
      Basic AST building blocks
   */
  case class KnownAtRuntime[A: Manifest](name: String) extends Def[MDArray[A]] { override def toString() = "KnownAtRuntime(" + name + ") " }
  case class KnownAtCompileTime[A: Manifest](value: MDArray[A]) extends Def[MDArray[A]] {override def toString() = "KnownAtCompileTime(" + value.toString + ")" }

  // Conversions within the staged universe
  case class FromList[A: Manifest](value: Exp[List[A]]) extends Def[MDArray[A]] { override def toString() = "FromList(" + value.toString + ")" }
  case class FromArray[A: Manifest](value: Exp[Array[A]]) extends Def[MDArray[A]] { override def toString() = "FromArray(" + value.toString + ")" }
  case class FromValue[A: Manifest](value: Exp[A]) extends Def[MDArray[A]] { override def toString() = "FromValue(" + value.toString + ")" }

  // Going back to the real world
  case class ToList[A: Manifest](value: Exp[MDArray[A]]) extends Def[List[A]] { override def toString() = "ToList(" + value.toString + ")" }
  case class ToArray[A: Manifest](value: Exp[MDArray[A]]) extends Def[Array[A]] { override def toString() = "ToArray(" + value.toString + ")" }
  case class ToValue[A: Manifest](value: Exp[MDArray[A]]) extends Def[A] { override def toString() = "ToValue(" + value.toString + ")" }

  // With
  case class IndexVector(lb: Exp[MDArray[Int]], lbStrict: Exp[Boolean], ub: Exp[MDArray[Int]], ubStrict: Exp[Boolean], step: Exp[MDArray[Int]], width: Exp[MDArray[Int]]) extends Def[MDArray[Int]] { override def toString() = "IndexVector(" + lb + ", " + lbStrict + ", " + ub + ", " + ubStrict + ", " + step + ", " + width + ")" }
  case class WithNode[A: Manifest](iv: Exp[MDArray[Int]], expr: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "With(" + iv.toString + " => " + expr.toString + ")" }
  case class GenArrayWith[A: Manifest](lExpr: List[Exp[MDArray[A]]], shp: Exp[MDArray[Int]]) extends Def[MDArray[A]] { override def toString() = "GenArrayWith(" + shp.toString + " - " + lExpr.mkString(", ") + ")" }
  case class ModArrayWith[A: Manifest](lExpr: List[Exp[MDArray[A]]], a: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "ModArrayWith(" + a.toString + " - " + lExpr.mkString(", ") + ")" }
  // TODO: Important implicit assumption made here -- we assume foldFunction has no outside dependencies. According to the SAC spec, it should indeed be the case, but proving it would be better
  // TODO: Decide if this way of expressing the foldFunction eliminates the "no external dependencies" problem -- maybe this should be checked explicitly
  case class FoldTerm[A: Manifest](like: Exp[MDArray[A]], id: Int) extends Def[MDArray[A]] { override def toString() = "FoldTerm(" + id + ") like " + like.toString }
  case class FoldArrayWith[A: Manifest](wExpr: Exp[MDArray[A]], neutral: Exp[MDArray[A]], foldTerm1: Exp[MDArray[A]], foldTerm2: Exp[MDArray[A]], foldExpression: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "FoldArrayWith(" + neutral + ", fold (" + foldTerm1 + ", " + foldTerm2 + ") => " + foldExpression + ", " + wExpr + ")" }

  // Base functions
  case class ToDim[A: Manifest](a: Exp[MDArray[A]]) extends Def[Int] { override def toString() = "Dim(" + a + ")" }
  case class ToShape[A: Manifest](a: Exp[MDArray[A]]) extends Def[MDArray[Int]] { override def toString() = "Shape(" + a + ")" }
  case class Reshape[A: Manifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "Reshape(" + shp + ", " + a + ")" }
  case class Sel[A: Manifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "Sel(" + iv + ", " + a + ")" }
  case class Cat[A: Manifest](d: Exp[MDArray[Int]], a: Exp[MDArray[A]], b: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "Cat(" + d + ", " + a + ", " + b + ")" }

  // Operations and Values
  // XXX: In an optimization phase, operations and values will end up as With loops => the only reason to keep them
  // as independent operations are to have more powerful constraints in the typing system
  case class InfixOp[A: Manifest, B: Manifest](array1: Exp[MDArray[A]], array2: Exp[MDArray[A]], op: (A, A) => B, opName: String) extends Def[MDArray[B]] { override def toString() = "InfixOp(" + opName + ": " + array1 + " and " + array2 + ")" }
  case class UnaryOp[A: Manifest, B: Manifest](array: Exp[MDArray[A]], op: A => B, opName: String) extends Def[MDArray[B]] { override def toString() = "UnaryOp(" + opName + ": " + array + ")" }
  case class Where[A: Manifest](cond: Exp[MDArray[Boolean]], array1: Exp[MDArray[A]], array2: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "Where(" + cond + ", " + array1 + ", " + array2 + ")" }
  case class Values[A: Manifest](dim: Exp[MDArray[Int]], value: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "Values(" + value + ", " + dim + ")" }

  // Nothing (a replacement for the null)
  case object Nothing extends Def[MDArray[Int]] { override def toString() = "<nothing>" }

  // Function wrapping
  case class ScalarOperatorWrapper[A: Manifest, B: Manifest, C: Manifest](f: (A,B)=>C, operator: String) extends ((Exp[MDArray[A]], Exp[MDArray[B]]) => Exp[MDArray[C]]) {
    def apply(a: Exp[MDArray[A]], b: Exp[MDArray[B]]): Exp[MDArray[C]] = ScalarOperatorApplication(f, operator, a, b)
  }
  case class ScalarOperatorApplication[A, B, C](f: (A,B)=>C, operator: String, a: Exp[MDArray[A]], b: Exp[MDArray[B]])(implicit mfA: Manifest[A], mfB: Manifest[B], mfC: Manifest[C]) extends Def[MDArray[C]] {
    def getMfA = manifest[A]
    def getMfB = manifest[B]
    def getMfC = manifest[C]
    override def toString() = "ScalarOperator " + a + " " + operator + " " + b
  }

  /*
      Abstract function implementation
   */
  // Implicit conversions
  implicit def convertFromListRep[A: Manifest](a: List[A]): Exp[MDArray[A]] = KnownAtCompileTime(a)
  implicit def convertFromArrayRep[A: Manifest](a: Array[A]): Exp[MDArray[A]] = KnownAtCompileTime(a)
  implicit def convertFromValueRep[A: Manifest](a: A): Exp[MDArray[A]] = KnownAtCompileTime(a)

  // Implicit conversions from unknown elements
  implicit def convertFromListRepRep[A: Manifest](a: Exp[List[A]]): Exp[MDArray[A]] = FromList(a)
  implicit def convertFromArrayRepRep[A: Manifest](a: Exp[Array[A]]): Exp[MDArray[A]] = FromArray(a)
  implicit def convertFromValueRepRep[A: Manifest](a: Exp[A]): Exp[MDArray[A]] = FromValue(a)

  implicit def convertToListRep[A: Manifest](a: Exp[MDArray[A]]): Exp[List[A]] = ToList(a)
  implicit def convertToArrayRep[A: Manifest](a: Exp[MDArray[A]]): Exp[Array[A]] = ToArray(a)
  implicit def convertToValueRep[A: Manifest](a: Exp[MDArray[A]]): Exp[A] = ToValue(a)

  // Explicit conversion for elements known only at runtime, including shape
  // To create an element with known shape, use reshape(<known shape>, knownOnlyAtRuntime(...))
  def knownOnlyAtRuntime[A](name: String)(implicit mf: Manifest[A]): Exp[MDArray[A]] = KnownAtRuntime[A](name)

  // Basic operations
  def dim[A: Manifest](a: Exp[MDArray[A]]): Exp[Int] = ToDim(a)
  def shape[A: Manifest](a: Exp[MDArray[A]]): Exp[MDArray[Int]] = ToShape(a)
  def sel[A: Manifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = Sel(iv, a)
  def reshape[A: Manifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = Reshape(iv, a)
  def cat[A: Manifest](d: Rep[MDArray[Int]], one: Exp[MDArray[A]], two: Exp[MDArray[A]]): Exp[MDArray[A]] = Cat(d, one, two)

  // Zeroes, ones and values
  def values(dim: Exp[MDArray[Int]], value: Exp[MDArray[Int]]): Exp[MDArray[Int]] = {
    // XXX: Let's make values a primitive, before with loops
    // With().GenArray(convertFromValueRepRep(dim), iv => value)
    Values(dim, value)
  }

  // Where
  def where[A: Manifest](p: Exp[MDArray[Boolean]], a: Exp[MDArray[A]], b: Exp[MDArray[A]]): Exp[MDArray[A]] = Where(p, a, b)

  // Restructuring operations - implemented as with-loops
  def genarray[A: Manifest](shp: Exp[MDArray[Int]], value: Exp[MDArray[A]]): Exp[MDArray[A]] =
    With(function = iv => value).GenArray(shp)
  def modarray[A: Manifest](a: Exp[MDArray[A]], iv: Exp[MDArray[Int]], value: Exp[MDArray[A]]): Exp[MDArray[A]] =
    With(lb = iv, ub = iv, function = iv => value).ModArray(a)

  // TODO: Redesign these functions for lower dimensions in the given vectors, filling in with zeros or shape elements
  // TODO: Decide if this hasn't been already encoded by the use of with loops
  def take[A: Manifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] =
    With(function = iv => sel(iv, a)).GenArray(shp)
  def drop[A: Manifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] =
    With(function = iv => sel(iv + shp, a)).GenArray(shape(a) - shp)
  def tile[A: Manifest](sv: Exp[MDArray[Int]], ov: Exp[MDArray[Int]], a:Exp[MDArray[A]]): Exp[MDArray[A]] =
    With(function = iv => sel(iv + ov, a)).GenArray(sv)
  def rotate[A: Manifest](ov: Exp[MDArray[Int]], a:Exp[MDArray[A]]): Exp[MDArray[A]] =
    With(function = iv => a(((iv - ov) + shape(a)) % shape(a))).GenArray(shape(a))
  def shift[A: Manifest](ov: Exp[MDArray[Int]], expr: Exp[MDArray[A]], a: Exp[MDArray[A]]): Exp[MDArray[A]] =
    With[A](function = iv => if ((any((iv - ov) < zeros(dim(a)))) || (any((iv - ov) >= shape(a)))) expr else a(iv - ov)).GenArray(shape(a))

  // Reduction operations on matrices
  def sum[A](a: Exp[MDArray[A]])(implicit ev: Numeric[A], mf: Manifest[A]): Exp[A] = reduce[A](ev.zero, a, scalarOperationWrapper[A,A,A]((a: A, b: A) => ev.plus(a, b), "+"), "sum")
  def prod[A](a: Exp[MDArray[A]])(implicit ev: Numeric[A], mf: Manifest[A]): Exp[A] = reduce[A](ev.one, a, scalarOperationWrapper[A,A,A]((a: A, b: A) => ev.times(a, b), "*"), "prod")
  def all(a: Exp[MDArray[Boolean]]): Exp[Boolean] = reduce[Boolean](true, a, scalarOperationWrapper[Boolean,Boolean,Boolean]((x: Boolean, y: Boolean) => x && y, "&&"), "all")
  def any(a: Exp[MDArray[Boolean]]): Exp[Boolean] = reduce[Boolean](false, a, scalarOperationWrapper[Boolean,Boolean,Boolean]((x: Boolean, y: Boolean) => x || y, "||"), "any")
  def maxVal[A](a: Exp[MDArray[A]])(implicit ev: Ordering[A], mf: Manifest[A], mfb: Manifest[Boolean]): Exp[A] = reduce[A](sel(zeros(dim(a)),a), a, (a: Exp[MDArray[A]], b: Exp[MDArray[A]]) => if (scalarOperationWrapper[A, A, Boolean]((a, b) => ev.gt(a,b), ">")(mf, mf, mfb)(a, b)) a else b, "max")
  def minVal[A](a: Exp[MDArray[A]])(implicit ev: Ordering[A], mf: Manifest[A], mfb: Manifest[Boolean]): Exp[A] = reduce[A](sel(zeros(dim(a)),a), a, (a: Exp[MDArray[A]], b: Exp[MDArray[A]]) => if (scalarOperationWrapper[A, A, Boolean]((a, b) => ev.lt(a,b), "<")(mf, mf, mfb)(a, b)) a else b, "min")

  // Basic operations on matrices - they appear as private here
  def op[A, B](a:Exp[MDArray[A]], b:Exp[MDArray[A]])(op: (A, A) => B, opName: String)(implicit mfA: Manifest[A], mfB: Manifest[B]): Exp[MDArray[B]] =
    InfixOp(a, b, op, opName)
  def uop[A, B](a:Exp[MDArray[A]])(op: A => B, opName: String)(implicit mfA: Manifest[A], mfB: Manifest[B]): Exp[MDArray[B]] =
    UnaryOp(a, op, opName)
  def reduce[A](z: Exp[MDArray[A]], a: Exp[MDArray[A]], op: (Exp[MDArray[A]], Exp[MDArray[A]]) => Exp[MDArray[A]], opName: String)(implicit mfA: Manifest[A]): Exp[MDArray[A]] =
    With[A](lb = zeros(dim(a)), lbStrict = false, ubStrict = true, ub = shape(a), function = (iv) => sel(iv, a)).Fold(op, z)
  // TODO: Eliminate Reduce completely
  //  Reduce(z, a, op(ReduceMember[B], ReduceMember[A]), opName)

  // With-comprehensions
  def toWithNode[A: Manifest](withObject: With[A]): Exp[MDArray[A]] = {
    val iv: Exp[MDArray[Int]] = IndexVector(withObject.lb, withObject.lbStrict, withObject.ub, withObject.ubStrict, withObject.step, withObject.width)
    WithNode(iv, withObject.function(iv))
  }

  def genArrayWith[A: Manifest](l: List[With[A]], shp: Exp[MDArray[Int]]): Exp[MDArray[A]] = GenArrayWith(l.map(w => toWithNode(w)), shp)
  def modArrayWith[A: Manifest](l: List[With[A]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = ModArrayWith(l.map(w => toWithNode(w)), a)
  def foldArrayWith[A: Manifest](w: With[A], foldFunction: (Exp[MDArray[A]], Exp[MDArray[A]]) => Exp[MDArray[A]], neutral: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    val foldTerm1 = toAtom(FoldTerm[A](neutral, foldTermIndex))
    foldTermIndex = foldTermIndex + 1
    val foldTerm2 = toAtom(FoldTerm[A](neutral, foldTermIndex))
    foldTermIndex = foldTermIndex + 2
    val foldExpression = foldFunction(foldTerm1, foldTerm2)
    FoldArrayWith(toWithNode(w), neutral, foldTerm1, foldTerm2, foldExpression)
  }

  // ToString
  def doToString[A](a: Exp[MDArray[A]]) = a.toString()

  // Function wrapping for scalar elements to mdarrays
  def scalarOperationWrapper[A: Manifest, B: Manifest, C: Manifest](f: (A,B)=>C, operator: String): ((Exp[MDArray[A]], Exp[MDArray[B]]) => Exp[MDArray[C]]) = ScalarOperatorWrapper(f, operator)

  protected val nothing: Exp[MDArray[Int]] = Nothing
}
