package scala.virtualization.lms
package epfl
package test7

import common._
import java.io.PrintWriter
import original._


trait MDArrayBase extends Base with util.OverloadHack {
  // TODO: What are the implication of having something like MDArray[Rep[T]]? Do the implicits still work?

  /**
   * This is a hack to compute MDArray.apply()
   * NOTE: MDArrays are immutable, therefore there's no update
   */
  class RepMDArray[A: Manifest](array: Rep[MDArray[A]]) {
    def apply(iv: Rep[MDArray[Int]]): Rep[MDArray[A]] = sel(iv, array)
    def unary_-(implicit numeric: Numeric[A]): Rep[MDArray[A]] = uop(array)(numeric.negate, "-")
    def unary_!(implicit ev: A =:= Boolean): Rep[MDArray[Boolean]] = uop(array)((a) => !(ev(a)), "!")
    override def toString() = doToString(array)
    def getString() = doToString(array)
  }
  implicit def toRepMDArray[A: Manifest](a: Rep[MDArray[A]]): RepMDArray[A] = new RepMDArray[A](a)

  // Element-wise operations
  def infix_+ [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit numeric: Numeric[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[A]] = opAA(o1, o2)(numeric.plus, "+")
  def infix_- [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit numeric: Numeric[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[A]] = opAA(o1, o2)(numeric.minus, "-")
  def infix_* [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit numeric: Numeric[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[A]] = opAA(o1, o2)(numeric.times, "*")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit fractional: Fractional[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[A]] = opAA(o1, o2)(fractional.div, "/[div]")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit integral: Integral[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[A]] = opAA(o1, o2)(integral.quot, "/[quot]")
  def infix_% [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit integral: Integral[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[A]] = opAA(o1, o2)(integral.rem, "%[rem]")
  def infix_< [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ordering: Ordering[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = opAA(o1, o2)(ordering.lt, "<")
  def infix_<=[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ordering: Ordering[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = opAA(o1, o2)(ordering.lteq, "<=")
  def infix_> [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ordering: Ordering[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = opAA(o1, o2)(ordering.gt, ">")
  def infix_>=[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ordering: Ordering[A], mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = opAA(o1, o2)(ordering.gteq, ">=")
  def infix_===[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = opAA(o1, o2)((a, b) => a == b, "===")
  def infix_!==[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit mf: Manifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = opAA(o1, o2)((a, b) => !(a == b), "!==")

  def infix_+ [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit numeric: Numeric[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[A]] = opAE(o1, o2)(numeric.plus, "+")
  def infix_- [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit numeric: Numeric[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[A]] = opAE(o1, o2)(numeric.minus, "-")
  def infix_* [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit numeric: Numeric[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[A]] = opAE(o1, o2)(numeric.times, "*")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit fractional: Fractional[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[A]] = opAE(o1, o2)(fractional.div, "/[div]")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit integral: Integral[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[A]] = opAE(o1, o2)(integral.quot, "/[quot]")
  def infix_% [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit integral: Integral[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[A]] = opAE(o1, o2)(integral.rem, "%[rem]")
  def infix_< [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit ordering: Ordering[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = opAE(o1, o2)(ordering.lt, "<")
  def infix_<=[A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit ordering: Ordering[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = opAE(o1, o2)(ordering.lteq, "<=")
  def infix_> [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit ordering: Ordering[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = opAE(o1, o2)(ordering.gt, ">")
  def infix_>=[A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit ordering: Ordering[A], mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = opAE(o1, o2)(ordering.gteq, ">=")
  def infix_===[A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = opAE(o1, o2)((a, b) => a == b, "===")
  def infix_!==[A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit mf: Manifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = opAE(o1, o2)((a, b) => !(a == b), "!==")

  def infix_+ [A](o1: Rep[MDArray[A]], o2: A)(implicit numeric: Numeric[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[A]] = opAENR(o1, o2)(numeric.plus, "+")
  def infix_- [A](o1: Rep[MDArray[A]], o2: A)(implicit numeric: Numeric[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[A]] = opAENR(o1, o2)(numeric.minus, "-")
  def infix_* [A](o1: Rep[MDArray[A]], o2: A)(implicit numeric: Numeric[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[A]] = opAENR(o1, o2)(numeric.times, "*")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: A)(implicit fractional: Fractional[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[A]] = opAENR(o1, o2)(fractional.div, "/[div]")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: A)(implicit integral: Integral[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[A]] = opAENR(o1, o2)(integral.quot, "/[quot]")
  def infix_% [A](o1: Rep[MDArray[A]], o2: A)(implicit integral: Integral[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[A]] = opAENR(o1, o2)(integral.rem, "%[rem]")
  def infix_< [A](o1: Rep[MDArray[A]], o2: A)(implicit ordering: Ordering[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = opAENR(o1, o2)(ordering.lt, "<")
  def infix_<=[A](o1: Rep[MDArray[A]], o2: A)(implicit ordering: Ordering[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = opAENR(o1, o2)(ordering.lteq, "<=")
  def infix_> [A](o1: Rep[MDArray[A]], o2: A)(implicit ordering: Ordering[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = opAENR(o1, o2)(ordering.gt, ">")
  def infix_>=[A](o1: Rep[MDArray[A]], o2: A)(implicit ordering: Ordering[A], mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = opAENR(o1, o2)(ordering.gteq, ">=")
  def infix_===[A](o1: Rep[MDArray[A]], o2: A)(implicit mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = opAENR(o1, o2)((a, b) => a == b, "===")
  def infix_!==[A](o1: Rep[MDArray[A]], o2: A)(implicit mf: Manifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = opAENR(o1, o2)((a, b) => !(a == b), "!==")

  def infix_&&[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ev: A =:= Boolean, mf: Manifest[A]): Rep[MDArray[Boolean]] = opAA(o1, o2)((a, b) => ev(a) && ev(b), "&&")
  def infix_||[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ev: A =:= Boolean, mf: Manifest[A]): Rep[MDArray[Boolean]] = opAA(o1, o2)((a, b) => ev(a) || ev(b), "||")

  // Concatenation shortcut
  def +++[A: Manifest](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]]): Rep[MDArray[A]] = cat(0, o1, o2)

  // toString operation
  def doToString[A](a: Rep[MDArray[A]]): String

  /*
    Implicit conversions discussion
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Initial assumption: We don't want the user to call the MDArray constructor directly, we only want to let him use
    lists and arrays that automatically get transformed to MDArrays
    TODO: Check if this is a sane assumption. It worked for the PDE1BenchmarkStaged, does it always work?
    TODO: If the assumption is sane, enforce it by making MDArray private, so the constructor cannot be called

    In the context of the no-constructor assumption, we will consider all MDArray as Rep[MDArray]s. There are two
    categories of Rep[MDArrays]s:
      - known at compile time (default) -- these automatically are transformed from Lists and Array
      - known only at runtime           -- are either:
                                           * results of operations
                                           * EXPLICITLY declared as known only at runtime by the programmer
                                           * IMPLICITLY created from Rep[List[T]]s, Rep[Array[T]]s, Rep[T]s

    The functions used to signal a value is only available at runtime are:
      - knownOnlyAtRuntime(a:Array[A])
      - knownOnlyAtRuntime(a:List[A])
      - knownOnlyAtRuntime(a:A)

    Since the index vectors are also MDArrays, they will be transformed in the same fashion
   */
  // TODO: Conversions going back to the "real" world are problematic... Decide what to do about them
  // Implicit conversions
  implicit def convertFromListRep[A: Manifest](a: List[A]): Rep[MDArray[A]]
  implicit def convertFromArrayRep[A: Manifest](a: Array[A]): Rep[MDArray[A]]
  implicit def convertFromValueRep[A: Manifest](a: A): Rep[MDArray[A]]
  implicit def convertToListRep[A: Manifest](a: Rep[MDArray[A]]): Rep[List[A]]
  implicit def convertToArrayRep[A: Manifest](a: Rep[MDArray[A]]): Rep[Array[A]]
  implicit def convertToValueRep[A: Manifest](a: Rep[MDArray[A]]): Rep[A]

  // Implicit conversions from unknown elements
  implicit def convertFromListRepRep[A: Manifest](a: Rep[List[A]]): Rep[MDArray[A]]
  implicit def convertFromArrayRepRep[A: Manifest](a: Rep[Array[A]]): Rep[MDArray[A]]
  implicit def convertFromValueRepRep[A: Manifest](a: Rep[A]): Rep[MDArray[A]]

  // Explicit conversions for elements known only at runtime
  def knownOnlyAtRuntime[A](name: String)(implicit mf: Manifest[A]): Rep[MDArray[A]]

  // Private value to take the place of "null" in the With object
  protected val nothing: Rep[MDArray[Int]]

  // With operation
  class With[A: Manifest] (val lb: Rep[MDArray[Int]] = nothing,
                           val lbStrict: Boolean = false,
                           val ubStrict: Boolean = false,
                           val ub: Rep[MDArray[Int]] = nothing,
                           val step: Rep[MDArray[Int]] = nothing,
                           val width: Rep[MDArray[Int]] = nothing,
                           val function: Rep[MDArray[Int]] => Rep[MDArray[A]]) {

    def GenArray(shp: Rep[MDArray[Int]]): Rep[MDArray[A]] = genArrayWith(fillShape(shp)::Nil, shp)
    def ModArray(a: Rep[MDArray[A]]): Rep[MDArray[A]] = modArrayWith(fillShape(shape(a))::Nil, a)
    def Fold(foldFunction: (Rep[MDArray[A]], Rep[MDArray[A]]) => Rep[MDArray[A]], neutral: Rep[MDArray[A]]): Rep[MDArray[A]] = foldArrayWith(fillShapeForFold(), foldFunction, neutral)

    // This function will fill in the values correctly on construction :)
    def fillShape(shp: Rep[MDArray[Int]]): With[A] = {
      val _lb = if (lb == nothing) zeros(sel(0::Nil, shape(shp))) else lb
      val _lbStrict = lbStrict
      val _ub = if (ub == nothing) shp - 1  else ub
      val _ubStrict = ubStrict
      val _step = if (step == nothing) values(sel(0::Nil, shape(shp)), 1) else step
      val _width = if (width == nothing) values(sel(0::Nil, shape(shp)), 0) else width
      With(_lb, _lbStrict, _ubStrict, _ub, _step, _width, function)
    }

    def fillShapeForFold(): With[A] = {
      if (lb == nothing) throw new Exception("fold: lower bound must be specified")
      if (ub == nothing) throw new Exception("fold: upper bound must be specified")
      val _step = if (step == nothing) values(sel(0::Nil, shape(lb)), 1) else step
      val _width = if (width == nothing) values(sel(0::Nil, shape(lb)), 0) else width
      With(lb, lbStrict, ubStrict, ub, _step, _width, function)
    }

    override def toString() = "With(" + lb + ", " + lbStrict + ", " + ub + ", " + ubStrict + ", " + step + ", " + width + ", f= " + function + ")"
  }

  object With {
    def apply[A: Manifest](lb: Rep[MDArray[Int]] = nothing,
           lbStrict: Boolean = false,
           ubStrict: Boolean = false,
           ub: Rep[MDArray[Int]] = nothing,
           step: Rep[MDArray[Int]] = nothing,
           width: Rep[MDArray[Int]] = nothing,
           function: Rep[MDArray[Int]] => Rep[MDArray[A]]): With[A] =
    new With[A](lb, lbStrict, ubStrict, ub, step, width, function)
  }

  // Language operations
  // Basic operations
  def dim[A: Manifest](a: Rep[MDArray[A]]): Rep[Int]
  def shape[A: Manifest](a: Rep[MDArray[A]]): Rep[MDArray[Int]]
  def sel[A: Manifest](iv: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def reshape[A: Manifest](iv: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def cat[A: Manifest](d: Int, one: Rep[MDArray[A]], two: Rep[MDArray[A]]): Rep[MDArray[A]]

  // Reduction operations on matrices
  def sum[A](a: Rep[MDArray[A]])(implicit ev: Numeric[A], mf: Manifest[A]): Rep[A]
  def prod[A](a: Rep[MDArray[A]])(implicit ev: Numeric[A], mf: Manifest[A]): Rep[A]
  def all(a: Rep[MDArray[Boolean]]): Rep[Boolean]
  def any(a: Rep[MDArray[Boolean]]): Rep[Boolean]
  def maxVal[A](a: Rep[MDArray[A]])(implicit ev: Ordering[A], mf: Manifest[A]): Rep[A]
  def minVal[A](a: Rep[MDArray[A]])(implicit ev: Ordering[A], mf: Manifest[A]): Rep[A]

  // Zeroes, ones and values
  def values(dim: Rep[Int], value: Rep[Int]): Rep[MDArray[Int]]
  def zeros(dim: Rep[Int]): Rep[MDArray[Int]] = values(dim, 0)
  def ones(dim: Rep[Int]): Rep[MDArray[Int]] = values(dim, 1)

  // Element-Wise operations on matrices
  def max[A](a: Rep[MDArray[A]], b: Rep[MDArray[A]])(implicit mf: Manifest[A], or: Ordering[A]): Rep[MDArray[A]] = opAA(a,b)((a,b) => if (or.gt(a, b)) a else b, "max")
  def min[A](a: Rep[MDArray[A]], b: Rep[MDArray[A]])(implicit mf: Manifest[A], or: Ordering[A]): Rep[MDArray[A]] = opAA(a,b)((a,b) => if (or.lt(a, b)) a else b, "min")
  def where[A: Manifest](p: Rep[MDArray[Boolean]], a: Rep[MDArray[A]], b: Rep[MDArray[A]]): Rep[MDArray[A]]

  // Restructuring operations - implemented as with-loops
  def genarray[A: Manifest](shp: Rep[MDArray[Int]], value: Rep[MDArray[A]]): Rep[MDArray[A]]
  def modarray[A: Manifest](a: Rep[MDArray[A]], iv: Rep[MDArray[Int]], value: Rep[MDArray[A]]): Rep[MDArray[A]]
  def take[A: Manifest](shp: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def drop[A: Manifest](shp: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def tile[A: Manifest](sv: Rep[MDArray[Int]], ov: Rep[MDArray[Int]], a:Rep[MDArray[A]]): Rep[MDArray[A]]
  def rotate[A: Manifest](ov: Rep[MDArray[Int]], a:Rep[MDArray[A]]): Rep[MDArray[A]]
  def shift[A: Manifest](ov: Rep[MDArray[Int]], expr: Rep[A], a:Rep[MDArray[A]]): Rep[MDArray[A]]

  // Basic operations on matrices - they appear as private here
  def opAA[A, B](a:Rep[MDArray[A]], b:Rep[MDArray[A]])(op: (A, A) => B, opName: String)(implicit mfA: Manifest[A], mfB: Manifest[B], ov1: Overloaded4): Rep[MDArray[B]]
  def opAE[A, B](a:Rep[MDArray[A]], b:Rep[A])(op: (A, A) => B, opName: String)(implicit mfA: Manifest[A], mfB: Manifest[B], ov2: Overloaded5): Rep[MDArray[B]]
  def opAENR[A, B](a:Rep[MDArray[A]], b:A)(op: (A, A) => B, opName: String)(implicit mfA: Manifest[A], mfB: Manifest[B], ov2: Overloaded5): Rep[MDArray[B]]
  def uop[A, B](a:Rep[MDArray[A]])(op: A => B, opName: String)(implicit mfA: Manifest[A], mfB: Manifest[B]): Rep[MDArray[B]]

  // With-comprehensions
  def genArrayWith[A: Manifest](l: List[With[A]], shp: Rep[MDArray[Int]]): Rep[MDArray[A]]
  def modArrayWith[A: Manifest](l: List[With[A]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def foldArrayWith[A: Manifest](w: With[A], foldFunction: (Rep[MDArray[A]], Rep[MDArray[A]]) => Rep[MDArray[A]], neutral: Rep[MDArray[A]]): Rep[MDArray[A]]

  // Integer operations for Game of Life, NumericOps is too much
  def infix_+(a: Rep[Int], b: Rep[Int]): Rep[Int]
  def infix_-(a: Rep[Int], b: Rep[Int]): Rep[Int]
  def infix_===(a: Rep[Int], b: Rep[Int]): Rep[Boolean]
  def infix_<<(a: Rep[Int], b: Rep[Int]): Rep[Boolean]
}
