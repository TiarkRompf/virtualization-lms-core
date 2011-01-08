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
  class RepMDArray[A: ClassManifest](array: Rep[MDArray[A]]) {
    def apply(iv: Rep[MDArray[Int]]): Rep[MDArray[A]] = sel(iv, array)
    def unary_-(implicit numeric: Numeric[A]): Rep[MDArray[A]] = uop(array)(numeric.negate, "-[unary]")
    def unary_!(implicit ev: A =:= Boolean): Rep[MDArray[Boolean]] = uop(array)((a) => !(ev(a)), "!")
    override def toString() = doToString(array)
    def getString() = doToString(array)
  }
  implicit def toRepMDArray[A: ClassManifest](a: Rep[MDArray[A]]): RepMDArray[A] = new RepMDArray[A](a)

  // Element-wise operations
  def infix_+ [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit numeric: Numeric[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[A]] = op(o1, o2)(numeric.plus, "-")
  def infix_- [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit numeric: Numeric[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[A]] = op(o1, o2)(numeric.minus, "-")
  def infix_* [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit numeric: Numeric[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[A]] = op(o1, o2)(numeric.times, "*")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit fractional: Fractional[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[A]] = op(o1, o2)(fractional.div, "/[div]")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit integral: Integral[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[A]] = op(o1, o2)(integral.quot, "/[quot]")
  def infix_% [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit integral: Integral[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[A]] = op(o1, o2)(integral.rem, "%[rem]")
  def infix_< [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ordering: Ordering[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.lt, "<")
  def infix_<=[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ordering: Ordering[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.lteq, "<=")
  def infix_> [A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ordering: Ordering[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.gt, ">")
  def infix_>=[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ordering: Ordering[A], mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.gteq, ">=")
  def infix_===[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = op(o1, o2)((a, b) => a == b, "===")
  def infix_!==[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit mf: ClassManifest[A], ov1: Overloaded1): Rep[MDArray[Boolean]] = op(o1, o2)((a, b) => !(a == b), "!==")

  def infix_+ [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit numeric: Numeric[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[A]] = op(o1, o2)(numeric.plus, "+")
  def infix_- [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit numeric: Numeric[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[A]] = op(o1, o2)(numeric.minus, "-")
  def infix_* [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit numeric: Numeric[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[A]] = op(o1, o2)(numeric.times, "*")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit fractional: Fractional[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[A]] = op(o1, o2)(fractional.div, "/[div]")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit integral: Integral[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[A]] = op(o1, o2)(integral.quot, "/[quot]")
  def infix_% [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit integral: Integral[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[A]] = op(o1, o2)(integral.rem, "%[rem]")
  def infix_< [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit ordering: Ordering[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.lt, "<")
  def infix_<=[A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit ordering: Ordering[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.lteq, "<=")
  def infix_> [A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit ordering: Ordering[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.gt, ">")
  def infix_>=[A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit ordering: Ordering[A], mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.gteq, ">=")
  def infix_===[A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = op(o1, o2)((a, b) => a == b, "===")
  def infix_!==[A](o1: Rep[MDArray[A]], o2: Rep[A])(implicit mf: ClassManifest[A], ov2: Overloaded2): Rep[MDArray[Boolean]] = op(o1, o2)((a, b) => !(a == b), "!==")

  def infix_+ [A](o1: Rep[MDArray[A]], o2: A)(implicit numeric: Numeric[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[A]] = op(o1, o2)(numeric.plus, "+")
  def infix_- [A](o1: Rep[MDArray[A]], o2: A)(implicit numeric: Numeric[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[A]] = op(o1, o2)(numeric.minus, "-")
  def infix_* [A](o1: Rep[MDArray[A]], o2: A)(implicit numeric: Numeric[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[A]] = op(o1, o2)(numeric.times, "*")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: A)(implicit fractional: Fractional[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[A]] = op(o1, o2)(fractional.div, "/[div]")
  def infix_/ [A](o1: Rep[MDArray[A]], o2: A)(implicit integral: Integral[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[A]] = op(o1, o2)(integral.quot, "/[quot]")
  def infix_% [A](o1: Rep[MDArray[A]], o2: A)(implicit integral: Integral[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[A]] = op(o1, o2)(integral.rem, "%[rem]")
  def infix_< [A](o1: Rep[MDArray[A]], o2: A)(implicit ordering: Ordering[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.lt, "<")
  def infix_<=[A](o1: Rep[MDArray[A]], o2: A)(implicit ordering: Ordering[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.lteq, "<=")
  def infix_> [A](o1: Rep[MDArray[A]], o2: A)(implicit ordering: Ordering[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.gt, ">")
  def infix_>=[A](o1: Rep[MDArray[A]], o2: A)(implicit ordering: Ordering[A], mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = op(o1, o2)(ordering.gteq, ">=")
  def infix_===[A](o1: Rep[MDArray[A]], o2: A)(implicit mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = op(o1, o2)((a, b) => a == b, "===")
  def infix_!==[A](o1: Rep[MDArray[A]], o2: A)(implicit mf: ClassManifest[A], ov3: Overloaded3): Rep[MDArray[Boolean]] = op(o1, o2)((a, b) => !(a == b), "!==")

  def infix_&&[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ev: A =:= Boolean, mf: ClassManifest[A]): Rep[MDArray[Boolean]] = op(o1, o2)((a, b) => ev(a) && ev(b), "&&")
  def infix_||[A](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]])(implicit ev: A =:= Boolean, mf: ClassManifest[A]): Rep[MDArray[Boolean]] = op(o1, o2)((a, b) => ev(a) || ev(b), "||")

  // Concatenation shortcut
  def +++[A: ClassManifest](o1: Rep[MDArray[A]], o2: Rep[MDArray[A]]): Rep[MDArray[A]] = cat(0, o1, o2)

  // toString operation
  def doToString[A](a: Rep[MDArray[A]]): String

  /*
    Implicit conversions discussion
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    Initial assumption: We don't want the user to call the MDArray constructor directly, we only want to let him use
    lists and arrays that automatically get transformed to MDArrays
    TODO: Check if this is a sane assumption. It worked for the PDE1Benchmark, does it always work?
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
  implicit def convertFromListRep[A: ClassManifest](a: List[A]): Rep[MDArray[A]]
  implicit def convertFromArrayRep[A: ClassManifest](a: Array[A]): Rep[MDArray[A]]
  implicit def convertFromValueRep[A: ClassManifest](a: A): Rep[MDArray[A]]
  implicit def convertFromListOfMDArraysRep[A: ClassManifest](a: List[Rep[MDArray[A]]]): Rep[MDArray[A]]
  implicit def convertFromArrayOfMDArraysRep[A: ClassManifest](a: Array[Rep[MDArray[A]]]): Rep[MDArray[A]]
  implicit def convertToListRep[A: ClassManifest](a: Rep[MDArray[A]]): Rep[List[A]]
  implicit def convertToArrayRep[A: ClassManifest](a: Rep[MDArray[A]]): Rep[Array[A]]
  implicit def convertToValueRep[A: ClassManifest](a: Rep[MDArray[A]]): Rep[A]

  // Implicit conversions from unknown elements
  implicit def convertFromListRepRep[A: ClassManifest](a: Rep[List[A]]): Rep[MDArray[A]]
  implicit def convertFromArrayRepRep[A: ClassManifest](a: Rep[Array[A]]): Rep[MDArray[A]]
  implicit def convertFromValueRepRep[A: ClassManifest](a: Rep[A]): Rep[MDArray[A]]

  // Explicit conversions for elements known only at runtime
  def knownOnlyAtRuntime[A](a: List[A])(implicit mf: ClassManifest[A], o1: Overloaded1): Rep[MDArray[A]]
  def knownOnlyAtRuntime[A](a: Array[A])(implicit mf: ClassManifest[A], o2: Overloaded2): Rep[MDArray[A]]
  def knownOnlyAtRuntime[A](a: A)(implicit mf: ClassManifest[A], o3: Overloaded3): Rep[MDArray[A]]

  // Private value to take the place of "null" in the With object
  protected val nothing: Rep[MDArray[Int]]

  // With operation
  class With(_lb: Rep[MDArray[Int]] = nothing,
             _lbStrict: Boolean = false,
             _ub: Rep[MDArray[Int]] = nothing,
             _ubStrict: Boolean = false,
             _step: Rep[MDArray[Int]] = nothing,
             _width: Rep[MDArray[Int]] = nothing) {

    def GenArray[A: ClassManifest](shp: Rep[MDArray[Int]], f: Rep[MDArray[Int]]=> Rep[MDArray[A]]): Rep[MDArray[A]] = genArrayWith((fillShape(shp), f)::Nil, shp)
    def ModArray[A: ClassManifest](a: Rep[MDArray[A]], f: Rep[MDArray[Int]] => Rep[MDArray[A]]): Rep[MDArray[A]] = modArrayWith((fillShape(shape(a)), f)::Nil, a)
    def Fold[A: ClassManifest](foldFunction: (Rep[MDArray[A]], Rep[MDArray[A]]) => Rep[MDArray[A]], neutral: Rep[MDArray[A]], f: Rep[MDArray[Int]] => Rep[MDArray[A]]): Rep[MDArray[A]] = foldArrayWith(this, foldFunction, neutral, f)

    // This function will fill in the values correctly on construction :)
    def fillShape(shape: Rep[MDArray[Int]]): With = {
      val lb = if (_lb == nothing) zeros(dim(shape)) else _lb
      val lbStrict = if (_lb == nothing) false else _lbStrict
      val ub = if (_ub == nothing) shape  else _ub
      val ubStrict = if (_ub == nothing) true else _ubStrict
      val step = if (_step == nothing) values(dim(shape), 1) else _step
      val width = if (_width == nothing) values(dim(shape), 0) else _width
      With(lb, lbStrict, ub, ubStrict, step, width)
    }

    def usedDefs(): List[Rep[Any]] = _lb::_ub::_step::_width::Nil

    override def toString() = "With(" + _lb + ", " + _lbStrict + ", " + _ub + ", " + _ubStrict + ", " + _step + ", " + _width + ")"
  }

  object With {
    def apply(_lb: Rep[MDArray[Int]] = nothing,
           _lbStrict: Boolean = false,
           _ub: Rep[MDArray[Int]] = nothing,
           _ubStrict: Boolean = false,
           _step: Rep[MDArray[Int]] = nothing,
           _width: Rep[MDArray[Int]] = nothing): With =
    new With(_lb, _lbStrict, _ub, _ubStrict, _step, _width)
  }

  // Language operations
  // Basic operations
  def dim[A: ClassManifest](a: Rep[MDArray[A]]): Rep[Int]
  def shape[A: ClassManifest](a: Rep[MDArray[A]]): Rep[MDArray[Int]]
  def sel[A: ClassManifest](iv: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def reshape[A: ClassManifest](iv: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def cat[A: ClassManifest](d: Int, one: Rep[MDArray[A]], two: Rep[MDArray[A]]): Rep[MDArray[A]]

  // Reduction operations on matrices
  def sum[A](a: Rep[MDArray[A]])(implicit ev: Numeric[A], mf: ClassManifest[A]): Rep[A]
  def prod[A](a: Rep[MDArray[A]])(implicit ev: Numeric[A], mf: ClassManifest[A]): Rep[A]
  def all(a: Rep[MDArray[Boolean]]): Rep[Boolean]
  def any(a: Rep[MDArray[Boolean]]): Rep[Boolean]
  def maxVal[A](a: Rep[MDArray[A]])(implicit ev: Ordering[A], mf: ClassManifest[A]): Rep[A]
  def minVal[A](a: Rep[MDArray[A]])(implicit ev: Ordering[A], mf: ClassManifest[A]): Rep[A]

  // Zeroes, ones and values
  def values(dim: Rep[Int], value: Rep[Int]): Rep[MDArray[Int]]
  def zeros(dim: Rep[Int]): Rep[MDArray[Int]] = values(dim, 0)
  def ones(dim: Rep[Int]): Rep[MDArray[Int]] = values(dim, 1)

  // Element-Wise operations on matrices
  def max[A](a: Rep[MDArray[A]], b: Rep[MDArray[A]])(implicit mf: ClassManifest[A], or: Ordering[A]): Rep[MDArray[A]] = op(a,b)((a,b) => if (or.gt(a, b)) a else b, "max")
  def min[A](a: Rep[MDArray[A]], b: Rep[MDArray[A]])(implicit mf: ClassManifest[A], or: Ordering[A]): Rep[MDArray[A]] = op(a,b)((a,b) => if (or.lt(a, b)) a else b, "min")
  def where[A: ClassManifest](p: Rep[MDArray[Boolean]], a: Rep[MDArray[A]], b: Rep[MDArray[A]]): Rep[MDArray[A]]

  // Restructuring operations - implemented as with-loops
  def genarray[A: ClassManifest](shp: Rep[MDArray[Int]], value: Rep[MDArray[A]]): Rep[MDArray[A]]
  def modarray[A: ClassManifest](a: Rep[MDArray[A]], iv: Rep[MDArray[Int]], value: Rep[MDArray[A]]): Rep[MDArray[A]]
  def take[A: ClassManifest](shp: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def drop[A: ClassManifest](shp: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def tile[A: ClassManifest](sv: Rep[MDArray[Int]], ov: Rep[MDArray[Int]], a:Rep[MDArray[A]]): Rep[MDArray[A]]
  def rotate[A: ClassManifest](ov: Rep[MDArray[Int]], a:Rep[MDArray[A]]): Rep[MDArray[A]]
  def shift[A: ClassManifest](ov: Rep[MDArray[Int]], expr: A, a:Rep[MDArray[A]]): Rep[MDArray[A]]

  // Basic operations on matrices - they appear as private here
  def op[A, B](a:Rep[MDArray[A]], b:Rep[MDArray[A]])(op: (A, A) => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B], ov1: Overloaded4): Rep[MDArray[B]]
  def op[A, B](a:Rep[MDArray[A]], b:Rep[A])(op: (A, A) => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B], ov2: Overloaded5): Rep[MDArray[B]]
  def uop[A, B](a:Rep[MDArray[A]])(op: A => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B]): Rep[MDArray[B]]

  // With-comprehensions
  def genArrayWith[A: ClassManifest](l: List[Pair[With, Rep[MDArray[Int]]=> Rep[MDArray[A]]]], shp: Rep[MDArray[Int]]): Rep[MDArray[A]]
  def modArrayWith[A: ClassManifest](l: List[Pair[With, Rep[MDArray[Int]]=> Rep[MDArray[A]]]], a: Rep[MDArray[A]]): Rep[MDArray[A]]
  def foldArrayWith[A: ClassManifest](w: With, foldFunction: (Rep[MDArray[A]], Rep[MDArray[A]]) => Rep[MDArray[A]], neutral: Rep[MDArray[A]], f: Rep[MDArray[Int]] => Rep[MDArray[A]]): Rep[MDArray[A]]

  // Assertions
  def assertPrefixLt(iv: Rep[MDArray[Int]], shp: Rep[MDArray[Int]]): Rep[Unit]
  def assertOneDimensional(iv: Rep[MDArray[Int]]): Rep[Unit]
  def assertEqualExcept(d: Rep[Int], shp1: Rep[MDArray[Int]], shp2: Rep[MDArray[Int]]): Rep[Unit]
  def assertContentSizeEqual(shp1: Rep[MDArray[Int]], shp2: Rep[MDArray[Int]]): Rep[Unit]
  def assertShapesEqual(shp1: Rep[MDArray[Int]], shp2: Rep[MDArray[Int]]): Rep[Unit]
  def assertShapeGreater(shpGreater: Rep[MDArray[Int]], shpLower: Rep[MDArray[Int]]): Rep[Unit]
  def assertShapeSameLength(shpGreater: Rep[MDArray[Int]], shpLower: Rep[MDArray[Int]]): Rep[Unit]
}
