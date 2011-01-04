package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

trait MDArrayBaseExp extends MDArrayBase with BaseExp with IfThenElse {
  /*
      Some counters
   */
  var nList: Int = 0
  var nArray: Int = 0
  var nValue: Int = 0
  var nFunction: Int = 0

  /*
      The types listed here are copied from SAC
      TODO: It would be interesting to use a type reconstruction system and infer as much as possible of the type
      TODO: Decide if the above idea is useful. Would knowing the shape of a matrix is 5:5:5:6:7:...:9:2:3?
      XXX: It would be useful for type checking and displaying errors as soon as possible - do we want that?
      TODO: The current implementation of types does not use subtyping correctly. "case class `class KnownShapeDim' has case class ancestor `class AnyShape'.  This has been deprecated for unduly complicating both usage and implementation.  You should instead use extractors for pattern matching on non-leaf nodes."
   */
  abstract class Shape
  case object AnyShape extends Shape
  case object PlusShape extends Shape
  case class KnownShapeDim(dim: Int) extends Shape
  case class KnownShape(shape: MDArray[Int]) extends Shape

  /*
      Basic AST building blocks
   */
  case class KnownAtRuntime[A: ClassManifest](name: String) extends Rep[MDArray[A]]
  case class KnownAtCompileTime[A: ClassManifest](value: MDArray[A]) extends Rep[MDArray[A]]
  case class ListOfMDArrays[A: ClassManifest](value: List[Rep[MDArray[A]]]) extends Rep[MDArray[A]]
  case class ArrayOfMDArrays[A: ClassManifest](value: Array[Rep[MDArray[A]]]) extends Rep[MDArray[A]]
  case class GenArrayWith[A](l: List[Pair[With, Rep[MDArray[Int]]=> Rep[MDArray[A]]]], shp: Rep[MDArray[Int]])(implicit mf: ClassManifest[A]) extends Rep[MDArray[A]]
  case class ModArrayWith[A](l: List[Pair[With, Rep[MDArray[Int]]=> Rep[MDArray[A]]]], a: Rep[MDArray[A]])(implicit mf: ClassManifest[A]) extends Rep[MDArray[A]]
  case class FoldArrayWith[A](w: With, foldFunction: (Rep[MDArray[A]], Rep[MDArray[A]]) => Rep[MDArray[A]], neutral: Rep[MDArray[A]], f: Rep[MDArray[Int]] => Rep[MDArray[A]])(implicit mf: ClassManifest[A]) extends Rep[MDArray[A]]
  case class ToDim[A: ClassManifest](a: Rep[MDArray[A]]) extends Rep[Int]
  case class ToShape[A: ClassManifest](a: Rep[MDArray[A]]) extends Rep[MDArray[Int]]
  case class Reshape[A: ClassManifest](shp: Rep[MDArray[Int]], a: Rep[MDArray[A]]) extends Rep[MDArray[A]]
  case class Sel[A: ClassManifest](iv: Rep[MDArray[Int]], a: Rep[MDArray[A]]) extends Rep[MDArray[A]]
  case class Cat[A: ClassManifest](i: Rep[Int], a: Rep[MDArray[A]], b: Rep[MDArray[A]]) extends Rep[MDArray[A]]
  case class Reduce[A: ClassManifest, B](z: B, op: (B,A)=>B, a: Rep[MDArray[A]]) extends Rep[A]

  // Assertions
  case class AssertPrefixLt[A: ClassManifest](iv: Rep[MDArray[Int]], shp: Rep[MDArray[Int]], andThen: Rep[MDArray[A]]) extends Rep[MDArray[A]]
  case class AssertOneDimensional[A: ClassManifest](iv: Rep[MDArray[Int]], andThen: Rep[MDArray[A]]) extends Rep[MDArray[A]]
  case class AssertEqualExcept[A: ClassManifest](d: Rep[Int], shp1: Rep[MDArray[Int]], shp2: Rep[MDArray[Int]], andThen: Rep[MDArray[A]]) extends Rep[MDArray[A]]
  case class AssertContentSizeEqual[A: ClassManifest](a: Rep[MDArray[Int]], b: Rep[MDArray[Int]], andThen: Rep[MDArray[A]]) extends Rep[MDArray[A]]
  case class AssertShapesEqual[A: ClassManifest, B: ClassManifest](a: Rep[MDArray[A]], b: Rep[MDArray[A]], andThen: Rep[MDArray[B]]) extends Rep[MDArray[B]]
  case class AssertShapeGreater[A: ClassManifest](shpA: Rep[MDArray[Int]], shpB: Rep[MDArray[Int]], andThen: Rep[MDArray[A]]) extends Rep[MDArray[A]]
  case class AssertShapeSameLength[A: ClassManifest](shpGreater: Rep[MDArray[Int]], shpLower: Rep[MDArray[Int]], andThen: Rep[MDArray[A]]) extends Rep[MDArray[A]]

  // Conversions within the staged universe
  case class FromList[A: ClassManifest](value: Rep[List[A]]) extends Rep[MDArray[A]]
  case class FromArray[A: ClassManifest](value: Rep[Array[A]]) extends Rep[MDArray[A]]
  case class FromValue[A: ClassManifest](value: Rep[A]) extends Rep[MDArray[A]]

  // Going back to the real world
  case class ToList[A: ClassManifest](value: Rep[MDArray[A]]) extends Rep[List[A]]
  case class ToArray[A: ClassManifest](value: Rep[MDArray[A]]) extends Rep[Array[A]]
  case class ToValue[A: ClassManifest](value: Rep[MDArray[A]]) extends Rep[A]

  // FunctionWrapper
  case class TwoArgFunctionWrapper[A, B](f: (A, A) => B, opName: String = "") extends ((Rep[A], Rep[A]) => Rep[B]) {
    override def apply(a: Rep[A], b: Rep[A]) = TwoArgApplication(this, a, b)
  }
  case class OneArgFunctionWrapper[A, B](f: (A) => B, opName: String = "") extends (Rep[A] => Rep[B]) {
    override def apply(a: Rep[A]) = OneArgApplication(this, a)
  }
  case class TwoArgFunctionWrapperOnMatrices[A, B](f: (A, A) => B, opName: String = "") extends ((Rep[MDArray[A]], Rep[MDArray[A]]) => Rep[MDArray[B]]) {
    override def apply(a: Rep[MDArray[A]], b: Rep[MDArray[A]]) = TwoArgApplication(this, a, b)
  }
  case class OneArgFunctionWrapperOnMatrices[A, B](f: (A) => B, opName: String = "") extends (Rep[MDArray[A]] => Rep[MDArray[B]]) {
    override def apply(a: Rep[MDArray[A]]) = OneArgApplication(this, a)
  }
  case class TwoArgApplication[A, B, C](f: (Rep[A], Rep[B]) => Rep[C], a1: Rep[A], a2: Rep[B]) extends Rep[C]
  case class OneArgApplication[A, B](f: Rep[A] => Rep[B], a1: Rep[A]) extends Rep[B]

  /*
      Abstract function implementation
   */
  // Implicit conversions
  implicit def convertFromListRep[A: ClassManifest](a: List[A]): Rep[MDArray[A]] = KnownAtCompileTime(a)
  implicit def convertFromArrayRep[A: ClassManifest](a: Array[A]): Rep[MDArray[A]] = KnownAtCompileTime(a)
  implicit def convertFromValueRep[A: ClassManifest](a: A): Rep[MDArray[A]] = KnownAtCompileTime(a)
  implicit def convertFromListOfMDArraysRep[A: ClassManifest](a: List[Rep[MDArray[A]]]): Rep[MDArray[A]] = ListOfMDArrays(a)
  implicit def convertFromArrayOfMDArraysRep[A: ClassManifest](a: Array[Rep[MDArray[A]]]): Rep[MDArray[A]] = ArrayOfMDArrays(a)

  // Implicit conversions from unknown elements
  implicit def convertFromListRepRep[A: ClassManifest](a: Rep[List[A]]): Rep[MDArray[A]] = FromList(a)
  implicit def convertFromArrayRepRep[A: ClassManifest](a: Rep[Array[A]]): Rep[MDArray[A]] = FromArray(a)
  implicit def convertFromValueRepRep[A: ClassManifest](a: Rep[A]): Rep[MDArray[A]] = FromValue(a)

  // TODO: Add type assertions here for the array
  // TODO: Change the assertions mechanism such that it can accomomdate type assertions on non-MDArray objects
  implicit def convertToListRep[A: ClassManifest](a: Rep[MDArray[A]]): Rep[List[A]] = ToList(a)
  implicit def convertToArrayRep[A: ClassManifest](a: Rep[MDArray[A]]): Rep[Array[A]] = ToArray(a)
  implicit def convertToValueRep[A: ClassManifest](a: Rep[MDArray[A]]): Rep[A] = ToValue(a)

  // Explicit conversions for elements known only at runtime
  def knownOnlyAtRuntime[A](a: List[A])(implicit mf: ClassManifest[A], o1: Overloaded1): Rep[MDArray[A]] = KnownAtRuntime[A]("list " + {nList = nList + 1; nList})
  def knownOnlyAtRuntime[A](a: Array[A])(implicit mf: ClassManifest[A], o2: Overloaded2): Rep[MDArray[A]] = KnownAtRuntime[A]("array " + {nArray = nArray + 1; nArray})
  def knownOnlyAtRuntime[A](a: A)(implicit mf: ClassManifest[A], o3: Overloaded3): Rep[MDArray[A]] = KnownAtRuntime[A]("value " + {nValue = nValue + 1; nValue})

  // Basic operations
  def dim[A: ClassManifest](a: Rep[MDArray[A]]): Rep[Int] = ToDim(a)
  def shape[A: ClassManifest](a: Rep[MDArray[A]]): Rep[MDArray[Int]] = ToShape(a)
  def sel[A: ClassManifest](iv: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]] = AssertPrefixLt(iv, shape(a), Sel(iv, a))
  def reshape[A: ClassManifest](iv: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]] = AssertContentSizeEqual(iv, shape(a), Reshape(iv, a))
  def cat[A: ClassManifest](d: Int, one: Rep[MDArray[A]], two: Rep[MDArray[A]]): Rep[MDArray[A]] = AssertEqualExcept(d:Int, shape(one), shape(two), Cat(d, one, two))

  // Zeroes, ones and values
  def values(dim: Rep[Int], value: Rep[Int]): Rep[MDArray[Int]] = With().GenArray(convertFromValueRepRep(dim), iv => value)

  // Where
  def where[A: ClassManifest](p: Rep[MDArray[Boolean]], a: Rep[MDArray[A]], b: Rep[MDArray[A]]): Rep[MDArray[A]] = AssertShapesEqual(p, a, AssertShapesEqual(p, a, With().GenArray(a.shape, iv => if (sel(iv, p)) sel(iv, a) else sel(iv, b))))

  // Restructuring operations - implemented as with-loops
  def genarray[A: ClassManifest](shp: Rep[MDArray[Int]], value: Rep[MDArray[A]]): Rep[MDArray[A]] =
    With().GenArray(shp, iv => value)
  def modarray[A: ClassManifest](a: Rep[MDArray[A]], iv: Rep[MDArray[Int]], value: Rep[MDArray[A]]): Rep[MDArray[A]] =
    AssertPrefixLt(iv, shape(a), With(_lb = iv, _ub = iv).ModArray(a, iv => value))
  // TODO: Redesign these functions for lower dimensions in the given vectors, filling in with zeros or shape elements
  def take[A: ClassManifest](shp: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]] =
    AssertOneDimensional(shp, AssertShapeSameLength(shp, shape(a), AssertShapeGreater(shape(a), shp,
      With().GenArray(shp, iv => sel(iv, a))
    )))
  def drop[A: ClassManifest](shp: Rep[MDArray[Int]], a: Rep[MDArray[A]]): Rep[MDArray[A]] =
    AssertOneDimensional(shp, AssertShapeSameLength(shp, shape(a), AssertShapeGreater(shape(a), shp,
      With().GenArray(shape(a) - shp, iv => sel(iv + shp, a))
    )))
  def tile[A: ClassManifest](sv: Rep[MDArray[Int]], ov: Rep[MDArray[Int]], a:Rep[MDArray[A]]): Rep[MDArray[A]]  =
    AssertOneDimensional(sv, AssertOneDimensional(ov, AssertShapeSameLength(sv, shape(a), AssertShapeSameLength(ov, shape(a), AssertShapeGreater(shape(a), sv + ov,
      With().GenArray(sv, iv => sel(iv + ov, a))
    )))))
  def rotate[A: ClassManifest](ov: Rep[MDArray[Int]], a:Rep[MDArray[A]]): Rep[MDArray[A]] =
    AssertOneDimensional(ov, AssertShapeSameLength(ov, shape(a),
      With().GenArray(shape(a), iv => a(((iv - ov) + shape(a)) % shape(a)))
    ))
  def shift[A: ClassManifest](ov: Rep[MDArray[Int]], expr: A, a: Rep[MDArray[A]]): Rep[MDArray[A]] =
    AssertOneDimensional(ov, AssertShapeSameLength(ov, shape(a),
      With().GenArray(shape(a), iv => if ((any((iv - ov) < zeros(dim(a)))) || (any((iv - ov) >= shape(a)))) expr else a(iv - ov))
    ))

  // Reduction operations on matrices
  def sum[A](a: Rep[MDArray[A]])(implicit ev: Numeric[A], mf: ClassManifest[A]): Rep[A] = reduceA(ev.zero, ev.plus, a, "sum")
  def prod[A](a: Rep[MDArray[A]])(implicit ev: Numeric[A], mf: ClassManifest[A]): Rep[A] = reduceA(ev.one, ev.times, a, "prod")
  def all(a: Rep[MDArray[Boolean]]): Rep[Boolean] = reduceA(true, (x:Boolean, y:Boolean) => x && y, a, "all")
  def any(a: Rep[MDArray[Boolean]]): Rep[Boolean] = reduceA(false, (x:Boolean, y:Boolean) => x || y, a, "any")
  def maxVal[A](a: Rep[MDArray[A]])(implicit ev: Ordering[A], mf: ClassManifest[A]): Rep[A] = reduceB(sel(zeros(dim(a)),a), (a:A, b:A) => if (ev.gt(a, b)) a else b, a, "maxVal")
  def minVal[A](a: Rep[MDArray[A]])(implicit ev: Ordering[A], mf: ClassManifest[A]): Rep[A] = reduceB(sel(zeros(dim(a)),a), (a:A, b:A) => if (ev.lt(a, b)) a else b, a, "minVal")

  // Basic operations on matrices - they appear as private here
  def op[A, B](a:Rep[MDArray[A]], b:Rep[MDArray[A]])(op: (A, A) => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B], ov1: Overloaded3): Rep[MDArray[B]] = AssertShapesEqual(a, b, With().GenArray(shape(a), iv => TwoArgFunctionWrapper(op, opName)(sel(iv, a), sel(iv, b))))
  def op[A, B](a:Rep[MDArray[A]], b:A)(op: (A, A) => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B], ov2: Overloaded4): Rep[MDArray[B]] = With().GenArray(shape(a), iv => TwoArgFunctionWrapper(op, opName)(sel(iv, a), b))
  def uop[A, B](a:Rep[MDArray[A]])(op: A => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B]): Rep[MDArray[B]] = With().GenArray(shape(a), iv => OneArgFunctionWrapper(op, opName)(sel(iv,a)))
  def reduceA[A](z: A, op: (A, A) => A, a: Rep[MDArray[A]], opName: String)(implicit mfA: ClassManifest[A]): Rep[A] = reduceC(Const(z), TwoArgFunctionWrapperOnMatrices(op, opName), a, opName)
  def reduceB[A](z: Rep[MDArray[A]], op: (A, A) => A, a: Rep[MDArray[A]], opName: String)(implicit mfA: ClassManifest[A]): Rep[A] = reduceC(z, TwoArgFunctionWrapperOnMatrices(op, opName), a, opName)
  def reduceC[A](z: Rep[MDArray[A]], op: (Rep[MDArray[A]], Rep[MDArray[A]]) => Rep[MDArray[A]], a: Rep[MDArray[A]], opName: String)(implicit mfA: ClassManifest[A]): Rep[A] = With(_lb = zeros(dim(a)), _ub = shape(a), _ubStrict = true).Fold(op, z, iv => sel(iv, a))

  // With-comprehensions
  def genArrayWith[A: ClassManifest](l: List[Pair[With, Rep[MDArray[Int]]=> Rep[MDArray[A]]]], shp: Rep[MDArray[Int]]): Rep[MDArray[A]] = GenArrayWith(l, shp)
  def modArrayWith[A: ClassManifest](l: List[Pair[With, Rep[MDArray[Int]]=> Rep[MDArray[A]]]], a: Rep[MDArray[A]]): Rep[MDArray[A]] = ModArrayWith(l, a)
  def foldArrayWith[A: ClassManifest](w: With, foldFunction: (Rep[MDArray[A]], Rep[MDArray[A]]) => Rep[MDArray[A]], neutral: Rep[MDArray[A]], f: Rep[MDArray[Int]] => Rep[MDArray[A]]): Rep[MDArray[A]] = FoldArrayWith(w, foldFunction, neutral, f)
}
