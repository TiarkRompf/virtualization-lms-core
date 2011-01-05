package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

trait MDArrayBaseExp extends MDArrayBase with BaseExp with IfThenElseExp {
  /*
      Some counters
   */
  var nList: Int = 0
  var nArray: Int = 0
  var nValue: Int = 0

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
  case class KnownAtRuntime[A: ClassManifest](name: String) extends Def[MDArray[A]] { override def toString() = "KnownAtRuntime(" + name + ")" }
  case class KnownAtCompileTime[A: ClassManifest](value: MDArray[A]) extends Def[MDArray[A]] { override def toString() = "KnownAtCompileTime(" + value.toString + ")" }
  case class ListOfMDArrays[A: ClassManifest](value: List[Exp[MDArray[A]]]) extends Def[MDArray[A]] { override def toString() = "ListOfMDArrays(" + value.toString + ")" }
  case class ArrayOfMDArrays[A: ClassManifest](value: Array[Exp[MDArray[A]]]) extends Def[MDArray[A]] { override def toString() = "ArrayOfMDArrays(" + value.toString + ")" }

  // With
  case class GenArrayWith[A](l: List[Pair[With, Exp[MDArray[Int]]=> Exp[MDArray[A]]]], shp: Exp[MDArray[Int]])(implicit mf: ClassManifest[A]) extends Def[MDArray[A]] { override def toString() = "GenArrayWith(" + shp.toString + " - " + l.foldLeft("")((left, p) => left + "(" + p._1 + " => " + p._2 + ")") + ")" }
  case class ModArrayWith[A](l: List[Pair[With, Exp[MDArray[Int]]=> Exp[MDArray[A]]]], a: Exp[MDArray[A]])(implicit mf: ClassManifest[A]) extends Def[MDArray[A]] { override def toString() = "ModArrayWith(" + a.toString + " - " + l.foldLeft("")((left, p) => left + "(" + p._1 + " => " + p._2 + ")") + ")" }
  case class FoldArrayWith[A](w: With, foldFunction: (Exp[MDArray[A]], Exp[MDArray[A]]) => Exp[MDArray[A]], neutral: Exp[MDArray[A]], f: Exp[MDArray[Int]] => Exp[MDArray[A]])(implicit mf: ClassManifest[A]) extends Def[MDArray[A]] { override def toString() = "FoldArrayWith(" + w.toString + ", " + foldFunction + ", " + neutral + ", " + f }

  // Base functions
  case class ToDim[A: ClassManifest](a: Exp[MDArray[A]]) extends Def[Int] { override def toString() = "Dim(" + a + ")" }
  case class ToShape[A: ClassManifest](a: Exp[MDArray[A]]) extends Def[MDArray[Int]] { override def toString() = "Shape(" + a + ")" }
  case class Reshape[A: ClassManifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "Reshape(" + shp + ", " + a + ")" }
  case class Sel[A: ClassManifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "Sel(" + iv + ", " + a + ")" }
  case class Cat[A: ClassManifest](d: Exp[Int], a: Exp[MDArray[A]], b: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "Cat(" + d + ", " + a + ", " + b + ")" }
  case class Reduce[A: ClassManifest, B](z: B, op: (B,A)=>B, a: Exp[MDArray[A]]) extends Def[A] { override def toString() = "Reduce(" + z + ", " + op + ", " + a + ")" }

  // Assertions
  case class AssertPrefixLt[A: ClassManifest](iv: Exp[MDArray[Int]], shp: Exp[MDArray[Int]], andThen: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "AssertPrefixLt(" + iv + ", " + shp + ", " + andThen + ")" }
  case class AssertOneDimensional[A: ClassManifest](iv: Exp[MDArray[Int]], andThen: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "AssertOneDimensional(" + iv + ", " + andThen + ")" }
  case class AssertEqualExcept[A: ClassManifest](d: Exp[Int], shp1: Exp[MDArray[Int]], shp2: Exp[MDArray[Int]], andThen: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "AssertEqualExcept(" + d + ", " + shp1 + ", " + shp2 + ", " + andThen + ")" }
  case class AssertContentSizeEqual[A: ClassManifest](a: Exp[MDArray[Int]], b: Exp[MDArray[Int]], andThen: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "AssertContentSizeEqual(" + a + ", " + b + ", " + andThen + ")" }
  case class AssertShapesEqual[A: ClassManifest, B: ClassManifest](a: Exp[MDArray[A]], b: Exp[MDArray[A]], andThen: Exp[MDArray[B]]) extends Def[MDArray[B]] { override def toString() = "AssertShapesEqual(" + a + ", " + b + ", " + andThen + ")" }
  case class AssertShapeGreater[A: ClassManifest](shpGreater: Exp[MDArray[Int]], shpLower: Exp[MDArray[Int]], andThen: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "AssertShapeGreater(" + shpGreater + ", " + shpLower + ", " + andThen + ")" }
  case class AssertShapeSameLength[A: ClassManifest](shpA: Exp[MDArray[Int]], shpB: Exp[MDArray[Int]], andThen: Exp[MDArray[A]]) extends Def[MDArray[A]] { override def toString() = "AssertShapeSameLength(" + shpA + ", " + shpB + ", " + andThen + ")" }

  // Conversions within the staged universe
  case class FromList[A: ClassManifest](value: Exp[List[A]]) extends Def[MDArray[A]] { override def toString() = "FromList(" + value.toString + ")" }
  case class FromArray[A: ClassManifest](value: Exp[Array[A]]) extends Def[MDArray[A]] { override def toString() = "FromArray(" + value.toString + ")" }
  case class FromValue[A: ClassManifest](value: Exp[A]) extends Def[MDArray[A]] { override def toString() = "FromValue(" + value.toString + ")" }

  // Going back to the real world
  case class ToList[A: ClassManifest](value: Exp[MDArray[A]]) extends Def[List[A]] { override def toString() = "ToList(" + value.toString + ")" }
  case class ToArray[A: ClassManifest](value: Exp[MDArray[A]]) extends Def[Array[A]] { override def toString() = "ToArray(" + value.toString + ")" }
  case class ToValue[A: ClassManifest](value: Exp[MDArray[A]]) extends Def[A] { override def toString() = "ToValue(" + value.toString + ")" }

  // FunctionWrapper
  case class TwoArgFunctionWrapper[A, B](f: (A, A) => B, opName: String = "<no name>") extends ((Exp[A], Exp[A]) => Exp[B]) {
    override def apply(a: Exp[A], b: Exp[A]) = TwoArgApplication(this, a, b)
    override def toString() = "TwoArgFunctionWrapper(" + opName + ")"
  }
  case class OneArgFunctionWrapper[A, B](f: (A) => B, opName: String = "<no name>") extends (Exp[A] => Exp[B]) {
    override def apply(a: Exp[A]) = OneArgApplication(this, a)
    override def toString() = "OneArgFunctionWrapper(" + opName + ")"
  }
  case class TwoArgFunctionWrapperOnMatrices[A, B](f: (A, A) => B, opName: String = "<no name>") extends ((Exp[MDArray[A]], Exp[MDArray[A]]) => Exp[MDArray[B]]) {
    override def apply(a: Exp[MDArray[A]], b: Exp[MDArray[A]]) = TwoArgApplication(this, a, b)
    override def toString() = "TwoArgFunctionWrapperOnMatrices(" + opName + ")"
  }
  case class OneArgFunctionWrapperOnMatrices[A, B](f: (A) => B, opName: String = "<no name>") extends (Exp[MDArray[A]] => Exp[MDArray[B]]) {
    override def apply(a: Exp[MDArray[A]]) = OneArgApplication(this, a)
    override def toString() = "OneArgFunctionWrapperOnMatrices(" + opName + ")"
  }
  case class TwoArgApplication[A, B, C](f: (Exp[A], Exp[B]) => Exp[C], a1: Exp[A], a2: Exp[B]) extends Def[C] { override def toString() = "TwoArgApplication(" + f + ", " + a1 + ", " + a2 + ")" }
  case class OneArgApplication[A, B](f: Exp[A] => Exp[B], a1: Exp[A]) extends Def[B] { override def toString() = "OneArgApplication(" + f + ", " + a1 + ")" }
  case object Nothing extends Def[MDArray[Int]] { override def toString() = "<null>" }

  /*
      Abstract function implementation
   */
  // Implicit conversions
  implicit def convertFromListRep[A: ClassManifest](a: List[A]): Exp[MDArray[A]] = KnownAtCompileTime(a)
  implicit def convertFromArrayRep[A: ClassManifest](a: Array[A]): Exp[MDArray[A]] = KnownAtCompileTime(a)
  implicit def convertFromValueRep[A: ClassManifest](a: A): Exp[MDArray[A]] = KnownAtCompileTime(a)
  implicit def convertFromListOfMDArraysRep[A: ClassManifest](a: List[Exp[MDArray[A]]]): Exp[MDArray[A]] = ListOfMDArrays(a)
  implicit def convertFromArrayOfMDArraysRep[A: ClassManifest](a: Array[Exp[MDArray[A]]]): Exp[MDArray[A]] = ArrayOfMDArrays(a)

  // Implicit conversions from unknown elements
  implicit def convertFromListRepRep[A: ClassManifest](a: Exp[List[A]]): Exp[MDArray[A]] = FromList(a)
  implicit def convertFromArrayRepRep[A: ClassManifest](a: Exp[Array[A]]): Exp[MDArray[A]] = FromArray(a)
  implicit def convertFromValueRepRep[A: ClassManifest](a: Exp[A]): Exp[MDArray[A]] = FromValue(a)

  // TODO: Add type assertions here for the array
  // TODO: Change the assertions mechanism such that it can accomomdate type assertions on non-MDArray objects
  implicit def convertToListRep[A: ClassManifest](a: Exp[MDArray[A]]): Exp[List[A]] = ToList(a)
  implicit def convertToArrayRep[A: ClassManifest](a: Exp[MDArray[A]]): Exp[Array[A]] = ToArray(a)
  implicit def convertToValueRep[A: ClassManifest](a: Exp[MDArray[A]]): Exp[A] = ToValue(a)

  // Explicit conversions for elements known only at runtime
  def knownOnlyAtRuntime[A](a: List[A])(implicit mf: ClassManifest[A], o1: Overloaded1): Exp[MDArray[A]] = KnownAtRuntime[A]("list " + {nList = nList + 1; nList})
  def knownOnlyAtRuntime[A](a: Array[A])(implicit mf: ClassManifest[A], o2: Overloaded2): Exp[MDArray[A]] = KnownAtRuntime[A]("array " + {nArray = nArray + 1; nArray})
  def knownOnlyAtRuntime[A](a: A)(implicit mf: ClassManifest[A], o3: Overloaded3): Exp[MDArray[A]] = KnownAtRuntime[A]("value " + {nValue = nValue + 1; nValue})

  // Basic operations
  def dim[A: ClassManifest](a: Exp[MDArray[A]]): Exp[Int] = ToDim(a)
  def shape[A: ClassManifest](a: Exp[MDArray[A]]): Exp[MDArray[Int]] = ToShape(a)
  def sel[A: ClassManifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = AssertPrefixLt(iv, shape(a), toAtom(Sel(iv, a)))
  def reshape[A: ClassManifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = AssertContentSizeEqual(iv, shape(a), toAtom(Reshape(iv, a)))
  def cat[A: ClassManifest](d: Int, one: Exp[MDArray[A]], two: Exp[MDArray[A]]): Exp[MDArray[A]] = AssertEqualExcept(d:Int, shape(one), shape(two), toAtom(Cat(d, one, two)))

  // Zeroes, ones and values
  def values(dim: Exp[Int], value: Exp[Int]): Exp[MDArray[Int]] = With().GenArray(convertFromValueRepRep(dim), iv => value)

  // Where
  def where[A: ClassManifest](p: Exp[MDArray[Boolean]], a: Exp[MDArray[A]], b: Exp[MDArray[A]]): Exp[MDArray[A]] = AssertShapesEqual(p, a, toAtom(AssertShapesEqual(p, a, With().GenArray(a.shape, iv => if (sel(iv, p)) sel(iv, a) else sel(iv, b)))))

  // Restructuring operations - implemented as with-loops
  def genarray[A: ClassManifest](shp: Exp[MDArray[Int]], value: Exp[MDArray[A]]): Exp[MDArray[A]] =
    With().GenArray(shp, iv => value)
  def modarray[A: ClassManifest](a: Exp[MDArray[A]], iv: Exp[MDArray[Int]], value: Exp[MDArray[A]]): Exp[MDArray[A]] =
    AssertPrefixLt(iv, shape(a), With(_lb = iv, _ub = iv).ModArray(a, iv => value))
  // TODO: Redesign these functions for lower dimensions in the given vectors, filling in with zeros or shape elements
  def take[A: ClassManifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] =
    AssertOneDimensional(shp, toAtom(AssertShapeSameLength(shp, shape(a), toAtom(AssertShapeGreater(shape(a), shp,
      With().GenArray(shp, iv => sel(iv, a))
    )))))
  def drop[A: ClassManifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] =
    AssertOneDimensional(shp, toAtom(AssertShapeSameLength(shp, shape(a), toAtom(AssertShapeGreater(shape(a), shp,
      With().GenArray(shape(a) - shp, iv => sel(iv + shp, a))
    )))))
  def tile[A: ClassManifest](sv: Exp[MDArray[Int]], ov: Exp[MDArray[Int]], a:Exp[MDArray[A]]): Exp[MDArray[A]]  =
    AssertOneDimensional(sv, toAtom(AssertOneDimensional(ov, toAtom(AssertShapeSameLength(sv, shape(a), toAtom(AssertShapeSameLength(ov, shape(a), toAtom(AssertShapeGreater(shape(a), sv + ov,
      With().GenArray(sv, iv => sel(iv + ov, a))
    )))))))))
  def rotate[A: ClassManifest](ov: Exp[MDArray[Int]], a:Exp[MDArray[A]]): Exp[MDArray[A]] =
    AssertOneDimensional(ov, toAtom(AssertShapeSameLength(ov, shape(a),
      With().GenArray(shape(a), iv => a(((iv - ov) + shape(a)) % shape(a)))
    )))
  def shift[A: ClassManifest](ov: Exp[MDArray[Int]], expr: A, a: Exp[MDArray[A]]): Exp[MDArray[A]] =
    AssertOneDimensional(ov, toAtom(AssertShapeSameLength(ov, shape(a),
      With().GenArray(shape(a), iv => if ((any((iv - ov) < zeros(dim(a)))) || (any((iv - ov) >= shape(a)))) expr else a(iv - ov))
    )))

  // Reduction operations on matrices
  def sum[A](a: Exp[MDArray[A]])(implicit ev: Numeric[A], mf: ClassManifest[A]): Exp[A] = reduceA(ev.zero, ev.plus, a, "sum")
  def prod[A](a: Exp[MDArray[A]])(implicit ev: Numeric[A], mf: ClassManifest[A]): Exp[A] = reduceA(ev.one, ev.times, a, "prod")
  def all(a: Exp[MDArray[Boolean]]): Exp[Boolean] = reduceA(true, (x:Boolean, y:Boolean) => x && y, a, "all")
  def any(a: Exp[MDArray[Boolean]]): Exp[Boolean] = reduceA(false, (x:Boolean, y:Boolean) => x || y, a, "any")
  def maxVal[A](a: Exp[MDArray[A]])(implicit ev: Ordering[A], mf: ClassManifest[A]): Exp[A] = reduceB(sel(zeros(dim(a)),a), (a:A, b:A) => if (ev.gt(a, b)) a else b, a, "maxVal")
  def minVal[A](a: Exp[MDArray[A]])(implicit ev: Ordering[A], mf: ClassManifest[A]): Exp[A] = reduceB(sel(zeros(dim(a)),a), (a:A, b:A) => if (ev.lt(a, b)) a else b, a, "minVal")

  // Basic operations on matrices - they appear as private here
  def op[A, B](a:Exp[MDArray[A]], b:Exp[MDArray[A]])(op: (A, A) => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B], ov1: Overloaded4): Exp[MDArray[B]] = AssertShapesEqual(a, b, With().GenArray(shape(a), iv => TwoArgFunctionWrapper(op, opName)(sel(iv, a), sel(iv, b))))
  def op[A, B](a:Exp[MDArray[A]], b:Exp[A])(op: (A, A) => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B], ov2: Overloaded5): Exp[MDArray[B]] = With().GenArray(shape(a), iv => TwoArgFunctionWrapper(op, opName)(sel(iv, a), b))
  def uop[A, B](a:Exp[MDArray[A]])(op: A => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B]): Exp[MDArray[B]] = With().GenArray(shape(a), iv => OneArgFunctionWrapper(op, opName)(sel(iv,a)))
  def reduceA[A](z: A, op: (A, A) => A, a: Exp[MDArray[A]], opName: String)(implicit mfA: ClassManifest[A]): Exp[A] = reduceC(Const(z), TwoArgFunctionWrapperOnMatrices(op, opName), a, opName)
  def reduceB[A](z: Exp[MDArray[A]], op: (A, A) => A, a: Exp[MDArray[A]], opName: String)(implicit mfA: ClassManifest[A]): Exp[A] = reduceC(z, TwoArgFunctionWrapperOnMatrices(op, opName), a, opName)
  def reduceC[A](z: Exp[MDArray[A]], op: (Exp[MDArray[A]], Exp[MDArray[A]]) => Exp[MDArray[A]], a: Exp[MDArray[A]], opName: String)(implicit mfA: ClassManifest[A]): Exp[A] = With(_lb = zeros(dim(a)), _ub = shape(a), _ubStrict = true).Fold(op, z, iv => sel(iv, a))

  // With-comprehensions
  def genArrayWith[A: ClassManifest](l: List[Pair[With, Exp[MDArray[Int]]=> Exp[MDArray[A]]]], shp: Exp[MDArray[Int]]): Exp[MDArray[A]] = GenArrayWith(l, shp)
  def modArrayWith[A: ClassManifest](l: List[Pair[With, Exp[MDArray[Int]]=> Exp[MDArray[A]]]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = ModArrayWith(l, a)
  def foldArrayWith[A: ClassManifest](w: With, foldFunction: (Exp[MDArray[A]], Exp[MDArray[A]]) => Exp[MDArray[A]], neutral: Exp[MDArray[A]], f: Exp[MDArray[Int]] => Exp[MDArray[A]]): Exp[MDArray[A]] = FoldArrayWith(w, foldFunction, neutral, f)

  // ToString
  def doToString[A](a: Exp[MDArray[A]]) = a.toString()

  protected val nothing: Exp[MDArray[Int]] = Nothing
}
