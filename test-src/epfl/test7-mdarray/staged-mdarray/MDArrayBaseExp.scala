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

  // Override the type given by the program
  // TODO: Make this work
  //override type Type = Shape

  /*
      Basic AST building blocks
   */
  case class KnownAtRuntime[A: ClassManifest](name: String) extends RichDef[MDArray[A]](Nil) { override def toString() = "KnownAtRuntime(" + name + ")" }
  case class KnownAtCompileTime[A: ClassManifest](value: MDArray[A]) extends RichDef[MDArray[A]](Nil) { override def toString() = "KnownAtCompileTime(" + value.toString + ")" }
  case class ListOfMDArrays[A: ClassManifest](value: List[Exp[MDArray[A]]]) extends RichDef[MDArray[A]](value) { override def toString() = "ListOfMDArrays(" + value.toString + ")" }
  case class ArrayOfMDArrays[A: ClassManifest](value: Array[Exp[MDArray[A]]]) extends RichDef[MDArray[A]](value.toList) { override def toString() = "ArrayOfMDArrays(" + value.toString + ")" }

  // With
  // TODO: Explain function "execution"
  case class GenArrayWith[A: ClassManifest](l: List[WithNode[A]], shp: Exp[MDArray[Int]]) extends RichDef[MDArray[A]](shp::l.map(w => toAtom(w))) { override def toString() = "GenArrayWith(" + shp.toString + " - " + l.mkString(", ") + ")" }
  case class ModArrayWith[A: ClassManifest](l: List[WithNode[A]], a: Exp[MDArray[A]]) extends RichDef[MDArray[A]](a::l.map(w => toAtom(w))) { override def toString() = "ModArrayWith(" + a.toString + " - " + l.mkString(", ") + ")" }
  // TODO: Important implicit assumption made here -- we assume foldFunction has no outside dependencies. According to the SAC spec, it should indeed be the case, but proving it would be better
  case class FoldArrayWith[A: ClassManifest](neutral: Exp[MDArray[A]], foldFunction: (Exp[MDArray[A]], Exp[MDArray[A]]) => Exp[MDArray[A]], w: WithNode[A]) extends RichDef[MDArray[A]](neutral::toAtom(w)::Nil) { override def toString() = "FoldArrayWith(" + neutral + ", " + foldFunction + ", " + w + ")" }
  case class IndexVector(w: With) extends RichDef[MDArray[Int]](w.usedDefs) { override def toString() = "IndexVector(" + w.toString + ")" }
  case class WithNode[A: ClassManifest](iv: IndexVector, f: Exp[MDArray[Int]] => Exp[MDArray[A]]) extends RichDef[Unit](toAtom(iv)::f(iv)::Nil) { override def toString() = "With(" + iv.toString + " => " + f(iv).toString + ")" }


  // Base functions
  case class ToDim[A: ClassManifest](a: Exp[MDArray[A]]) extends RichDef[Int](a::Nil) { override def toString() = "Dim(" + a + ")" }
  case class ToShape[A: ClassManifest](a: Exp[MDArray[A]]) extends RichDef[MDArray[Int]](a::Nil) { override def toString() = "Shape(" + a + ")" }
  case class Reshape[A: ClassManifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]) extends RichDef[MDArray[A]](shp::a::Nil) { override def toString() = "Reshape(" + shp + ", " + a + ")" }
  case class Sel[A: ClassManifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]) extends RichDef[MDArray[A]](iv::a::Nil) { override def toString() = "Sel(" + iv + ", " + a + ")" }
  case class Cat[A: ClassManifest](d: Exp[Int], a: Exp[MDArray[A]], b: Exp[MDArray[A]]) extends RichDef[MDArray[A]](d::a::b::Nil) { override def toString() = "Cat(" + d + ", " + a + ", " + b + ")" }
  case class Reduce[A: ClassManifest, B](z: Exp[B], a: Exp[MDArray[A]], op: (B,A)=>B, opName: String) extends RichDef[A](z::a::Nil) { override def toString() = "Reduce(" + z + ", " + opName + ", " + a + ")" }

  // Assertions
  case class AssertPrefixLt[A: ClassManifest](iv: Exp[MDArray[Int]], shp: Exp[MDArray[Int]]) extends RichDef[Unit](iv::shp::Nil) { override def toString() = "AssertPrefixLt(" + iv + ", " + shp + ")" }
  case class AssertOneDimensional[A: ClassManifest](iv: Exp[MDArray[Int]]) extends RichDef[Unit](iv::Nil) { override def toString() = "AssertOneDimensional(" + iv + ")" }
  case class AssertEqualExcept[A: ClassManifest](d: Exp[Int], shp1: Exp[MDArray[Int]], shp2: Exp[MDArray[Int]]) extends RichDef[Unit](d::shp1::shp2::Nil) { override def toString() = "AssertEqualExcept(" + d + ", " + shp1 + ", " + shp2 + ")" }
  case class AssertContentSizeEqual[A: ClassManifest](a: Exp[MDArray[Int]], b: Exp[MDArray[Int]]) extends RichDef[Unit](a::b::Nil) { override def toString() = "AssertContentSizeEqual(" + a + ", " + b + ")" }
  case class AssertShapesEqual(a: Exp[MDArray[Int]], b: Exp[MDArray[Int]]) extends RichDef[Unit](a::b::Nil) { override def toString() = "AssertShapesEqual(" + a + ", " + b + ")" }
  case class AssertShapeGreater[A: ClassManifest](shpGreater: Exp[MDArray[Int]], shpLower: Exp[MDArray[Int]]) extends RichDef[Unit](shpGreater::shpLower::Nil) { override def toString() = "AssertShapeGreater(" + shpGreater + ", " + shpLower + ")" }
  case class AssertShapeSameLength[A: ClassManifest](shpA: Exp[MDArray[Int]], shpB: Exp[MDArray[Int]]) extends RichDef[Unit](shpA::shpB::Nil) { override def toString() = "AssertShapeSameLength(" + shpA + ", " + shpB + ")" }

  // Conversions within the staged universe
  case class FromList[A: ClassManifest](value: Exp[List[A]]) extends RichDef[MDArray[A]](value::Nil) { override def toString() = "FromList(" + value.toString + ")" }
  case class FromArray[A: ClassManifest](value: Exp[Array[A]]) extends RichDef[MDArray[A]](value::Nil) { override def toString() = "FromArray(" + value.toString + ")" }
  case class FromValue[A: ClassManifest](value: Exp[A]) extends RichDef[MDArray[A]](value::Nil) { override def toString() = "FromValue(" + value.toString + ")" }

  // Values
  case class Values[A: ClassManifest](dim: Exp[Int], value: Exp[A]) extends RichDef[MDArray[A]](dim::value::Nil) { override def toString() = "Values(" + value + ", " + dim + ")"}

  // Going back to the real world
  case class ToList[A: ClassManifest](value: Exp[MDArray[A]]) extends RichDef[List[A]](value::Nil) { override def toString() = "ToList(" + value.toString + ")" }
  case class ToArray[A: ClassManifest](value: Exp[MDArray[A]]) extends RichDef[Array[A]](value::Nil) { override def toString() = "ToArray(" + value.toString + ")" }
  case class ToValue[A: ClassManifest](value: Exp[MDArray[A]]) extends RichDef[A](value::Nil) { override def toString() = "ToValue(" + value.toString + ")" }

  // Operations
  case class InfixOpAA[A: ClassManifest, B: ClassManifest](array1: Exp[MDArray[A]], array2: Exp[MDArray[A]], op: (A, A) => B, opName: String) extends RichDef[MDArray[B]](array1::array2::Nil) { override def toString() = "InfixOpAA(" + opName + ": " + array1 + " and " + array2 + ")" }
  case class InfixOpAE[A: ClassManifest, B: ClassManifest](array: Exp[MDArray[A]], element: Exp[A], op: (A, A) => B, opName: String) extends RichDef[MDArray[B]](array::element::Nil) { override def toString() = "InfixOpAE(" + opName + ": " + array + " and " + element + ")" }
  case class UnaryOp[A: ClassManifest, B: ClassManifest](array: Exp[MDArray[A]], op: A => B, opName: String) extends RichDef[MDArray[B]](array::Nil) { override def toString() = "UnaryOp(" + opName + ": " + array + ")" }

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
  def sel[A: ClassManifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertPrefixLt(iv, shape(a))
    Sel(iv, a)
  }
  def reshape[A: ClassManifest](iv: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertContentSizeEqual(iv, shape(a))
    Reshape(iv, a)
  }
  def cat[A: ClassManifest](d: Int, one: Exp[MDArray[A]], two: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertEqualExcept(d:Int, shape(one), shape(two))
    Cat(d, one, two)
  }

  // Zeroes, ones and values
  def values(dim: Exp[Int], value: Exp[Int]): Exp[MDArray[Int]] = {
    // XXX: Let's make values a primitive, before with loops
    // With().GenArray(convertFromValueRepRep(dim), iv => value)
    Values(dim, value)
  }

  // Where
  def where[A: ClassManifest](p: Exp[MDArray[Boolean]], a: Exp[MDArray[A]], b: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertShapesEqual(shape(p), shape(a))
    assertShapesEqual(shape(p), shape(b))
    With().GenArray(shape(a), iv => if (sel(iv, p)) sel(iv, a) else sel(iv, b))
  }

  // Restructuring operations - implemented as with-loops
  def genarray[A: ClassManifest](shp: Exp[MDArray[Int]], value: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    With().GenArray(shp, iv => value)
  }
  def modarray[A: ClassManifest](a: Exp[MDArray[A]], iv: Exp[MDArray[Int]], value: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertPrefixLt(iv, shape(a))
    With(_lb = iv, _ub = iv).ModArray(a, iv => value)
  }
  // TODO: Redesign these functions for lower dimensions in the given vectors, filling in with zeros or shape elements
  def take[A: ClassManifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertOneDimensional(shp)
    assertShapeSameLength(shp, shape(a))
    assertShapeGreater(shape(a), shp)
    With().GenArray(shp, iv => sel(iv, a))
  }
  def drop[A: ClassManifest](shp: Exp[MDArray[Int]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertOneDimensional(shp)
    assertShapeSameLength(shp, shape(a))
    assertShapeGreater(shape(a), shp)
    With().GenArray(shape(a) - shp, iv => sel(iv + shp, a))
  }
  def tile[A: ClassManifest](sv: Exp[MDArray[Int]], ov: Exp[MDArray[Int]], a:Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertOneDimensional(sv)
    assertOneDimensional(ov)
    assertShapeSameLength(sv, shape(a))
    assertShapeSameLength(ov, shape(a))
    assertShapeGreater(shape(a), sv + ov)
    With().GenArray(sv, iv => sel(iv + ov, a))
  }
  def rotate[A: ClassManifest](ov: Exp[MDArray[Int]], a:Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertOneDimensional(ov)
    assertShapeSameLength(ov, shape(a))
    With().GenArray(shape(a), iv => a(((iv - ov) + shape(a)) % shape(a)))
  }
  def shift[A: ClassManifest](ov: Exp[MDArray[Int]], expr: A, a: Exp[MDArray[A]]): Exp[MDArray[A]] = {
    assertOneDimensional(ov)
    assertShapeSameLength(ov, shape(a))
    With().GenArray(shape(a), iv => if ((any((iv - ov) < zeros(dim(a)))) || (any((iv - ov) >= shape(a)))) expr else a(iv - ov))
  }

  // Reduction operations on matrices
  def sum[A](a: Exp[MDArray[A]])(implicit ev: Numeric[A], mf: ClassManifest[A]): Exp[A] = reduce[A](ev.zero, a, ev.plus, "sum")
  def prod[A](a: Exp[MDArray[A]])(implicit ev: Numeric[A], mf: ClassManifest[A]): Exp[A] = reduce[A](ev.one, a, ev.times, "prod")
  def all(a: Exp[MDArray[Boolean]]): Exp[Boolean] = reduce[Boolean](true, a, (x:Boolean, y:Boolean) => x && y, "all")
  def any(a: Exp[MDArray[Boolean]]): Exp[Boolean] = reduce[Boolean](false, a, (x:Boolean, y:Boolean) => x || y, "any")
  def maxVal[A](a: Exp[MDArray[A]])(implicit ev: Ordering[A], mf: ClassManifest[A]): Exp[A] = reduce[A](sel(zeros(dim(a)),a), a, (a:A, b:A) => if (ev.gt(a, b)) a else b, "maxVal")
  def minVal[A](a: Exp[MDArray[A]])(implicit ev: Ordering[A], mf: ClassManifest[A]): Exp[A] = reduce[A](sel(zeros(dim(a)),a), a, (a:A, b:A) => if (ev.lt(a, b)) a else b, "minVal")

  // Basic operations on matrices - they appear as private here
  def op[A, B](a:Exp[MDArray[A]], b:Exp[MDArray[A]])(op: (A, A) => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B], ov1: Overloaded4): Exp[MDArray[B]] = {
    assertShapesEqual(shape(a), shape(b))
    InfixOpAA(a, b, op, opName)
  }
  def op[A, B](a:Exp[MDArray[A]], b:Exp[A])(op: (A, A) => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B], ov2: Overloaded5): Exp[MDArray[B]] =
    InfixOpAE(a, b, op, opName)
  def uop[A, B](a:Exp[MDArray[A]])(op: A => B, opName: String)(implicit mfA: ClassManifest[A], mfB: ClassManifest[B]): Exp[MDArray[B]] =
    UnaryOp(a, op, opName)
  def reduce[A](z: Exp[A], a: Exp[MDArray[A]], op: (A, A) => A, opName: String)(implicit mfA: ClassManifest[A]): Exp[A] =
    Reduce(z, a, op, opName)

  // With-comprehensions
  def genArrayWith[A: ClassManifest](l: List[Pair[With, Exp[MDArray[Int]]=> Exp[MDArray[A]]]], shp: Exp[MDArray[Int]]): Exp[MDArray[A]] = GenArrayWith(l.map(p => WithNode(IndexVector(p._1), p._2)), shp)
  def modArrayWith[A: ClassManifest](l: List[Pair[With, Exp[MDArray[Int]]=> Exp[MDArray[A]]]], a: Exp[MDArray[A]]): Exp[MDArray[A]] = ModArrayWith(l.map(p => WithNode(IndexVector(p._1), p._2)), a)
  def foldArrayWith[A: ClassManifest](w: With, foldFunction: (Exp[MDArray[A]], Exp[MDArray[A]]) => Exp[MDArray[A]], neutral: Exp[MDArray[A]], f: Exp[MDArray[Int]] => Exp[MDArray[A]]): Exp[MDArray[A]] = FoldArrayWith(neutral, foldFunction, WithNode(IndexVector(w), f))

  // ToString
  def doToString[A](a: Exp[MDArray[A]]) = a.toString()

  // Assertions
  def assertPrefixLt(iv: Exp[MDArray[Int]], shp: Exp[MDArray[Int]]): Exp[Unit] = AssertPrefixLt(iv, shp)
  def assertOneDimensional(iv: Exp[MDArray[Int]]): Exp[Unit] = AssertOneDimensional(iv)
  def assertEqualExcept(d: Exp[Int], shp1: Exp[MDArray[Int]], shp2: Exp[MDArray[Int]]): Exp[Unit] = AssertEqualExcept(d, shp1, shp2)
  def assertContentSizeEqual(shp1: Exp[MDArray[Int]], shp2: Exp[MDArray[Int]]): Exp[Unit] = AssertContentSizeEqual(shp1, shp2)
  def assertShapesEqual(shp1: Exp[MDArray[Int]], shp2: Exp[MDArray[Int]]): Exp[Unit] = AssertShapesEqual(shp1, shp2)
  def assertShapeGreater(shpGreater: Exp[MDArray[Int]], shpLower: Exp[MDArray[Int]]): Exp[Unit] = AssertShapeGreater(shpGreater, shpLower)
  def assertShapeSameLength(shpA: Exp[MDArray[Int]], shpB: Exp[MDArray[Int]]): Exp[Unit] = AssertShapeSameLength(shpA, shpB)

  protected val nothing: Exp[MDArray[Int]] = Nothing
}
