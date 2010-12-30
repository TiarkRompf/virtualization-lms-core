package scala.virtualization.lms
package epfl
package test7

import common._
import java.io.PrintWriter
import util.OverloadHack

trait MDArrayBase extends Base {

  class RepMDArray[A: ClassManifest]() {
    def apply(iv: RepMDArray[Int]): RepMDArray[A] = sel(iv, this)
    
    // Element-wise operations
    def +(that: RepMDArray[A])(implicit numeric: Numeric[A]): RepMDArray[A] = op(this, that)(numeric.plus _, "+")
    def -(that: RepMDArray[A])(implicit numeric: Numeric[A]): RepMDArray[A] = op(this, that)(numeric.minus, "-")
    def *(that: RepMDArray[A])(implicit numeric: Numeric[A]): RepMDArray[A] = op(this, that)(numeric.times, "*")
    def /(that: RepMDArray[A])(implicit fractional: Fractional[A]): RepMDArray[A] = op(this, that)(fractional.div, "/[div]")
    def div(that: RepMDArray[A])(implicit integral: Integral[A]): RepMDArray[A] = op(this, that)(integral.quot, "/[quot]")
    def rem(that: RepMDArray[A])(implicit integral: Integral[A]): RepMDArray[A] = op(this, that)(integral.rem, "%[rem]")
    def <(that: RepMDArray[A])(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.lt, "<")
    def <=(that: RepMDArray[A])(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.lteq, "<=")
    def >(that: RepMDArray[A])(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.gt, ">")
    def >=(that: RepMDArray[A])(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.gteq, ">=")
    def ==(that: RepMDArray[A])(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.equiv, "==")
    def !=(that: RepMDArray[A])(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)((a, b) => !ordering.equiv(a, b), "!=")

    def +(that: A)(implicit numeric: Numeric[A]): RepMDArray[A] = op(this, that)(numeric.plus, "+")
    def -(that: A)(implicit numeric: Numeric[A]): RepMDArray[A] = op(this, that)(numeric.minus, "-")
    def *(that: A)(implicit numeric: Numeric[A]): RepMDArray[A] = op(this, that)(numeric.times, "*")
    def /(that: A)(implicit fractional: Fractional[A]): RepMDArray[A] = op(this, that)(fractional.div, "/[div]")
    def div(that: A)(implicit integral: Integral[A]): RepMDArray[A] = op(this, that)(integral.quot, "/[quot]")
    def rem(that: A)(implicit integral: Integral[A]): RepMDArray[A] = op(this, that)(integral.rem, "%[rem]")
    def <(that: A)(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.lt, "<")
    def <=(that: A)(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.lteq, "<=")
    def >(that: A)(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.gt, ">")
    def >=(that: A)(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.gteq, ">=")
    def ==(that: A)(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)(ordering.equiv, "==")
    def !=(that: A)(implicit ordering: Ordering[A]): RepMDArray[Boolean] = op(this, that)((a, b) => !ordering.equiv(a, b), "!=")

    def unary_-()(implicit numeric: Numeric[A]): RepMDArray[A] = uop(this)(numeric.negate, "-[unary]")

    def &&(that: RepMDArray[A])(implicit ev: A =:= Boolean): RepMDArray[Boolean] = op(this, that)((a, b) => ev(a) && ev(b), "&&")
    def ||(that: RepMDArray[A])(implicit ev: A =:= Boolean): RepMDArray[Boolean] = op(this, that)((a, b) => ev(a) || ev(b), "||")
    def unary_! ()(implicit ev: A =:= Boolean): RepMDArray[Boolean] = uop(this)((a) => !(ev(a)), "!")

    // Concatenation shortcut
    def +++(that: RepMDArray[A]): RepMDArray[A] = cat(0, this, that)

    // toString operation
    // XXX: Defined by Any, only needs override at the AST layer
  }

  // Implicit conversions
  implicit def convertFromList[A: ClassManifest](a: List[A]): RepMDArray[A]
  implicit def convertToList[A: ClassManifest](a: RepMDArray[A]): List[A]
  implicit def convertFromArray[A: ClassManifest](a: Array[A]): RepMDArray[A]
  implicit def convertToArray[A: ClassManifest](a: RepMDArray[A]): Array[A]
  implicit def convertFromValue[A: ClassManifest](a: A): RepMDArray[A]
  implicit def convertToValue[A: ClassManifest](a: RepMDArray[A]): A
  implicit def convertFromListOfMDArrays[A: ClassManifest](a: List[RepMDArray[A]]): RepMDArray[A]
  implicit def convertFromArrayOfMDArrays[A: ClassManifest](a: Array[RepMDArray[A]]): RepMDArray[A]
  implicit def convertToRealScalar[A: ClassManifest](a: RepMDArray[A]): RepMDArray[A]

  // Explicit conversions for elements known only at runtime
  def knownOnlyAtRuntime[A: ClassManifest](a: List[A]): RepMDArray[A]
  def knownOnlyAtRuntime[A: ClassManifest](a: Array[A]): RepMDArray[A]

  // TODO: Find some way to allow values to be known at runtime. The OverloadHack doesn't work here
  // since we also have the ClassManifest type bound:
  // "cannot have both implicit parameters and context bounds `: ...' or view bounds `<% ...' on type parameters"
  //def knownOnlyAtRuntime[A: ClassManifest](a: A): RepMDArray[A]

  // With operation
  class With(_lb: RepMDArray[Int] = null,
             _lbStrict: Boolean = false,
             _ub: RepMDArray[Int] = null,
             _ubStrict: Boolean = false,
             _step: RepMDArray[Int] = null,
             _width: RepMDArray[Int] = null) {

    def GenArray[A: ClassManifest](shp: RepMDArray[Int], f: RepMDArray[Int]=> RepMDArray[A]): RepMDArray[A] = genArray((this, f)::Nil, shp)
    def ModArray[A: ClassManifest](a: RepMDArray[A], f: RepMDArray[Int] => RepMDArray[A]): RepMDArray[A] = modArray((this, f)::Nil, a)
    def Fold[A: ClassManifest](foldFunction: (RepMDArray[A], RepMDArray[A]) => RepMDArray[A], neutral: RepMDArray[A], f: RepMDArray[Int] => RepMDArray[A]): RepMDArray[A] = foldArray(this, foldFunction, neutral, f)
  }
  
  object With {
    def apply(_lb: RepMDArray[Int] = null,
           _lbStrict: Boolean = false,
           _ub: RepMDArray[Int] = null,
           _ubStrict: Boolean = false,
           _step: RepMDArray[Int] = null,
           _width: RepMDArray[Int] = null): With = 
    new With(_lb, _lbStrict, _ub, _ubStrict, _step, _width)
  }

  // Language operations
  // Basic operations
  def dim[A: ClassManifest](a: RepMDArray[A]): Int
  def shape[A: ClassManifest](a: RepMDArray[A]): RepMDArray[Int]
  def sel[A: ClassManifest](iv: RepMDArray[Int], a: RepMDArray[A]): RepMDArray[A] = a(iv) 
  def reshape[A: ClassManifest](iv: RepMDArray[Int], a: RepMDArray[A]): RepMDArray[A] 
  def reshape[A: ClassManifest](iv: RepMDArray[Int], a: Array[A], opName: String): RepMDArray[A] 
  def cat[A: ClassManifest](d: Int, one: RepMDArray[A], two: RepMDArray[A]): RepMDArray[A] 

  // Reduction operations on matrices
  def sum[A](a: RepMDArray[A])(implicit ev: Numeric[A]): A 
  def prod[A](a: RepMDArray[A])(implicit ev: Numeric[A]): A 
  def all(a: RepMDArray[Boolean]): Boolean 
  def any(a: RepMDArray[Boolean]): Boolean 
  def maxVal[A](a: RepMDArray[A])(implicit ev: Ordering[A]): A 
  def minVal[A](a: RepMDArray[A])(implicit ev: Ordering[A]): A 

  // Element-Wise operations on matrices
  def max[A: Ordering : ClassManifest](a: RepMDArray[A], b: RepMDArray[A]): RepMDArray[A] 
  def min[A: Ordering : ClassManifest](a: RepMDArray[A], b: RepMDArray[A]): RepMDArray[A] 
  def where[A: ClassManifest](p: RepMDArray[Boolean], a: RepMDArray[A], b: RepMDArray[A]): RepMDArray[A] 
  // note: the rest are defined in the RepMDArray class!

  // Restructuring operations - implemented as with-loops
  def genarray[A: ClassManifest](shp: RepMDArray[Int], value: RepMDArray[A]): RepMDArray[A] 
  def modarray[A: ClassManifest](a: RepMDArray[A], iv: RepMDArray[Int], value: RepMDArray[A]): RepMDArray[A] 
  def take[A: ClassManifest](shp: RepMDArray[Int], a: RepMDArray[A]): RepMDArray[A] 
  def drop[A: ClassManifest](shp: RepMDArray[Int], a: RepMDArray[A]): RepMDArray[A] 
  def tile[A: ClassManifest](sv: RepMDArray[Int], ov: RepMDArray[Int], a:RepMDArray[A]): RepMDArray[A]
  def rotate[A: ClassManifest](ov: RepMDArray[Int], a:RepMDArray[A]): RepMDArray[A] 
  def shift[A: ClassManifest](ov: RepMDArray[Int], expr: A, a:RepMDArray[A]): RepMDArray[A]

  // Basic operations on matrices - they appear as private here
  def op[A: ClassManifest, B: ClassManifest](a:RepMDArray[A], b:RepMDArray[A])(op: (A, A) => B, opName: String): RepMDArray[B]
  def op[A: ClassManifest, B: ClassManifest](a:RepMDArray[A], b:A)(op: (A, A) => B, opName: String): RepMDArray[B]
  def uop[A: ClassManifest, B: ClassManifest](a:RepMDArray[A])(op: A => B, opName: String): RepMDArray[B]

  // With-comprehensions
  def genArray[A: ClassManifest](l: List[Pair[With, RepMDArray[Int]=> RepMDArray[A]]], shp: RepMDArray[Int]): RepMDArray[A]
  def modArray[A: ClassManifest](l: List[Pair[With, RepMDArray[Int]=> RepMDArray[A]]], a: RepMDArray[A]): RepMDArray[A]
  def foldArray[A: ClassManifest](w: With, foldFunction: (RepMDArray[A], RepMDArray[A]) => RepMDArray[A], neutral: RepMDArray[A], f: RepMDArray[Int] => RepMDArray[A]): RepMDArray[A]
}
