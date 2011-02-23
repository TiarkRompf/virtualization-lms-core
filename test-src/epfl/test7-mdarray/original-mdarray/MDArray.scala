package scala.virtualization.lms
package epfl
package test7
package original

import Conversions._
import Operations._
import SpecificOperations._
import With._

class MDArray[A: ClassManifest](_shape: Array[Int], _content: Array[A]) {
  require(_shape.foldLeft(1)((a,b) => a * b) == _content.length)

  def dim(): Int = _shape.length
  def shape(): MDArray[Int] = _shape
  private def shapeAsArray(): Array[Int] = _shape
  def content(): Array[A] = _content
  def sel(iv: MDArray[Int]): MDArray[A] = {
    val opName = "sel"

    // Check if the array is a vector
    if (iv.dim != 1)
      throw new Exception(opName + ": the index vector (" + iv + ") must be one-dimensional")
    
    // Check the selection size
    if (!prefixLt(iv, shape, opName))
      throw new Exception(opName + ": the index vector (" + iv + ") components are greater or equal to the shape: " + shape)

    _shape.length match {
      case 0 =>
        this
      case 1 =>
        new MDArray(new Array[Int](0), Array(_content(iv.content()(0))))
      case _ =>
        // Compute the new array and iterate
        val suffix: MDArray[Int] = prefixMinus(iv, this.shape, opName)
        val arraySize: Int = prod(suffix)
        val array: Array[A] = new Array[A](arraySize)

        // Compute values in the new array
        for (i <- iterateShape(suffix, opName))
          array(flatten(suffix, i, opName)) = this.content()(flatten(shape, (iv ::: i), opName))

        reshape(suffix, array, opName)
    }
  }
  def apply(iv: MDArray[Int]) = sel(iv)

  // Element-wise operations
  def +(that: MDArray[A])(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.plus, "+")
  def -(that: MDArray[A])(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.minus, "-")
  def *(that: MDArray[A])(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.times, "*")
  def /(that: MDArray[A])(implicit fractional: Fractional[A]): MDArray[A] = op(that)(fractional.div, "/[div]")
  def div(that: MDArray[A])(implicit integral: Integral[A]): MDArray[A] = op(that)(integral.quot, "/[quot]")
  def rem(that: MDArray[A])(implicit integral: Integral[A]): MDArray[A] = op(that)(integral.rem, "%[rem]")
  def <(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.lt, "<")
  def <=(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.lteq, "<=")
  def >(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.gt, ">")
  def >=(that: MDArray[A])(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.gteq, ">=")
  def ===(that: MDArray[A]): MDArray[Boolean] = op(that)((a:A, b:A) => a == b, "===")
  def !==(that: MDArray[A]): MDArray[Boolean] = op(that)((a:A, b:A) => !(a == b), "!==")

  def +(that: A)(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.plus, "+")
  def -(that: A)(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.minus, "-")
  def *(that: A)(implicit numeric: Numeric[A]): MDArray[A] = op(that)(numeric.times, "*")
  def /(that: A)(implicit fractional: Fractional[A]): MDArray[A] = op(that)(fractional.div, "/[div]")
  def div(that: A)(implicit integral: Integral[A]): MDArray[A] = op(that)(integral.quot, "/[quot]")
  def rem(that: A)(implicit integral: Integral[A]): MDArray[A] = op(that)(integral.rem, "%[rem]")
  def <(that: A)(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.lt, "<")
  def <=(that: A)(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.lteq, "<=")
  def >(that: A)(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.gt, ">")
  def >=(that: A)(implicit ordering: Ordering[A]): MDArray[Boolean] = op(that)(ordering.gteq, ">=")
  def ===(that: A): MDArray[Boolean] = op(that)((a:A, b:A) => a == b, "===")
  def !==(that: A): MDArray[Boolean] = op(that)((a:A, b:A) => !(a == b), "!==")

  def unary_-()(implicit numeric: Numeric[A]): MDArray[A] = uop(numeric.negate, "-[unary]")

  def &&(that: MDArray[A])(implicit ev: A =:= Boolean): MDArray[Boolean] = op(that)((a, b) => ev(a) && ev(b), "&&")
  def ||(that: MDArray[A])(implicit ev: A =:= Boolean): MDArray[Boolean] = op(that)((a, b) => ev(a) || ev(b), "||")
  def unary_! ()(implicit ev: A =:= Boolean): MDArray[Boolean] = uop((a) => !(ev(a)), "!")

  // Special purpose definitions
  /** Greater than definition for min and max */
  def >(that: MDArray[A], opName: String)(implicit ordering: Ordering[A]): MDArray[Boolean] =
    op(that)(ordering.gt, opName)
  def remGtZero(that: MDArray[A])(implicit integral: Integral[A]): MDArray[A] = op(that)((a, b) => {
    val result = integral.rem(a, b)
    if (integral.gteq(result, integral.zero))
      result
    else
      integral.plus(b, result)}, "%[rem]")

  // Reducing all array elements, in order
  def reduce(z: A)(f: (A, A)=>A): A =
    With(zeros(this.dim), false, true, this.shape, function = iv => this(iv)).Fold((a:MDArray[A], b:MDArray[A]) => f(a, b), z)

  // Concatenation shortcut (note: it's three pluses instead of two)
  def +++(that: MDArray[A]): MDArray[A] = cat(0, this, that)

  /** Binary element-wise operation */
  private def op[B: ClassManifest](that:MDArray[A])(op: (A, A) => B, opName: String): MDArray[B] = {
    if (!shapeEqual(this.shape, that.shape))
      throw new Exception(opName + ": matrices of different shapes: " + this.shape + " vs " + that.shape)
    // for debugging: {println(opName + "("+ this(iv) + "," + that(iv) + ")"); op(this(iv), that(iv))}
    With(function = iv => op(this(iv), that(iv))).GenArray(this.shape)
  }

  private def op[B: ClassManifest](that:A)(op: (A, A) => B, opName: String): MDArray[B] = {
    With(function = iv => op(this(iv), that)).GenArray(this.shape)
  }

  /** Unary element-wise operation */
  private def uop[B: ClassManifest](op: A => B, opName: String): MDArray[B] = {
    With(function = iv => op(this(iv))).GenArray(this.shape)
  }

  override def toString(): String = _shape.length match {
    case 0 =>
      "Scalar: " + _content(0)
    case 1 =>
      "Vector(" + _content.length + "):" + _content.foldLeft("")((b: String, a: A) => b + " " + a.toString())
    case _ =>
      val sb: StringBuffer = new StringBuffer()
      sb.append("Array(")
      sb.append(shape.toString)
      sb.append("):")

      for (i <- List.range(0, _shape(0))) {
        sb.append("\n")
        for (j <- List.range(0, _shape(1))) {
          sb.append("<")
          val bp = i * (_content.length / _shape(0)) + j * (_content.length / _shape(0) / _shape(1))
          for (k <- List.range(0, (_content.length / _shape(0) / _shape(1)))) {
            if (k!=0) sb.append(" ")
            sb.append(_content(bp + k))
          }
          sb.append(">  ")
        }
      }

      sb.toString()
  }

  // XXX: Staged MDArrayBaseExp: We need to have a working ==
  // and we need to get rid of the == override, so we call it ===

  override def equals(other: Any) = other match {
    case that: MDArray[_] => that.canEqual(this) &&
      checkArrayEquality(this.shapeAsArray, that.shapeAsArray) &&
      checkArrayEquality(this.content, that.content)
    case _ => false
  }

  def canEqual(other: Any): Boolean = other match {
    case that: MDArray[_] => true
    case _ => false
  }

  def checkArrayEquality(a: Array[_], b: Array[_]): Boolean = {
    if (a.length != b.length)
      false
    else {
      var result = true
      for (i <- Stream.range(0, a.length))
        result = result && (a(i) == b(i))
      result
    }
  }

//  TODO: Understand why this is wrong
//  def equals[B](that: MDArray[B])(implicit mfB: ClassManifest[B]): Boolean = {
//    val mfA = implicitly[ClassManifest[A]]
//    println("array == "+mfA.toString+", "+mfB.toString)
//
//    if (mfA == mfB)
//      that.canEqual(this) && this.shape == that.shape && all(this === that.asInstanceOf[MDArray[A]])
//    else
//      false
//  }
//  override def equals(other: Any): Boolean = {println("simple =="); false}

  override def hashCode = content.foldLeft(41)((b,a) => b + 41 * a.hashCode)
}