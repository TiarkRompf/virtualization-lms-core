package scala.virtualization.lms
package epfl
package test7
import Conversions._
import SpecificOperations._

object Operations {  
  // Element-Wise operations on matrices
  def max[A: Ordering : ClassManifest](a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(a.>(b, "max"), a, b, "max")
  def min[A: Ordering : ClassManifest](a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(a.>(b, "min"), b, a, "min")
  def where[A: ClassManifest](p: MDArray[Boolean], a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(p, a, b, "where")
  // note: the rest are defined in the MDArray class!

  // Basic operations
  def dim[A: ClassManifest](a: MDArray[A]): Int = a.dim
  def shape[A: ClassManifest](a: MDArray[A]): MDArray[Int] = a.shape
  def sel[A: ClassManifest](iv: MDArray[Int], a: MDArray[A]): MDArray[A] = a.sel(iv)
  def reshape[A: ClassManifest](iv: MDArray[Int], a: MDArray[A]): MDArray[A] = internalReshape(iv, a.content, "reshape")
  def reshape[A: ClassManifest](iv: MDArray[Int], a: Array[A], opName: String): MDArray[A] = internalReshape(iv, a, opName)

  // Reduction operations on matrices
  def sum[A](a: MDArray[A])(implicit ev: Numeric[A], ev2: ClassManifest[A]): A = a.reduce(ev.zero)((a: A, b: A) => ev.plus(a, b))
  def prod[A](a: MDArray[A])(implicit ev: Numeric[A], ev2: ClassManifest[A]): A = a.reduce(ev.one)((a: A, b: A) => ev.times(a, b))
  def all(a: MDArray[Boolean]): Boolean = a.reduce(true)((a, b) => a && b)
  def any(a: MDArray[Boolean]): Boolean = a.reduce(false)((a, b) => a || b)
  def maxVal[A](a: MDArray[A])(implicit ev: Ordering[A], ev2: ClassManifest[A]): A = a.reduce(a.content()(0))((a, b) => if (ev.gt(a, b)) a else b)
  def minVal[A](a: MDArray[A])(implicit ev: Ordering[A], ev2: ClassManifest[A]): A = a.reduce(a.content()(0))((a, b) => if (ev.lt(a, b)) a else b)
  
  // Restructuring operations - implemented as with-loops
  def genarray[A: ClassManifest](shp: MDArray[Int], value: MDArray[A]): MDArray[A] = With().GenArray(shp, iv => value)
  def modarray[A: ClassManifest](a: MDArray[A], iv: MDArray[Int], value: MDArray[A]): MDArray[A] = With(iv, false, iv, false).ModArray(a, iv => value)
  def take[A: ClassManifest](shp: MDArray[Int], a: MDArray[A]): MDArray[A] = {
    val opName = "take"

    if (shp.dim != 1)
      throw new Exception(opName + ": The shape vector shp (" + shp + ") must be one-dimensional")

    if ((a.dim != convertToValue(shp.shape.content()(0))) || (any(shp > a.shape)))
      throw new Exception(opName + ": The given shape and array do not match")

    With().GenArray(shp, iv => a(iv))
  }

  def drop[A: ClassManifest](shp: MDArray[Int], a: MDArray[A]): MDArray[A] = {
    val opName = "drop"

    if (shp.dim != 1)
      throw new Exception(opName + ": The shape vector shp (" + shp + ") must be one-dimensional")

    if ((a.dim != shp.shape.content()(0)) || (any(shp > a.shape)))
      throw new Exception(opName + ": The given shape and array do not match")

    With().GenArray(a.shape - shp, iv => a(shp + iv))
  }

  def tile[A: ClassManifest](sv: MDArray[Int], ov: MDArray[Int], a:MDArray[A]): MDArray[A] = {
    val opName = "tile"

    if (sv.dim != 1)
      throw new Exception(opName + ": The shape vector sv (" + sv + ") must be one-dimensional")

    if (ov.dim != 1)
      throw new Exception(opName + ": The offset vector ov (" + ov + ") must be one-dimensional")

    if ((sv.shape.content()(0) < ov.shape.content()(0)) || (sv.shape.content()(0) > a.dim))
      throw new Exception(opName + ": The shapes given as sv and ov are not compatible")
    val shape = sv + (ov +++ zeros(sv.shape.content()(0) - ov.shape.content()(0)))
    if (any(a.shape < shape +++ zeros(a.dim - shape.shape.content()(0))))
      throw new Exception(opName + ": The size of the tile goes outside the array")

    With().GenArray(sv, iv => a(iv + ov +++ zeros(sv.shape.content()(0) - ov.shape.content()(0))))
  }

  def rotate[A: ClassManifest](ov: MDArray[Int], a:MDArray[A]): MDArray[A] = {
    val opName = "rotate"

    if (ov.dim != 1)
      throw new Exception(opName + ": The offset vector ov (" + ov + ") must be one-dimensional")

    if (a.dim != ov.shape.content()(0))
      throw new Exception(opName + ": The offset vector does not correspond to the array shape")

    With().GenArray(a.shape, iv => a((iv - ov) remGtZero a.shape))
  }

  def shift[A: ClassManifest](ov: MDArray[Int], expr: A, a:MDArray[A]): MDArray[A] = {
    val opName = "shift"

    if (ov.dim != 1)
      throw new Exception(opName + ": The offset vector ov (" + ov + ") must be one-dimensional")

    if (a.dim != ov.shape.content()(0))
      throw new Exception(opName + ": The offset vector does not correspond to the array shape")

    With().GenArray(a.shape, iv => if ((any((iv - ov) < zeros(a.dim))) || (any((iv - ov) >= a.shape))) expr else a(iv - ov))
  }

  def cat[A: ClassManifest](d: Int, one: MDArray[A], two: MDArray[A]): MDArray[A] = {
    val opName = "cat"
    if ((d >= one.shape.content.length) || (d >= two.shape.content.length))
      throw new Exception(opName + ": The concatenation axis is outside the shapes of the two arrays")

    val eq1 = With().ModArray(one.shape, iv => if (iv.content()(0) == d) 0 else one.shape.content()(iv))
    val eq2 = With().ModArray(two.shape, iv => if (iv.content()(0) == d) 0 else two.shape.content()(iv))
    if (!(all(eq1 == eq2)))
      throw new Exception(opName + ": The shapes of the two arrays are not compatible for concatenation")

    val shape:  MDArray[Int] = With().GenArray(one.shape.shape, iv => if (iv.content()(0) == d) one.shape.content()(iv.content()(0)) + two.shape.content()(iv.content()(0)) else one.shape.content()(iv.content()(0)))
    val offset: MDArray[Int] = With().GenArray(one.shape.shape, iv => if (iv.content()(0) == d) one.shape.content()(iv.content()(0)) else 0)

    With().GenArray(shape, iv => if (iv.content()(d) < one.shape.content()(d)) one(iv) else two(iv - offset))
  }

  // Some additional functions
  def zeros(size: Int): MDArray[Int] = new Array[Int](size)
  def ones(size: Int) = value(size, 1)
  def value(size: Int, value: Int): MDArray[Int] = {
    val valArray = new Array[Int](size)
    for (i <- valArray.indices)
      valArray(i) = value
    valArray
  }
}