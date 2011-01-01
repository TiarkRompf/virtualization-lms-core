package lms
import lms.Conversions._
import lms.SpecificOperations._

object Operations {
  type IndexVector = SDArray[Int]
  
  // Element-Wise operations on matrices
  def max[A: Ordering : ClassManifest](a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(a.>(b, "max"), a, b, "max")
  def min[A: Ordering : ClassManifest](a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(a.>(b, "min"), b, a, "min")
  def where[A: ClassManifest](p: MDArray[Boolean], a: MDArray[A], b: MDArray[A]): MDArray[A] = internalWhere(p, a, b, "where")
  // note: the rest are defined in the MDArray class!

  // Basic operations
  def dim[A: ClassManifest](a: MDArray[A]): Int = a.dim
  def shape[A: ClassManifest](a: MDArray[A]): SDArray[Int] = a.shape
  def sel[A: ClassManifest](iv: IndexVector, a: MDArray[A]): MDArray[A] = a.sel(iv)
  def reshape[A: ClassManifest](iv: IndexVector, a: MDArray[A]): MDArray[A] = internalReshape(iv, a.content, "reshape")
  def reshape[A: ClassManifest](iv: IndexVector, a: Array[A], opName: String): MDArray[A] = internalReshape(iv, a, opName)

  // Reduction operations on matrices
  def sum[A](a: MDArray[A])(implicit ev: Numeric[A], ev2: ClassManifest[A]): A = a.reduce(ev.zero)((a: A, b: A) => ev.plus(a, b))
  def prod[A](a: MDArray[A])(implicit ev: Numeric[A], ev2: ClassManifest[A]): A = a.reduce(ev.one)((a: A, b: A) => ev.times(a, b))
  def all(a: MDArray[Boolean]): Boolean = a.reduce(true)((a, b) => a && b)
  def any(a: MDArray[Boolean]): Boolean = a.reduce(false)((a, b) => a || b)
  def maxVal[A](a: MDArray[A])(implicit ev: Ordering[A], ev2: ClassManifest[A]): A = a.reduce(a.content()(0))((a, b) => if (ev.gt(a, b)) a else b)
  def minVal[A](a: MDArray[A])(implicit ev: Ordering[A], ev2: ClassManifest[A]): A = a.reduce(a.content()(0))((a, b) => if (ev.lt(a, b)) a else b)
  
  // Restructuring operations - implemented as with-loops
  def genarray[A: ClassManifest](shp: IndexVector, value: MDArray[A]): MDArray[A] = With().GenArray(shp, iv => value)
  def modarray[A: ClassManifest](a: MDArray[A], iv: IndexVector, value: MDArray[A]): MDArray[A] = With(iv, false, iv, false).ModArray(a, iv => value)
  def take[A: ClassManifest](shp: IndexVector, a: MDArray[A]): MDArray[A] = {
    val opName = "take"
    if ((a.dim != shp.shape()(0)) || (any(shp > a.shape)))
      throw new Exception(opName + ": The given shape and array do not match")
    With().GenArray(shp, iv => a(iv))
  }
  def drop[A: ClassManifest](shp: IndexVector, a: MDArray[A]): MDArray[A] = {
    val opName = "drop"
    if ((a.dim != shp.shape()(0)) || (any(shp > a.shape)))
      throw new Exception(opName + ": The given shape and array do not match")
    With().GenArray(a.shape - shp, iv => a(shp + iv))
  }
  def tile[A: ClassManifest](sv: IndexVector, ov: IndexVector, a:MDArray[A]): MDArray[A] = {
    val opName = "tile"
    if ((sv.shape()(0) < ov.shape()(0)) || (sv.shape()(0) > a.dim))
      throw new Exception(opName + ": The shapes given as sv and ov are not compatible")
    val shape = sv + (ov +++ zeros(sv.shape()(0) - ov.shape()(0)))
    if (any(a.shape < shape +++ zeros(a.dim - shape.shape()(0))))
      throw new Exception(opName + ": The size of the tile goes outside the array")
    With().GenArray(sv, iv => a(iv + ov +++ zeros(sv.shape()(0) - ov.shape()(0))))
  }
  def rotate[A: ClassManifest](ov: IndexVector, a:MDArray[A]): MDArray[A] = {
    val opName = "rotate"
    if (a.dim != ov.shape()(0))
      throw new Exception(opName + ": The offset vector does not correspond to the array shape")
    With().GenArray(a.shape, iv => a((iv - ov) remGtZero a.shape))
  }
  def shift[A: ClassManifest](ov: IndexVector, expr: A, a:MDArray[A]): MDArray[A] = {
    val opName = "shift"
    if (a.dim != ov.shape()(0))
      throw new Exception(opName + ": The offset vector does not correspond to the array shape")
    With().GenArray(a.shape, iv => if ((any((iv - ov) < zeros(a.dim))) || (any((iv - ov) >= a.shape))) expr else a(iv - ov))
  }
  def cat[A: ClassManifest](d: Int, one: MDArray[A], two: MDArray[A]): MDArray[A] = {
    val opName = "cat"
    if ((d >= one.shape.content.length) || (d >= two.shape.content.length))
      throw new Exception(opName + ": The concatenation axis is outside the shapes of the two arrays")

    val eq1 = With().ModArray(one.shape, iv => if (iv(0) == d) 0 else one.shape()(iv))
    val eq2 = With().ModArray(two.shape, iv => if (iv(0) == d) 0 else two.shape()(iv))
    if (!(all(eq1 == eq2)))
      throw new Exception(opName + ": The shapes of the two arrays are not compatible for concatenation")

    val shape:  IndexVector = With().GenArray(one.shape.shape, iv => if (iv(0) == d) one.shape()(iv) + two.shape()(iv) else one.shape()(iv))
    val offset: IndexVector = With().GenArray(one.shape.shape, iv => if (iv(0) == d) one.shape()(iv) else 0)
    With().GenArray(shape, iv => if (iv(d) < one.shape()(d)) one(iv) else two(iv - offset))
  }

  // Some additional functions
  def zeros(size: Int): SDArray[Int] = new SDArray[Int](new Array(size))
  def ones(size: Int) = value(size, 1)
  def value(size: Int, value: Int): SDArray[Int] = new SDArray[Int]({
    val valArray = new Array[Int](size)
    for (i <- valArray.indices)
      valArray(i) = value
    valArray
  })
}