package scala.virtualization.lms
package epfl
package test7
import Operations._
import Conversions._
import SpecificOperations._

class With(_lb: MDArray[Int] = null,
           _lbStrict: Boolean = false,
           _ub: MDArray[Int] = null,
           _ubStrict: Boolean = false,
           _step: MDArray[Int] = null,
           _width: MDArray[Int] = null) {

  def GenArray[A: ClassManifest](shp:MDArray[Int], f: MDArray[Int] => MDArray[A]): MDArray[A] = {
    val opName = "genarray"

    if (shp == null)
      throw new Exception(opName + ": The index vector cannot be null!")
    if (shp.dim != 1)
      throw new Exception(opName + ": The shape vector (" + shp + ") must be one-dimensional")

    val outsideSize = shp.foldLeft(1)((x,y) => x * y) // ops equiv: prod(shp)
    var iterator = doIteration(shp, opName)
    if ((iterator == Stream.empty) && (outsideSize != 0))
      throw new Exception(opName + ": Empty with iteration")

    if (outsideSize == 0) {
      reshape(shp, new Array[A](0))
    } else {
      // 1. Create the array, for which we need the second shape:
      val firstIV = iterator.head
      val fshape = f(firstIV).shape
      val shape: MDArray[Int] = shp.content().toList ::: fshape.content.toList
      val array = new Array(shape.foldLeft(1)((x,y) => x * y)) // ops equiv: prod(shape))

      // 2. Populate the array and return it
      if (iterator != Stream.empty)
        modifyArray(iterator, f, fshape, array, shape, opName)
      else
        throw new Exception(opName + ": Nothing to iterate. Please make sure the limits are set correctly.")
    }
  }

  def ModArray[A: ClassManifest](a: MDArray[A], f: MDArray[Int] => MDArray[A]): MDArray[A] = {
    val opName = "modarray"

    if (a == null)
      throw new Exception(opName + ": The array cannot be null!")

    var iterator = doIteration(a.shape(), opName)

    // 1. Create the array, for which we need the second shape
    // TODO: Check this is deep copy -- not sure about it
    val array = a.content().clone

    // 2. Populate the array and return it
    if (iterator != Stream.empty)
      modifyArray(iterator, f, prefixMinus(iterator.head, a.shape(), opName), array, a.shape, opName)
    else
      a
  }

  def Fold[A: ClassManifest](foldFunction: (MDArray[A], MDArray[A]) => MDArray[A], neutral: MDArray[A], f: MDArray[Int] => MDArray[A]): MDArray[A] = {
    val opName = "fold"

    var iterator = doIteration(null, opName)

    // 1. Create the array, for which we need the second shape:
    var result: MDArray[A] = neutral

    // 2. Compute the fold
    for (i <- iterator)
      result = foldFunction(result, f(i))

    result
  }


  private def doIteration(mainShape: MDArray[Int], opName: String): Stream[MDArray[Int]] = {

    var lb: MDArray[Int] = _lb
    var ub: MDArray[Int] = _ub

    mainShape match {
      case null =>
        (lb, ub) match {
          case (null, _) =>
            throw new Exception(opName+": With lower bound must be specified")
          case (_, null) =>
            throw new Exception(opName+": With upper bound must be specified")
          case _ =>
            ; // do nothing, it's fine
        }
      case _ =>
        lb = _lb match {
          case null => zeros(mainShape.content.length)
          case _ => _lb
        }
        ub = _ub match {
          case null => mainShape.map(x => x - 1)
          case _ => _ub
        }
    }

    val step = _step match {
      case null => ones(lb.content.length)
      case _ => _step
    }

    val width = _width match {
      case null => zeros(lb.content.length)
      case _ => _width
    }
    
    // Create the iterator and implicitly check the sizes
    iterateWithStep(lb, _lbStrict, ub, _ubStrict, step, width, opName)
  }

  private def modifyArray[A: ClassManifest](iterator: Stream[MDArray[Int]],
                                            f: MDArray[Int] => MDArray[A],
                                            expectedShape: MDArray[Int],
                                            array: Array[A],
                                            shape: MDArray[Int],
                                            opName: String): MDArray[A] = {
    
    for (i <- iterator) {
      // Compute the application of f
      val localArray = f(i)
      // implicit conversions should do their magic here:
      val basePointer = flatten(shape, i.content.toList ::: zeros(localArray.dim), opName)

      // Check validity
      if (!shapeEqual(localArray.shape, expectedShape))
        throw new Exception(opName + ": The function given produces different shapes for different index vectors: \n" +
                "f(iv = " + i + ") => MDArray(" + localArray.shape + ") while expecting shape: " + expectedShape)

      // Merge result
      for (j <- Stream.range(0, prod(expectedShape)))
        array(basePointer + j) = localArray.content()(j)
    }

    internalReshape(shape, array, opName)    
  }  
}

object With {
  def apply(_lb: MDArray[Int] = null,
           _lbStrict: Boolean = false,
           _ub: MDArray[Int] = null,
           _ubStrict: Boolean = false,
           _step: MDArray[Int] = null,
           _width: MDArray[Int] = null): With = {
    new With(_lb, _lbStrict, _ub, _ubStrict, _step, _width)
  }
}