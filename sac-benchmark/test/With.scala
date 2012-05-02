package scala.virtualization.lms
package epfl
package test7
package original

import Operations._
import Conversions._
import SpecificOperations._

class With[A: ClassManifest](
           lb: MDArray[Int] = null,
           lbStrict: Boolean = false,
           ubStrict: Boolean = false,
           ub: MDArray[Int] = null,
           step: MDArray[Int] = null,
           width: MDArray[Int] = null,
           function: (MDArray[Int] => MDArray[A])) {

  def GenArray(shp:MDArray[Int]): MDArray[A] = {
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
      val fshape = function(firstIV).shape
      val shape: MDArray[Int] = shp.content().toList ::: fshape.content.toList
      val array = new Array(shape.foldLeft(1)((x,y) => x * y)) // ops equiv: prod(shape))

      // 2. Populate the array and return it
      if (iterator != Stream.empty)
        modifyArray(iterator, function, fshape, array, shape, opName)
      else
        throw new Exception(opName + ": Nothing to iterate. Please make sure the limits are set correctly.")
    }
  }

  def ModArray(a: MDArray[A]): MDArray[A] = {
    val opName = "modarray"

    if (a == null)
      throw new Exception(opName + ": The array cannot be null!")

    var iterator = doIteration(a.shape(), opName)

    // 1. Create the array, for which we need the second shape
    // TODO: Check this is deep copy -- not sure about it
    val array = a.content().clone

    // 2. Populate the array and return it
    if (iterator != Stream.empty)
      modifyArray(iterator, function, prefixMinus(iterator.head, a.shape(), opName), array, a.shape, opName)
    else
      a
  }

  def Fold(foldFunction: (MDArray[A], MDArray[A]) => MDArray[A], neutral: MDArray[A]): MDArray[A] = {
    val opName = "fold"

    var iterator = doIteration(null, opName)

    // 1. Create the array, for which we need the second shape:
    var result: MDArray[A] = neutral

    // 2. Compute the fold
    for (i <- iterator)
      result = foldFunction(result, function(i))

    result
  }


  private def doIteration(mainShape: MDArray[Int], opName: String): Stream[MDArray[Int]] = {

    var _lb: MDArray[Int] = lb
    var _ub: MDArray[Int] = ub

    mainShape match {
      case null =>
        (_lb, _ub) match {
          case (null, _) =>
            throw new Exception(opName+": With lower bound must be specified")
          case (_, null) =>
            throw new Exception(opName+": With upper bound must be specified")
          case _ =>
            ; // do nothing, it's fine
        }
      case _ =>
        _lb = lb match {
          case null => zeros(mainShape.content.length)
          case _ => lb
        }
        _ub = ub match {
          case null => mainShape.map(x => x - 1)
          case _ => ub
        }
    }

    val _step = step match {
      case null => ones(_lb.content.length)
      case _ => step
    }

    val _width = width match {
      case null => zeros(_lb.content.length)
      case _ => width
    }
    
    // Create the iterator and implicitly check the sizes
    iterateWithStep(_lb, lbStrict, ubStrict, _ub, _step, _width, opName)
  }

  private def modifyArray(iterator: Stream[MDArray[Int]],
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
  def apply[A: ClassManifest](lb: MDArray[Int] = null,
           lbStrict: Boolean = false,
           ubStrict: Boolean = false,
           ub: MDArray[Int] = null,
           step: MDArray[Int] = null,
           width: MDArray[Int] = null,
           function: MDArray[Int] => MDArray[A]): With[A] = {
    new With[A](lb, lbStrict, ubStrict, ub, step, width, function)
  }
}