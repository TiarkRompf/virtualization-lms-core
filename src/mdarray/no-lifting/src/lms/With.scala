package lms
import lms.MDArray._
import lms.Operations._
import lms.Conversions._
import lms.SpecificOperations._

class With(_lb: IndexVector = null,
           _lbStrict: Boolean = false,
           _ub: IndexVector = null,
           _ubStrict: Boolean = false,
           _step: IndexVector = null,
           _width: IndexVector = null) {

  def GenArray[A: ClassManifest](shp:IndexVector, f: IndexVector => MDArray[A]): MDArray[A] = {
    val opName = "genarray"

    if (shp == null)
      throw new Exception(opName + ": The index vector cannot be null!")

    var iterator = doIteration(shp, opName)

    // 1. Create the array, for which we need the second shape:
    val firstIV = iterator.head
    val fshape = f(firstIV).shape
    val shape = shp.content().toList ::: fshape.content.toList
    val array = new Array(prod(shape)) // Let the implicit conversions do the magic

    // 2. Populate the array and return it
    if (iterator != Stream.empty)
      modifyArray(iterator, f, fshape, array, shape, opName)    
    else
      throw new Exception(opName + ": Nothing to iterate. Please make sure the limits are set correctly.")      
  }

  def ModArray[A: ClassManifest](a: MDArray[A], f: IndexVector => MDArray[A]): MDArray[A] = {
    val opName = "modarray"

    if (a == null)
      throw new Exception(opName + ": The array cannot be null!")

    var iterator = doIteration(a.shape(), opName)

    // 1. Create the array, for which we need the second shape:
    val array = a.content().clone

    // 2. Populate the array and return it
    if (iterator != Stream.empty)
      modifyArray(iterator, f, prefixMinus(iterator.head, a.shape(), opName), array, a.shape, opName)
    else
      a
  }

  def Fold[A: ClassManifest](foldFunction: (MDArray[A], MDArray[A]) => MDArray[A], neutral: MDArray[A], f: IndexVector => MDArray[A]): MDArray[A] = {
    val opName = "fold"

    var iterator = doIteration(null, opName)

    // 1. Create the array, for which we need the second shape:
    var result: MDArray[A] = neutral

    // 2. Compute the fold
    for (i <- iterator)
      result = foldFunction(result, f(i))

    result
  }


  private def doIteration(mainShape: IndexVector, opName: String): Stream[IndexVector] = {

    var lb: IndexVector = _lb
    var ub: IndexVector = _ub

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
          case null => (mainShape - 1)
          case _ => _ub
        }
    }

    if (_lbStrict) lb = lb + 1
    if (_ubStrict) ub = ub - 1    

    val step = _step match {
      case null => ones(lb.content.length)
      case _ => _step
    }

    val width = _width match {
      case null => zeros(lb.content.length)
      case _ => _width
    }

    // Create the iterator and implicitly check the sizes
    iterateWithStep(lb, ub, step, width, opName)
  }

  private def modifyArray[A: ClassManifest](iterator: Stream[Operations.IndexVector],
                                            f: IndexVector => MDArray[A],
                                            expectedShape: IndexVector,
                                            array: Array[A],
                                            shape: Operations.IndexVector,
                                            opName: String): MDArray[A] = {
    
    for (i <- iterator) {
      // Compute the application of f
      val localArray = f(i)
      // implicit conversions should do their magic here:
      val basePointer = flatten(shape, i.content.toList ::: zeros(localArray.content.length), opName)

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
  def apply(_lb: IndexVector = null,
           _lbStrict: Boolean = false,
           _ub: IndexVector = null,
           _ubStrict: Boolean = false,
           _step: IndexVector = null,
           _width: IndexVector = null): With = {
    new With(_lb, _lbStrict, _ub, _ubStrict, _step, _width)
  }
}