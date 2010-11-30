package lms
import Operations._
import SpecificOperations._

object Conversions {
  implicit def convertFromList[A: ClassManifest](a: List[A]): SDArray[A] = new SDArray[A](a.toArray)
  implicit def convertToList[A](a: SDArray[A]): List[A] = a.content.toList
  implicit def convertFromArray[A: ClassManifest](a: Array[A]): SDArray[A] = new SDArray[A](a)
  implicit def convertToArray[A](a: SDArray[A]): Array[A] = a.content
  implicit def convertFromValue[A: ClassManifest](a: A): Scalar[A] = new Scalar[A](a)
  implicit def convertToValue[A](a: Scalar[A]): A = a.value
  implicit def convertFromArrayOfMDArrays[A:ClassManifest](l: Array[MDArray[A]]): MDArray[A] = convertFromListOfMDArrays(l.toList)
  implicit def convertFromListOfMDArrays[A:ClassManifest](l: List[MDArray[A]]): MDArray[A] = {
    if (l.length == 0)
      throw new Exception("convertFromListOfMDArrays: Cannot convert an empty list to a MDArray.")
    else {
      // Verify shapes
      if (l.filter(a => !shapeEqual(a.shape, l.head.shape)).length != 0)
        throw new Exception("convertFromListOfMDArrays: Unhomogenous shapes in list of MDArray.")

      // Add the content
      var result:Array[A] = new Array[A](0)
      l.foreach(a => result = result ++ a.content)

      // Create the objects
      val newShape = cat(0, new SDArray(Array(l.length)), l.head.shape).asInstanceOf[SDArray[Int]]
      val matrix = new MDArray(new SDArray(Array(result.length)), result)

      // Reshape the matrix correctly
      reshape(newShape, matrix)
    }
  }
}