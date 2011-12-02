package scala.virtualization.lms
package epfl
package test10
package original

import Operations._
import SpecificOperations._

object Conversions {
  implicit def convertFromList[A: ClassManifest](a: List[A]): MDArray[A] = new MDArray[A]((a.length::Nil).toArray, a.toArray)
  implicit def convertToList[A](a: MDArray[A]): List[A] =
    if (a.dim == 1)
      a.content().toList
    else
      throw new Exception("convertToList: The array cannot be converted to a list: "+a)

  implicit def convertFromArray[A: ClassManifest](a: Array[A]): MDArray[A] = new MDArray[A]((a.length::Nil).toArray, a)
  implicit def convertToArray[A](a: MDArray[A]): Array[A] =
    if (a.dim == 1)
      a.content()
    else
      throw new Exception("convertToList: The array cannot be converted to a list: "+a)

  implicit def convertFromValue[A: ClassManifest](a: A): MDArray[A] = new MDArray[A](new Array(0), Array(a))
  implicit def convertToValue[A](a: MDArray[A]): A =
    if (a.dim == 0)
      a.content()(0)
    else
      throw new Exception("convertToValue: The array cannot be converted to a value: "+a)

//  implicit def convertFromArrayOfMDArrays[A:ClassManifest](l: Array[MDArray[A]]): MDArray[A] = convertFromListOfMDArrays(l.toList)
//  implicit def convertFromListOfMDArrays[A:ClassManifest](l: List[MDArray[A]]): MDArray[A] = {
//    if (l.length == 0)
//      throw new Exception("convertFromListOfMDArrays: Cannot convert an empty list to a MDArray.")
//    else {
//      // Verify shapes
//      if (l.filter(a => !shapeEqual(a.shape, l.head.shape)).length != 0)
//        throw new Exception("convertFromListOfMDArrays: Unhomogenous shapes in list of MDArray.")
//
//      // Add the content
//      var result:Array[A] = new Array[A](0)
//      l.foreach(a => result = result ++ a.content)
//
//      // Create the objects
//      val newShape = l.length :: l.head.shape
//
//      // Reshape the matrix correctly
//      reshape(newShape, result)
//    }
//  }
}
