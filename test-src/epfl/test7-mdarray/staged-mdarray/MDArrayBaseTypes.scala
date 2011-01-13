package scala.virtualization.lms
package epfl
package test7

import common._
import original._
import original.Conversions._

//class Environment {
//  var varCount = 0
//  // TODO: Add constraints here
//}
//
//abstract class Shape {
//  def infix_+(other: Shape): Shape = other match {
//    case c: Concat => (this + c.l.head) + Concat(c.l.tail)
//    case _ => Concat(this::other::Nil)
//  }
//}
//
//case class Concat(l: List[Shape]) extends Shape {
//  override def infix_+(other: Shape) = other match {
//    case c: Concat => Concat(this.l ::: c.l)
//    case _ => super.infix_+(other)
//  }
//}
//
//case class Fixed(l: List[Int]) extends Shape {
//  override def infix_+(other: Shape) = other match {
//    case that: Fixed => Fixed(this.l ::: that.l)
//    case _ => super.infix_+(other)
//  }
//}
//
//case class KnownLength(n: Int) extends Shape {
//  override def infix_+(other: Shape) = other match {
//    case that: KnownLength => KnownLength(this.n + that.n)
//    case _ => super.infix_+(other)
//  }
//}
//
//case class Variable(n: Int) extends Shape
//
//trait MDArrayBaseTypes extends MDArrayBaseExp {
//
//  // Override the type given by the program
//  // TODO: Make this work
//  //override type Type = Shape
//
//  /*
//      The types listed here are copied from SAC
//      TODO: It would be interesting to use a type reconstruction system and infer as much as possible of the type
//      TODO: Decide if the above idea is useful. Would knowing the shape of a matrix is 5:5:5:6:7:...:9:2:3?
//      XXX: It would be useful for type checking and displaying errors as soon as possible - do we want that?
//      TODO: The current implementation of types does not use subtyping correctly. "case class `class KnownShapeDim' has case class ancestor `class AnyShape'.  This has been deprecated for unduly complicating both usage and implementation.  You should instead use extractors for pattern matching on non-leaf nodes."
//   */
//  abstract class Shape
//  case object AnyShape extends Shape
//  case object PlusShape extends Shape
//  case class KnownShapeDim(dim: Int) extends Shape
//  case class KnownShape(shape: MDArray[Int]) extends Shape
//
//}