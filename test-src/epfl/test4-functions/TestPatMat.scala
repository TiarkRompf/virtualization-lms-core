package scala.virtualization.lms
package epfl
package test4

import common._
import internal.GraphVizExport
import test1._
import test2._
import test3._


trait Intf {
 type Rep[+T]
 type M[+T] = Rep[Maybe[T]]

 val __match: Matcher
 abstract class Matcher {
   // runs the matcher on the given input
   def runOrElse[T, U](in: Rep[T])(matcher: Rep[T] => M[U]): Rep[U]

   def zero: M[Nothing]
   def one[T](x: Rep[T]): M[T]
   def guard[T](cond: Rep[Boolean], then: => Rep[T]): M[T]
   def isSuccess[T, U](x: Rep[T])(f: Rep[T] => M[U]): Rep[Boolean] // used for isDefinedAt
 }

 abstract class Maybe[+A] {
   def flatMap[B](f: Rep[A] => M[B]): M[B]
   def orElse[B >: A](alternative: => M[B]): M[B]
 }

 implicit def proxyMaybe[A](m: M[A]): Maybe[A]
 implicit def repInt(x: Int): Rep[Int]
 implicit def repBoolean(x: Boolean): Rep[Boolean]
 implicit def repString(x: String): Rep[String]
}

trait Impl extends Intf {
 type Rep[+T] = String

 object __match extends Matcher {
   def runOrElse[T, U](in: Rep[T])(matcher: Rep[T] => M[U]): Rep[U] = 
    ("runOrElse("+ in +", ?" + matcher("?") + ")")
   def zero: M[Nothing] = "zero"
   def one[T](x: Rep[T]): M[T] = "one("+x.toString+")"
   def guard[T](cond: Rep[Boolean], then: => Rep[T]): M[T] = 
    "guard("+cond+","+then+")"
   def isSuccess[T, U](x: Rep[T])(f: Rep[T] => M[U]): Rep[Boolean] = 
    ("isSuccess("+x+", ?" + f("?") + ")")
 }

 implicit def proxyMaybe[A](m: M[A]): Maybe[A] = new Maybe[A] {
   def flatMap[B](f: Rep[A] => M[B]): M[B] = m + ".flatMap(? =>"+ f("?") +")"
   def orElse[B >: A](alternative: => M[B]): M[B] = m + ".orElse("+ alternative +")"
 }

 def repInt(x: Int): Rep[Int] = x.toString
 def repBoolean(x: Boolean): Rep[Boolean] = x.toString
 def repString(x: String): Rep[String] = x
}



class TestPatMat extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test4-"
  
  def `testPatMat1 ` = {
    withOutFile(prefix+"patmat1") {
      trait Prog extends Intf {
        println("NOTE: doesn't work yet :-(")
        def test = repInt(7) match { case 5 => "foo" case _ => "bar" }
        println(test)
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"patmat1")
  }

}
