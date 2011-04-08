package scala.virtualization.lms
package epfl
package test8

import common._
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


/*

  THIS DOESN'T WORK YET -- see #4363 (fixed in 2.9.RC1 but not yet merged into scala-virtualized)
  
*/

class TestMutation extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test8-"
  
  trait DSL extends Arith with Variables with IfThenElse with Print {
    def test(x: Rep[Int]): Rep[Any]
  }
  trait Impl extends ArithExp with VariablesExp with IfThenElseExp with PrintExp { self => 
    val codegen = new ScalaGenArith with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }
  
  def testMutation1 = {
    withOutFile(prefix+"mutation1") {
      trait Prog extends Arith {
        def test(x: Rep[Int]) = {
/*
Restriction: cannot have arbitrary instances that might be mutable or immutable
Result: you must explicitly mark mutable objects (and/or have separate mutable/immutable objects)
Reason:

val a = if (..) Vector1 else Vector3
a(i) = …
val b = Vector3
b map { .. }
a map { .. }

If everything is potentially mutable, you cannot have CSE, fusing, etc. unless you can prove disjointly mutable
*/
          x
        }
      }
      
      new Arith with ArithExp { }
      println("bla")
    }
    assertFileEqualsCheck(prefix+"mutation1")
  }

  def testMutation2 = {
    withOutFile(prefix+"mutation2") {
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
/*
Restriction: cannot have nested mutable objects
Result: compiler error on assignment
Reason:
val a: Vector[Vectors[Int]] = … //mutable
for (i <- …) {
a(i) = b      // b mutable
}
for (i <- …) {
a(i) = c
}
Question: are a and b separate? too hard to analyze precisely ...
*/
          x
        }
      }
      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation2")
  }

  def testMutation3 = {
    withOutFile(prefix+"mutation3") {
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
/*
Restriction: mutable objects cannot potentially alias
Result: compiler error
Reason: consider:

val a = new Vector[Vector[Int]].mutable
val b = new Vector[Int].mutable
for (i <- ...) {
a(i) = … // #1
b(i) = .... // #2
f(a)         // serialized with #1
g(b)        // 
a(i) = b
}

We build the Read/Write dependencies as we encounter nodes, but a(i) = b is encountered last, 
which creates an alias, so we have missing dependencies in the schedule.
*/
          x
        }
      }
      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation3")
  }

  def testMutation4 = {
    withOutFile(prefix+"mutation4") {
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
/*
Restriction: vars cannot reference mutable objects
Result: compiler error
Reason:
var a = Vector.zeros(100).mutable
val b = new Vector[Int].mutable
for (i <- ...) {
a = ...
b(i) = ....
f(a)
g(b)
a = b
}

Like before, on the second iteration a=b, but the b operations are only ordered 
with respect to operations using b. The aliasing operation is encountered last, 
so we have missing dependencies.

val c = a + b
val u = f(c) // costly!!

for (i <- …) {
val d = a +b 
val v = f(d) // costly!! // is u == v? if we don’t now what follows we have to assume no
….....
}

if we later find that we don’t have to calculate a+b a new to define 
d (but could reuse c), we have to modify everything that depends on d
*/
          x
        }
      }
      
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"mutation4")
  }

}