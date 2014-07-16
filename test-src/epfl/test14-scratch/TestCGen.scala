package scala.virtualization.lms
package epfl
package test14

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}



class TestCGen extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test14-"
  
  trait DSL extends ScalaOpsPkg with TupledFunctions with UncheckedOps with LiftPrimitives with LiftString with LiftVariables {
    // keep track of top level functions
    case class TopLevel[A,B](name: String, mA: Manifest[A], mB:Manifest[B], f: Rep[A] => Rep[B])
    val rec = new scala.collection.mutable.HashMap[String,TopLevel[_,_]]
    def toplevel[A:Manifest,B:Manifest](name: String)(f: Rep[A] => Rep[B]): Rep[A] => Rep[B] = {
      val g = (x: Rep[A]) => unchecked[B](name,"(",x,")")
      rec.getOrElseUpdate(name, TopLevel(name, manifest[A], manifest[B], f))
      g
    }
  }

  trait Impl extends DSL with ScalaOpsPkgExp with TupledFunctionsRecursiveExp with UncheckedOpsExp { self => 
    val codegen = new CCodeGenPkg with CGenVariables with CGenTupledFunctions with CGenUncheckedOps { val IR: self.type = self }
    def emitAll(): Unit = {
      assert(codegen ne null) //careful about initialization order
      rec.foreach { case (k,x) =>
        val stream = new PrintWriter(System.out)
        stream.println("/* FILE: " + x.name + ".c */")
        for ((_,v) <- rec) codegen.emitForwardDef(mtype(v.mA)::Nil, v.name, stream)(mtype(v.mB))
        codegen.emitSource(x.f, x.name, stream)(mtype(x.mA), mtype(x.mB))
      }
    }
    emitAll()
  }

  
  def testCGen1 = {
    withOutFile(prefix+"cgen1") {
      trait Prog extends DSL {
        toplevel("main") { x: Rep[Int] =>

          var i = 0
          while (i < 10) {
            printf("Hello, world! %d\n", i)
            i = i + 1
          }

          0
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"cgen1")
  }


  // the generated code will contain nested functions; it needs to be
  // compiled with gcc -fnested-functions
  def testCGen2 = {
    withOutFile(prefix+"cgen2") {
      trait Prog extends DSL {
        toplevel("main") { x: Rep[Int] =>

          def fac: Rep[((Int,Int))=>Int] = fun { (n, dummy) =>
            if (n == 0) 1 else n * fac(n - 1, dummy)
          }

          printf("Hello, world! %d\n", fac(4,0))

          0
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"cgen2")
  }


  def testCGen3 = {
    withOutFile(prefix+"cgen3") {
      trait Prog extends DSL {
        val main = toplevel("main") { x: Rep[Int] =>
          printf("Hello, world: main\n")
          test1(x)
        }

        val test1 = toplevel("test1") { x: Rep[Int] =>
          printf("Hello, world: test1\n")
          test2(x)
        }

        val test2 = toplevel("test2") { x: Rep[Int] =>
          printf("Hello, world: test2\n")
          x
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"cgen3")
  }


}
