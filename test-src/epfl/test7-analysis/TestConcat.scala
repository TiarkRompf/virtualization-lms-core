package scala.virtualization.lms
package epfl
package test7

import common._
import test1._
import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{ PrintWriter, StringWriter, FileOutputStream }
import scala.reflect.SourceContext

/**
 * Simplest concat example.
 */
trait ConcatProg extends Arith with ArrayLoops with Print {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {
    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    val constant = array(100) { i => 1 }
    val linear = array(100) { i => 2 * i }
    val constantLinear = concat(constant, linear)
    
    val res = map(constantLinear)(x => x)
    print(res)
  }

}

class TestConcat extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-"

  private[this] def printExceptions(b: => Unit) = {
    try b catch {
      case e =>
        val writer = new PrintWriter(System.out)
        e.printStackTrace(writer)
        writer.flush
    }
  }

  def testConcat01 = {
    withOutFile(prefix + "concat01") {
      printExceptions {
        new ConcatProg with ArithExp with ArrayLoopsExp with PrintExp { self =>
          val codegen = new ScalaGenArrayLoops with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }

          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "concat01")
  }

}