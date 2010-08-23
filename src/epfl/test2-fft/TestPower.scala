package scala.virtualization.lms
package epfl
package test2

import common._
import test1._

import java.io.PrintWriter

trait TestPower1 { this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double] = 
    if (x == 0) 1.0 else b * power(b, x - 1)
}

trait TestPower2 { this: Arith =>
  def power(b: Rep[Double], x: Int): Rep[Double] = 
    if (x == 0) 1.0
    else if ((x&1) == 0) { val y = power(b, x/2); y * y }
    else b * power(b, x - 1)
}


trait BaseStr extends Base {
  type Rep[+T] = String
  //todo added this to provide required unit implicit conversion
  implicit def unit[T](x: T): Rep[T] = x.toString
}

trait ArithStr extends Arith with BaseStr {
  //todo removed below
  //implicit def unit(x: Double) = x.toString

  def __ext__+(x: Rep[Double], y: Rep[Double]) = "(%s+%s)".format(x,y)
  def __ext__-(x: Rep[Double], y: Rep[Double]) = "(%s-%s)".format(x,y)
  def __ext__*(x: Rep[Double], y: Rep[Double]) = "(%s*%s)".format(x,y)
  def __ext__/(x: Rep[Double], y: Rep[Double]) = "(%s/%s)".format(x,y)
}

object TestTestPower {
  
  def main(args: Array[String]) = {
/*    
    println {
      val o = new TestPower with ArithRepDirect
      import o._
      power(2,4)
    }

    println {
      val o = new TestPower with ArithRepString
      import o._
      power(2,4)
    }
    
    println {
      val o = new TestPower with ArithRepString
      import o._
      power("x",4)
    }

    println {
      val o = new TestPower with ArithRepString
      import o._
      power("(x + y)",4)
    }
*/
    {
      val o = new TestPower1 with ArithStr
      import o._

      val r = power(__ext__+("x0","x1"),4)
      println(r)
    }
    {
      val o = new TestPower2 with ArithStr
      import o._

      val r = power(__ext__+("x0","x1"),4)
      println(r)
    }
    {
      val o = new TestPower1 with ArithExp with ExportGraph
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test2-power1-dot")
    }

    {
      val o = new TestPower1 with ArithExpOpt with ExportGraph
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test2-power2-dot")
    }
    {
      val o = new TestPower1 with ArithExpOpt with CompileScala with ScalaGenArith
      import o._
      val f = (x: Rep[Double]) => power(x + x, 4)
      emitScalaSource(f, "Power2", new PrintWriter(System.out))
    }

    {
      val o = new TestPower2 with ArithExpOpt with ExportGraph
      import o._

      val r = power(fresh[Double] + fresh[Double],4)
      println(globalDefs.mkString("\n"))
      println(r)
      emitDepGraph(r, "test2-power3-dot")
    }
    {
      val o = new TestPower2 with ArithExpOpt with CompileScala with ScalaGenArith
      import o._
      val f = (x: Rep[Double]) => power(x + x, 4)
      emitScalaSource(f, "Power3", new PrintWriter(System.out))
    }



    {
      val o = new TestPower1 with ArithExpOpt with CompileScala with ScalaGenArith
      import o._

      val power4 = (x:Rep[Double]) => power(x,4)
      emitScalaSource(power4, "Power4", new PrintWriter(System.out))
      val power4c = compile(power4)
      println(power4c(2))
    }
  }
}