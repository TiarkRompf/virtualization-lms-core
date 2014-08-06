/*TODO DISABLED
package scala.virtualization.lms
package epfl
package test9

import common._
import test1._

import test7.{Print,PrintExp,ScalaGenPrint}
import internal.ScalaCompile

import scala.util.continuations._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait CpsProg1 extends Arith with IfThenElse with Equal with Print with Compile {
  
  def choose[A:Manifest](x: Rep[Boolean]): Boolean @cps[Rep[A]] = shift { k: (Boolean => Rep[A]) =>
    if (x)
      k(true)
    else
      k(false)
  }
  
  def test(x: Rep[Boolean]): Rep[Unit] = { // recompile
    reset {
      val c = choose[Unit](x)
      if (c) {
        this.print("is true")
      } else {
        this.print("is false")
      }
    }
  }
  
}

trait CpsProg2 extends Arith with IfThenElse with Equal with Print with Compile {
  
  def choose[A:Manifest](x: Rep[Boolean]): Boolean @cps[Rep[A]] = shift { k: (Boolean => Rep[A]) =>
    if (x)
      k(true)
    else
      k(false)
  }
  
  
  def pickValue[A:Manifest](x: Rep[Boolean]): Rep[Int] @cps[Rep[A]] = { 
    val c = choose[A](x)
    if (c) 
      unit(7) 
    else 
      unit(9)
  }
  
  def test(x: Rep[Boolean]): Rep[Unit] = { // recompile
    reset {
      val z = pickValue[Unit](x)
      this.print(z)
    }
  }
  
}


trait AmbProg1 extends Arith with IfThenElse with Equal with Print with Compile {
  
  //def __ifThenElse[T:Manifest,U](cond: Rep[Boolean], thenp: => Rep[T]@cps[U], elsep: => Rep[T]@cps[U]): Rep[T]@cps[U] = cond match { case true => thenp case false => elsep }
  
  
  // xs could be either Rep[List[T]] or List[Rep[T]]
  // if List[Rep[T]], code paths could be duplicated or not...

  // this is the BAM variant of AMB: be careful, it can cause code explosion.
  def amb[T](xs: List[Rep[T]]): Rep[T] @cps[Rep[Unit]] = shift { k =>
    xs foreach k 
  }  
  
  def require(x: Rep[Boolean]): Rep[Unit] @cps[Rep[Unit]] = shift { k: (Rep[Unit]=>Rep[Unit]) =>
    if (x) k() else ()
  }
  
  
  def test(x: Rep[Int]): Rep[Unit] = {
    
    reset {
      val a = amb(List(unit(1),unit(2),x))
      val b = amb(List(unit(1),unit(2),unit(3)))
      require(a == b)
      this.print("found:")
      this.print(a)
      this.print(b)
    }
    
    ()
/*
    def joins(s1:String, s2:String) = s1.endsWith(s2.substring(0,1))
    val w1 = amb(List("the","that","a"))
    val w2 = amb(List("frog","elephant","thing"))
    val w3 = amb(List("walked","treaded","grows"))
    val w4 = amb(List("slowly","quickly"))
    require(joins(w1,w2))
    require(joins(w2,w3))
    require(joins(w3,w4))
    yld(List(w1,w2,w3,w4))
    
    // result: that thing grows slowly
*/    

/*
val i = amb(low to high)
val j = amb(i to high)
val k = amb(j to high)
require(i*i + j*j == k*k)
yld((i,j,k))

//output using (low=1,high=20):
// (3,4,5)
// (5,12,13)
// (6,8,10)
// (8,15,17)
// (9,12,15)
// (12,16,20)
*/
  }
}


trait AmbProg2 extends AmbProg1 {
  
  override def test(x: Rep[Int]): Rep[Unit] = {
    
    reset {
      val a = amb(List(unit(1),unit(2),unit(3),unit(4)))
      val b = amb(List(unit(1),unit(2),unit(3),unit(4)))
      val c = amb(List(unit(1),unit(2),unit(3),unit(4)))
      require(a != b)
      require(b != c)
//      require(c != a)

      this.print("found:")
      this.print(a)
      this.print(b)
      this.print(c)
    }
    
    ()
  }
  
  
}




class TestCPS extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test9-"
  
  def testCps1 = {
    withOutFile(prefix+"cps1") {
      new CpsProg1 with ArithExp with EqualExp with IfThenElseExp with PrintExp with ScalaCompile { self =>
        val codegen = new ScalaGenArith with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        //override def compile
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"cps1")
  }

  def testCps2 = {
    withOutFile(prefix+"cps2") {
      new CpsProg2 with ArithExp with EqualExp with IfThenElseExp with PrintExp with ScalaCompile { self =>
        val codegen = new ScalaGenArith with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        //override def compile
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"cps2")
  }
 
  def testAmb1a = {
    withOutFile(prefix+"amb1a") {
      new AmbProg1 with ArithExp with EqualExp with IfThenElseExp with PrintExp with ScalaCompile { self =>
        val codegen = new ScalaGenArith with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        //override def compile
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"amb1a")
  }
  
  def testAmb1b = {
    withOutFile(prefix+"amb1b") {
      new AmbProg1 with ArithExp with EqualExpOpt with IfThenElseExpOpt with BooleanOpsExp with PrintExp with ScalaCompile { self =>
        val codegen = new ScalaGenArith with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        //override def compile
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"amb1b")
  }

  def testAmb2a = {
    withOutFile(prefix+"amb2a") {
      new AmbProg2 with ArithExp with EqualExp with IfThenElseExp with PrintExp with ScalaCompile { self =>
        val codegen = new ScalaGenArith with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        //override def compile
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"amb2a")
  }
  
  def testAmb2b = {
    withOutFile(prefix+"amb2b") {
      new AmbProg2 with ArithExp with EqualExpOpt with IfThenElseExpOpt with BooleanOpsExp with PrintExp with ScalaCompile { self =>
        val codegen = new ScalaGenArith with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenPrint { val IR: self.type = self }
        //override def compile
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"amb2b")
  }

}
*/
