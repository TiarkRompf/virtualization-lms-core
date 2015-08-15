package scala.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext


trait ScalaGenFatArrayLoopsFusionOpt extends ScalaGenArrayLoopsFat with ScalaGenIfThenElseFat with LoopFusionOpt {
  val IR: ArrayLoopsFatExp with IfThenElseFatExp
  import IR._  
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }
  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLen(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(Block(a)) => Some(a) //TODO: block??
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayIfElem(c,Block(a)) => Some((a,List(c))) //TODO: block?
    case _ => super.unapplySimpleCollectIf(e)
  }

  override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match { //TODO: should c be list or not?
    case ArrayElem(a) if c.length == 1 => ArrayIfElem(c(0),a)
    case ReduceElem(a) if c.length == 1 => ReduceIfElem(c(0),a)
    case _ => super.applyAddCondition(e,c)
  }



}


// trait NestLambdaProg extends Arith with Functions with Print 
// --> from TestCodeMotion.scala

trait FusionProg extends LiftPrimitives with PrimitiveOps with ArrayLoops with Print {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
  
  def test(x: Rep[Unit]) = {
    
    val constant = array(100) { i => 1.0 }

    val linear = array(100) { i => 2.0*i }

    val affine = array(100) { i => constant.at(i) + linear.at(i) }
    
    def square(x: Rep[Double]) = x*x
    def mean(x: Rep[Array[Double]]) = sum(x.length) { i => x.at(i) } / x.length
    def variance(x: Rep[Array[Double]]) = sum(x.length) { i => square(x.at(i)) } / x.length - square(mean(x))
    
    val data = affine
    
    val m = mean(data)
    val v = variance(data)

    print(m)
    print(v)
  }
  
}

trait FusionProg2 extends LiftPrimitives with PrimitiveOps with ArrayLoops with Print with OrderingOps {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
  
  def test(x: Rep[Unit]) = {
    
    def filter[T:Typ](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) = 
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }
    
    val range = array(100) { i => i }
    
    val odds = filter(range) { z => z > 50 }
    
    val res = sum(odds.length) { i => odds.at(i) }
        
    print(res)
  }
  
}



/* 
  some thoughts on cse/gvn :
  
    - currently cse works fine for first-order, point-free things:
        val x = a + b
        val y = a + b
      will always be represented internally as
        val x = a + b
        val y = x
    
    - if bound variables are involved, cse no longer works:
        val a = array { i => 0 }
        val b = array { i => 0 }
      will create two separate objects:
        val a = array { i0 => 0 }
        val b = array { i1 => 0 }
      the same holds for lambdas.
    
    - this is due to the choice of representing bound vars using fresh symbols.
      alternatively we could use DeBruijn indices. 
      
      however, some care would have to be taken in managing the indices:
        val a = array { i => 
          val b = array { j => f(j) }
          sum(b)
        }
      code motion will move b out of a ... but we know that only after looking at b's body
  
    - for now this is not really a problem because loop fusion will take
      care of duplicate loops (effectively lifting scalar cse to array cse)

    - another solution (as done by delite) is to wrap array { i => 0 }
      as ArrayZero(len) extends DeliteOP(array(len) { i => 0}).
      here, cse will be done on the case class representation
*/



class TestFusion extends FileDiffSuite {
  
  val prefix = home + "test-out/epfl/test7-"
  
  def testFusion1 = {
    withOutFile(prefix+"fusion1") {
      new FusionProg with CoreOpsPkgExp with ArrayLoopsExp with PrintExp { self =>
        override def arrayTyp[T:Typ]: Typ[Array[T]] = typ[T].arrayTyp
        val codegen = new ScalaGenArrayLoops with ScalaGenPrimitiveOps with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion1")
  }

  def testFusion2 = {
    withOutFile(prefix+"fusion2") {
      // LoopsExp2 with ArithExp with PrintExp with BaseFatExp
      new FusionProg with CoreOpsPkgExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp  { self =>
        override val verbosity = 1
        override def arrayTyp[T:Typ]: Typ[Array[T]] = typ[T].arrayTyp
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenPrimitiveOps with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion2")
  }
 
  def testFusion3 = {
    withOutFile(prefix+"fusion3") {
      new FusionProg2 with CoreOpsPkgExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp  { self =>
        override val verbosity = 1
        override def arrayTyp[T:Typ]: Typ[Array[T]] = typ[T].arrayTyp
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenPrimitiveOps with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion3")
  }

  def testFusion4 = {
    withOutFile(prefix+"fusion4") {
      new FusionProg2 with CoreOpsPkgExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp  { self =>
        override val verbosity = 1
        override def arrayTyp[T:Typ]: Typ[Array[T]] = typ[T].arrayTyp
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenPrimitiveOps with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion4")
  }
 
}
