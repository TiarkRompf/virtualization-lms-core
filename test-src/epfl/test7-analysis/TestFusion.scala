package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait TransformingStuff extends internal.Transforming with ArrayLoopsExp with ArithExp with PrintExp {

  // TODO: should call constructor functions instead of directly creating objects (i.e. array_length instead of ArrayLength)

  override def mirror[A:Manifest](e: Def[A], f: Transformer): Exp[A] = (e match {
    //case Copy(a) => f(a)
    case SimpleLoop(s,i, ArrayElem(y)) => toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ArrayElem(f(y))))
    case SimpleLoop(s,i, ReduceElem(y)) => toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ReduceElem(f(y))))
    case SimpleLoop(s,i, ArrayIfElem(c,y)) => toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ArrayIfElem(f(c),f(y))))
    case SimpleLoop(s,i, ReduceIfElem(c,y)) => toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ReduceIfElem(f(c),f(y))))
    case ArrayIndex(a,i) => toAtom(ArrayIndex(f(a), f(i)))
    case ArrayLength(a) => toAtom(ArrayLength(f(a)))
    case Plus(x,y) => infix_+(f(x), f(y))
    case Minus(x,y) => infix_-(f(x), f(y))
    case Times(x,y) => infix_*(f(x), f(y))
    case Div(x,y) => infix_/(f(x), f(y))
    case Reflect(Print(x), u, es) => reflectMirrored(Reflect(Print(f(x)), mapOver(f,u), f(es)))
    case Reify(x, u, es) => toAtom(Reify(f(x), mapOver(f,u), f(es)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer): Def[A] = (e match {
    case ArrayElem(y) => ArrayElem(f(y))
    case ReduceElem(y) => ReduceElem(f(y))
    case ArrayIfElem(c,y) => ArrayIfElem(f(c),f(y))
    case ReduceIfElem(c,y) => ReduceIfElem(f(c),f(y))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]
    
}



trait ScalaGenFatArrayLoopsFusionOpt extends ScalaGenArrayLoopsFat with LoopFusionOpt {
  val IR: ArrayLoopsFatExp
  import IR._  
  
  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }
  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(a) => Some(a)
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayIfElem(c,a) => Some((a,List(c)))
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

trait FusionProg extends Arith with ArrayLoops with Print {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
  
  def test(x: Rep[Unit]) = {
    
    val constant = array(100) { i => 1 }

    val linear = array(100) { i => 2*i }

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

trait FusionProg2 extends Arith with ArrayLoops with Print with OrderingOps {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
  
  def test(x: Rep[Unit]) = {
    
    def filter[T:Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) = 
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
  
  val prefix = "test-out/epfl/test7-"
  
  def testFusion1 = {
    withOutFile(prefix+"fusion1") {
      new FusionProg with ArithExp with ArrayLoopsExp with PrintExp { self =>
        val codegen = new ScalaGenArrayLoops with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion1")
  }

  def testFusion2 = {
    withOutFile(prefix+"fusion2") {
      // LoopsExp2 with ArithExp with PrintExp with BaseFatExp
      new FusionProg with ArithExp with ArrayLoopsFatExp with PrintExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion2")
  }
 
  def testFusion3 = {
    withOutFile(prefix+"fusion3") {
      new FusionProg2 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = false }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion3")
  }

  def testFusion4 = {
    withOutFile(prefix+"fusion4") {
      new FusionProg2 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion4")
  }
 
}