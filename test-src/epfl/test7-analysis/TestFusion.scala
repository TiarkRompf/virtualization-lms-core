package scala.virtualization.lms
package epfl
package test7

import common._
import test1._
import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{ PrintWriter, StringWriter, FileOutputStream }
import scala.reflect.SourceContext

trait TransformingStuff extends internal.Transforming with ArrayLoopsExp with ArithExp with PrintExp {

  // TODO: should call constructor functions instead of directly creating objects (i.e. array_length instead of ArrayLength)

  override def mirror[A: Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    /*case SimpleLoop(s, i, ArrayElem(g, y)) if f.hasContext =>
      // Here we have a need for specific order of mirroring due to generator g which is duplicated in IteratorCollect
      val ns = f(s)
      val ni = f(i).asInstanceOf[Sym[Int]]
      val nb = reifyEffectsHere(f.reflectBlock(y))
      val ng = f(g)
      toAtom(SimpleLoop(ns, ni, ArrayElem(ng, nb)))(mtype(manifest[A]), implicitly[SourceContext])*/
    
    // old code 
  /*  case SimpleLoop(s, i, ForeachElem(y)) =>
      toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ForeachElem(f(y))))(mtype(manifest[A]), implicitly[SourceContext])
    case SimpleLoop(s, i, ArrayElem(g, y)) =>
      toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ArrayElem(f(g), f(y))))(mtype(manifest[A]), implicitly[SourceContext])
    case SimpleLoop(s, i, ReduceElem(g, y)) =>
      toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ReduceElem(f(g), f(y))))(mtype(manifest[A]), implicitly[SourceContext])*/
    case ArrayIndex(a, i) => toAtom(ArrayIndex(f(a), f(i)))(mtype(manifest[A]), implicitly[SourceContext])
    case ArrayLength(a) => toAtom(ArrayLength(f(a)))(mtype(manifest[A]), implicitly[SourceContext])
    case Plus(x, y) => infix_+(f(x), f(y))
    case Minus(x, y) => infix_-(f(x), f(y))
    case Times(x, y) => infix_*(f(x), f(y))
    case Div(x, y) => infix_/(f(x), f(y))
    case Reflect(Print(x), u, es) => reflectMirrored(Reflect(Print(f(x)), mapOver(f, u), f(es)))
    case Reify(x, u, es) => toAtom(Reify(f(x), mapOver(f, u), f(es)))(mtype(manifest[A]), implicitly[SourceContext])
    case _ => super.mirror(e, f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A: Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ForeachElem(y) => ForeachElem(if (f.hasContext) reifyEffectsHere(f.reflectBlock(y)) else f(y))
    case ArrayElem(g, y) =>
//      println("f.hasContext = " + f.hasContext )
      ArrayElem(f(g), if (f.hasContext) reifyEffectsHere(f.reflectBlock(y)) else f(y))
    case ReduceElem(g, y) => ReduceElem(f(g), if (f.hasContext) reifyEffectsHere(f.reflectBlock(y)) else f(y))
    case _ => super.mirrorFatDef(e, f)
  }).asInstanceOf[Def[A]]

}

trait ScalaGenFatArrayLoopsFusionOpt extends ScalaGenArrayLoopsFat with ScalaFatLoopsFusionOpt 

trait ScalaFatLoopsFusionOpt extends ScalaGenArrayLoops with ScalaGenIfThenElseFat with LoopFusionOpt {
  val IR: ArrayLoopsFatExp with IfThenElseFatExp
  import IR._

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a, i))
    case _ => super.unapplySimpleIndex(e)
  }
  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(Def(Reflect(Yield(_, a), _, _)), _) => Some(a.head)
    case _ => super.unapplySimpleCollect(e)
  }

  /*def toAtom2[A:Manifest](d: Def[A])(implicit pos: SourceContext): Exp[A] = {
    val tp = findOrCreateDefinition(d, List(pos))
    tp.lhs.head
  }*/

  // take d's context (everything between loop body and yield) and duplicate it into r
  override def plugInHelper[A, T: Manifest, U: Manifest](oldGen: Exp[Gen[A]], context: Exp[Gen[T]], plug: Exp[Gen[U]]): Exp[Gen[U]] = context match {
    case `oldGen` => plug

    case Def(Reify(y, s, e)) =>
      getBlockResultFull(reifyEffects(plugInHelper(oldGen, y, plug)))

    case Def(Reflect(IfThenElse(c, Block(a), /*Block(Def(Skip(x)))), _, _))*/ Block(Def(Reify(Def(Reflect(Skip(x), _, b)), _, _)))), u, es)) =>
      // this is wrong but we need to check if it works at all
      ifThenElse(c, reifyEffects(plugInHelper(oldGen, a, plug)), reifyEffects(skip[U](reflectMutableSym(fresh[Int]), x)))

    case Def(Reflect(SimpleLoop(sh, x, ForeachElem(Block(y))), _, _)) =>
      val body = reifyEffects(plugInHelper(oldGen, y, plug))
      reflectEffect(SimpleLoop(sh, x, ForeachElem(body)), summarizeEffects(body).star)

    case Def(x) =>
      error("Missed me => " + x + " should find " + Def.unapply(oldGen).getOrElse("None"))
  }

  override def applyPlugIntoContext(d: Def[Any], r: Def[Any]) = (d, r) match {
    case (ArrayElem(g, Block(a)), ArrayElem(g2, Block(b))) =>
      ArrayElem(g2, Block(plugInHelper(g, a, b)))
      
    case (ReduceElem(g, Block(a)), ArrayElem(g2, Block(b))) =>
      ArrayElem(g2, Block(plugInHelper(g, a, b)))
      
    case (ArrayElem(g, Block(a)), ReduceElem(g2, Block(b))) =>
      ReduceElem(g2, Block(plugInHelper(g, a, b)))
      
    case (ReduceElem(g, Block(a)), ReduceElem(g2, Block(b))) =>
      ReduceElem(g2, Block(plugInHelper(g, a, b)))
      
    case _ => super.applyPlugIntoContext(d, r)
  }

  /*override def applyExtendGenerator[A](d: Def[Any], r: Def[Any]) = (d, r) match {
    case (ArrayElem(g@Def(Yield(varList, _)), _), ArrayElem(g2@Def(Yield(l, y)), _)) =>
      (g2, toAtom2(Yield(varList ::: l, y))).asInstanceOf[(Exp[A], Exp[A])]
    case (ReduceElem(g@Def(Yield(varList, _)), _), ReduceElem(g2@Def(Yield(l, y)), _)) =>
      (g2, toAtom2(Yield(varList ::: l, y))).asInstanceOf[(Exp[A], Exp[A])]
    case (ArrayElem(g@Def(Yield(varList, _)), _), ReduceElem(g2@Def(Yield(l, y)), _)) =>
      (g2, toAtom2(Yield(varList ::: l, y))).asInstanceOf[(Exp[A], Exp[A])]
    case (ReduceElem(g@Def(Yield(varList, _)), _), ArrayElem(g2@Def(Yield(l, y)), _)) =>
      (g2, toAtom2(Yield(varList ::: l, y))).asInstanceOf[(Exp[A], Exp[A])]
  }*/
}

// trait NestLambdaProg extends Arith with Functions with Print 
// --> from TestCodeMotion.scala

/**
 * No fusion should be happening.
 */
trait FusionProg extends Arith with ArrayLoops with Print {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    val constant = array(100) { i => 1 }

    val linear = array(100) { i => 2 * i }

    val affine = array(100) { i => constant.at(i) + linear.at(i) }

    def square(x: Rep[Double]) = x * x
    def mean(x: Rep[Array[Double]]) = sum(x.length) { i => x.at(i) } / x.length
    def variance(x: Rep[Array[Double]]) = sum(x.length) { i => square(x.at(i)) } / x.length - square(mean(x))

    val data = affine

    val m = mean(data)
    val v = variance(data)

    print(m)
    print(v)
  }

}


/**
 * Simple stuff map => map.
 */
trait FusionProg1 extends Arith with ArrayLoops with Print {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    val range = array(100) { i => i }

    val odds = map(range) { z => z + 3 }

    val res = map(odds) { i => i + 5 }

    print(res)
  }

}

/**
 * Generate => Filter => Reduce chain. Can be fused into a single loop.
 */
trait FusionProg2 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def filter[T: Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    val range = array(100) { i => i }

    val deadRange = map(range) { i => i + 1 }

    val dead = filter(range) { z => z > 10 }

    val odds = filter(range) { z => z > 50 }

    val res = sum(odds.length) { i => odds.at(i) }

    print(res)
  }

}

trait FusionProg3 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]) =
      arrayFlat(x.length) { i => x.at(i) }

    val range = array(100) { i => i }

    val nested = array(10) { i => range }

    val flat = flatten(nested)

    val res = sum(flat.length) { i => flat.at(i) }

    print(res)
  }

}

trait FusionProg4 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    def filter[T: Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]) =
      arrayFlat(x.length) { i => x.at(i) }

    val range = array(100) { i => i }

    val nested = array(10) { i => range }

    val flat = flatten(nested)

    val filtered = filter(flat) { i => i > 50 }

    print(filtered)
  }

}

/**
 * Harder example:
 * Array[Array[Int]] -> flatMap -> map(x => another Array[Array[Int]]) -> flatMap -> filter -> reduce
 */
trait FusionProg5 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    def filter[T: Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]) =
      arrayFlat(x.length) { i => x.at(i) }

    val range = array(100) { i => i }
    //    val range1 = array(101) { i => i }

    val nested = array(10) { i => range }

    val flat = flatten(nested)

    val mapped = map(flat) { i => i + 5 }

    //    val flattenedAgain = flatten(mapped)

    print(mapped)
  }

}

/**
 * Hard example:
 * Array[Array[Int]] -> flatMap -> map(x => Array[Int]) -> flatMap -> filter -> reduce
 */
trait FusionProg6 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    def filter[T: Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]) =
      arrayFlat(x.length) { i => x.at(i) }

    val range = array(1000) { i => i }
    val range1 = array(1001) { i => i }

    val nested = array(10) { i => range }

    val flat = flatten(nested)

    val filtered1 = filter(flat)(i => i > 1111)

    val mapped = map(filtered1) { i => range1 }

    val flattenedAgain = flatten(mapped)

    val filtered2 = filter(flattenedAgain) { i => i < 2222 }

    val reduced = sum(filtered2.length) { i => filtered2.at(i) }

    print(reduced)
  }

}

/**
 * Hardest example (includes nesting of flatMaps):
 * Array[Array[Int]] -> flatMap -> map(x => another Array[Array[Int]]) -> flatMap -> filter -> reduce
 */
trait FusionProg7 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    def filter[T: Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]) =
      arrayFlat(x.length) { i => x.at(i) }

    val range = array(1000) { i => i }
    val range1 = array(1001) { i => i }

    val nested = array(10) { i => range }
    val nested2 = array(11) { i => nested }
    
    val flat = flatten(flatten(nested2))

    val filtered1 = filter(flat)(i => i > 1111)

//    val mapped = map(filtered1) { i => range1 }
//
//    val flattenedAgain = flatten(mapped)

//    val filtered2 = filter(flattenedAgain) { i => i < 2222 }

//    val reduced = sum(filtered2.length) { i => filtered2.at(i) }

    print(filtered1)
//    print(mapped)
  }

}

// test nested flatMap fusion
trait FusionProg8 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    def flatten[T: Manifest](x: Rep[Array[Array[T]]]): Rep[Array[T]] =
      arrayFlat(x.length) { i => x.at(i) }

    val range: Rep[Array[Array[Array[Array[Array[Array[Array[Int]]]]]]]] = array(1000) { i => array(1000) {i => array(1000) {i => array(1000) {i => array(1000) {i => array(1000){i => array(1000) {i => i}}}}}}}
    val flat = map(flatten(map(flatten(map(flatten(map(flatten(map(flatten(map(range){x => x})){x => x})){x => x})){x => x})){x => x})){x => x}

    print(flat)
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

  private[this] def printExceptions(b: => Unit) = {
    try b catch {
      case e =>
        val writer = new PrintWriter(System.out)
        e.printStackTrace(writer)
        writer.flush
    }
  }

  def testFusion01 = {
    withOutFile(prefix + "fusion01") {
      printExceptions {
        new FusionProg with ArithExp with ArrayLoopsExp with PrintExp { self =>
          val codegen = new ScalaGenArrayLoops with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }

          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion01")
  }

  def testFusion02 = {
    withOutFile(prefix + "fusion02") {
      printExceptions {
        // LoopsExp2 with ArithExp with PrintExp with BaseFatExp
        new FusionProg1 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint {
            val IR: self.type = self
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion02")
  }

  // Test with filter clause that does not apply fusion.
  def testFusion03 = {
    withOutFile(prefix + "fusion03") {
      printExceptions {
        new FusionProg2 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion03")
  }

  // Test with filter clause that applies fusion.
  def testFusion04 = {
    withOutFile(prefix + "fusion04") {
      printExceptions {
        new FusionProg2 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion04")
  }

  // Test with flatMap clause that does not apply fusion.
  def testFusion5 = {
    withOutFile(prefix + "fusion05") {
      printExceptions {
        new FusionProg3 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion05")
  }

  // Test with flatMap clause that applies fusion.
  def testFusion6 = {
    withOutFile(prefix + "fusion06") {
      printExceptions {
        new FusionProg3 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion06")
  }

  // Test with (flatMap => filter => reduce) clause without fusion.
  def testFusion7 = {
    withOutFile(prefix + "fusion07") {
      printExceptions {
        new FusionProg4 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion07")
  }

  // Test with (flatMap => filter => reduce) clause that applies fusion.
  def testFusion8 = {
    withOutFile(prefix + "fusion08") {
      printExceptions {
        new FusionProg4 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion08")
  }

  // Test with (flatMap => filter => reduce) clause that applies fusion.
  def testFusion9 = {
    withOutFile(prefix + "fusion09") {
      printExceptions {
        new FusionProg5 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion09")
  }

  // Test with (flatMap => filter => reduce) clause that applies fusion.
  def testFusion10 = {
    withOutFile(prefix + "fusion10") {
      printExceptions {
        new FusionProg5 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion10")
  }

  def testFusion11 = {
    withOutFile(prefix + "fusion11") {
      printExceptions {
        new FusionProg6 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }

    assertFileEqualsCheck(prefix + "fusion11")
  }

  def testFusion12 = {
    withOutFile(prefix + "fusion12") {
      printExceptions {
        new FusionProg7 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion12")
  }
  
  def testFusion13 = {
    withOutFile(prefix + "fusion13") {
      printExceptions {
        new FusionProg7 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 0
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion13")
  }
  
  def testFusion14 = {
    withOutFile(prefix + "fusion14") {
      printExceptions {
        new FusionProg8 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 1
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = false
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion14")
  }
  
  def testFusion15 = {
    withOutFile(prefix + "fusion15") {
      printExceptions {
        new FusionProg8 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
          override val verbosity = 1
          val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint with ScalaGenIfThenElse with ScalaGenOrderingOps {
            val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true
          }
          codegen.emitSource(test, "Test", new PrintWriter(System.out))
        }
      }
    }
    assertFileEqualsCheck(prefix + "fusion15")
  }

}