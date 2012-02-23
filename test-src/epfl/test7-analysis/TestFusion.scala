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
    case SimpleLoop(s,i, ForeachElem(y)) => toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ForeachElem(f(y))))(mtype(manifest[A]))
    case SimpleLoop(s,i, ArrayElem(g,y)) => toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ArrayElem(f(g),f(y))))(mtype(manifest[A]))
    case SimpleLoop(s,i, ReduceElem(g,y)) => toAtom(SimpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ReduceElem(f(g),f(y))))(mtype(manifest[A]))
    case ArrayIndex(a,i) => toAtom(ArrayIndex(f(a), f(i)))(mtype(manifest[A]))
    case ArrayLength(a) => toAtom(ArrayLength(f(a)))(mtype(manifest[A]))
    case Plus(x,y) => infix_+(f(x), f(y))
    case Minus(x,y) => infix_-(f(x), f(y))
    case Times(x,y) => infix_*(f(x), f(y))
    case Div(x,y) => infix_/(f(x), f(y))
    case Reflect(Print(x), u, es) => reflectMirrored(Reflect(Print(f(x)), mapOver(f,u), f(es)))
    case Reify(x, u, es) => toAtom(Reify(f(x), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer): Def[A] = (e match {
    case ForeachElem(y) => ForeachElem(f(y))
    case ArrayElem(g,y) => ArrayElem(f(g),f(y))
    case ReduceElem(g,y) => ReduceElem(f(g),f(y))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]
    
}



trait ScalaGenFatArrayLoopsFusionOpt extends ScalaGenArrayLoopsFat with ScalaGenIfThenElseFatYield with LoopFusionOpt {
  val IR: ArrayLoopsFatExp with IfThenElseFatExp
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
    case ArrayElem(Def(Yield(_,a)), _) => Some(a)
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayElem(g,Block(Def(IfThenElse(c,Block(Def(SimpleCollectIf(a,cs))),Block(Def(Skip(_))))))) => Some((a,c::cs))
    case _ => super.unapplySimpleCollectIf(e)
  }
  
  
  // TODO: more variants...
  
  // take d's context (everything between loop body and yield) and duplicate it into r

  def toAtom2[A:Manifest](d: Def[A]): Exp[A] = {
    val tp = findOrCreateDefinition(d)
    tp.sym
  }

  override def plugInHelper[A,T:Manifest,U:Manifest](oldGen: Exp[Gen[A]], context: Block[Gen[T]], plug: Block[Gen[U]]): Block[Gen[U]] = context match {
    case Block(`oldGen`) => plug
    case Block(Def(IfThenElse(c,a,b@Block(Def(Skip(x)))))) => Block(toAtom2(IfThenElse(c,plugInHelper(oldGen,a,plug),Block(toAtom2(Skip(x))))))
    case Block(Def(SimpleLoop(sh,x,ForeachElem(y)))) => Block(toAtom2(SimpleLoop(sh,x,ForeachElem(plugInHelper(oldGen,y,plug)))))
    case Block(Def(x)) => error("Missed me => " + x + " should find " + oldGen); throw new RuntimeException("Missed me => " + x + " should find " + oldGen)
  }

  override def applyPlugIntoContext(d: Def[Any], r: Def[Any], newGen: Exp[Any]) = (d, r) match {
    case (ArrayElem(g, a), ArrayElem(g2, b)) => ArrayElem(newGen.asInstanceOf[Exp[Gen[Nothing]]], plugInHelper(g, a, b))
    case (ReduceElem(g, a), ArrayElem(g2, b)) => ArrayElem(newGen.asInstanceOf[Exp[Gen[Nothing]]], plugInHelper(g, a, b))
    case (ArrayElem(g, a), ReduceElem(g2, b)) => ReduceElem(newGen.asInstanceOf[Exp[Gen[Double]]], plugInHelper(g, a, b))
    case (ReduceElem(g, a), ReduceElem(g2, b)) => ReduceElem(newGen.asInstanceOf[Exp[Gen[Double]]], plugInHelper(g, a, b))
    case _ => super.applyPlugIntoContext(d, r, newGen)
  }

  override def applyExtendGenerator[A](d: Def[Any], r: Def[Any]) = (d, r) match {
    case (ArrayElem(g@Def(Yield(varList, _)), _), ArrayElem(g2@Def(Yield(l, y)), _)) =>
      (g2, toAtom2(Yield(varList ::: l, y))).asInstanceOf[(Exp[A], Exp[A])]
    case (ReduceElem(g@Def(Yield(varList, _)), _), ReduceElem(g2@Def(Yield(l, y)), _)) =>
      (g2, toAtom2(Yield(varList ::: l, y))).asInstanceOf[(Exp[A], Exp[A])]
    case (ArrayElem(g@Def(Yield(varList, _)), _), ReduceElem(g2@Def(Yield(l, y)), _)) =>
      (g2, toAtom2(Yield(varList ::: l, y))).asInstanceOf[(Exp[A], Exp[A])]
    case (ReduceElem(g@Def(Yield(varList, _)), _), ArrayElem(g2@Def(Yield(l, y)), _)) =>
      (g2, toAtom2(Yield(varList ::: l, y))).asInstanceOf[(Exp[A], Exp[A])]
  }
}

/**
 * If then else code generator. It optimizes out the Skip statement.
 * It requires IfThenElseFatExp and ArrayLoopsExp to be mixed in.
 */
trait ScalaGenIfThenElseFatYield extends ScalaGenIfThenElse with ScalaGenFat with BaseGenIfThenElseFat {
  val IR: IfThenElseFatExp with ArrayLoopsExp
  import IR._

  private def quoteList[T](xs: List[Exp[T]]) =
    if (xs.length > 1) xs.map(quote).mkString("(", ",", ")") else xs.map(quote).mkString(",")

  /**
   * Helper method for emitting just the ifThen block.
   */
  private def emitFatIfThenBlock(symList: List[Sym[Any]], rhs: FatDef,
                                 c: Exp[Boolean], as: List[Block[Any]])
                                (implicit stream: PrintWriter) {
    if (symList.length > 1) stream.println("// TODO: use vars instead of tuples to return multiple values")
    stream.println("val " + quoteList(symList) + " = if (" + quote(c) + ") {")
    emitFatBlock(as)
    stream.println(quoteList(as.map(getBlockResult)))
    stream.print("}")
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef)(implicit stream: PrintWriter) = rhs match {
    // Eliminate the else branch if it only contains the Skip clause
    case SimpleFatIfThenElse(c, as, List(Block(Def(Skip(_))))) =>
      emitFatIfThenBlock(symList, rhs, c, as)(stream)
      stream.println("")

    // Generate both then and else
    case SimpleFatIfThenElse(c, as, bs) =>
      emitFatIfThenBlock(symList, rhs, c, as)(stream)
      stream.println(" else {")
      emitFatBlock(bs)
      stream.println(quoteList(bs.map(getBlockResult)))
      stream.println("}")
    case _ => super.emitFatNode(symList, rhs)
  }
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

/**
 * Generate => Filter => Reduce chain. Can be fused into a single loop.
 */
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


trait FusionProg3 extends Arith with ArrayLoops with Print with OrderingOps {
  
  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
  
  def test(x: Rep[Unit]) = {

    def flatten[T:Manifest](x: Rep[Array[Array[T]]]) =
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

    def filter[T:Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    def flatten[T:Manifest](x: Rep[Array[Array[T]]]) =
      arrayFlat(x.length) { i => x.at(i) }

    val range = array(100) { i => i }

    val nested = array(10) { i => range }

    val flat = flatten(nested)

    val filtered = filter(flat) {i => i > 50 }

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

    def filter[T:Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    def flatten[T:Manifest](x: Rep[Array[Array[T]]]) =
      arrayFlat(x.length) { i => x.at(i) }

    val range = array(100) { i => i }
//    val range1 = array(101) { i => i }

    val nested = array(10) { i => range }

    val flat = flatten(nested)

    val mapped = map(flat){ i => i + 5 }

//    val flattenedAgain = flatten(mapped)

    print(mapped)
  }

}

/**
 * Hardest example (includes nesting of flatMaps):
 * Array[Array[Int]] -> flatMap -> map(x => another Array[Array[Int]]) -> flatMap -> filter -> reduce
 */
trait FusionProg6 extends Arith with ArrayLoops with Print with OrderingOps {

  implicit def bla(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]

  def test(x: Rep[Unit]) = {

    def map[T: Manifest, V: Manifest](x: Rep[Array[T]])(f: Rep[T] => Rep[V]) =
      array(x.length)(i => f(x.at(i)))

    def filter[T:Manifest](x: Rep[Array[T]])(p: Rep[T] => Rep[Boolean]) =
      arrayIf(x.length) { i => (p(x.at(i)), x.at(i)) }

    def flatten[T:Manifest](x: Rep[Array[Array[T]]]) =
      arrayFlat(x.length) { i => x.at(i) }

    val range = array(1000) { i => i }
    val range1 = array(1001) { i => i }

    val nested = array(10) { i => range }

    val flat = flatten(nested)

    val filtered1 = filter(flat)(i => i > 1111)

    val mapped = map(filtered1){ i => range1 }

    val flattenedAgain = flatten(mapped)

    val filtered2 = filter(flattenedAgain) {i => i < 2222 }

    val reduced = sum(filtered2.length) { i => filtered2.at(i) }

    print(reduced)
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
      new FusionProg with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion2")
  }

  // Test with filter clause that does not apply fusion.
  def testFusion3 = {
    withOutFile(prefix+"fusion3") {
      new FusionProg2 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = false }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion3")
  }

  // Test with filter clause that applies fusion.
  def testFusion4 = {
    withOutFile(prefix+"fusion4") {
      new FusionProg2 with ArithExp with ArrayLoopsFatExp with IfThenElseFatExp with PrintExp with IfThenElseExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        globalDefs.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"fusion4")
  }

  // Test with flatMap clause that does not apply fusion.
  def testFusion5 = {
    withOutFile(prefix+"fusion5") {
      new FusionProg3 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = false }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
      }
    }
    assertFileEqualsCheck(prefix+"fusion5")
  }

  // Test with flatMap clause that applies fusion.
  def testFusion6 = {
    withOutFile(prefix+"fusion6") {
      new FusionProg3 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint 
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        globalDefs.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"fusion6")
  }

  // Test with (flatMap => filter => reduce) clause that applies fusion.
  def testFusion7 = {
    withOutFile(prefix+"fusion7") {
      new FusionProg4 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = false }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        globalDefs.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"fusion7")
  }

  // Test with (flatMap => filter => reduce) clause that applies fusion.
  def testFusion8 = {
    withOutFile(prefix+"fusion8") {
      new FusionProg4 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        globalDefs.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"fusion8")
  }

  // Test with (flatMap => filter => reduce) clause that applies fusion.
  def testFusion9 = {
    withOutFile(prefix+"fusion9") {
      new FusionProg5 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = false }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        globalDefs.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"fusion9")
  }

  // Test with (flatMap => filter => reduce) clause that applies fusion.
  def testFusion10 = {
    withOutFile(prefix+"fusion10") {
      new FusionProg5 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        globalDefs.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"fusion10")
  }

  def testFusion11 = {
    withOutFile(prefix+"fusion11") {
      new FusionProg6 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        globalDefs.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"fusion10")
  }

  def testFusion12 = {
    withOutFile(prefix+"fusion12") {
      new FusionProg6 with ArithExp with ArrayLoopsFatExp with PrintExp with IfThenElseFatExp with OrderingOpsExp with TransformingStuff { self =>
        override val verbosity = 1
        val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenArith with ScalaGenPrint
          with ScalaGenIfThenElse with ScalaGenOrderingOps { val IR: self.type = self;
            override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
        codegen.emitSource(test, "Test", new PrintWriter(System.out))
        globalDefs.foreach(println)
      }
    }
    assertFileEqualsCheck(prefix+"fusion10")
  }

}
