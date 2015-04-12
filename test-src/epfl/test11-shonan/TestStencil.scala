package scala.virtualization.lms
package epfl
package test11

import common._
import test1._
import test7._
import test8.{ArrayMutation,ArrayMutationExp,ScalaGenArrayMutation,OrderingOpsExpOpt}

import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext


class TestStencil extends FileDiffSuite {
  
  trait DSL extends LiftNumeric with NumericOps with PrimitiveOps with ArrayOps with RangeOps 
    with BooleanOps with OrderingOps
    with LiftVariables with IfThenElse with Print {
    def staticData[T:Manifest](x: T): Rep[T]
    def infix_toDouble(x: Rep[Int]): Rep[Double]
    def test(x: Rep[Array[Double]]): Rep[Array[Double]]
  }
  trait Impl extends DSL with Runner with ArrayOpsExpOpt with NumericOpsExpOpt
      with OrderingOpsExpOpt with BooleanOpsExp 
      with EqualExpOpt with VariablesExpOpt with RangeOpsExp with StaticDataExp
      with IfThenElseExpOpt with PrintExp with PrimitiveOpsExp
      with CompileScala { self => 
    //override val verbosity = 1
    def infix_toDouble(x: Rep[Int]): Rep[Double] = int_double_value(x)
    
    val codegen = new ScalaGenNumericOps with ScalaGenStaticData with ScalaGenOrderingOps 
      with ScalaGenArrayOps with ScalaGenRangeOps with ScalaGenBooleanOps
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenPrimitiveOps
      with ScalaGenPrint /*with LivenessOpt*/ { val IR: self.type = self }
    dumpGeneratedCode = true
    run()
  }
  trait Runner extends Compile {
    def test(x: Rep[Array[Double]]): Rep[Array[Double]]
    def run() {
      val f = compile(test)
      val v0 = Array.tabulate(10)(i => 1.0 + 0.1 * i)
      val v1 = f(v0)
      v1 foreach println
    }
  }

  trait Sliding extends DSL {
    
    def infix_sliding[T:Manifest](n: Rep[Int], f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
      val a = NewArray[T](n)
      sliding(0,n)(i => a(i) = f(i))
      a
    }

    def infix_sliding(r: Rep[Range]) = new {
      def foreach(f: Rep[Int] => Rep[Unit]): Rep[Unit] =
        sliding(r.start, r.end)(f)
    }

    def sliding(start: Rep[Int], end: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit]

  }
  
  //trait SlidingExp extends Impl with Sliding {
  trait SlidingExp extends ArrayOpsExpOpt with NumericOpsExpOpt with PrimitiveOpsExpOpt
      with OrderingOpsExpOpt with BooleanOpsExp 
      with EqualExpOpt with VariablesExpOpt with RangeOpsExp
      with IfThenElseExpOpt {
    
    object trans extends ForwardTransformer { 
      val IR: SlidingExp.this.type = SlidingExp.this
    }
    
    // some arithemetic rewrites
    override def int_plus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = ((lhs,rhs) match {
      case (Def(IntPlus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => int_plus(x, unit(y+z)) // (x+y)+z --> x+(y+z)
      case (Def(IntMinus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => int_minus(x, unit(y-z)) // (x-y)+z --> x-(y-z)
      case (x: Exp[Int], Const(z:Int)) if z < 0 => int_minus(x, unit(-z))
      case _ => super.int_plus(lhs,rhs)
    }).asInstanceOf[Exp[Int]]

    override def int_minus(lhs: Exp[Int], rhs: Exp[Int])(implicit pos: SourceContext): Exp[Int] = ((lhs,rhs) match {
      case (Def(IntMinus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => int_minus(x, unit(y+z)) // (x-y)-z --> x-(y+z)
      case (Def(IntPlus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => int_plus(x, unit(y-z)) // (x+y)-z --> x+(y-z)
      case (x: Exp[Int], Const(z:Int)) if z < 0 => int_plus(x, unit(-z))
      case _ => super.int_minus(lhs,rhs)
    }).asInstanceOf[Exp[Int]]

    override def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = ((lhs,rhs) match {
      case (Def(NumericPlus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => numeric_plus(x, unit(y+z)) // (x+y)+z --> x+(y+z)
      case (Def(NumericMinus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => numeric_minus(x, unit(y-z)) // (x-y)+z --> x-(y-z)
      case _ => super.numeric_plus(lhs,rhs)
    }).asInstanceOf[Exp[T]]

    override def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = ((lhs,rhs) match {
      case (Def(NumericMinus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => numeric_minus(x, unit(y+z)) // (x-y)-z --> x-(y+z)
      case (Def(NumericPlus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => numeric_plus(x, unit(y-z)) // (x+y)-z --> x+(y-z)
      case _ => super.numeric_minus(lhs,rhs)
    }).asInstanceOf[Exp[T]]
    
    
    // stencil implementation
    def sliding(start: Rep[Int], end: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit] = {
      val i = fresh[Int]
      
      val save = context // reset effect context later
      
      // evaluate loop contents f(i)
      val (r0,stms0) = reifySubGraph(f(i))
      
      val (((r1,stms1,subst1),(r2,stms2,subst2)), _) = reifySubGraph {
        
        reflectSubGraph(stms0)
        context = save
        
        // evaluate loop contents f(i+1)
        val ((r1,subst1),stms1) = reifySubGraph(trans.withSubstScope(i -> (i+1)) {
          stms0.foreach(s=>trans.traverseStm(s))
          (trans(r0), trans.subst)
        })
        

        val ((r2,stms2,subst2), _) = reifySubGraph {

          reflectSubGraph(stms1)
          context = save
          
          // evaluate loop contents f(i+2)
          val ((r2,subst2),stms2) = reifySubGraph(trans.withSubstScope(i -> (i+2)) {
            stms0.foreach(s=>trans.traverseStm(s))
            (trans(r0), trans.subst)
          })
          (r2,stms2,subst2)
        }

        println(subst1)
        
        ((r1,stms1,subst1), (r2,stms2,subst2))

      }

      context = save
      
      val defs = stms0.flatMap(_.lhs)
      
      println("r0:")
      stms0.foreach(println)

      println("r1:")
      stms1.foreach(println)

      println("r2:")
      stms2.foreach(println)
          
      println(subst1)
        

      // find overlap syms: defined by f(i), used by f(i+1) and f(i+2)

      val overlap01 = stms1.flatMap { case TP(s,d) => syms(d) filter (defs contains _) }.distinct
      val overlap02 = stms2.flatMap { case TP(s,d) => syms(d) filter (defs contains _) }.distinct

      println("overlap1:")
      overlap01 map (x=>(x,subst1(x))) foreach println

      println("overlap2:")
      overlap02 map (x=>(x,subst2(x))) foreach println

      if (overlap02.nonEmpty)
        println("NOTE: overlap beyond a single loop iteration will be ignored (not yet implemented)")


      val overlap0 = (overlap01++overlap02).distinct
      val overlap1 = overlap0 map subst1
      //val overlap2 = overlap0 map subst2


      // build a variable for each overlap sym.
      // init variables by peeling first loop iteration.
      
      if (end > start) {

        val (rX,substX) = trans.withSubstScope(i -> start) {
          stms0.foreach(s=>trans.traverseStm(s))
          (trans(r0), trans.subst)
        }

        val vars = overlap0 map (x => var_new(substX(x))(x.tp,x.pos.head))

        println("var inits: " + overlap0 + " -> " + vars)
        println("will become var reads: " + overlap0)
        println("will become var writes: " + overlap1)

      
        // now generate the loop
      
        for (j <- (start + unit(1)) until end) {
        
          // read the overlap variables
        
          val reads = (overlap0 zip vars) map (p => (p._1, readVar(p._2)))
        
          println("var reads: " + reads)
        
          // emit the transformed loop body
        
          val (r,substY1) = trans.withSubstScope((reads:+(i->(j-unit(1)))): _*) {
            stms1.foreach(s=>trans.traverseStm(s))
            (trans(r1), trans.subst)
          }
        
          // write the new values to the overlap vars
        
          val writes = (overlap1 zip vars) map (p => (p._1, var_assign(p._2, substY1(p._1))))

          println("var writes: " + writes)
        }
      
      }
    }
    
    
  }



  // test cases below

  val prefix = home + "test-out/epfl/test11-"

  def testStencil0 = withOutFileChecked(prefix+"stencil0") {
    trait Prog extends DSL {
      
      // not actually sliding -- just to have a baseline reference
      def infix_sliding[T:Manifest](n: Rep[Int], f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
        val a = NewArray[T](n)
        (0 until n) foreach { i =>
          a(i) = f(i)
        }
        a
      }
      
      def test(v: Rep[Array[Double]]) = {

        val n = 20
        
        def compute(i: Rep[Int]) = 2.0 * i.toDouble + 3.0
        
        val res = n sliding { i =>
          compute(i) + compute(i+1)
        }
        
        res
      }
    }
    new Prog with Impl
  }


  def testStencil1 = withOutFileChecked(prefix+"stencil1") {
    trait Prog extends DSL with Sliding {
      def test(v: Rep[Array[Double]]) = {

        val n = 20
        
        def compute(i: Rep[Int]) = 2.0 * i.toDouble + 3.0
        
        val res = n sliding { i =>
          compute(i) + compute(i+1)
        }
        
        res
      }
    }
    new Prog with Impl with SlidingExp
  }


  /*

  const int N = 20;
  void program0(vector<double> input, vector<double>& output) {
    vector<double> input2(N);
    vector<double> wind(N);
    for (int i = 0; i < N; ++i) {
      input2[i]=input[i];
    }
    for (int i = 0; i < N-1; ++i) {
      wind[i] = input2[i] * input2[i+1];
    }
    for (int i = 1; i < N+1; ++i) {
      output[i] = input2[i] - wind[i] + wind[i-1];
    }
  }
  */

  def testStencil2a = withOutFileChecked(prefix+"stencil2a") {
    trait Prog extends DSL with Sliding {
      def test(v: Rep[Array[Double]]) = {

        val n = v.length
        val input = v
        val output = NewArray[Double](n)
        
        // single iteration

        def a(j: Rep[Int]) = input(j)
        
        def w(j: Rep[Int]) = a(j) * a(j+1)
        
        // regular for loop
        
        // result has 3 reads, 1 write, 4 flops = 32/4 bytes/flop

        for (i <- (1 until n-1)) {
          output(i) = a(i) - w(i) + w(i-1)
        }
        
        output
      }
    }
    new Prog with Impl with SlidingExp
  }

  def testStencil2b = withOutFileChecked(prefix+"stencil2b") {
    trait Prog extends DSL with Sliding {
      def test(v: Rep[Array[Double]]) = {

        val n = v.length
        val input = v
        val output = NewArray[Double](n)
        
        // single iteration

        def a(j: Rep[Int]) = input(j)
        
        def w(j: Rep[Int]) = a(j) * a(j+1)
        
        // sliding for loop
        
        // result has 1 read, 1 write, 3 flops = 16/3 bytes/flop

        for (i <- (1 until n-1).sliding) {
          output(i) = a(i) - w(i) + w(i-1)
        }
        
        output
      }
    }
    new Prog with Impl with SlidingExp
  }


  def testStencil3a = withOutFileChecked(prefix+"stencil3a") {
    trait Prog extends DSL with Sliding {
      def test(v: Rep[Array[Double]]) = {

        val n = v.length
        val input = v
        val output = NewArray[Double](n)
        
        // perform two iterations at once

        def a(j: Rep[Int]) = input(j)
        
        def w1(j: Rep[Int]) = a(j) * a(j+1)

        def wm(j: Rep[Int]) = a(j) - w1(j) + w1(j-1)

        def w2(j: Rep[Int]) = wm(j) * wm(j+1)
        
        def b(j: Rep[Int]) = wm(j) - w2(j) + w2(j-1)

        // regular for loop

        // result has 5 reads, 1 write, 14 flops = 48/14 bytes/flop
        
        for (i <- (2 until n-2)) {
          output(i) = b(i)
        }
        output
      }
    }
    new Prog with Impl with SlidingExp
  }

  def testStencil3b = withOutFileChecked(prefix+"stencil3b") {
    trait Prog extends DSL with Sliding {
      def test(v: Rep[Array[Double]]) = {

        val n = v.length
        val input = v
        val output = NewArray[Double](n)
        
        // perform two iterations at once

        def a(j: Rep[Int]) = input(j)
        
        def w1(j: Rep[Int]) = a(j) * a(j+1)

        def wm(j: Rep[Int]) = a(j) - w1(j) + w1(j-1)

        def w2(j: Rep[Int]) = wm(j) * wm(j+1)
        
        def b(j: Rep[Int]) = wm(j) - w2(j) + w2(j-1)

        // sliding for loop
        
        // result has 1 read, 1 write, 6 flops = 16/6 bytes/flop
        
        for (i <- (2 until n-2).sliding) {
          output(i) = b(i)
        }
        output
      }
    }
    new Prog with Impl with SlidingExp
  }

 
}
