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
  
  trait DSL extends LiftNumeric with NumericOps with ArrayOps with RangeOps 
    with BooleanOps with OrderingOps
    with LiftVariables with IfThenElse with Print {
    def staticData[T:Manifest](x: T): Rep[T]
    def infix_toDouble(x: Rep[Int]): Rep[Double]
    def test(x: Rep[Array[Double]]): Rep[Array[Double]]
  }
  trait Impl extends DSL with Runner with ArrayOpsExpOpt with NumericOpsExpOpt 
      with OrderingOpsExpOpt with BooleanOpsExp with OrderingOpsExp
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
      sliding(NewArray[T](n),0,n)(f)
    }

    def sliding[T:Manifest](array: Rep[Array[T]], start: Rep[Int], end: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]]

    
  }
  
  //trait SlidingExp extends Impl with Sliding {
  trait SlidingExp extends ArrayOpsExpOpt with NumericOpsExpOpt 
      with OrderingOpsExpOpt with BooleanOpsExp 
      with EqualExpOpt with VariablesExpOpt with RangeOpsExp
      with IfThenElseExpOpt {
    
    /*
    idea:
      
      loop(n) { i => f(i) }
      
      evaluate f(i), stms0 be all statements computed
      treat all defined syms as outputs: (s1,s2,s3,r)
      
      loop(n) { i => (s1,s2,s3,r) }
    */
    
    
    object trans extends ForwardTransformer { 
      val IR: SlidingExp.this.type = SlidingExp.this

      // perform only one step of lookup, otherwise we confuse things: 
      // TODO is this a general problem?
      //
      //                     x4 --> x7 (input)
      // val x5 = 2 * x4     x5 --> x8
      // val x6 = x5 + 3     x6 --> x9          
      // val x7 = x4 + 1                val x12 = x7 + 1
      // val x8 = 2 * x7                val x13 = 2 * x12
      // val x9 = x8 + 3                val x14 = x13 + 3     // this sets x9 --> x14
      // val x10 = x6 + x9              val x15 = x14 + x14   // here, transitively x6 --> x9 --> x14
      //                                                      // but we'd rather have x15 = x9 + x14
      
      override def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
        case Some(y) => y.asInstanceOf[Exp[A]] case _ => x 
      }
    }
    
    // some arithemetic rewrites
    override def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = ((lhs,rhs) match {
      case (Def(NumericMinus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => if (y > z) numeric_minus(x, unit(y-z)) else numeric_plus(x,unit(z-y))  // (x-y)+z --> x-(y-z) or x+(z-y)
      case _ => super.numeric_plus(lhs,rhs)
    }).asInstanceOf[Exp[T]]
    override def numeric_minus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = ((lhs,rhs) match {
      case (Def(NumericPlus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => numeric_plus(x, unit(y-z)) // (x+y)-z --> x+(y-z)
      case _ => super.numeric_minus(lhs,rhs)
    }).asInstanceOf[Exp[T]]
    
    
    // stencil implementation
    def sliding[T:Manifest](a: Rep[Array[T]], start: Rep[Int], end: Rep[Int])(f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
      val i = fresh[Int]
      
      // evaluate loop contents f(i)
      val (r0,stms0) = reifySubGraph(f(i))
      
      val (((r1,stms1,subst1),(r2,stms2,subst2)), _) = reifySubGraph {
        
        reflectSubGraph(stms0)
        
        val iPlusOne = i+1
        
        // evaluate loop contents f(i+1)
        val ((r1,subst1),stms1) = reifySubGraph(trans.withSubstScope(i -> iPlusOne) {
          stms0.foreach(s=>trans.traverseStm(s))
          (trans(r0), trans.subst)
        })
        

        val ((r2,stms2,subst2), _) = reifySubGraph {

          reflectSubGraph(stms1)

          val iPlusTwo = iPlusOne+1
          
          // evaluate loop contents f(i+2)
          val ((r2,subst2),stms2) = reifySubGraph(trans.withSubstScope(i -> iPlusTwo) {
            stms0.foreach(s=>trans.traverseStm(s))
            (trans(r0), trans.subst)
          })
          (r2,stms2,subst2)
        }

        println(subst1)
        
        ((r1,stms1,subst1), (r2,stms2,subst2))

      }

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

      val overlap0 = (stms1++stms2).flatMap { case TP(s,d) => syms(d) filter (defs contains _) }.distinct
      val overlap1 = overlap0 map subst1
      val overlap2 = overlap0 map subst2

      println("overlap1:")
      overlap01 map (x=>(x,subst1(x))) foreach println

      println("overlap2:")
      overlap02 map (x=>(x,subst2(x))) foreach println


      if (overlap02.nonEmpty)
        println("WARNING: overlap beyond a single loop iteration not yet implemented")

      // build a variable for each overlap sym.
      // init variables by peeling first loop iteration.
      
      if (end > start) {

        val (rX,substX) = trans.withSubstScope(i -> start) {
          stms0.foreach(s=>trans.traverseStm(s))
          (trans(r0), trans.subst)
        }

        val vars = overlap0 map (x => var_new(substX(x))(x.tp,x.pos.head))

        a(start) = rX

        println("var inits: " + overlap0 + " -> " + vars)
        println("will become var reads: " + overlap0)
        println("will become var writes: " + overlap1)

      
        // now generate the loop
      
        for (j <- (start+unit(1)) until end) {
        
          // read the overlap variables
        
          val reads = (overlap0 zip vars) map (p => (p._1, readVar(p._2)))
        
          println("var reads: " + reads)
        
          // emit the transformed loop body
        
          val (r,jPlusOne,substY1) = trans.withSubstScope((reads:+((i+1)->j)): _*) {
            stms1.foreach(s=>trans.traverseStm(s))
            (trans(r1), trans(i+1), trans.subst)
          }
        
          // write the new values to the overlap vars
        
          val writes = (overlap1 zip vars) map (p => (p._1, var_assign(p._2, substY1(p._1))))

          println("var writes: " + writes)
        
          a(j) = r
        }
      
      }
      a
    }
    
    
  }


  val prefix = "test-out/epfl/test11-"

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

  def testStencil2 = withOutFileChecked(prefix+"stencil2") {
    trait Prog extends DSL with Sliding {
      def test(v: Rep[Array[Double]]) = {

        val n = v.length
        val input = v
        val output = NewArray[Double](v.length)
        for (i <- 0 until n) output(i) = -1.0
        

        def a(j: Rep[Int]) = input(j) //if (j > 0 && j < input.length) input(j) else 0
        
        def w(j: Rep[Int]) = a(j) * a(j+1)

        
        // result has 1 read, 1 write, 3 flops = 16/3 bytes/flop

        sliding(output,1,n-1) { i =>
          a(i) - w(i) + w(i-1)
        }
      }
    }
    new Prog with Impl with SlidingExp
  }

  def testStencil3 = withOutFileChecked(prefix+"stencil3") {
    trait Prog extends DSL with Sliding {
      def test(v: Rep[Array[Double]]) = {

        val n = v.length
        val input = v

        val output = NewArray[Double](v.length)
        for (i <- 0 until n) output(i) = -1.0
        
        // perform two iterations at once

        def a(j: Rep[Int]) = input(j) //if (j > 0 && j < input.length) input(j) else 0.0
        
        def w1(j: Rep[Int]) = a(j) * a(j+1)

        def wm(j: Rep[Int]) = a(j) - w1(j) + w1(j-1)

        def w2(j: Rep[Int]) = wm(j) * wm(j+1)
        
        def b(j: Rep[Int]) = wm(j) - w2(j) + w2(j-1)

        // result has 1 read, 1 write, 6 flops = 16/6 bytes/flop
        
        sliding(output,2,n-2) { i =>
          b(i)
        }
      }
    }
    new Prog with Impl with SlidingExp
  }

 
}