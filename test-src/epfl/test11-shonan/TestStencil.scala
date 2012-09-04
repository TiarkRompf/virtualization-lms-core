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
    def test(x: Rep[Array[Int]]): Rep[Array[Int]]
  }
  trait Impl extends DSL with Runner with ArrayOpsExpOpt with NumericOpsExpOpt 
      with OrderingOpsExpOpt with BooleanOpsExp with OrderingOpsExp
      with EqualExpOpt with VariablesExpOpt with RangeOpsExp with StaticDataExp
      with IfThenElseExpOpt with PrintExp 
      with CompileScala { self => 
    //override val verbosity = 1
    val codegen = new ScalaGenNumericOps with ScalaGenStaticData with ScalaGenOrderingOps 
      with ScalaGenArrayOps with ScalaGenRangeOps with ScalaGenBooleanOps
      with ScalaGenVariables with ScalaGenIfThenElse
      with ScalaGenPrint /*with LivenessOpt*/ { val IR: self.type = self }
    dumpGeneratedCode = true
    run()
  }
  trait Runner extends Compile {
    def test(x: Rep[Array[Int]]): Rep[Array[Int]]
    def run() {
      val f = compile(test)
      val v0 = Array(3, 1, 5, -2, 4)
      val v1 = f(v0)
      v1 foreach println
    }
  }

  trait Sliding extends DSL {
    
    def infix_sliding(n: Rep[Int], f: Rep[Int] => Rep[Double]): Rep[Array[Double]]
    
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
    
    def infix_sliding[T:Manifest](n: Rep[Int], f: Rep[Int] => Rep[T]): Rep[Array[T]] = {
      val a = NewArray[T](n)
      
      
      val i = fresh[Int]
      
      val ((r0,r1,stms1,subst), stms0) = reifySubGraph {
        
        // evaluate loop contents f(i)
        val (r0,stms0) = reifySubGraph(f(i))
        reflectSubGraph(stms0)
        
        val iPlusOne = i+1
        
        // evaluate loop contents f(i+1)
        val ((r1,subst),stms1) = reifySubGraph(trans.withSubstScope(i -> iPlusOne) {
          stms0.foreach(s=>trans.traverseStm(s))
          (trans(r0), trans.subst)
        })
        
        println(subst)
        
        (r0,r1,stms1,subst)
      }

      val defs = stms0.flatMap(_.lhs)
      
      println("r0:")
      stms0.foreach(println)

      println("r1:")
      stms1.foreach(println)
          
      println(subst)
        

      // find overlap syms: defined by f(i), used by f(i+1)

      val overlap0 = stms1.flatMap { case TP(s,d) => syms(d) filter (defs contains _) }.distinct
      val overlap1 = overlap0 map subst

      overlap0 zip overlap1 foreach println


      // build a variable for each overlap sym.
      // init variables by peeling first loop iteration.
      
      // if (n > 0) {}

      val (rX,substX) = trans.withSubstScope(i -> unit(0)) {
        stms0.foreach(s=>trans.traverseStm(s))
        (trans(r0), trans.subst)
      }

      val vars = overlap0 map (x => var_new(substX(x))(x.tp,x.pos.head))

      a(unit(0)) = rX

      println("var inits: " + overlap0 + " -> " + vars)
      println("will become var reads: " + overlap0)
      println("will become var writes: " + overlap1)

      
      // now generate the loop
      
      for (j <- unit(1) until (n)) {
        
        // read the overlap variables
        
        val reads = (overlap0 zip vars) map (p => (p._1, readVar(p._2)))
        
        println("var reads: " + reads)
        
        // emit the transformed loop body
        
        val (r,jPlusOne,subst2) = trans.withSubstScope((reads:+((i+1)->j)): _*) {
          stms1.foreach(s=>trans.traverseStm(s))
          (trans(r1), trans(i+1), trans.subst)
        }
        
        // write the new values to the overlap vars
        
        val writes = (overlap1 zip vars) map (p => (p._1, var_assign(p._2, subst2(p._1))))

        println("var writes: " + writes)
        
        a(j) = r
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
      
      def test(v: Rep[Array[Int]]) = {

        val n = 20
        
        def compute(i: Rep[Int]) = 2 * i + 3
        
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
      def test(v: Rep[Array[Int]]) = {

        val n = 20
        
        def compute(i: Rep[Int]) = 2 * i + 3
        
        val res = n sliding { i =>
          compute(i) + compute(i+1)
        }
        
        res
      }
    }
    new Prog with Impl with SlidingExp
  }



/*
  def testStencil2 = withOutFileChecked(prefix+"stencil2") {
    trait Prog {
      type Rep[T]
      def test(v: Rep[Array[Int]]) = {
      }
    }
  }

*/

  def testStencil2 = withOutFileChecked(prefix+"stencil2") {
    trait Prog extends DSL with Sliding {
      def test(v: Rep[Array[Int]]) = {

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
        
        /*
        w = a_0 * a_1
        b = a_0 - w_0 + w_(-1)


        w1 = a_0 * a_1
        wm = a_0 - w1_0 + w1_(-1)
        w2 = wm_0 * wm_1
        b  = wm_0 - w2_0 + w2_(-1)
        */
        
        
        
        
        /*
        .~(loop0 2 0 n .<a>. .<b>. (fun a ->
        let w1 j = a j *@ a (j+1) in 
        let wm j = a j -@ w1 j +@ w1 (j-1) in 
        let w2 j = wm j *@ wm (j+1) in 
        wm 0 -@ w2 0 +@ w2 (-1)))
        */
        
        /*
        
        def compute(b: Rep[Array[Int]], a: Rep[Array[Int]], n: Rep[Int]) = {
          def w(j: Rep[Int]) = a(j) * a(j+1)
          for (i <- 0 until n: Rep[Range]) {
            b(i) = a(i) - w(i) + w(i-1)
          }
        }
        */

        
        /*def compute(a: Rep[Array[Int]], n: Rep[Int]) = {
          def w(j: Rep[Int]) = a(j) * a(j+1)
          n sliding { i =>
            a(i) - w(i) + w(i-1)
          }
        }*/
        val n = v.length
        val input = v

        def a(j: Rep[Int]) = if (j > 0 && j < input.length) input(j) else 0
        
        def w(j: Rep[Int]): Rep[Int] = a(j) * a(j+1)

        val b = n sliding { i =>
          a(i) - w(i) + w(i-1)
        }
        
        b
      }
    }
    new Prog with Impl with SlidingExp {
      override def numeric_plus[T:Numeric:Manifest](lhs: Exp[T], rhs: Exp[T])(implicit pos: SourceContext): Exp[T] = ((lhs,rhs) match {
        case (Def(NumericMinus(x:Exp[Int],Const(y:Int))), Const(z:Int)) => if (y > z) numeric_minus(x, y-z) else numeric_plus(x,z-y) // x-y+z  x-3+2=x-(3-2) x-2+3=x+3-2
        case _ => super.numeric_plus(lhs,rhs)
      }).asInstanceOf[Exp[T]]
    }
  }


 
}