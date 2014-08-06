/*TODO DISABLED
package scala.virtualization.lms
package epfl
package test10

import common._
import test1._
import test2._
import test3._
import test4._

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext

class TestTransformRec extends FileDiffSuite {

  val prefix = home + "test-out/epfl/test10-"

  trait DSL extends Arith with Functions with Equal with IfThenElse {
    def testFun: Rep[Double => Double]
    def test(x: Rep[Double]): Rep[Double] = testFun(x)
  }

  trait Impl extends DSL with ArithExpOpt with EqualExp with IfThenElseFatExp with LoopsFatExp with FunctionsExternalDef1 { self =>
    override val verbosity = 1

    case class DefineFun2[A,B](res: Block[B])(val arg1: Sym[A], val arg2: Sym[Int]) extends Def[A=>B]

    override def boundSyms(e: Any): List[Sym[Any]] = e match {
      case f@DefineFun2(y) => f.arg1::f.arg2::effectSyms(y)
      case _ => super.boundSyms(e)
    }

    override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
      case DefineFun2(y) => freqHot(y)
      case _ => super.symsFreq(e)
    }

    override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
      case Apply(x,y) => toAtom(Apply(f(x),f(y)))
      case g@DefineFun(y) => toAtom(DefineFun(f(y))(g.arg))
      case _ => super.mirror(e,f)
    }).asInstanceOf[Exp[A]]

    val runner = new Runner { val p: self.type = self }
    runner.run()
  }

  trait Runner {
    val p: Impl
    def run() = {
      val x = p.fresh[Double]
      val y = p.reifyEffects(p.test(x))

      val codegen = new Codegen { val IR: p.type = p }

      println("-- before transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(y)
      }
      val trans0 = new RecursiveTransformer {
        val IR: p.type = p
      }
      try {
        val z0 = trans0.run(y)
        println("-- after null transformation")
        codegen.withStream(new PrintWriter(System.out)) {
          codegen.emitBlock(z0)
        }
      } catch {
        case ex =>
        println("error: " + ex)
      }
      val trans = new MyTransformer {
        val IR: p.type = p
      }
      try {
        val z = trans.run(y)

        println("-- after transformation")
        codegen.withStream(new PrintWriter(System.out)) {
          codegen.emitBlock(z)
        }
      } catch {
        case ex =>
        println("error: " + ex)
      }
      println("-- done")
    }
  }

  trait MyTransformer extends RecursiveTransformer {
    val IR: Impl
    import IR._

    override def transformDef[A](lhs: Sym[A], rhs: Def[A]) = (rhs match {
      case g@DefineFun(y) => Some(() => DefineFun2(apply(g.res))(g.arg, fresh[Int]))
      case _ => super.transformDef(lhs, rhs)
    }).asInstanceOf[Option[() => Def[A]]]
  }

  trait Codegen extends ScalaGenArith with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenFunctionsExternal {
    val IR: Impl
    import IR._

    override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
      case e@DefineFun2(y) =>
        emitValDef(sym, "/*2*/{" + quote(e.arg1) + ": (" + e.arg1.tp + ") => "/*}*/)
        emitBlock(y)
        stream.println(quote(getBlockResult(y)))
        stream.println("}")
      case _ => super.emitNode(sym, rhs)
    }
  }

  def testSimple = withOutFileChecked(prefix+"transformrec1") {
    trait Prog extends DSL {
      def testFun = doLambda { n => n + 1.0 }
    }
    new Prog with Impl
  }

  def testRec = withOutFileChecked(prefix+"transformrec2") {
    trait Prog extends DSL {
      def testFun = doLambda { n =>
        if (n == 0) 1.0 else n * testFun(n - 1.0)
      }
    }
    new Prog with Impl
  }

  def testMutuallyRec = withOutFileChecked(prefix+"transformrec3") {
    trait Prog extends DSL {
      def testFun = doLambda { n =>
        if (n == 0) 1.0 else n * other(n)
      }
      def other: Rep[Double=>Double] = doLambda { n =>
        testFun(n-1.0)
      }
    }
    new Prog with Impl
  }
}
*/
