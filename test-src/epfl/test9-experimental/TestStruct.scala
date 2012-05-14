package scala.virtualization.lms
package epfl
package test9

import common._
import internal.{FatExpressions,GenericFatCodegen}


import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ArrayLoopsFatExp,ScalaGenArrayLoops,ScalaGenFatArrayLoopsFusionOpt,TransformingStuff}
import scala.reflect.SourceContext

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait ComplexArith extends Arith with ComplexBase with OverloadHack {
  
  def infix_+(x: Rep[Complex], y: Rep[Complex])(implicit o: Overloaded1): Rep[Complex] = Complex(x.re + y.re, x.im + y.im)
  def infix_-(x: Rep[Complex], y: Rep[Complex])(implicit o: Overloaded1): Rep[Complex] = Complex(x.re - y.re, x.im - y.im)
  //def infix_*(x: Rep[Complex], y: Rep[Complex]): Rep[Complex] = Complex(x.re + y.re, x.im + y.im)
  
}

trait ComplexBase extends Arith {
  
  class Complex
  
  def Complex(re: Rep[Double], im: Rep[Double]): Rep[Complex]
  def infix_re(c: Rep[Complex]): Rep[Double]
  def infix_im(c: Rep[Complex]): Rep[Double]
}

trait ComplexStructExp extends ComplexBase with StructExp {

  def Complex(re: Rep[Double], im: Rep[Double]) = struct[Complex](List("Complex"), Map("re"->re, "im"->im))
  def infix_re(c: Rep[Complex]): Rep[Double] = field[Double](c, "re")
  def infix_im(c: Rep[Complex]): Rep[Double] = field[Double](c, "im")
  
}



// ------ struct impl follows, will move to common once stable

trait StructExpOptLoops extends StructExpOptCommon with ArrayLoopsExp {
  
  override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case ArrayElem(Block(Def(Struct(tag, elems)))) => 
      struct[A]("Array"::tag, elems.map(p=>(p._1,simpleLoop(size, v, ArrayElem(Block(p._2)))(p._2.Type.arrayManifest))))
    case ArrayElem(Block(Def(ArrayIndex(b,v)))) if infix_length(b) == size => b.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
    case _ => super.simpleLoop(size, v, body)
  }
  
  override def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = a match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[T]]])) =>
      assert(pre == "Array")
      def unwrap[A](m:Manifest[Array[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ =>
          if (m.erasure.isArray) mtype(Manifest.classType(m.erasure.getComponentType))
          else { printerr("warning: expect type Array[A] but got "+m); mtype(manifest[Any]) }
      }
      struct[T](tag, elems.map(p=>(p._1,infix_at(p._2, i)(unwrap(p._2.Type)))))
    case _ => super.infix_at(a,i)
  }
  
  override def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int] = a match {
    case Def(Struct(pre::tag,elems:Map[String,Exp[Array[T]]])) =>
      assert(pre == "Array")
      val ll = elems.map(p=>infix_length(p._2)) // all arrays must have same length!
      ll reduceLeft { (a1,a2) => assert(a1 == a2); a1 }
    case _ => super.infix_length(a)
  }

}

// ----- test cases


class TestStruct extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends ComplexArith with ArrayLoops with Arith with OrderingOps with Variables with LiftVariables with IfThenElse with RangeOps with Print {
    def infix_toDouble(x: Rep[Int]): Rep[Double]
    def test(x: Rep[Int]): Rep[Any]
  }

  trait Impl extends DSL with ComplexStructExp with ArrayLoopsExp with StructExpOptLoops with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp { self => 
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x match { case Const(c) => Const(c.toDouble) case Sym(n) => Sym[Double](n) }//Hack
    override val verbosity = 2
    val codegen = new ScalaGenArrayLoops with ScalaGenStruct with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }

  trait ImplFused extends DSL with ComplexStructExp with StructExpOptLoops with StructFatExpOptCommon with ArrayLoopsFatExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp with TransformingStuff { self => 
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x match { case Const(c) => Const(c.toDouble) case Sym(n) => Sym[Double](n) }//Hack
    override val verbosity = 2
    val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenFatStruct with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self;
        override def shouldApplyFusion(currentScope: List[TTP])(result: List[Exp[Any]]): Boolean = true }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }

  
  
  def testStruct1 = {
    withOutFile(prefix+"struct1") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = Complex(x.toDouble, 0)
          c = c + Complex(0,x.toDouble)
          print(c)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"struct1")
  }

  def testStruct2 = {
    withOutFile(prefix+"struct2") {
      // test basic struct flattening (loops, variables, conditionals)
      println("REMARK: this makes only sense with fat codegen (computation duplicated and some structs not removed otherwise)")
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          // split loops (rely on fusion, don't want to duplicate computation!)

          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => Complex(0.0 - i.toDouble, i.toDouble) }

          var vvar = vector2

          // split conditionals (be careful about effects)

          val vector3 = if (x > 7) vector1 else vvar

          // conditional is reflected because it reads vvar -- effect ordering for split terms?

          vvar = vector1

          print(vvar)
          print(vector3)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"struct2")
  }

  def testStruct2b = {
    withOutFile(prefix+"struct2b") {
      // test basic struct flattening (loops, variables, conditionals)
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          // split loops (rely on fusion, don't want to duplicate computation!)

          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => Complex(0.0 - i.toDouble, i.toDouble) }

          var vvar = vector2

          // split conditionals (be careful about effects)

          val vector3 = if (x > 7) { 
            print("foobar true")
            vector1
          } else {
            print("foobar false")
            vvar 
          }

          vvar = vector1

          print(vvar)
          print(vector3)
        }
      }
      new Prog with ImplFused
    }
    assertFileEqualsCheck(prefix+"struct2b")
  }

  def testStruct3 = {
    withOutFile(prefix+"struct3") {
      // fuse conjugate computation with construction, essentially a no-op
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {

          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }

          def conj(c: Rep[Complex]) = Complex(c.re, 0.0 - c.im)
          def infix_map[A:Manifest,B:Manifest](c: Rep[Array[A]], f: Rep[A] => Rep[B]) = array(c.length) { i => f(c.at(i)) }

          val vector3 = vector1.map(conj)

          print(vector3)
        }
      }
      new Prog with ImplFused {
        // TODO: use a generic Opt trait instead of defining rewrites here...
        override def infix_-(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = (x, y) match {
          case (x, Def(Minus(Const(0.0),y))) => infix_+(x,y)
          case _ => super.infix_-(x,y)
        }
        override def infix_+(x: Exp[Double], y: Exp[Double])(implicit ctx: SourceContext) = (x, y) match {
          case (Const(0.0), y) => y
          case _ => super.infix_+(x,y)
        }
      }
    }
    assertFileEqualsCheck(prefix+"struct3")
  }

  def testStruct4 = {
    withOutFile(prefix+"struct4") {
      trait Prog extends DSL {
        // recognize that only imaginary part is modified, real part untouched
        def test(x: Rep[Int]) = {

          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => Complex(0.0 - i.toDouble, i.toDouble) }

          def conj(c: Rep[Complex]) = Complex(c.re, 0.0 - c.im)
          def infix_map[A:Manifest,B:Manifest](c: Rep[Array[A]], f: Rep[A] => Rep[B]) = array(c.length) { i => f(c.at(i)) }

          var vvar = vector1 // force access outside conditional, otherwise construction will be moved inside, defeating purpose of test

          // result of this conditional should be a *single* array 
          // containing the flattened im fields. re fields should be
          // unconditional.
          val vector3 = if (x > 7) {
            vector1.map(conj)
          } else {
            vector1
          }

          print(vector3)
        }
      }
      new Prog with ImplFused
    }
    assertFileEqualsCheck(prefix+"struct4")
  }

}