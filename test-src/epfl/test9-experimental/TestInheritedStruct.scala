package scala.virtualization.lms
package epfl
package test9

import common._
import internal.{FatExpressions,GenericFatCodegen}


import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ArrayLoopsFatExp,ScalaGenArrayLoops,ScalaGenFatArrayLoopsFusionOpt}
import scala.reflect.SourceContext

import util.OverloadHack

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait ComplexInheritArith extends Arith with ComplexInheritBase with OverloadHack {
  

  def infix_+(x: Rep[Complex], y: Rep[Complex]): Rep[Complex] = {
   val xCart = x.toCartesian
   val yCart = y.toCartesian
   CartesianComplex(xCart.re + yCart.re, xCart.im + yCart.im) 
  }
//  def infix_-(x: Rep[Complex], y:0 Rep[Complex])(implicit o: Overloaded1): Rep[Complex] = Complex(x.re - y.re, x.im - y.im)
  //def infix_*(x: Rep[Complex], y: Rep[Complex]): Rep[Complex] = Complex(x.re + y.re, x.im + y.im)
  
}


// not very useful to have separate traits for every subclass of 
// a complex
trait ComplexInheritBase extends Arith {
  
  abstract class Complex
  class CartesianComplex extends Complex
  class PolarComplex extends Complex

  //constructor
  def CartesianComplex(re: Rep[Double], im: Rep[Double]) : Rep[CartesianComplex]
  def infix_re(c: Rep[CartesianComplex]) : Rep[Double]
  def infix_im(c: Rep[CartesianComplex]) : Rep[Double]

  def PolarComplex(r: Rep[Double], phi: Rep[Double]): Rep[PolarComplex]
  def infix_r(c: Rep[PolarComplex]) : Rep[Double]
  def infix_phi(c: Rep[PolarComplex]) : Rep[Double]

  def infix_toCartesian(c: Rep[Complex]): Rep[CartesianComplex]

}

//the exps

trait StructInheritanceExp extends StructExp{

  case class SubclassTag[T](name: ClassTag[T], lineage: List[ClassTag[Any]]) extends StructTag[T]
}

trait ComplexInheritStructExp extends ComplexInheritBase with StructInheritanceExp{

  def CartesianComplex(re: Exp[Double], im: Exp[Double]) = {
    val superclassTag = ClassTag[Any]("Complex") //not ClassTag[Complex]
    val cartesianTag = ClassTag[CartesianComplex]("CartesianComplex")
    val subclassTag = SubclassTag[CartesianComplex](cartesianTag, List(superclassTag))

    struct[CartesianComplex](subclassTag, Map("re"->re, "im"->im))
  }
  def infix_re(c: Exp[CartesianComplex]): Exp[Double] = field[Double](c, "re")
  def infix_im(c: Exp[CartesianComplex]): Exp[Double] = field[Double](c, "im")

  //Polar complex numbers
  def PolarComplex(r: Exp[Double], phi: Exp[Double]) = {
    val superclassTag = ClassTag[Any]("Complex") //not ClassTag[Complex]
    val polarTag = ClassTag[PolarComplex]("PolarComplex")
    val subclassTag = SubclassTag[PolarComplex](polarTag, List(superclassTag))

    struct[PolarComplex](subclassTag, Map("r"->r, "phi"->phi))
  } 
  
  def infix_r(c: Exp[PolarComplex]): Exp[Double] = field[Double](c, "r")
  def infix_phi(c: Exp[PolarComplex]): Exp[Double] = field[Double](c, "phi")
 
  def infix_toCartesian(c: Exp[Complex]): Exp[CartesianComplex] = c match {
    //for now not doing the actual transformation
    case Def(cp@SimpleStruct(SubclassTag(ClassTag("PolarComplex"), _), _)) => CartesianComplex(0,0)
    case Def(cc@SimpleStruct(SubclassTag(ClassTag("CartesianComplex"), _), elems)) => CartesianComplex(elems("re").asInstanceOf[Exp[Double]],
    elems("im").asInstanceOf[Exp[Double]])
  }
 }

// ------ struct impl follows, will move to common once stable

trait StructInheritExpOptLoops extends StructExpOptCommon with ArrayLoopsExp with StructInheritanceExp{
  
  case class ArraySoaTag[T](base: StructTag[T], len: Exp[Int]) extends StructTag[T]

  //should assume a topmost type like "Object"
  private def lub(hierarchy1: List[ClassTag[Any]], hierarchy2: List[ClassTag[Any]]) : ClassTag[Any] = {
    def inner(temp: ClassTag[Any], h1: List[ClassTag[Any]], h2: List[ClassTag[Any]]): ClassTag[Any] = {
      (h1, h2) match {
        case (x::xs, y::ys) => if(x ==y) inner(x, xs, ys) else return temp
        case _ => temp
      }
    }
    inner(hierarchy1.reverse.head, hierarchy1.reverse.tail, hierarchy2.reverse.tail)
  }
  
  override def ifThenElse[T:Manifest](cond: Exp[Boolean], a: Block[T], b: Block[T])(implicit pos: SourceContext) = (a,b) match {
    case (Block(Def(Struct(tagA,elemsA))), Block(Def(Struct(tagB, elemsB)))) => 
      //match the tags, which can now be subclasses
      //the superclass is abstract, and has no keySet 
      (tagA, tagB) match {
        case (SubclassTag(c1, parents1), SubclassTag(c2, parents2)) => {

          val hierarchy1 = (c1::parents1).asInstanceOf[List[ClassTag[Any]]]
          val hierarchy2 = (c2::parents2).asInstanceOf[List[ClassTag[Any]]]

          //find the least upperbound
          val commonClass: ClassTag[Any] = lub(hierarchy1, hierarchy2) 

          //merge the keyset of c1 and c2
          val elemsNewkeys = elemsA.keySet union elemsB.keySet
          val elemsNew = for (k <- elemsNewkeys) yield (
            k -> ifThenElse(cond, Block(elemsA.getOrElse(k, 0.asInstanceOf[Exp[Any]])), Block(elemsB.getOrElse(k, 0.asInstanceOf[Exp[Any]]))) //TODO : add sourcecontext
          )
          struct(commonClass, elemsNew.toMap).asInstanceOf[Exp[T]]
        }

        case _ => super.ifThenElse(cond, a, b)
      }
    case _ => super.ifThenElse(cond, a, b)
  }

/*override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case ArrayElem(Block(Def(Struct(tag:StructTag[A], elems)))) => 
      struct[A](ArraySoaTag[A](tag,size), elems.map(p=>(p._1,simpleLoop(size, v, ArrayElem(Block(p._2)))(p._2.tp.arrayManifest))))
    case ArrayElem(Block(Def(ArrayIndex(b,v)))) if infix_length(b) == size => b.asInstanceOf[Exp[A]] // eta-reduce! <--- should live elsewhere, not specific to struct
    case _ => super.simpleLoop(size, v, body)
  }
  
  override def infix_at[T:Manifest](a: Rep[Array[T]], i: Rep[Int]): Rep[T] = a match {
    case Def(Struct(ArraySoaTag(tag,len),elems:Map[String,Exp[Array[T]]])) =>
      def unwrap[A](m:Manifest[Array[A]]): Manifest[A] = m.typeArguments match {
        case a::_ => mtype(a)
        case _ =>
          if (m.erasure.isArray) mtype(Manifest.classType(m.erasure.getComponentType))
          else { printerr("warning: expect type Array[A] but got "+m); mtype(manifest[Any]) }
      }
      struct[T](tag.asInstanceOf[StructTag[T]], elems.map(p=>(p._1,infix_at(p._2, i)(unwrap(p._2.tp)))))
    case _ => super.infix_at(a,i)
  }
  
  override def infix_length[T:Manifest](a: Rep[Array[T]]): Rep[Int] = a match {
    case Def(Struct(ArraySoaTag(tag,len),elems:Map[String,Exp[Array[T]]])) => len
    case _ => super.infix_length(a)
  }*/

}

// ----- test cases


class TestInheritedStruct extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends ComplexInheritArith with ArrayLoops with Arith with OrderingOps with Variables with LiftVariables with IfThenElse with RangeOps with Print {
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
    def test(x: Rep[Int]): Rep[Any]
  }

  trait Impl extends DSL with ComplexInheritStructExp with ArrayLoopsExp with StructExpOptLoops with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp { self => 
    override val verbosity = 1
    val codegen = new ScalaGenArrayLoops with ScalaGenStruct with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }

  trait ImplFused extends DSL with ComplexInheritStructExp with StructExpOptLoops with StructFatExpOptCommon with ArrayLoopsFatExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp  { self => 
    override val verbosity = 1
    val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenFatStruct with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self;
        override def shouldApplyFusion(currentScope: List[Stm])(result: List[Exp[Any]]): Boolean = true }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }

  
  
  def testStruct1 = {
    withOutFile(prefix+"inheritedstruct1") {
      // test variable splitting
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          var c = CartesianComplex(x.toDouble, 0)
          c = c + PolarComplex(0,x.toDouble)
          print(c)
        }
      }
      new Prog with Impl
    }
    assertFileEqualsCheck(prefix+"inheritedstruct1")
  }

/*  def testStruct2 = {
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
        override def infix_-(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
          case (x, Def(Minus(Const(0.0),y))) => infix_+(x,y)
          case _ => super.infix_-(x,y)
        }
        override def infix_+(x: Exp[Double], y: Exp[Double])(implicit pos: SourceContext) = (x, y) match {
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
*/
}
