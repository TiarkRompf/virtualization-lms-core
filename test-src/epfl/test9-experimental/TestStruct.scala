package scala.virtualization.lms
package epfl
package test9

import common._
import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ArrayLoopsFatExp,ScalaGenArrayLoops,ScalaGenFatArrayLoopsFusionOpt,TransformingStuff}

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

  def Complex(re: Rep[Double], im: Rep[Double]) = struct[Complex]("Complex", Map("re"->re, "im"->im))
  def infix_re(c: Rep[Complex]): Rep[Double] = field[Double](c, "re")
  def infix_im(c: Rep[Complex]): Rep[Double] = field[Double](c, "im")
  
}


trait StructExp extends BaseExp with VariablesExp with IfThenElseExp with ArrayLoopsExp {
  
  case class Struct[T](tag: String, elems: Map[String,Rep[Any]]) extends Def[T]
  case class Field[T](struct: Rep[Any], index: String) extends Def[T]
  
  
  // FIXME: need  syms override because Map is not a Product
  override def syms(x: Any): List[Sym[Any]] = x match {
    case z:Iterable[_] => z.toList.flatMap(syms)
    case _ => super.syms(x)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case z:Iterable[_] => z.toList.flatMap(symsFreq)
    case _ => super.symsFreq(e)
  }
  
  def struct[T:Manifest](tag: String, elems: Map[String,Rep[Any]]): Rep[T] = Struct[T](tag, elems)
  
  def field[T:Manifest](struct: Rep[Any], index: String): Rep[T] = struct match {
    case Def(Struct(tag, elems)) => elems(index).asInstanceOf[Rep[T]]
    case _ => Field[T](struct, index)
  }
  
  
  override def var_new[T:Manifest](init: Exp[T]): Var[T] = init match {
    case Def(Struct(tag, elems)) => 
      //val r = Variable(struct(tag, elems.mapValues(e=>var_new(e).e))) // DON'T use mapValues!! <--lazy
      Variable(struct[Variable[T]](tag, elems.map(p=>(p._1,var_new(p._2).e))))
    case _ => 
      super.var_new(init)
  }

  override def var_assign[T:Manifest](lhs: Var[T], rhs: Exp[T]): Exp[Unit] = (lhs,rhs) match {
    case (Variable(Def(Struct(tagL,elemsL:Map[String,Exp[Variable[Any]]]))), Def(Struct(tagR, elemsR))) => 
      assert(tagL == tagR)
      assert(elemsL.keySet == elemsR.keySet)
      for (k <- elemsL.keySet)
        var_assign(Variable(elemsL(k)), elemsR(k))
      Const(())
    case _ => super.var_assign(lhs, rhs)
  }
  
  override def readVar[T:Manifest](v: Var[T]) : Exp[T] = v match {
    case Variable(Def(Struct(tag, elems: Map[String,Exp[Variable[Any]]]))) => 
      struct[T](tag, elems.map(p=>(p._1,readVar(Variable(p._2)))))
    case _ => super.readVar(v)
  }
  
  
  def reReify[T:Manifest](a: Rep[T]): Rep[T] = a match { // TODO: should work with loop bodies, too (Def!)
    // TODO: this seems inherently unsafe because it duplicates effects. what should we do about it?
    case Def(Reify(Def(Struct(tag,elems)),es,u)) =>
      struct[T](tag, elems.map(p=>(p._1,toAtom(Reify(p._2, es, u)))))
    case _ => a
  }
  
  override def ifThenElse[T:Manifest](cond: Rep[Boolean], a: Rep[T], b: Rep[T]) = (reReify(a),reReify(b)) match {
    case (Def(Struct(tagA,elemsA)), Def(Struct(tagB, elemsB))) => 
      assert(tagA == tagB)
      assert(elemsA.keySet == elemsB.keySet)
      val elemsNew = for (k <- elemsA.keySet) yield (k -> ifThenElse(cond, elemsA(k), elemsB(k)))
      struct[T](tagA, elemsNew.toMap)
    case _ => super.ifThenElse(cond,a,b)
  }
  
  override def simpleLoop[A:Manifest](size: Exp[Int], v: Sym[Int], body: Def[A]): Exp[A] = body match {
    case ArrayElem(Def(Struct(tag, elems))) => 
      struct[A](tag, elems.map(p=>(p._1,simpleLoop(size, v, ArrayElem(p._2)))))
    case _ => super.simpleLoop(size, v, body)
  }
  
}


trait ScalaGenStruct extends ScalaGenBase {
  val IR: StructExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Struct(tag, elems) => 
      emitValDef(sym, "XXX " + rhs)
    case Field(struct, index) =>  
      emitValDef(sym, "XXX " + rhs)
    case _ => super.emitNode(sym, rhs)
  }
}



class TestStruct extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test9-"
  
  trait DSL extends ComplexArith with ArrayLoops with Arith with OrderingOps with Variables with LiftVariables with IfThenElse with RangeOps with Print {
    def infix_toDouble(x: Rep[Int]): Rep[Double] = x.asInstanceOf[Rep[Double]]
    def test(x: Rep[Int]): Rep[Any]
  }

  trait Impl extends DSL with ComplexStructExp with ArrayLoopsExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp { self => 
    override val verbosity = 2
    val codegen = new ScalaGenArrayLoops with ScalaGenStruct with ScalaGenArith with ScalaGenOrderingOps 
      with ScalaGenVariables with ScalaGenIfThenElse with ScalaGenRangeOps 
      with ScalaGenPrint { val IR: self.type = self }
    codegen.emitSource(test, "Test", new PrintWriter(System.out))
  }

  trait ImplFused extends DSL with ComplexStructExp with ArrayLoopsFatExp with ArithExp with OrderingOpsExp with VariablesExp 
      with IfThenElseExp with RangeOpsExp with PrintExp with TransformingStuff { self => 
    override val verbosity = 2
    val codegen = new ScalaGenFatArrayLoopsFusionOpt with ScalaGenStruct with ScalaGenArith with ScalaGenOrderingOps 
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
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          
          // TODO: how to split loops? don't want to duplicate computation!
          
          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => Complex(0.0 - i.toDouble, i.toDouble) }

          var vvar = vector2

          // TODO: how to split conditionals? might have struct in only one branch ...
          
          val vector3 = if (x > 7) vector1 else vvar
          
          // conditional is reflected because it reads vvar -- effect ordering for split terms?
          // don't want them to be reordered. plus: will reflect/reify prevent matching on struct?
          
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
      trait Prog extends DSL {
        def test(x: Rep[Int]) = {
          
          // TODO: how to split loops? don't want to duplicate computation!
          
          val vector1 = array(100) { i => Complex(i.toDouble, 0.0 - i.toDouble) }
          val vector2 = array(100) { i => Complex(0.0 - i.toDouble, i.toDouble) }

          var vvar = vector2

          // TODO: how to split conditionals? might have struct in only one branch ...
          
          val vector3 = if (x > 7) vector1 else vvar
          
          // conditional is reflected because it reads vvar -- effect ordering for split terms?
          // don't want them to be reordered. plus: will reflect/reify prevent matching on struct?
          
          vvar = vector1
            
          print(vvar)
          print(vector3)
        }
      }
      new Prog with ImplFused
    }
    assertFileEqualsCheck(prefix+"struct2b")
  }

}