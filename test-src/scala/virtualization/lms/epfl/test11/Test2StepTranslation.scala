package scala.virtualization.lms.epfl.test11

import scala.virtualization.lms.epfl.FileDiffSuite
import scala.virtualization.lms.epfl.test1._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal._
import scala.virtualization.lms.util.IndentWriter
import java.io.PrintWriter



class TestArith {
  this: Arith with Base =>
  
  def testFunction(a: Rep[Double]) = {
    (a + 15) / 16 * 20
  }
}



/*
 * Dear sweet limitations of the current codegen design...
 */
trait Let extends Base {
  // let x1 <- x2 in y
  // no programmer-facing construct here, Sym is not visible :)
}

trait LetExp extends EffectExp {
  case class Let[A: Manifest, B: Manifest](x1: Sym[A], x2: Rep[A], y: Rep[B]) extends Def[B]
  def let_in[A: Manifest, B: Manifest](x1: Sym[A], x2: Rep[A], y: Rep[B]) = Let(x1, x2, y)

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case l: Let[_,_] => l.x1 :: boundSyms(l.y)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case l: Let[_,_] => freqNormal(l.x1) ::: freqNormal(l.x2) ::: freqCold(l.y)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenLet extends ScalaGenEffect {
  val IR: LetExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case Let(x1,x2,y) => 
      stream.println("val " + quote(sym) + " = {")
      stream.println("val " + quote(x1) + " = " + quote(x2))
      emitBlock(y)
      stream.println(quote(getBlockResult(y)))
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
  
}

/**
 * The translation trait: this trait gives you the translate function and you need to implement the translate_node 
 * function
 */
trait TranslateExp extends LetExp {
  val IR: BaseExp
  
  var mapping: Map[IR.Sym[Any], Sym[Any]] = Map.empty
  
  def translate[A: Manifest, B: Manifest](f: IR.Exp[A] => IR.Exp[B]): (Exp[A] => Exp[B]) = {
    val x = IR.fresh[A]
    val y = f(x)
    val x_sym = fresh[A]
    
    mapping += (x -> x_sym)
    
    //println("x1 = " + x + " => x_sym = " + x_sym)
    
    (x_val: Exp[A]) => { 
      val result = toAtom(let_in(x_sym, x_val, y)) 
      //println("global defs: " + this.globalDefs)
      //println("result: " + result)
      result
    }
  } 
  
  // Common subexpression elimination and kicking in automatically :)
  implicit def tr[A:Manifest](y1: IR.Exp[A]): Exp[A] = y1 match {
    case yc: IR.Const[A] => Const[A](yc.x)
    case ys: IR.Sym[A] =>
      //println("Looking for mapping: " + ys + " in " + mapping + ")")
      if (mapping.isDefinedAt(ys))
        mapping(ys).asInstanceOf[Exp[A]]
      else {
        //println("Looking for definition: " + ys + " in " + IR.globalDefs + ")")
        val tp = IR.findDefinition(ys).get
        val tr = translate_node(tp.sym, tp.rhs)(ys.Type)
        tr match {
          case trs: Sym[_] => mapping += (ys -> trs) 
        }
        tr
      }
    case _ => sys.error("unknown value")
  }
  
  def translate_node[A:Manifest](sym: IR.Sym[A], rhs: IR.Def[A]): Exp[A]
}


trait TranslateArith extends TranslateExp with ArithExp {
  val IR: ArithExp
  
  // Translation is currently very awkward... 
  def translate_node[A:Manifest](sym: IR.Sym[A], rhs: IR.Def[A]): Exp[A] = rhs match {
    case IR.Plus(a, b) => ((a - b) : Exp[Double]).asInstanceOf[Exp[A]]
    case IR.Minus(a, b) => ((a + b) : Exp[Double]).asInstanceOf[Exp[A]]
    case IR.Times(a, b) => ((a / b) : Exp[Double]).asInstanceOf[Exp[A]]
    case IR.Div(a, b) => ((a * b) : Exp[Double]).asInstanceOf[Exp[A]]    
  }
} 



class Test2StepTranslation extends FileDiffSuite{

  val prefix = "test-out/epfl/test11-2stage-"

  val test = new TestArith with ArithExpOpt
  val gen  = new ScalaGenArith { val IR: test.type = test }
  
  withOutFile(prefix+"1st-stage"){
    gen.emitSource(test.testFunction, "test", new PrintWriter(new IndentWriter(System.out)))
  }
  
  val tst2 = new TranslateArith { val IR: test.type = test }
  val gen2 = new ScalaGenArith with ScalaGenLet { val IR: tst2.type = tst2 }

  withOutFile(prefix+"2nd-stage"){
    gen2.emitSource(tst2.translate(test.testFunction), "test", new PrintWriter(new IndentWriter(System.out)))
  }

}