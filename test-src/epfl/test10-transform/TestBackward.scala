package scala.virtualization.lms
package epfl
package test10

import common._
import internal._

import scala.reflect.SourceContext

// investigate backward analysis -- these are preliminary ideas ...
//
//  def foobar(x: Exp[Int]) = onBwdInfo { bw => ...  }
//
//  def mutableClone[T](x: Exp[Vector[T]]) = 
//    MutableClone(x) onBwdInfo { (selfsym, bw) => 
//      if (!bw.isMutated(selfsym)) x  // <---  question: this replaces a mutable with a non-mutable symbol
//      else selfsym
//    }
//

trait TestDSL extends BaseExp with LiftAll {
  
  case class BlockStm[+T](stms: List[Stm], res: Exp[T])

  case class IfThenElse[T](c: Rep[Boolean], a: BlockStm[T], b: BlockStm[T]) extends Def[T]

  type BwdInfo = Effects

  case class WaitBwd[T](e: Effects, body: (Sym[T], BwdInfo) => (List[Stm], BwdInfo)) extends Def[T]

  type Effects = List[String]
  
  case class Print(e: Exp[Any]) extends Def[Unit]
  case class Forward[T](e: Exp[T]) extends Def[Unit]
  case class Abstract[T]() extends Def[T]

  def getEffects(block: BlockStm[Any]): Effects = block.stms.flatMap(getEffects)
  def getEffects(stm: Stm): Effects = stm match {
    case TP(_, WaitBwd(e, _)) => e
    case TP(_, Print(_)) => List("io")
  }

  def getUses(stm: Stm): Effects = stm match {
    case TP(_, Print(x)) => List(x.toString)
    case TP(_, IfThenElse(c,a,b)) => List(c.toString)
  }

  def getKills(stm: Stm): Effects = stm match {
    case TP(s, _) => List(s.toString)
  }

/*  
  def readVar[T](v: Rep[Variable[T]]): Rep[T] = {
    onContext(v) { fw =>
      fs.last(v) match {
        case Assign(v, rhs) => rhs
        case NewVar(rhs) => 
      }
    }
  }
*/

  def ifThenElse[T](c: Rep[Boolean], a: => Rep[T], b: => Rep[T]): Rep[T] = {
    
    //onReachableFw { // automatically called
      if (c == Const(true)) a
      else if (c == Const(false)) b
      else {      
      
        val ab = reifyBlock(a)
        val bb = reifyBlock(b) // result is what statements can be computed ...
        onValuesEffectsNeeded(getEffects(ab) ++ getEffects(bb)) { (s,bw) =>
          val (au, ae) = flowBwd(bw filterNot (_ == s.toString), ab)
          val (bu, be) = flowBwd(bw filterNot (_ == s.toString), bb)
          
          if (au == bu && ae == be)
            (au.stms:+createDefinition(s,Forward(au.res)), ae)
          else
            (List(createDefinition(s,IfThenElse(c, au, bu))), ae ++ be ++ List(c.toString))
        }
      
      }
    //}
  }
  

  def print(e: Rep[Any]): Rep[Unit] = {
    reflectIO(Print(e))
  }
  
  def onValuesEffectsNeeded[T](e: Effects)(body: (Sym[T], BwdInfo) => (List[Stm], Effects)): Exp[T] = {
    reflect(WaitBwd(e,body))
  }

  def reflectPure[T](d: => Def[T]) = {
    onValuesEffectsNeeded(List()) { (s,bw) => 
      if (bw contains s.toString)
        (List(createDefinition(s,d)), (bw filterNot (_ == s.toString)) ++ (syms(d) map (_.toString)))
      else
        (List(), bw)
    }
  }

  def reflectIO[T](d: => Def[T]) = {
    onValuesEffectsNeeded(List("io")) { (s,bw) => 
      if ((bw contains "io") || (bw contains s.toString))
        (List(createDefinition(s,d)), (bw filterNot (_ == s.toString)) ++ (syms(d) map (_.toString)))
      else
        (List(), bw)
    }
  }


  override def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    // remove previous definition and update in place. TODO: find a better way ...
    globalDefs = globalDefs filterNot (_.lhs contains s)
    globalDefsCache = globalDefsCache - s
    super.createDefinition(s,d)
  }

  def reflect[T](d: Def[T]): Exp[T] = {
    val sym = fresh[T](List(implicitly[SourceContext]))(mtype(manifest[Any]))
    curStms = curStms :+ createDefinition(sym,d)
    sym
  }
    
  
  var curStms: List[Stm] = null
  def reifyBlock[T](body: => Rep[T]): BlockStm[T] = {
    val save = curStms
    curStms = Nil
    val res = body
    val stms = curStms
    curStms = save
    BlockStm(stms, res)
  }
  
  
  def emitStm[T](stm: Stm): Unit = stm match {
    case TP(sym, IfThenElse(c,a,b)) => println(sym + " = if (" + c + ")"); emitBlock(a); println("else"); emitBlock(b)
    case TP(sym, rhs) => println(sym + " = " + rhs)
  }

  def emitBlock[T](block: BlockStm[T]): Unit = {
    println("{")
    block.stms.foreach(emitStm)
    println(block.res)    
    println("}")
  }
  
  
  def flowBwd[T](e: Effects, block: BlockStm[T]) = {
    var ee = e ++ List(block.res.toString)
    val stms2 = block.stms.reverse.flatMap {
      case TP(sym, WaitBwd(e, r: ((Sym[Any], BwdInfo) => (List[Stm], BwdInfo)))) => // FIXME: type annot?
        val (stm2, ee2) = r(sym, e)
        ee = ee2
        stm2
    }
    (BlockStm(stms2,block.res), ee)
  }
  
  
  def emitSource[A,B](f: Rep[A] => Rep[B]) = {
    
    val block1 = reifyBlock {
      f(fresh[A](List(implicitly[SourceContext]))(mtype(manifest[Any])))
    }
    
    println("===== first round")
    emitBlock(block1)
    
    val (block2, _) = flowBwd(List("io"), block1)
    
    println("===== second round")
    emitBlock(block2)
  }
  
  
  
}





class TestBackwards extends FileDiffSuite {
  
  val prefix = "test-out/epfl/test10-"
  
  def testBwd1 = {
    withOutFile(prefix+"bwd1") {
      object Prog extends TestDSL {
        def test(x: Rep[Int]) = {
          ifThenElse(true, { print("t"); 1 }, { print("f"); 4 })
          ifThenElse(reflectPure(Abstract()), { print("x"); 1 }, { print("x"); 1 })
          8
        }
        def run = {
          emitSource((x:Rep[Int]) => test(x))
        }
      }
      Prog.run
    }
    assertFileEqualsCheck(prefix+"bwd1")
  }

}
