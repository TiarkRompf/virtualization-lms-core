package test1

import util.GraphUtil
import java.io.PrintWriter

trait Base extends EmbeddedControls {
  type Rep[+T]
}

trait Expressions {
  
  // expressions
  abstract class Exp[+T]

  case class Const[T](x: T) extends Exp[T]
  case class Sym[T](n: Int) extends Exp[T]

  var nVars = 0
  def fresh[T] = Sym[T] { nVars += 1; nVars -1 }

  // definitions
  abstract class Def[T]

  case class TP[T](sym: Sym[T], rhs: Def[T]) {
    override def toString() = sym + " = " + rhs
  }
  
  var globalDefs: List[TP[_]] = Nil

  def findDefinition[T](s: Sym[T]): Option[TP[T]] = 
    globalDefs.find(_.sym == s).asInstanceOf[Option[TP[T]]]
    
  def findDefinition[T](d: Def[T]): Option[TP[T]] =
    globalDefs.find(_.rhs == d).asInstanceOf[Option[TP[T]]]
  
  def findOrCreateDefinition[T](d: Def[T]): TP[T] =
    findDefinition[T](d).getOrElse {
      createDefinition(fresh[T], d)
    }

  def createDefinition[T](s: Sym[T], d: Def[T]): TP[T] = {
    val f = TP(s, d)
    globalDefs = globalDefs:::List(f)
    f
  }

  implicit def toAtom[T](d: Def[T]): Exp[T] = {
    findOrCreateDefinition(d).sym
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) => 
        findDefinition(s).map(_.rhs)
      case _ =>
        None
    }
  }
  
  // dependencies
  
/*
  def syms(d: Product): List[Sym[Any]] = { // TODO: cleanup
    for {
      e <- d.productIterator.toList
      f <- (e match {
        case s: Sym[Any] => List(s)
        case p: Product => syms(p)
        case _ => Nil
      })
    } yield f
  }
*/

  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(syms(_))
    case _ => Nil
  }
  
  def dep(e: Exp[Any]): List[Sym[Any]] = e match {
    case Def(d: Product) => syms(d)
    case _ => Nil
  }

}


trait BaseExp extends Base with Expressions {

  type Rep[+T] = Exp[T]
  
}


trait Compile extends Base {
  
  //def compile[A,B](f: Rep[A] => Rep[B])(implicit mA: Manifest[A], mB: Manifest[B]): A=>B
  
}


trait Scheduling extends Expressions {
  def buildScheduleForResult(start: Exp[_]): List[TP[_]] = {
    val st = syms(start)
    GraphUtil.stronglyConnectedComponents[TP[_]](st.flatMap(e => findDefinition(e).toList), { d =>
      //println("dep"+d +"="+dep(d.rhs))
      dep(d.rhs).flatMap { e =>
        //println(d + "->" + e)
        findDefinition(e).toList
      }
    }).flatten.reverse // inefficient!
  }
}


trait GenericCodegen extends Expressions with Scheduling {
  
  def emitBlock(y: Exp[_], stream: PrintWriter): Unit = {
    val deflist = buildScheduleForResult(y)
    
    for (TP(sym, rhs) <- deflist) {
      emitNode(sym, rhs, stream)
    }
  }

  def getBlockResult[A](s: Exp[A]): Exp[A] = s
  
  def emitNode(sym: Sym[_], rhs: Def[_], stream: PrintWriter): Unit = {
    throw new Exception("don't know how to generate code for: " + rhs)
  }
  
  def emitValDef(sym: Sym[_], rhs: String, stream: PrintWriter): Unit

  def quote(x: Exp[_]) = x match {
    case Const(s: String) => "\""+s+"\""
    case Const(z) => z.toString
    case Sym(n) => "x"+n
  }
}


trait DisableCSE extends Expressions {
  override def findDefinition[T](d: Def[T]) = None
}

trait DisableDCE extends Scheduling {
  override def buildScheduleForResult(start: Exp[_]): List[TP[_]] =
    globalDefs
}
