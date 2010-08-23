package scala.virtualization.lms
package epfl
package test3

import common._
import test1._
import test2._

trait Extractors extends Base {
  
  def construct[A,B](c: Class[A], f: B => A, x: Rep[B]): Rep[A]
  def deconstruct[A,B](c: Class[A], f: A => Option[B], x: Rep[A]): Option[Rep[B]]

  def deconstruct2[A,B,C](c: Class[A], f: A => Option[(B,C)], x: Rep[A]): Option[(Rep[B], Rep[C])] = {
    val s = deconstruct[A,(B,C)](c, f, x).get
    Some((fst(s), snd(s)))
  }

  def tuple[A,B](x: Rep[A], y: Rep[B]): Rep[(A,B)]
  def fst[A,B](x: Rep[(A,B)]): Rep[A]
  def snd[A,B](x: Rep[(A,B)]): Rep[B]

  def test[A](x: Rep[A], y: A): Boolean
  
  trait GateKeeper[A] { // shouldn't need it ...
    def guard(y: A): Boolean
  }
  
  implicit def gateKeeper[A](x: Rep[A]) = new GateKeeper[A] { // new {
    def guard(y: A): Boolean = test(x, y)
  }
  
}


trait Matching extends Base {
  
  def evalMatch[A,B](x: Rep[A], alts: List[PartialFunction[Rep[A],Rep[B]]])(implicit mA: Manifest[A]): Rep[B]
  
  implicit def switchable[A](x:Rep[A]) = new {
    def switch[B](f: PartialFunction[Rep[A],Rep[B]]) = new Match[A,B](x, List(f))(null) // FIXME: won't find switch otherwise!!
  }
  
  class Match[A,B](x: Rep[A], alts: List[PartialFunction[Rep[A],Rep[B]]])(implicit mA: Manifest[A]) {
    def orElse[D>:B](f: PartialFunction[Rep[A],Rep[D]]) = {
      new Match[A,D](x, alts ::: List(f))
    }
  
    def end = evalMatch(x, alts)
  }
  
  implicit def endMatch[B](m: Match[_, B]) = m.end
  
  
  def _match[A,B](x: Rep[A])(cases: PartialFunction[Rep[A],Rep[B]]*)(implicit mA: Manifest[A]): Rep[B] = {
    evalMatch(x, List())
  }
}

/*
trait MatchingExtractorsRepString {
  type Rep[+T] = String
  
  var context: List[String] = Nil
  
  def construct[A,B](c: Class[A], f: B => A, x: Rep[B]): Rep[A] = "new " + c.getName + "(" + x + ")"
  def deconstruct[A,B](c: Class[A], f: A => Option[B], x: Rep[A]): Option[Rep[B]] = {
    val s = c.getName + ".unapply(" + x + ")"
    context = context ::: List(s)
    Some(s)
  }
  def tuple[A,B](x: Rep[A], y: Rep[B]): Rep[(A,B)] = "(" + x + "," + y + ")"
  def fst[A,B](x: Rep[(A,B)]): Rep[A] = x + "._1"
  def snd[A,B](x: Rep[(A,B)]): Rep[B] = x + "._2"
  
  def unit[A](x: A): Rep[A] = x.toString
  def test[A](x: Rep[A], y: A) = {
    val s = "assert(" + x + " == " + y + ")"
    context = context ::: List(s)
    true
  }
  
  def evalMatch[A,B](x: Rep[A], alts: List[PartialFunction[Rep[A],Rep[B]]]) = {
    
    def doApply(f: PartialFunction[Rep[A],Rep[B]]) = {
      
      val save = context
      context = Nil
      
      val result = f(x)
      val effects = context
      context = save
      result + " <- " + effects
    }

    alts.map(doApply).mkString("\n- orElse -\n")
  }
}
*/

trait MatchingExtractorsExp extends FunctionsExp with Effects with Control {

  case class Construct[A,B](c: Class[A], x: Rep[B]) extends Def[A]
  case class Deconstruct[A,B](c: Class[A], x: Rep[A]) extends Def[B]
  case class TupleR[A,B](x: Exp[A], y: Exp[B]) extends Def[(A,B)]
  case class First[A,B](x: Exp[(A,B)]) extends Def[A]
  case class Second[A,B](x: Exp[(A,B)]) extends Def[B]
  case class Test[A](x: Exp[A], y: A) extends Def[Unit]

//  case class OrElse[A](x: List[Exp[Effectful[A]]]) extends Def[A]
//  case class AndAlso[A](x: Exp[A], effects: List[Exp[_]]) extends Def[A]

  def construct[A,B](c: Class[A], f: B => A, x: Rep[B]): Rep[A] = Construct(c, x)
  def deconstruct[A,B](c: Class[A], f: A => Option[B], x: Rep[A]): Option[Rep[B]] = {
    val s = Deconstruct(c, x)
    Some(reflectEffect(s))
  }

  def tuple[A,B](x: Rep[A], y: Rep[B]): Rep[(A,B)] = TupleR(x, y)
  def fst[A,B](x: Rep[(A,B)]): Rep[A] = First(x)
  def snd[A,B](x: Rep[(A,B)]): Rep[B] = Second(x)

  //todo had to insert a unit here to remove ambiguous implicit conversion
  def test[A](x: Rep[A], y: A) = {
    reflectEffect(unit(Test(x, y)))
    true
  }
  
    
//  def andAlso[A](x: Rep[A], effects: List[Rep[_]]): Rep[A] = AndAlso(x, effects)

//  def orElse[A](xs: List[Rep[Effectful[A]]]): Rep[A] = reflectEffect(OrElse(xs))
  
  def evalMatch[A,B](x: Rep[A], alts: List[PartialFunction[Rep[A],Rep[B]]])(implicit mA: Manifest[A]): Rep[B] = {
    
    def liftAlt(f: PartialFunction[Rep[A],Rep[B]]) = doLambda { x: Rep[A] =>
      reifyEffects(f(x))
    }

//    alts.map(doApply).reduceRight((x: Exp[B], y: Exp[B]) => toAtom(OrElse(x, y)))
    orElse(alts.map(f => doApply(liftAlt(f), x)))
  }
  
}


trait MatchingExtractorsExpOpt extends MatchingExtractorsExp {
  override def construct[A,B](c: Class[A], f: B => A, x: Rep[B]): Rep[A] = x match {
    case Const(x) => Const(f(x))
    case _ => super.construct(c, f, x)
  }

  override def deconstruct[A,B](c: Class[A], f: A => Option[B], x: Rep[A]) = x match {
    case Def(Construct(`c`, y)) => Some(y.asInstanceOf[Rep[B]]) // CHECK: `c` was _ : c.type 
//    case Const(y) if c.isInstance(y) => ...
    case _ => super.deconstruct(c, f, x)
  }

  override def test[A](x: Rep[A], y: A) = x match {
    case Const(x) if x == y =>
      true
    case _ => super.test(x, y)
  }

/*  
  override def andAlso[A](x: Rep[A], effects: List[Rep[_]]): Rep[A] = effects match {
    case Nil => x
    case _ => super.andAlso(x, effects)
  }
*/
/*
  override def orElse[A](xs: List[Rep[A]]): Rep[A] = xs match {
    case List(x) => x
    case _ => super.orElse(xs)
  }
*/
}


/*
trait MatchingExtractorsExpOpt2 extends MatchingExtractorsExpOpt {

  override def deconstruct[A,B](c: Class[A], f: A => Option[B], x: Rep[A]): Option[Rep[B]] = (x match {
    case Def(OrElse(Def(AndAlso(Def(Construct(`c`, y)), ands)) :: ors)) => 
      Some(orElse[B](andAlso[B](y.asInstanceOf[Rep[B]], ands) :: ors.asInstanceOf[List[Rep[B]]]))
    case _ => None
  }) orElse {
    super.deconstruct(c, f, x)
  }

}
*/


