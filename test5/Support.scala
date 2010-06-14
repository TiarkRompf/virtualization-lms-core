package test5

import test1._
import test2._

import java.io.PrintWriter


trait Effects extends BaseExp {
  
  type State = List[Rep[_]]
  
  var context: State = _
  
  def reflectEffect[A](x: Def[A]): Rep[A] = {
     // don't do CSE on effectful computations
    val r: Rep[A] = createDefinition(fresh[A], Reflect(x, context)).sym
    context :+= r
    r
  }
  
  def reifyEffects[A](block: => Rep[A]): Rep[A] = {
    val save = context
    context = Nil
    
    val result = block
    val resultR = if (context.isEmpty) result else Reify(result, context): Rep[A]
    context = save
    resultR
  }
  
  case class Reflect[A](x: Def[A], effects: List[Exp[Any]]) extends Def[A]
  case class Reify[A](x: Exp[A], effects: List[Exp[Any]]) extends Def[A]
}




trait Blocks extends Base {
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]): Rep[T]
}

trait BlockExp extends Blocks with BaseExp {
  case class IfThenElse[T](cond: Exp[Boolean], thenp: Exp[T], elsep: Exp[T]) extends Def[T]
  
  def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = IfThenElse(cond, thenp, elsep)
}


trait BlockExpEffect extends BlockExp with Effects {
  override def __ifThenElse[T](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T]) = {
    val a = reifyEffects(thenp)
    val b = reifyEffects(elsep)
    (a,b) match {
      case (Def(Reify(_,_)), _) | (_, Def(Reify(_,_))) => reflectEffect(IfThenElse(cond,a,b))
      case _ => super.__ifThenElse(cond,a,b)
    }
  }
}


trait BlockCompile extends ScalaCodegen with BlockExpEffect {
  
  override def emitScalaSource[A,B](f: Exp[A] => Exp[B], className: String, stream: PrintWriter)
      (implicit mA: Manifest[A], mB: Manifest[B]): Unit = {
    super.emitScalaSource[A,B](x => reifyEffects(f(x)), className, stream)
  }

  var shallow = false

  override def dep(e: Exp[Any]): List[Sym[Any]] = e match {
    case Def(IfThenElse(c, t, e)) if shallow => syms(c) // in shallow mode, don't count deps from nested blocks
    case Def(Reflect(IfThenElse(c, t, e), effects)) if shallow => syms(c) ::: effects.flatMap(syms)
    case Def(Reflect(d: Product, effects)) => syms(d) ::: effects.flatMap(syms)
//    case Def(Reify(s, effects)) => syms(s) ::: effects.flatMap(dep) // don't count effects, but do count their params
    case _ => super.dep(e)
  }

  var scope: List[TP[_]] = Nil

  override def emitBlock(start: Exp[_], stream: PrintWriter): Unit = {

    val e1 = buildScheduleForResult(start) // deep list of deps
    shallow = true
    val e2 = buildScheduleForResult(start) // shallow list of deps (exclude stuff only needed by nested blocks)
    shallow = false

    //println("==== deep")
    //e1.foreach(println)
    //println("==== shallow")
    //e2.foreach(println)

    val e3 = e1.filter(e2 contains _) // shallow, but with the ordering of deep!!

    val e4 = e3.filterNot(scope contains _) // remove stuff already emitted

    val save = scope
    scope = e4 ::: scope

    for (TP(sym, rhs) <- e4) {
      emitNode(sym, rhs, stream)
    }
    
    start match {
      case Def(Reify(x, effects0)) =>
        
        // with the current implementation the code below is not
        // really necessary. all effects should have been emitted
        // because of the Reflect dependencies. it's still a good
        // sanity check though
        
        val effects = effects0.map { case s: Sym[a] => findDefinition(s).get }
        val actual = e4.filter(effects contains _)

        // actual must be a prefix of effects!
        assert(effects.take(actual.length) == actual, 
            "violated ordering of effects: expected \n    "+effects+"\nbut got\n    " + actual)
        
        val e5 = effects.drop(actual.length)
        
        for (TP(_, rhs) <- e5) {
          emitNode(Sym(-1), rhs, stream)
        }
        
        stream.println(quote(x))
      case x =>
        stream.println(quote(x))
    }
    
    scope = save
  }
  

  override def quote(x: Exp[_]) = x match {
    case Sym(-1) => "_"
    case Const(s: String) => "\""+s+"\""  // TODO: more principled check elsewhere
    case _ => super.quote(x)
  }

  override def emitNode(sym: Sym[_], rhs: Def[_], stream: PrintWriter) = rhs match {
    case Reflect(s, effects) =>
      emitNode(sym, s, stream)
    case Reify(s, effects) =>
      // just ignore -- effects are accounted for in emitBlock
      //stream.println("val " + quote(sym) + " = " + quote(s))
    case IfThenElse(c,a,b) =>  
      stream.println("val " + quote(sym) + " = if (" + quote(c) + ") {")
      emitBlock(a, stream)
      stream.println("} else {")
      emitBlock(b, stream)
      stream.println("}")
    case _ => super.emitNode(sym, rhs, stream)
  }

  
}
