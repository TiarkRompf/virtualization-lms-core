package scala.virtualization.lms
package internal

trait Effects extends Expressions {
  
  type State = List[Exp[_]]
  
  var context: State = _
  
  def reflectEffect[A:Manifest](x: Def[A]): Exp[A] = {
     // don't do CSE on effectful computations
    val r: Exp[A] = createDefinition(fresh[A], Reflect(x, context)).sym
    context :+= r
    r
  }

  def reflectMutation[A:Manifest](x: Def[A]): Exp[A]  = {
    val r: Exp[A] = createDefinition(fresh[A], Mutation(x, context)).sym
    context :+= r
    r
  }
  
  def reifyEffects[A:Manifest](block: => Exp[A]): Exp[A] = {
    val save = context
    context = Nil
    
    val result = block
    val resultR = if (context.isEmpty) result else Reify(result, context): Exp[A]
    context = save
    resultR
  }


  def effectSyms(x: Exp[Any]): List[Sym[Any]] = x match {  //TODO: move to scheduling/codegen?
    case Def(Reify(y, es)) => es.asInstanceOf[List[Sym[Any]]]
    case _ => Nil
  }


  case class Reflect[A](x:Def[A], effects: List[Exp[Any]]) extends Def[A]
  case class Mutation[A](override val x: Def[A], override val effects: List[Exp[Any]]) extends Reflect[A](x, effects) //FIXME: case class inheritance!
  case class Reify[A](x: Exp[A], effects: List[Exp[Any]]) extends Def[A]
}
