package scala.virtualization.lms
package internal

trait Effects extends Expressions {
  
  type State = List[Exp[_]]
  
  var context: State = _
  
  def reflectEffect[A](x: Def[A]): Exp[A] = {
     // don't do CSE on effectful computations
    val r: Exp[A] = createDefinition(fresh[A], Reflect(x, context)).sym
    context :+= r
    r
  }
  
  def reifyEffects[A](block: => Exp[A]): Exp[A] = {
    val save = context
    context = Nil
    
    val result = block
    val resultR = if(context.isEmpty) result else Reify(result, context): Exp[A]
    context = save
    resultR
  }

  case class Reflect[A](x:Def[A], effects: List[Exp[Any]]) extends Def[A] {
    override def toString = "Reflect(" + x + ", " + effectsStr(effects) + ")"
  }
  case class Reify[A](x: Exp[A], effects: List[Exp[Any]]) extends Def[A] {
    override def toString = "Reify(" + x + ", " + effectsStr(effects) + ")"
  }

  // this allows printing nested contexts without blowing out the heap (there is a factorial explosion
  // in contexts when nesting effects currently)
  private def effectsStr(effects: List[Exp[Any]]) : String = {
    effects.map( e =>
      e match {
        case Def(Reflect(x, _)) => "Reflect(" + x + ")"
        case Def(Reify(x, _)) => "Reify(" + x + ")"
        case _ => e.toString
      }
    ).toString
  }
}
