package scala.virtualization.lms
package internal

trait Effects extends Expressions {
  
  type State = List[Exp[Any]]
  
  var context: State = _

  def reflectRead[A:Manifest](x: Exp[A]): Exp[A] = x match {
    case x: Sym[A] =>
/*
      TODO: 
      look at context. 
      if there is no access to sym create a read.
      if the last access to sym is a read, return that.
      if the last access to sym is a write a) return the rhs b) create a read
*/
      if (x.lastRead.id < x.version || !context.contains(x.lastRead))
        x.lastRead = reflectEffect(Read(x)).asInstanceOf[Sym[A]]
      x.lastRead
    case _ =>  x
  }

   //TODO: connect to mutation node? a write should depend on the last read of the sym
  
  def reflectWrite[A:Manifest](x: Exp[A]): Exp[A] = x match {
    case x: Sym[A] => x.version = nVars+1; x
    case _ => x
  }

  def reflectReadWrite[A:Manifest](x: Exp[A]): Exp[A] = {
    val y = reflectRead(x)
    reflectWrite(x)
    y
  }

  def reflectEffect[A:Manifest](x: Def[A]): Exp[A] = {
     // don't do CSE on effectful computations
    val r: Exp[A] = createDefinition(fresh[A], Reflect(x, context)).sym
    context :+= r
    r
  }

  // TODO: think about creating a Write node, so that we could remove reflectMutation. We would then
  // infer a mutation from a reflectEffect containing Writes.
  // Question: what about Reflect(IfThenElse(...))? how to track writes there?
  // Alternative: add a field to Reflect that describes what it modifies
  def reflectMutation[A:Manifest](x: Def[A]): Exp[A]  = {
//    val r: Exp[A] = createDefinition(fresh[A], Reflect/*Mutation*/(x, context)).sym
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

  case class Read[A](x: Exp[A]) extends Def[A]
  case class Mutation[A](override val x: Def[A], override val effects: List[Exp[Any]]) extends Reflect[A](x, effects) //FIXME: case class inheritance!

  case class Reflect[A](x:Def[A], effects: List[Exp[Any]]) extends Def[A]
  case class Reify[A](x: Exp[A], effects: List[Exp[Any]]) extends Def[A]
}
