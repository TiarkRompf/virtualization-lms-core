package scala.virtualization.lms
package internal

trait Effects extends Expressions {
  
  type State = List[Exp[Any]] // TODO: maybe use TP instead to save lookup
  
  var context: State = _


  abstract class Summary
  
  case class Global() extends Summary

  case class Simple() extends Summary

  case class Alloc() extends Summary //TODO: may also read!
  
  case class Read(v: List[Sym[Any]]) extends Summary

  case class Write(v: List[Sym[Any]]) extends Summary //TODO: may also read!

  def mustAlloc(u: Summary): Boolean = u match {
    case Alloc() => true
    case _ => false
  }

  def mayWrite(u: Summary, a: List[Sym[Any]]): Boolean = u match {
    case Write(vs) => a exists (vs contains _)
    case Global() => true
    case _ => false
  }

  def mayRead(u: Summary, a: List[Sym[Any]]): Boolean = u match {
    case Read(vs) => a exists (vs contains _)
    case Global() => true
    case _ => false
  }


  def reflectMutable[A:Manifest](d: Def[A]): Exp[A] = {
    val mutableInputs = readSyms(d) filter { case Def(Reflect(_, u, _)) => mustAlloc(u) case _ => false }
    if (mutableInputs.nonEmpty)
      println("error: allocation "+d+" has mutable inputs "+mutableInputs.mkString(","))

    val deps = Nil // doesn't depend on anything! FIXME: may read!
    internalReflect(fresh[A], Reflect(d, Alloc(), deps))
  }

  // REMARK: making toAtom context-dependent is quite a departure from the 
  // earlier design. there are a number of implications especially for mirroring.

  protected override implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    // are we depending on a variable? then we need to be serialized -> effect
    
    if (d.isInstanceOf[Reflect[Any]]) {
      println("error: toAtom won't work on " + d + ". if this is a result of a mirror operation try reflectMirrored.")
    }
    
    val mutableInputs = readSyms(d) filter { case Def(Reflect(_, u, _)) => mustAlloc(u) case _ => false }
    
    if (mutableInputs.nonEmpty) {
      val deps = context filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, mutableInputs) || mutableInputs.contains(e) }
      // must come after allocation (if part of context) and all writes to input
      // could use last dep to optimize ...
      val zd = Reflect(d, Read(mutableInputs), deps)
      //context find { case e@Def(`zd`) => e } getOrElse { // local cse
      findDefinition(zd) map (_.sym) filter (context contains _) getOrElse { // local cse
        val z = fresh[T]
        println("promoting to effect: " + z + "=" + zd)
        for (t <- mutableInputs)
          println("depends on  " + t)
        internalReflect(z, zd)
      }
    } else
      super.toAtom(d)
  }

  def reflectMirrored[A:Manifest](d: Reflect[A]): Exp[A] = {
    createDefinition(fresh[A], d).sym
  }



  def reflectRead[A:Manifest](read: Exp[Any]*)(x: Def[A]): Exp[A] = {
    //reflectEffect(x)
    toAtom(x)
  }

  def reflectNew[A:Manifest](read: Exp[Any]*)(x: Def[A]): Exp[A] = {
    toAtom(x) //reflectEffect(x)
  }
  
    
  def readSyms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case Reflect(x, u, es) => readSyms(x) // ignore effect deps (they are not read!)
    case Reify(x, u, es) => if (es contains x) Nil else readSyms(x) // result of block is not read but passed through!
    case p: Product => p.productIterator.toList.flatMap(readSyms(_))
    case _ => Nil
  }

  def reflectWrite[A:Manifest](write0: Exp[Any]*)(read: Exp[Any]*)(x: Def[A]): Exp[A] = {
    val write = write0.toList.asInstanceOf[List[Sym[Any]]] // should check...

    val mutableInputs = readSyms(x) filterNot (write contains _) filter { case Def(Reflect(_, u, _)) => mustAlloc(u) case _ => false }
    if (mutableInputs.nonEmpty)
      println("error: write "+x+" has read-only mutable inputs "+mutableInputs.mkString(","))
    
    // TODO: write-deps should be weak (unless the var is also read! i.e. += )
    
    val deps = context filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, write) || mayRead(u, write) || write.contains(e) }
    
    val z = fresh[A]
    val zd = Reflect(x, Write(write), context)
    
    // make sure write goes to an alloc (reflect, need better check)
    for (w <- write) {
      findDefinition(w) match {
        case Some(TP(_, Reflect(_, Alloc(), _))) => // ok
        case o => 
          println("error: write to non-allocation " + o)
          println("at " + z + "=" + zd)
      }
    }
    
    internalReflect(z, zd)
  }

  
  def reflectEffect[A:Manifest](x: Def[A]): Exp[A] = {
    val deps = context // global effect: may anything, must nothing
    internalReflect(fresh[A], Reflect(x, Global(), context))
  }
  
  def internalReflect[A](s: Sym[A], x: Reflect[A]): Sym[A] = {
    createDefinition(s, x)
    context :+= s
    s
  }


  def reifyEffects[A:Manifest](block: => Exp[A]): Exp[A] = {
    val save = context
    context = Nil
    
    val result = block
    val summary = Global() // TODO: summarize
    val resultR = if (context.isEmpty) result else Reify(result, summary, context): Exp[A] // calls toAtom...
    context = save
    resultR
  }

  def reifyEffectsHere[A:Manifest](block: => Exp[A]): Exp[A] = {
    val save = context
    if (save eq null)
    context = Nil
    
    val result = block

    if ((save ne null) && context.take(save.length) != save) // TODO: use splitAt
      println("error: 'here' effects must leave outer information intact: " + save + " is not a prefix of " + context)

    val deps = if ((save eq null) || (context eq null)) context else context.drop(save.length)
    
    val summary = Global() // TODO: summarize
    val resultR = if (deps.isEmpty) result else Reify(result, summary, deps): Exp[A] // calls toAtom...
    context = save
    resultR
  }


  def effectSyms(x: Exp[Any]): List[Sym[Any]] = x match {  //TODO: move to scheduling/codegen?
    case Def(Reify(y, u, es)) => es.asInstanceOf[List[Sym[Any]]]
    case _ => Nil
  }

  case class Reflect[+A](x:Def[A], summary: Summary, deps: List[Exp[Any]]) extends Def[A]
  case class Reify[A](x: Exp[A], summary: Summary, effects: List[Exp[Any]]) extends Def[A]
}
