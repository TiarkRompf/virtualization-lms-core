package scala.virtualization.lms
package internal

trait Effects extends Expressions {
  
  // --- misc
  def effectSyms(x: Exp[Any]): List[Sym[Any]] = x match {  //TODO: move to scheduling/codegen?
    case Def(Reify(y, u, es)) => es.asInstanceOf[List[Sym[Any]]]
    case _ => Nil
  }
  
  // TODO: transform over Summary currently lives in common/Base.scala. move it here?
  
  
  // --- context

  type State = List[Exp[Any]] // TODO: maybe use TP instead to save lookup
  
  var context: State = _

  // --- class defs

  case class Reflect[+A](x:Def[A], summary: Summary, deps: List[Exp[Any]]) extends Def[A]
  case class Reify[A](x: Exp[A], summary: Summary, effects: List[Exp[Any]]) extends Def[A]

  // --- summary

  case class Summary(
    val maySimple: Boolean,
    val mstSimple: Boolean,
    val mayGlobal: Boolean,
    val mstGlobal: Boolean,
    val resAlloc: Boolean,
    val mayRead: List[Sym[Any]],
    val mstRead: List[Sym[Any]],
    val mayWrite: List[Sym[Any]],
    val mstWrite: List[Sym[Any]])
  
  def Pure() = new Summary(false,false,false,false,false,Nil,Nil,Nil,Nil)
  def Simple() = new Summary(true,true,false,false,false,Nil,Nil,Nil,Nil)
  def Global() = new Summary(false,false,true,true,false,Nil,Nil,Nil,Nil)
  def Alloc() = new Summary(false,false,false,false,true,Nil,Nil,Nil,Nil)
  
  def Read(v: List[Sym[Any]]) = new Summary(false,false,false,false,false,v.distinct,v.distinct,Nil,Nil)
  def Write(v: List[Sym[Any]]) = new Summary(false,false,false,false,false,Nil,Nil,v.distinct,v.distinct)

  def mayRead(u: Summary, a: List[Sym[Any]]): Boolean = u.mayGlobal || a.exists(u.mayRead contains _)
  def mayWrite(u: Summary, a: List[Sym[Any]]): Boolean = u.mayGlobal || a.exists(u.mayWrite contains _)

  def mustMutable(u: Summary): Boolean = u.resAlloc
  def mustIdempotent(u: Summary): Boolean = u == Pure().copy(mayRead=u.mayRead, mstRead=u.mstRead) // only reads allowed
  def mustPure(u: Summary): Boolean = u == Pure()


  def infix_orElse(u: Summary, v: Summary) = new Summary(
    u.maySimple || v.maySimple, u.mstSimple && v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal && v.mstGlobal,
    u.resAlloc && v.resAlloc,
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead intersect v.mstRead),
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite intersect v.mstWrite)
  )

  def infix_andAlso(u: Summary, v: Summary) = new Summary(
    u.maySimple || v.maySimple, u.mstSimple || v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal || v.mstGlobal,
    u.resAlloc || v.resAlloc,
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead ++ v.mstRead).distinct,
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite ++ v.mstWrite).distinct
  )
  
  def infix_andThen(u: Summary, v: Summary) = new Summary(
    u.maySimple || v.maySimple, u.mstSimple || v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal || v.mstGlobal,
    v.resAlloc,
    (u.mayRead ++ v.mayRead).distinct, (u.mstRead ++ v.mstRead).distinct,
    (u.mayWrite ++ v.mayWrite).distinct, (u.mstWrite ++ v.mstWrite).distinct
  )

  def infix_star(u: Summary) = Pure() orElse u // any number of repetitions, including 0


  def summarizeEffects(e: Exp[Any]) = e match {
    case Def(Reify(_,u,_)) => u
    case Def(Reflect(_,u,_)) => u
    case _ => Pure()
  }


  // --- reflect helpers

  def readSyms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case Reflect(x, u, es) => readSyms(x) // ignore effect deps (they are not read!)
    case Reify(x, u, es) => if (es contains x) Nil else readSyms(x) // result of block is not read but passed through!
    case p: Product => p.productIterator.toList.flatMap(readSyms(_))
    case _ => Nil
  }
  
  def getMutableInputs[A](d: Def[A]): List[Sym[Any]] = readSyms(d) filter { case Def(Reflect(_, u, _)) => mustMutable(u) case _ => false }

  // --- reflect

  // TODO: should reflectEffect try to add Read(mutableInputs) as well ??? or just toAtom ???

  def reflectMutable[A:Manifest](d: Def[A]): Exp[A] = {
    val mutableInputs = getMutableInputs(d)
    // TODO: check that d's result does not alias another mutable object
    
    reflectEffect(d, Alloc() andAlso Read(mutableInputs))
  }

  // REMARK: making toAtom context-dependent is quite a departure from the 
  // earlier design. there are a number of implications especially for mirroring.

  protected override implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    // are we depending on a variable? then we need to be serialized -> effect
    val mutableInputs = getMutableInputs(d)
    reflectEffect(d, Read(mutableInputs)) // will call super.toAtom if mutableInput.isEmpty
  }

  def reflectMirrored[A:Manifest](d: Reflect[A]): Exp[A] = {
    createDefinition(fresh[A], d).sym
  }

  def reflectWrite[A:Manifest](write0: Exp[Any]*)(d: Def[A]): Exp[A] = {
    val write = write0.toList.asInstanceOf[List[Sym[Any]]] // should check...
    val mutableInputs = getMutableInputs(d)
    reflectEffect(d, Write(write) andAlso Read(mutableInputs))
  }

  def reflectEffect[A:Manifest](x: Def[A]): Exp[A] = reflectEffect(x, Global()) // global effect (may anything, must nothing)  

  def reflectEffect[A:Manifest](x: Def[A], u: Summary): Exp[A] = {
    if (mustPure(u)) super.toAtom(x) else {
      val deps = calculateDependencies(u)
      val zd = Reflect(x,u,deps)
      if (mustIdempotent(u)) {
        findDefinition(zd) map (_.sym) filter (context contains _) getOrElse { // local cse
          val z = fresh[A]
          println("promoting to effect: " + z + "=" + zd)
          for (w <- u.mayRead)
            println("depends on  " + w)
          internalReflect(z, zd)
        }
      } else {
        val z = fresh[A]
        // make sure all writes go to allocs
        for (w <- u.mayWrite) {
          findDefinition(w) match {
            case Some(TP(_, Reflect(_, u, _))) if mustMutable(u) => // ok
            case o => 
              println("error: write to non-mutable " + o)
              println("at " + z + "=" + zd)
          }
        }
        internalReflect(z, zd)
      }
    }
  }
  
  def calculateDependencies(u: Summary): State = {
    if (u.mayGlobal) context else {
      val read = u.mayRead
      val write = u.mayWrite

      val readDeps = if (read.isEmpty) Nil else context filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, read) || read.contains(e) }
      // TODO: write-on-read deps should be weak
      val writeDeps = if (write.isEmpty) Nil else context filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, write) || mayRead(u, write) || read.contains(e) }
      val simpleDeps = if (!u.maySimple) Nil else context filter { case e@Def(Reflect(_, u, _)) => u.maySimple }
      val globalDeps = context filter { case e@Def(Reflect(_, u, _)) => u.mayGlobal }

      // TODO: optimize!!
      val allDeps = readDeps ++ writeDeps ++ simpleDeps ++ globalDeps
      context filter (allDeps contains _)
    }
  }

  def internalReflect[A](s: Sym[A], x: Reflect[A]): Sym[A] = {
    createDefinition(s, x)
    context :+= s
    s
  }
  

  // --- reify

  def summarizeAll(es: List[Exp[Any]]): Summary = {
    var u = Pure()
    for (Def(Reflect(_, u2, _)) <- es)
      u = u andThen u2
    u
  }

  def reifyEffects[A:Manifest](block: => Exp[A]): Exp[A] = {
    val save = context
    context = Nil
    
    val result = block
    val summary = summarizeAll(context)
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
    
    val summary = summarizeAll(deps)
    val resultR = if (deps.isEmpty) result else Reify(result, summary, deps): Exp[A] // calls toAtom...
    context = save
    resultR
  }

}
