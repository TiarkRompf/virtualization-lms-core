package scala.lms
package internal

import scala.reflect.SourceContext
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

trait Effects extends Expressions with Syms {
  // --- context
  type State = List[Exp[Any]]  // TODO: maybe use TP instead to save lookup

  var context: State = _
  var conditionalScope = false // used to construct Control nodes
  var globalMutableSyms: List[Sym[Any]] = Nil
  
  // --- summary
  case class Summary(
    maySimple: Boolean,
    mstSimple: Boolean,
    mayGlobal: Boolean,
    mstGlobal: Boolean,
    resAlloc: Boolean,
    control: Boolean,
    mayRead: List[Sym[Any]],
    mstRead: List[Sym[Any]],
    mayWrite: List[Sym[Any]],
    mstWrite: List[Sym[Any]]
  ) {
    def orElse(that: Summary) = new Summary (
      this.maySimple || that.maySimple, this.mstSimple && that.mstSimple,
	    this.mayGlobal || that.mayGlobal, this.mstGlobal && that.mstGlobal,
	    false, //u.resAlloc && v.resAlloc, <--- if/then/else will not be mutable!
	    this.control || that.control,
	    (this.mayRead ++ that.mayRead).distinct, (this.mstRead intersect that.mstRead),
	    (this.mayWrite ++ that.mayWrite).distinct, (this.mstWrite intersect that.mstWrite)
	  )
    def andAlso(that: Summary) = new Summary (
      this.maySimple || that.maySimple, this.mstSimple || that.mstSimple,
      this.mayGlobal || that.mayGlobal, this.mstGlobal || that.mstGlobal,
      this.resAlloc || that.resAlloc,
      this.control || that.control,
      (this.mayRead ++ that.mayRead).distinct, (this.mstRead ++ that.mstRead).distinct,
      (this.mayWrite ++ that.mayWrite).distinct, (this.mstWrite ++ that.mstWrite).distinct
    )
    def andThen(that: Summary) = new Summary (
      this.maySimple || that.maySimple, this.mstSimple || that.mstSimple,
      this.mayGlobal || that.mayGlobal, this.mstGlobal || that.mstGlobal,
      that.resAlloc,
      this.control || that.control,
      (this.mayRead ++ that.mayRead).distinct, (this.mstRead ++ that.mstRead).distinct,
      (this.mayWrite ++ that.mayWrite).distinct, (this.mstWrite ++ that.mstWrite).distinct
    )
    def star = Pure() orElse this // any number of repetitions, including 0
    
    def withoutControl = new Summary (
      maySimple, mstSimple,
      mayGlobal, mstGlobal,
      resAlloc,
      false,
      mayRead, mstRead,
      mayWrite, mstWrite
    )
  }
  def Pure() = new Summary(false,false,false,false,false,false,Nil,Nil,Nil,Nil)
  def Simple() = new Summary(true,true,false,false,false,false,Nil,Nil,Nil,Nil)
  def Global() = new Summary(false,false,true,true,false,false,Nil,Nil,Nil,Nil)
  def Alloc() = new Summary(false,false,false,false,true,false,Nil,Nil,Nil,Nil)
  def Control() = new Summary(false,false,false,false,false,true,Nil,Nil,Nil,Nil)
  def Read(v: List[Sym[Any]]) = new Summary(false,false,false,false,false,false,v.distinct,v.distinct,Nil,Nil)
  def Write(v: List[Sym[Any]]) = new Summary(false,false,false,false,false,false,Nil,Nil,v.distinct,v.distinct)

  def mayRead(u: Summary, a: List[Sym[Any]]): Boolean = u.mayGlobal || a.exists(u.mayRead contains _)
  def mayWrite(u: Summary, a: List[Sym[Any]]): Boolean = u.mayGlobal || a.exists(u.mayWrite contains _)
  def maySimple(u: Summary): Boolean = u.mayGlobal || u.maySimple

  def mustMutable(u: Summary): Boolean = u.resAlloc
  def mustPure(u: Summary): Boolean = u == Pure()
  def mustOnlyRead(u: Summary): Boolean = u == Pure().copy(mayRead=u.mayRead, mstRead=u.mstRead) // only reads allowed
  def mustIdempotent(u: Summary): Boolean = mustOnlyRead(u) // currently only reads are treated as idempotent
  
  // --- class definitions
  case class Reflect[+A](x:Def[A], summary: Summary, deps: List[Exp[Any]]) extends Def[A]
  case class Reify[A](x: Exp[A], summary: Summary, effects: List[Exp[Any]]) extends Def[A]

  object EatReflect {
    def unapply(d: Any): Option[Any] = d match {
      case Reflect(inner, _, _) => Some(inner)
      case _ => Some(d)
    }
  }
  
  def summarizeEffects(e: Block[Any]) = e match {
    case Block(Def(Reify(_,u,_))) => u
    case _ => Pure()
  }
  
  // --- Reflect helpers
  def controlDep(x: Exp[Any]) = x match {
    case Def(Reflect(y,u,es)) if u == Control() => true
    case _ => false
  }

  // performance hot spot: this is the same as: es.filterNot(controlDep).flatMap(syms)
  def nonControlSyms[R](es: List[Exp[Any]], ss: Any => List[R]): List[R] = {
    val out = new ListBuffer[R]
    var it = es.iterator
    while (it.hasNext) {
      val e = it.next()
      if (!controlDep(e)) out ++= ss(e)
    }
    out.result
  }
  
  /**
   * Remove an intermediate (dead) symbol from local def table, global def table,
   * and context symbol list. Needed to keep intermediate steps from causing 
   * code duplication by getting into Reflect/Reify node symbol lists
   * FIXME: Does NOT remove from symbol table - should it?
   * FIXME: This is rather hacky - is there API for this kind of thing?
   */
  def scrubSym(sym: Sym[Any]) = {
    def scrubIntermediateSym(stms: List[Stm]) = stms filterNot {
      case TP(lhs,rhs) => (lhs == sym)
      case _ => false
    }
    localDefs = scrubIntermediateSym(localDefs)
    globalDefs = scrubIntermediateSym(globalDefs)
    context = context filterNot {s => s == sym}
  }

  // --- Aliasing
  /*
   TODO: switch back to graph based formulation -- this will not work for circular dependencies
  */
  val shallowAliasCache = new HashMap[Sym[Any], List[Sym[Any]]]
  val deepAliasCache = new HashMap[Sym[Any], List[Sym[Any]]]
  val allAliasCache = new HashMap[Sym[Any], List[Sym[Any]]]
  
  def utilLoadStm[T](s: Sym[T]) = if (!isPrimitiveType(s.tp)) /*globalDefs.filter{e => e.lhs contains s}*/ findStm(s).toList else Nil
  def utilLoadStms(s: List[Sym[Any]]) = s.flatMap(utilLoadStm)
  def utilLoadSym[T](s: Sym[T]) = utilLoadStm(s).map(_.rhs)
  
  def shallowAliases(start: Any): List[Sym[Any]] = {
    val alias = noPrim(aliasSyms(start)) flatMap { a => a::shallowAliasCache.getOrElseUpdate(a, shallowAliases(utilLoadSym(a))) }
    val extract = noPrim(extractSyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    //println("shallowAliases("+start+") = "+alias+" ++ "+extract)
    (alias ++ extract).distinct
  }
  
  def deepAliases(start: Any): List[Sym[Any]] = {
    val alias = noPrim(aliasSyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    val copy = noPrim(copySyms(start)) flatMap { a => deepAliasCache.getOrElseUpdate(a, deepAliases(utilLoadSym(a))) }
    val contain = noPrim(containSyms(start)) flatMap { a => a::allAliasCache.getOrElseUpdate(a, allAliases(utilLoadSym(a))) }
    //println("aliasSyms("+start+") = "+aliasSyms(start) + "/" + noPrim(aliasSyms(start)))
    //println("copySyms("+start+") = "+copySyms(start) + "/" + noPrim(copySyms(start)))
    //println("containSyms("+start+") = "+containSyms(start) + "/" + noPrim(containSyms(start)))
    //println("deepAliases("+start+") = "+alias+" ++ "+copy+" ++ "+contain)
    (alias ++ copy ++ contain).distinct
  }


  def allAliases(start: Any): List[Sym[Any]] = {
    val r = (shallowAliases(start) ++ deepAliases(start)).distinct
    //printdbg("all aliases of " + start + ": " + r.mkString(", "))
    r
  }

  //def allTransitiveAliases(start: Any): List[Stm] = utilLoadStms(allAliases(start))
  //def transitiveAliases(start: List[Sym[Any]]): List[Stm] = start.flatMap(utilLoadSymTP)
  
  // TODO possible optimization: a mutable object never aliases another mutable object, so its inputs need not be followed
  
  def mutableTransitiveAliases(s: Any) = {
    val aliases = allAliases(s)
    val bareMutableSyms = aliases filter { o => globalMutableSyms.contains(o) }
    val definedMutableSyms = utilLoadStms(aliases) collect { case TP(s2, Reflect(_, u, _)) if mustMutable(u) => s2 }
    bareMutableSyms ++ definedMutableSyms
  }
  
  
  def getActuallyReadSyms[A](d: Def[A]) = {
    val bound = boundSyms(d)
    val r = readSyms(d).map{case Def(Reify(x,_,_)) => x case x => x} filterNot (bound contains _)
    //if (d.isInstanceOf[Reify[Any]] && r.nonEmpty) {
    //  println("** actually read: "+readSyms(d)+"\\"+bound+"="+r)
    //  println("** transitive shallow: " + shallowAliases(r))
    //  println("** transitive deep: " + deepAliases(r))
    //}
    r
  }
  
  def readMutableData[A](d: Def[A]) = {
    val bound = boundSyms(d)
    mutableTransitiveAliases(getActuallyReadSyms(d)) filterNot (bound contains _)
  }
  
  // --- Reflection
  /*
    are we depending on a variable or mutable object? then we need to be serialized -> effect
    the call chain goes like this:
      reflect
      reflectEffect(Pure())       // figure out dependencies on mutable objects
      reflectEffectInternal(u)    // extended summary Pure() -> u
        findOrCreateDefinitionExp // if summary is still pure
        createReflectDefinition   // if summary is not pure
  */
  protected def reflect[T:Typ](d: Def[T])(implicit ctx: SourceContext): Exp[T] = {
    // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine
    if (typ[T] == manifestTyp[Any]) cwarn("Possible missing mtype call - toAtom with Def of type Any " + d)
    
    // AKS NOTE: this was removed on 6/27/12, but it is still a problem in OptiML apps without it,
    // so I'm putting it back until we can get it resolved properly.
    d match {
      case Reify(x,_,_) => toAtom(d)
      case _ if conditionalScope && addControlDeps => reflectEffect(d, Control())
      case _ => reflectEffect(d, Pure())
    }
  }
  protected def reflectPure[T:Typ](d: Def[T])(implicit ctx: SourceContext): Exp[T] = reflect(d)
  
  /* Reflect a node during mirroring */
  def reflectMirrored[A:Typ](zd: Reflect[A])(implicit ctx: SourceContext): Exp[A] = {
    checkContext()
    // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine
    if (typ[A] == manifestTyp[Any]) cwarn("Possible missing mtype call - reflectMirrored with Def of type Any: " + zd)
    context.filter { case Def(d) if d == zd => true case _ => false }.reverse match {
      case _ => createReflectStm(fresh[A], zd)
    }
  }
  
  /* Reflect a mutable bound symbol - add to global mutable symbols list */
  def reflectMutableSym[A](s: Sym[A]): Sym[A] = {
    assert(findStm(s).isEmpty) // 
    globalMutableSyms = globalMutableSyms :+ s
    s
  }

  /* Reflect a mutable node */
  def reflectMutable[A:Typ](d: Def[A])(implicit ctx: SourceContext): Exp[A] = {
    val z = reflectEffect(d, Alloc())
    val mutableAliases = mutableTransitiveAliases(d)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  /* Reflect a writer node */
  def reflectWrite[A:Typ](write0: Exp[Any]*)(d: Def[A])(implicit ctx: SourceContext): Exp[A] = {
    val write = write0.toList.asInstanceOf[List[Sym[Any]]] // should check...

    val z = reflectEffect(d, Write(write))

    val mutableAliases = mutableTransitiveAliases(d) filterNot (write contains _)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  /* Reflect a simple effect (serialized with respect to other simples) */
  def reflectEffect[A:Typ](x: Def[A])(implicit ctx: SourceContext): Exp[A] = reflectEffect(x, Simple()) 
  
  /* Reflect a node with a given effect summary */
  def reflectEffect[A:Typ](d: Def[A], u: Summary)(implicit ctx: SourceContext): Exp[A] = {
    // are we depending on a variable? then we need to be serialized -> effect
    val mutableInputs = readMutableData(d)
    reflectEffectInternal(d, u andAlso Read(mutableInputs)) // will call super.toAtom if mutableInput.isEmpty
  }
 
  /*
   * prevent sharing between mutable objects / disallow mutable escape for non read-only operations
   * make sure no mutable object becomes part of mutable result (in case of allocation)
   * or is written to another mutable object (in case of write)
   *
   * val a = mzeros(100)
   * val b = zeros(100)
   * val c = if (..) {
   * 			a.update
   *    		b 
   *      	 } 
   *         else { a }
   *         
   * PROBLEM: the whole if Exp has summary mayWrite=List(a), mstWrite=Nil and allAliases=List(a,b)
   * what is the right thing?
   * - mutableAliases \ mstWrite <-- first try, but maybe to restrictive?
   * - mutableAliases \ mayWrite <-- too permissive?
   * - something else?
   */
  def reflectEffectInternal[A:Typ](x: Def[A], u: Summary)(implicit ctx: SourceContext): Exp[A] = {
    if (mustPure(u)) reflect(x) else {
      checkContext()
      // NOTE: reflecting mutable stuff *during mirroring* doesn't work right now.
      assert(!x.isInstanceOf[Reflect[_]], x)

      val deps = calculateDependencies(u)
      val zd = Reflect(x,u,deps)
      if (mustIdempotent(u)) {
        context find { case Def(d) => d == zd } map { _.asInstanceOf[Exp[A]] } getOrElse {
          val z = fresh[A]
          createReflectStm(z, zd)
        }
      } 
      else {
        val z = fresh[A]
        // make sure all writes go to allocs
        for (w <- u.mayWrite if !isMutable(w)) {
          //cerror("Write to non-mutable " + strDef(w))
          //cerror("at " + z + " = " + zd)
          //cerror("in " + quotePos(z))
          cerror(quoteCtx(ctx) + ": write to non-mutable " + w.tp.toString + "\n\t" + quoteCode(List(ctx)).getOrElse(strDef(w)))
        }
        createReflectStm(z, zd)
      }
    }
  }
  
  // --- Reflection helpers
  def checkIllegalSharing(z: Exp[Any], mutableAliases: List[Sym[Any]]) {
    if (mutableAliases.nonEmpty) {
      val zd = z match { case Def(zd) => zd }
      cerror("Illegal sharing of mutable objects " + mutableAliases.mkString(", "))
      cerror("at " + strDef(z))
      //cerror("Attempted to create illegal alias of mutable objects " + mutableAliases.mkString(", ") + "\n\t" +
      //         quoteCode(z.pos).getOrElse(strDef(z)))
    }
  }
  
  def calculateDependencies(u: Summary): State = {
    checkContext();
    calculateDependencies(context, u, true)
  }
  def calculateDependencies(scope: State, u: Summary, mayPrune: Boolean): State = {
    if (u.mayGlobal) scope else {
      val read = u.mayRead
      val write = u.mayWrite

      // TODO: in order to reduce the number of deps (need to traverse all those!)
      // we should only store those that are not transitively implied.
      // For simple effects, take the last one (implemented).
      // For mutations, take the last write to a particular mutable sym (TODO).

      def canonic(xs: List[Exp[Any]]) = xs // TODO
      def canonicLinear(xs: List[Exp[Any]]) = if (mayPrune) xs.takeRight(1) else xs

      // the mayPrune flag is for test8-speculative4: with pruning on, the 'previous iteration'
      // dummy is moved out of the loop. this is not per se a problem -- need to look some more into it.

      val readDeps = if (read.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, read) || read.contains(e) }
      val softWriteDeps = if (write.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayRead(u, write) }
      val writeDeps = if (write.isEmpty) Nil else scope filter { case e@Def(Reflect(_, u, _)) => mayWrite(u, write) || write.contains(e) }
      val simpleDeps = if (!u.maySimple) Nil else scope filter { case e@Def(Reflect(_, u, _)) => u.maySimple }
      val controlDeps = if (!u.control) Nil else scope filter { case e@Def(Reflect(_, u, _)) => u.control }
      val globalDeps = scope filter { case e@Def(Reflect(_, u, _)) => u.mayGlobal }

      // TODO: write-on-read deps should be weak
      // TODO: optimize!!
      val allDeps = canonic(readDeps ++ softWriteDeps ++ writeDeps ++ canonicLinear(simpleDeps) ++ canonicLinear(controlDeps) ++ canonicLinear(globalDeps))
      scope filter (allDeps contains _)
    }
  }

  // Special version of statement creation for reflect nodes
  def createReflectStm[A](s: Sym[A], x: Reflect[A]): Sym[A] = {
    x match {
      case Reflect(Reify(_,_,_),_,_) => cerror(quotePos(s) + ": Reflecting a reify node at \n" + quoteCode(s.pos).getOrElse(strDef(s)))
      case _ =>
    }
    createStm(List(s), x)
    context :+= s
    s
  }

  def checkContext() {
    if (context == null)
      cfatal("Uninitialized effect context: effectful statements may only be used within a reifyEffects { .. } block")
  }
  
  // --- Reify helpers
  def summarizeAll(es: List[Exp[Any]]): Summary = {
    // compute an *external* summary for a seq of nodes
    // don't report any reads/writes on data allocated within the block
    var u = Pure()
    var ux = u
    var allocs: List[Exp[Any]] = Nil
    def clean(xs: List[Sym[Any]]) = xs.filterNot(allocs contains _)
    for (s@Def(Reflect(_, u2, _)) <- es) {
      if (mustMutable(u2)) allocs ::= s
      u = u andThen (u2.copy(mayRead = clean(u2.mayRead), mstRead = clean(u2.mstRead),
              mayWrite = clean(u2.mayWrite), mstWrite = clean(u2.mstWrite)))
      ux = ux andThen u2
    }
    (u)
  }

  // TODO this doesn't work yet (because of loops!): filterNot { case Def(Reflect(_,u,_)) => mustOnlyRead(u) }
  def pruneContext(ctx: List[Exp[Any]]): List[Exp[Any]] = ctx 
  
  // Reify the effects of an isolated block.
  // no assumptions about the current context remain valid.
  def reifyEffects[A:Typ](block: => Exp[A], controlScope: Boolean = false): Block[A] = {
    val save = context
    context = Nil

    // only add control dependencies scopes where controlScope is explicitly true (i.e., the first-level of an IfThenElse)
    val saveControl = conditionalScope
    conditionalScope = controlScope

    val (result, defs) = reifySubGraph(block)
    reflectSubGraph(defs)

    conditionalScope = saveControl

    val deps = context
    val summary = summarizeAll(deps)
    context = save

    if (deps.isEmpty && mustPure(summary)) 
      Block(result) 
    else 
      Block(toAtom(Reify(result, summary, pruneContext(deps)))(result.tp,SourceContext.empty)).asInstanceOf[Block[A]]
  }

  // Reify the effects of a block that is executed 'here' (if it is executed at all).
  // all assumptions about the current context carry over unchanged.
  def reifyEffectsHere[A:Typ](block: => Exp[A], controlScope: Boolean = false)(implicit ctx: SourceContext): Block[A] = {
    val save = context
    if (save eq null)
      context = Nil

    val saveControl = conditionalScope
    conditionalScope = controlScope

    val (result, defs) = reifySubGraph(block)
    reflectSubGraph(defs)

    conditionalScope = saveControl

    if ((save ne null) && context.take(save.length) != save) // TODO: use splitAt
      cerror("'here' effects must leave outer information intact: " + save + " is not a prefix of " + context)
   
    val deps = if (save eq null) context else context.drop(save.length)

    val summary = summarizeAll(deps)
    context = save

    if (deps.isEmpty && mustPure(summary)) 
      Block(result) 
    else 
      Block(toAtom(Reify(result, summary, pruneContext(deps)))(result.tp,ctx)).asInstanceOf[Block[A]]
  }
  
  def isBound(e: Exp[Any]) = !e.isInstanceOf[Const[_]] && findStm(e.asInstanceOf[Sym[Any]]).isEmpty
  def isMutable(e: Exp[Any]): Boolean = e match {
    case Def(Reflect(_, u, _)) if mustMutable(u) => true
    case s: Sym[_] if isBound(s) => globalMutableSyms.contains(s)
    case _ => false
  }
  
  def getBlockResult[A](b: Block[A]): Exp[A] = b match {
    case Block(Def(Reify(x, _, _))) => x
    case Block(x) => x
  }
  
  override def toAtom[T:Typ](d: Def[T])(implicit ctx: SourceContext): Exp[T] = {
    // warn if type is Any. TODO: make optional, sometimes Exp[Any] is fine
    if (typ[T] == manifestTyp[Any]) cwarn(s"Possible missing mtype call - toAtom with Def of type Any $d")  
  
    d match {
      case Reify(x,_,_) => super.toAtom(d)  // TODO: What is the proper way of handling this case?
      case _ if conditionalScope && addControlDeps => reflectEffect(d, Control())
      case _ => reflectEffect(d, Pure())
    }
  }
}
