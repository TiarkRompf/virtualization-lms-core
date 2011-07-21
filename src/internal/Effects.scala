package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable

trait Effects extends Expressions with Utils {
  
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
  def mustPure(u: Summary): Boolean = u == Pure()
  def mustOnlyRead(u: Summary): Boolean = u == Pure().copy(mayRead=u.mayRead, mstRead=u.mstRead) // only reads allowed
  def mustIdempotent(u: Summary): Boolean = mustOnlyRead(u) // currently only reads are treated as idempotent



  def infix_orElse(u: Summary, v: Summary) = new Summary(
    u.maySimple || v.maySimple, u.mstSimple && v.mstSimple,
    u.mayGlobal || v.mayGlobal, u.mstGlobal && v.mstGlobal,
    false, //u.resAlloc && v.resAlloc, <--- if/then/else will not be mutable!
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

  override def syms(e: Any): List[Sym[Any]] = e match {
    case s: Summary => Nil // don't count effect summaries as dependencies!
    case _ => super.syms(e)
  }

  override def rsyms[T](e: Any)(f: Any => List[T]): List[T] = e match { // stack overflow ...
    case s: Summary => Nil // don't count effect summaries as dependencies!
    case _ => super.rsyms(e)(f)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Summary => Nil // don't count effect summaries as dependencies!
    case _ => super.symsFreq(e)
  }

  override def effectSyms(x: Any): List[Sym[Any]] = x match {
    case Def(Reify(y, u, es)) => es.asInstanceOf[List[Sym[Any]]]
    case _ => super.effectSyms(x)
  }

  def readSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => readSyms(x) // ignore effect deps (they are not read!)
    case Reify(x, u, es) => 
      if (es contains x) Nil // FIXME this piece of logic is not clear. is it a special case for unit??
      else readSyms(x) // result of block is not read but passed through!
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(readSyms(_))
    case _ => Nil
  }

  /*
    decisions to be made:
    1) does alias imply read? or are they separate?
    2) use a data structure to track transitive aliasing or recompute always?
  */

  def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => aliasSyms(x)
    case Reify(x, u, es) => syms(x)
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(aliasSyms(_))
    case _ => Nil
  }  
  
  def containSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => containSyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[Any] => Nil
    case p: Product => p.productIterator.toList.flatMap(containSyms(_))
    case _ => Nil
  }
  
  def extractSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => extractSyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[Any] => Nil
    case p: Product => p.productIterator.toList.flatMap(extractSyms(_))
    case _ => Nil
  }

  def copySyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => copySyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[Any] => Nil
    case p: Product => p.productIterator.toList.flatMap(copySyms(_))
    case _ => Nil
  }


  def isPrimitiveType[T](m: Manifest[T]) = m.toString match {
    case "Byte" | "Char" | "Short" | "Int" | "Long" | "Float" | "Double" | "Boolean" | "Unit" => true
    case _ => false
  }
  
/*
  def allTransitiveAliases(start: Any): List[TP[Any]] = {
    def deps(st: List[Sym[Any]]): List[TP[Any]] = {
      val st1 = st filterNot (s => isPrimitiveType(s.Type))
      globalDefs.filter(st1 contains _.sym)
    }
    GraphUtil.stronglyConnectedComponents[TP[Any]](deps(aliasSyms(start)), t => deps(aliasSyms(t.rhs))).flatten.reverse
  }
*/
  
  def noPrim(sm: List[Sym[Any]]): List[Sym[Any]] = sm.filterNot(s=>isPrimitiveType(s.Type))
  
  /*
   TODO: switch back to graph based formulation -- this will not work for circular deps
  */
  
  val shallowAliasCache = new mutable.HashMap[Sym[Any], List[Sym[Any]]]
  val deepAliasCache = new mutable.HashMap[Sym[Any], List[Sym[Any]]]
  val allAliasCache = new mutable.HashMap[Sym[Any], List[Sym[Any]]]
  
  def utilLoadSymTP[T](s: Sym[T]) = if (!isPrimitiveType(s.Type)) globalDefs.filter(List(s) contains _.sym) else Nil
  def utilLoadSym[T](s: Sym[T]) = utilLoadSymTP(s).map(_.rhs)
  
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

  def allTransitiveAliases(start: Any): List[TP[Any]] = allAliases(start).flatMap(utilLoadSymTP)
  
  
  // TODO optimization: a mutable object never aliases another mutable object, so its inputs need not be followed
  
  def mutableTransitiveAliases(s: Any) = {
    allTransitiveAliases(s) collect { case TP(s2, Reflect(_, u, _)) if mustMutable(u) => s2 }
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
  
  def readMutableData[A](d: Def[A]) = mutableTransitiveAliases(getActuallyReadSyms(d))


  // --- reflect

  // TODO: should reflectEffect try to add Read(mutableInputs) as well ??? or just toAtom ???

  // REMARK: making toAtom context-dependent is quite a departure from the 
  // earlier design. there are a number of implications especially for mirroring.

  /*
    wrapping reads in a reflect can also have an unfortunate effect on rewritings.
    consider 
      val a = ...       // mutable
      val b = a.foo     // usually Foo(a) but now Reflect(Foo(a))
      val c = b.costly  // costly(Foo(a)) would simplify to Cheap(a), 
                        // but this ends up as Reflect(Costly(Foo(a))) instead of Reflect(Cheap(a))
    
    TODO: find a solution ...
  */

  protected override implicit def toAtom[T:Manifest](d: Def[T]): Exp[T] = {
    // are we depending on a variable? then we need to be serialized -> effect
    val mutableInputs = readMutableData(d)
    reflectEffect(d, Read(mutableInputs)) // will call super.toAtom if mutableInput.isEmpty
  }

  def reflectMirrored[A:Manifest](d: Reflect[A]): Exp[A] = {
    createDefinition(fresh[A], d).sym
  }

  def checkIllegalSharing(z: Exp[Any], mutableAliases: List[Sym[Any]]) {
    if (mutableAliases.nonEmpty) {
      val zd = z match { case Def(zd) => zd }
      printerr("error: illegal sharing of mutable objects " + mutableAliases.mkString(", "))
      printerr("at " + z + "=" + zd)
    }
  }
  
  def reflectMutable[A:Manifest](d: Def[A]): Exp[A] = {
    val mutableInputs = readMutableData(d)    
    val z = reflectEffect(d, Alloc() andAlso Read(mutableInputs))

    val mutableAliases = mutableTransitiveAliases(d)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def reflectWrite[A:Manifest](write0: Exp[Any]*)(d: Def[A]): Exp[A] = {
    val write = write0.toList.asInstanceOf[List[Sym[Any]]] // should check...
    val mutableInputs = readMutableData(d)

    val z = reflectEffect(d, Write(write) andAlso Read(mutableInputs))

    val mutableAliases = mutableTransitiveAliases(d) filterNot (write contains _)
    checkIllegalSharing(z, mutableAliases)
    z
  }

  def reflectEffect[A:Manifest](x: Def[A]): Exp[A] = reflectEffect(x, Simple()) // simple effect (serialized with respect to other simples)

  def reflectEffect[A:Manifest](x: Def[A], u: Summary): Exp[A] = {
    if (mustPure(u)) super.toAtom(x) else {
      val deps = calculateDependencies(u)
      val zd = Reflect(x,u,deps)
      if (mustIdempotent(u)) {
        findDefinition(zd) map (_.sym) filter (context contains _) getOrElse { // local cse
          val z = fresh[A]
          if (!x.toString.startsWith("ReadVar")) { // supress output for ReadVar
            printlog("promoting to effect: " + z + "=" + zd)
            for (w <- u.mayRead)
              printlog("depends on  " + w)
          }
          internalReflect(z, zd)
        }
      } else {
        val z = fresh[A]
        // make sure all writes go to allocs
        for (w <- u.mayWrite) {
          findDefinition(w) match {
            case Some(TP(_, Reflect(_, u, _))) if mustMutable(u) => // ok
            case o => 
              printerr("error: write to non-mutable " + o)
              printerr("at " + z + "=" + zd)
          }
        }
        // prevent sharing between mutable objects / disallow mutable escape for non read-only operations
        // make sure no mutable object becomes part of mutable result (in case of allocation)
        // or is written to another mutable object (in case of write)
        /*
          val a = mzeros(100)
          val b = zeros(100)
          val c = if (..) {
            a.update
            b
          } else {
            a
          }
        
          PROBLEM: the whole if expr has summary mayWrite=List(a), mstWrite=Nil and allAliases=List(a,b)
          
          what is the right thing?
          - mutableAliases \ mstWrite <-- first try, but maybe to restrictive?
          - mutableAliases \ mayWrite <-- too permissive?
          - something else?
        
        */
        /*
        val mutableAliases = mutableTransitiveAliases(x) filterNot (u.mayWrite contains _)
        if (mutableAliases.nonEmpty) {
          printerr("error: illegal sharing of mutable objects " + mutableAliases.mkString(", "))
          printerr("at " + z + "=" + zd)
        }
        */
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
    x match {
      case Reflect(Reify(_,_,_),_,_) =>
        printerr("error: reflecting a reify node.")
        printerr("at " + s + "=" + x)
      case _ => //ok
    }
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

  def pruneContext(ctx: List[Exp[Any]]): List[Exp[Any]] = ctx // TODO this doesn't work yet (because of loops!): filterNot { case Def(Reflect(_,u,_)) => mustOnlyRead(u) }

  def reifyEffects[A:Manifest](block: => Exp[A]): Exp[A] = {
    val save = context
    context = Nil
    
    val result = block
    val deps = context
    val summary = summarizeAll(deps)
    context = save
    
    if (deps.isEmpty) result else Reify(result, summary, pruneContext(deps)): Exp[A] // calls toAtom...
  }

  def reifyEffectsHere[A:Manifest](block: => Exp[A]): Exp[A] = {
    val save = context
    if (save eq null)
      context = Nil
    
    val result = block

    if ((save ne null) && context.take(save.length) != save) // TODO: use splitAt
      printerr("error: 'here' effects must leave outer information intact: " + save + " is not a prefix of " + context)

    val deps = if (save eq null) context else context.drop(save.length)
    
    val summary = summarizeAll(deps)
    context = save
    
    if (deps.isEmpty) result else Reify(result, summary, pruneContext(deps)): Exp[A] // calls toAtom...
  }

  // --- bookkeping

  override def reset = {
    shallowAliasCache.clear()
    deepAliasCache.clear()
    allAliasCache.clear()
    super.reset
  }

}
