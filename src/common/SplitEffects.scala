package scala.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}


trait SplitEffectsExpFat extends IfThenElseFatExp with WhileExp with PreviousIterationDummyExp { this: BooleanOpsExp with EqualExpBridge =>
  
  // split effectful statements: one piece for each affected mutable object.
  // this provides for simple dce: pieces not referenced are eliminated.
  // code generation will recombine pieces of previously split statements.

  // TBD: should this be in a central method such as reflectEffectInternal ?
  // or rather individually in ifThenElse and whileDo ?
  
  // TODO: SimpleLoops
  
  // FIXME: wo do not account for mutable objectes allocated in a loop
  // (see test8-speculative6)

  override def reflectEffectInternal[A:Typ](x: Def[A], u: Summary)(implicit pos: SourceContext): Exp[A] = x match {
    case IfThenElse(cond, thenp, elsep) =>
      val affected = (u.mayRead ++ u.mayWrite).distinct

      for (s <- affected)
        super.reflectEffectInternal(IfThenElse(cond,projectB(thenp,List(s)),projectB(elsep,List(s))), projectS(u,List(s)))

      if (u.maySimple)
        super.reflectEffectInternal(IfThenElse(cond,projectSimpleB(thenp),projectSimpleB(elsep)), projectSimpleS(u))

      super.reflectEffectInternal(IfThenElse(cond,projectPureB(thenp),projectPureB(elsep)), projectPureS(u))

    case While(cond, body) =>
      val affected = (u.mayRead ++ u.mayWrite).distinct

      for (s <- affected) {
        val cc = projectPureWithB(cond,List(s))
        val bb = projectPureWithB(body,List(s))
  
        super.reflectEffectInternal(While(cc,bb), projectS(u,List(s)))
    
        // tie recursive knot!

        // find PreviousIteration nodes that write to s
        // for each one, add a reflect dep to the loop
    
        val TP(loopSym, Reflect(While(_,_),_,_)) = globalDefs.last
    
        def xtract(b:Block[Any]) = b match { 
          case Block(Def(Reify(_,_,es: List[Sym[Any]]))) => 
            es map (e=>findDefinition(e)) collect { 
              case Some(t@TP(s1,Reflect(PreviousIteration(_),u,_))) if mayWrite(u,List(s)) => t }}
    
        val pvs = xtract(cc) ++ xtract(bb)
        val pvss = (pvs map (_.sym))

        //println("pvs: "+ s + " // " + pvss + " // " + pvs)
    
        def xform(x:Stm) = x match { 
          case TP(s1,Reflect(PreviousIteration(k),u,es)) if (pvss contains s1) => 
          //println("replace " + s1)
            TP(s1, Reflect(PreviousIteration(k),u,es:+loopSym))
          case t => t
        }
        (this: EmbeddedControls).__assign(globalDefs, globalDefs map xform) // FIXME: SI-6100
        (this: EmbeddedControls).__assign(localDefs, localDefs map xform) // FIXME: SI-6100
      }

      if (u.maySimple)
        super.reflectEffectInternal(While(projectPureWithSimpleB(cond),projectSimpleB(body)), projectSimpleS(u))

      //no pure result!
      //super.reflectEffectInternal(While(projectPureB(cond),projectPureB(body)), projectPureS(u)).asInstanceOf[Exp[A]]
      Const(()).asInstanceOf[Exp[A]]

    case PreviousIteration(k) =>
      val affected = (u.mayRead ++ u.mayWrite).distinct
      
      for (s <- affected)
        super.reflectEffectInternal(PreviousIteration(k), projectS(u,List(s)))

      if (u.maySimple)
        super.reflectEffectInternal(PreviousIteration(k), projectSimpleS(u))

      Const(()).asInstanceOf[Exp[A]]

    case _ => super.reflectEffectInternal(x,u)
  }




  def projectL(a: List[Sym[Any]], s: List[Sym[Any]]): List[Sym[Any]] = a filter (s contains _)
  def projectS(u: Summary, s: List[Sym[Any]]): Summary = 
    Pure().copy(mayRead = projectL(u.mayRead,s), mstRead = projectL(u.mstRead,s),
           mayWrite = projectL(u.mayWrite,s), mstWrite = projectL(u.mstWrite,s))
  def projectB(b: Block[Any], s: List[Sym[Any]]): Block[Unit] = b match {
    case Block(Def(Reify(x, u, es))) => 
      //println("project block " + s + ": " + es.map(e=>findDefinition(e.asInstanceOf[Sym[Any]])))
      val deps = calculateDependencies(es, Write(s), false)
      //println("deps: " + deps.map(e=>findDefinition(e.asInstanceOf[Sym[Any]])))
      
      Block(Reify(Const(), projectS(u,s), deps))
    case _ => Block(Const(()))
  }

  def projectSimpleS(u: Summary) = 
    Pure().copy(maySimple = u.maySimple, mstSimple = u.mstSimple)
  def projectSimpleB(b: Block[Any]): Block[Unit] = b match {
    case Block(Def(Reify(x, u, es))) => Block(Reify(Const(), projectSimpleS(u), calculateDependencies(es, Simple(), false)))
    case _ => Block(Const(()))
  }

  def projectPureS(u: Summary) = Pure()
  def projectPureB[A](b: Block[A]): Block[A] = b match {
    case Block(Def(Reify(x, u, es))) => Block(x)
    case _ => b
  }

  def projectPureWithB[A:Typ](b: Block[A], s: List[Sym[Any]]): Block[A] = {
    (projectPureB(b), projectB(b,s)) match {
      case (Block(x), Block(Def(Reify(Const(()), u, es)))) => Block(Reify(x, u, es))
      case (a,_) => a
    }
  }
  def projectPureWithSimpleB[A:Typ](b: Block[A]): Block[A] = {
    (projectPureB(b), projectSimpleB(b)) match {
      case (Block(x), Block(Def(Reify(Const(()), u, es)))) => Block(Reify(x, u, es))
      case (a,_) => a
    }
  }

// REMOVE


case class SimpleFatWhile(cond: Block[Boolean], body: List[Block[Any]]) extends FatDef {
  var extradeps: List[Sym[Any]] = Nil //HACK
}

override def syms(e: Any): List[Sym[Any]] = e match {
  case x@SimpleFatWhile(c, b) => syms(c) ++ syms(b)     ++ syms(x.extradeps)
  case _ => super.syms(e)
}

override def boundSyms(e: Any): List[Sym[Any]] = e match {
  case SimpleFatWhile(c, b) => effectSyms(c):::effectSyms(b)
  case _ => super.boundSyms(e)
}

override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
  case x@SimpleFatWhile(c, b) => freqHot(c) ++ freqHot(b) ++ freqNormal(x.extradeps)
  case _ => super.symsFreq(e)
}

}


trait PreviousIterationDummyExp extends BaseFatExp with EffectExp {
  
  case class PreviousIteration(k: Exp[Unit]) extends Def[Unit]
  
  case class SimpleFatPrevious(k: Exp[Unit], extra: List[Exp[Any]]) extends FatDef
  
  def reflectPreviousDummy(k: Exp[Unit], u: Summary) = reflectEffect(PreviousIteration(k), u)
  // TBD: does previousIteration need to reference the loop?
  // what if we split the loop?

}




trait BaseGenSplitEffects extends BaseGenIfThenElseFat with GenericFatCodegen {
  val IR: SplitEffectsExpFat
  import IR._

  override def reifyBlock[T: Typ](x: => Exp[T]): Block[T] = {
    val sup = super.reifyBlock(x)
    projectPureWithSimpleB(sup)
  }


  override def fatten(e: Stm): Stm = e match {
    case TP(s,d@While(c,b)) => TTP(List(s),List(d),SimpleFatWhile(c,List(b)))
    case TP(s,d@Reflect(While(c,b),u,es)) => 
      val x = SimpleFatWhile(c,List(b))
      x.extradeps = es.asInstanceOf[List[Sym[Any]]]
      TTP(List(s),List(d),x)
    case TP(s,d@Reflect(PreviousIteration(k),u,es)) => 
      val x = SimpleFatPrevious(k,es.asInstanceOf[List[Sym[Any]]])
      TTP(List(s),List(d),x)
    case _ => super.fatten(e)
  }

  // this is really the 'OPT' case ... <--- merge stms that have been split by Opt trait before
  override def fattenAll(e: List[Stm]): List[Stm] = {
    // FIXME: will need to check dependencies -- just grouping by condition won't work

    val e1 = super.fattenAll(e)
    //return e1
    
    //println("FATTEN ALL")
    //println(e1)
    
    val e2 = e1 collect {
      case t@TTP(lhs, mhs, p @ SimpleFatIfThenElse(c,as,bs)) => t
      case t@TTP(lhs, mhs, p @ SimpleFatWhile(c,b)) => t
      case t@TTP(lhs, mhs, p @ SimpleFatPrevious(k,es)) => t
    }
    
    val m = e2 groupBy { 
      case t@TTP(lhs, mhs, p @ SimpleFatIfThenElse(c,as,bs)) => (c, "if")
      case t@TTP(lhs, mhs, p @ SimpleFatWhile(Block(Def(Reify(c,_,_))),b)) => (c, "while")
      case t@TTP(lhs, mhs, p @ SimpleFatPrevious(k,es)) => (k,"prev")
    }
    
    val e3 = e1 diff e2

    val g1 = m map {
      case ((c:Exp[Boolean], "if"), ifs: List[TTP]) => TTP(ifs.flatMap(_.lhs), ifs.flatMap(_.mhs), 
        SimpleFatIfThenElse(c, ifs.flatMap(_.rhs.asInstanceOf[SimpleFatIfThenElse].thenp), 
          ifs.flatMap(_.rhs.asInstanceOf[SimpleFatIfThenElse].elsep)))
      case ((c, "while"), wls: List[TTP]) => 
        val x = SimpleFatWhile(wls.map(_.rhs.asInstanceOf[SimpleFatWhile].cond).apply(0), //FIXME: merge cond!!!
          wls.flatMap(_.rhs.asInstanceOf[SimpleFatWhile].body))
        x.extradeps = wls.flatMap(_.rhs.asInstanceOf[SimpleFatWhile].extradeps) diff wls.flatMap(_.lhs)
        TTP(wls.flatMap(_.lhs), wls.flatMap(_.mhs), // TODO: merge cond blocks!
        x)
      case ((k:Exp[Nothing],"prev"), pvs: List[TTP]) => 
        TTP(pvs.flatMap(_.lhs), pvs.flatMap(_.mhs), SimpleFatPrevious(k,pvs.flatMap(_.rhs.asInstanceOf[SimpleFatPrevious].extra)))
    }

    val r = e3 ++ g1
    //println("RES")
    //println(r)
    r
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatWhile(c,b) => 
      def quoteList[T](xs: List[Exp[T]]) = if (xs.length > 1) xs.map(quote).mkString("(",",",")") else xs.map(quote).mkString(",")
      //if (symList.length > 1) stream.println("// TODO: use vars instead of tuples to return multiple values")
      stream.println("while ({ // " + symList)
      emitFatBlock(List(c))
      stream.println(quoteList(List(c).map(getBlockResult)))
      stream.println("}) {")
      emitFatBlock(b)
      //stream.println(quoteList(b.map(getBlockResult)))
      stream.println("}")
    case _ => super.emitFatNode(symList, rhs)
  }



}




trait ScalaGenSplitEffects extends BaseGenSplitEffects with ScalaGenIfThenElseFat {
  val IR: SplitEffectsExpFat
  import IR._  
}



trait ScalaGenPreviousIterationDummy extends ScalaGenFat {
  val IR: PreviousIterationDummyExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Reflect(PreviousIteration(k),u,es) =>
      // dummy ...
      //emitValDef(sym, "() // dummy placeholder for previous iteration " + u)
    case _ => super.emitNode(sym, rhs)
  }

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {
    case SimpleFatPrevious(k,es) => 
      // dummy ...
      stream.println("// dummy placeholder for previous iteration: " + symList + " = " + k + " / " + es)
    
    case _ => super.emitFatNode(symList, rhs)
  }

}
