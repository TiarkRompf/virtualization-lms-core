package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.reflect.SourceContext
import scala.virtualization.lms.internal.{GenericNestedCodegen, GenericFatCodegen, GenerationFailedException}


trait SplitEffectsExpFat extends IfThenElseFatExp with WhileExp { this: BooleanOpsExp with EqualExpBridge =>
  
  // TBD: should this be in a central method such as reflectEffectInternal ?
  // or rather individually in ifThenElse and whileDo ?
  
  override def reflectEffectInternal[A:Manifest](x: Def[A], u: Summary): Exp[A] = x match {
    case IfThenElse(cond, thenp, elsep) =>
      val affected = (u.mayRead ++ u.mayWrite).distinct

      for (s <- affected)
        super.reflectEffectInternal(IfThenElse(cond,projectB(thenp,List(s)),projectB(elsep,List(s))), projectS(u,List(s)))

      if (u.maySimple)
        super.reflectEffectInternal(IfThenElse(cond,projectSimpleB(thenp),projectSimpleB(elsep)), projectSimpleS(u))

      super.reflectEffectInternal(IfThenElse(cond,projectPureB(thenp),projectPureB(elsep)), projectPureS(u))

/*    
    TODO: doesn't work yet. a split variant that reads some var does not link to the other one that writes the var

    case While(cond, body) =>
      val affected = (u.mayRead ++ u.mayWrite).distinct

      for (s <- affected)
        super.reflectEffectInternal(While(projectPureWithB(cond,List(s)),projectB(body,List(s))), projectS(u,List(s)))

      if (u.maySimple)
        super.reflectEffectInternal(While(projectPureWithSimpleB(cond),projectSimpleB(body)), projectSimpleS(u))

      //super.reflectEffectInternal(While(projectPureB(cond),projectPureB(body)), projectPureS(u)).asInstanceOf[Exp[A]]
      Const(()).asInstanceOf[Exp[A]]
*/    
    case _ => super.reflectEffectInternal(x,u)
  }




  def projectL(a: List[Sym[Any]], s: List[Sym[Any]]): List[Sym[Any]] = a filter (s contains _)
  def projectS(u: Summary, s: List[Sym[Any]]): Summary = 
    Pure().copy(mayRead = projectL(u.mayRead,s), mstRead = projectL(u.mstRead,s),
           mayWrite = projectL(u.mayWrite,s), mstWrite = projectL(u.mstWrite,s))
  def projectB(b: Block[Any], s: List[Sym[Any]]): Block[Unit] = b match {
    case Block(Def(Reify(x, u, es))) => 
      //println("project block " + s + ": " + es.map(e=>findDefinition(e.asInstanceOf[Sym[Any]])))
      val deps = calculateDependencies(es, Write(s))
      //println("deps: " + deps.map(e=>findDefinition(e.asInstanceOf[Sym[Any]])))
      
      Block(Reify(Const(), projectS(u,s), deps))
    case _ => Block(Const(()))
  }

  def projectSimpleS(u: Summary) = 
    Pure().copy(maySimple = u.maySimple, mstSimple = u.mstSimple)
  def projectSimpleB(b: Block[Any]): Block[Unit] = b match {
    case Block(Def(Reify(x, u, es))) => Block(Reify(Const(), projectSimpleS(u), calculateDependencies(es, Simple())))
    case _ => Block(Const(()))
  }

  def projectPureS(u: Summary) = Pure()
  def projectPureB[A](b: Block[A]): Block[A] = b match {
    case Block(Def(Reify(x, u, es))) => Block(x)
    case _ => b
  }

  def projectPureWithB[A:Manifest](b: Block[A], s: List[Sym[Any]]): Block[A] = {
    (projectPureB(b), projectB(b,s)) match {
      case (Block(x), Block(Def(Reify(Const(()), u, es)))) => Block(Reify(x, u, es))
      case (a,_) => a
    }
  }
  def projectPureWithSimpleB[A:Manifest](b: Block[A]): Block[A] = {
    (projectPureB(b), projectSimpleB(b)) match {
      case (Block(x), Block(Def(Reify(Const(()), u, es)))) => Block(Reify(x, u, es))
      case (a,_) => a
    }
  }

}



trait BaseGenSplitEffects extends BaseGenIfThenElseFat with GenericFatCodegen {
  val IR: SplitEffectsExpFat
  import IR._

  override def reifyBlock[T: Manifest](x: => Exp[T]): Block[T] = {
    val sup = super.reifyBlock(x)
    projectPureWithSimpleB(sup)
  }



  // this is really the 'OPT' case ... <--- merge stms that have been split by Opt trait before
  override def fattenAll(e: List[Stm]): List[Stm] = {
    // FIXME: will need to check dependencies -- just grouping by condition won't work

    val e1 = super.fattenAll(e)
    //return e1
    
    val m = e1 collect { 
      case t@TTP(lhs, mhs, p @ SimpleFatIfThenElse(c,as,bs)) => t
    } groupBy { 
      case t@TTP(lhs, mhs, p @ SimpleFatIfThenElse(c,as,bs)) => c
    }
    
    val e2 = e1 filter { 
      case t@TTP(lhs, mhs, p @ SimpleFatIfThenElse(c,as,bs)) => false
      case _ => true
    }

    val g1 = m map {
      case (c, ifs) => TTP(ifs.flatMap(_.lhs), ifs.flatMap(_.mhs), 
        SimpleFatIfThenElse(c, ifs.flatMap(_.rhs.asInstanceOf[SimpleFatIfThenElse].thenp), 
          ifs.flatMap(_.rhs.asInstanceOf[SimpleFatIfThenElse].elsep)))
    }

    e2 ++ g1
  }

}


trait ScalaGenSplitEffects extends BaseGenSplitEffects with ScalaGenIfThenElseFat {
  val IR: SplitEffectsExpFat
  import IR._  
}