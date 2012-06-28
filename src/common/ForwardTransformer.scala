package scala.virtualization.lms
package common

import scala.collection.{immutable,mutable}
import scala.reflect.SourceContext

trait ForwardTransformer extends internal.AbstractSubstTransformer with internal.FatBlockTraversal { self =>
  val IR: LoopsFatExp with IfThenElseFatExp
  import IR._
  
  def transformBlock[A:Manifest](block: Block[A]): Block[A] = {
    reifyEffects {
      reflectBlock(block)
    }
  }

  override def hasContext = true
  
  override def apply[A:Manifest](xs: Block[A]): Block[A] = transformBlock(xs)
  
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    withSubstScope {
      traverseBlock(block)
      apply(getBlockResult(block))
    }
  }

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) => 
      val sym2 = apply(sym)
      if (sym2 == sym) {
        val replace = transformStm(stm)
        // printlog("registering forward transformation: " + sym + " to " + replace)
        // printlog("while processing stm: " + stm)          
        assert(!subst.contains(sym) || subst(sym) == replace)
        if (sym != replace) { // record substitution only if result is different
          subst += (sym -> replace)
        }
      } else {
        //printerr("warning: transformer already has a substitution " + sym + "->" + sym2 + " when encountering stm " + stm)
        // what to do? bail out? lookup def and then transform???
        // can happen in recursive case
        transformStm(stm)
      }
  }
  
  
  def transformStm(stm: Stm): Exp[Any] = stm match { // override this to implement custom transform
    case TP(sym,rhs) =>
      try {
        /*
        TBD: optimization from MirrorRetainBlockTransformer in TestMiscTransform -- is it worth doing??        
        // we want to skip those statements that don't have symbols that need substitution
        // however we need to recurse into any blocks
        if (!syms(rhs).exists(subst contains _) && blocks(rhs).isEmpty) {
          if (!globalDefs.contains(stm)) reflectSubGraph(List(stm))
          return sym
        }
        */
        mirror(rhs, self.asInstanceOf[Transformer])(mtype(sym.tp),mpos(sym.pos)) // cast needed why?
      } catch { //hack -- should not catch errors
        case e if e.toString contains "don't know how to mirror" => 
          printerr("error: " + e.getMessage)
          sym
        case e => 
          printerr("error: exception during mirroring of "+rhs+": "+ e)
          e.printStackTrace; 
          sym            
      }
  }
  
}


trait RecursiveTransformer extends ForwardTransformer { self =>
  import IR._

  var allocMap: Map[Sym[Any], (Sym[Any], () => Def[Any])] = _
  var allocPhase: Boolean = _

  def run[A:Manifest](s: Block[A]): Block[A] = {
    allocMap = Map.empty
    allocPhase = true
    transformBlock(s)

    subst = for ((s1, (s2, _)) <- allocMap) yield (s1 -> s2)
    allocPhase = false
    transformBlock(s)
  }

  def transformDef[A](lhs: Sym[A], rhs: Def[A]): Option[() => Def[A]] = None

  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(s, rhs) if allocPhase => transformDef(s, rhs) match {
      case Some(rhsThunk) =>
        allocMap += (s -> ((fresh(mtype(s.tp)), rhsThunk)))
        s
      case None => super.transformStm(stm)
    }
    case TP(s, rhs) if !allocPhase && allocMap.contains(s) =>
      val (s2, rhsThunk) = allocMap(s)
      createDefinition(s2, rhsThunk())
      s
    case _ => super.transformStm(stm)
  }
}


trait WorklistTransformer extends ForwardTransformer { // need backward version, too?
  val IR: LoopsFatExp with IfThenElseFatExp
  import IR._
  var curSubst: Map[Sym[Any],() => Exp[Any]] = Map.empty
  var nextSubst: Map[Sym[Any],() => Exp[Any]] = Map.empty
  def register[A](x: Exp[A])(y: => Exp[A]): Unit = {
    if (nextSubst.contains(x.asInstanceOf[Sym[A]]))
      printdbg("discarding, already have a replacement for " + x)
    else {
      printdbg("register replacement for " + x)
      nextSubst = nextSubst + (x.asInstanceOf[Sym[A]] -> (() => y))
    }
  }
  def isDone = nextSubst.isEmpty
  def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    subst = Map.empty
    curSubst = nextSubst
    nextSubst = Map.empty
    transformBlock(s)
  }
  def run[A:Manifest](s: Block[A]): Block[A] = {
    if (isDone) s else run(runOnce(s))
  }
  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(sym, rhs) => 
      curSubst.get(sym) match {
        case Some(replace) =>
          printdbg("install replacement for " + sym)
          val b = reifyEffects(replace())
          reflectBlock(b)
        case None => 
          super.transformStm(stm)
      }
  }

}
