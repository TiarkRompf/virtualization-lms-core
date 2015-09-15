package scala.virtualization.lms
package common

import scala.reflect.SourceContext
import scala.collection.{immutable,mutable}

import internal.{AbstractSubstTransformer, Traversal, IterativeTraversal}
import internal.{FatExpressions, Effects, Transforming}

trait ForwardTransformer extends AbstractSubstTransformer with Traversal { self =>
  val IR: FatExpressions with Effects with Transforming
  import IR._

  override def hasContext = true

  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    withSubstScope {
      traverseBlock(block)
      apply(getBlockResult(block))
    }
  }

  override def transformBlock[A](xs: Block[A]): Block[A] = {
    implicit val mA: Manifest[A] = xs.res.tp
    val block2 = reifyBlock { reflectBlock(xs) }
    if (!copyingBlocks) blockSubst += xs -> block2
    (block2)
  }

  /**
   * TODO: How to handle TTP here?
   */
  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) if apply(sym) == sym =>
      val replace = transformStm(stm)
      assert(!subst.contains(sym) || subst(sym) == replace)
      if (sym != replace) subst += (sym -> replace) // record substitution only if result is different

    case TP(sym, rhs) =>
      if (recursive.contains(sym))  // O(n) since recursive is a list!
        transformStm(stm)
      else
        // TODO: What to do when sym already has replacement? Bail out? Look up def and then transform???
        printerr("warning: transformer already has a substitution " + sym + "->" + apply(sym) + " when encountering stm " + stm)
  }

  /*
    TBD: optimization from MirrorRetainBlockTransformer in TestMiscTransform -- is it worth doing??
    We want to skip those statements that don't have symbols that need substitution
    However we need to recurse into any blocks
    if (!syms(rhs).exists(subst contains _) && blocks(rhs).isEmpty) {
    if (!globalDefs.contains(stm)) reflectSubGraph(List(stm))
      return sym
    }

    TODO: What to do for TTP?
  */
  // override this to implement custom transform
  def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(sym,rhs) => self_mirror(sym, rhs)
  }

  // TODO: Should be removed eventually
  def self_mirror[A](sym: Sym[A], rhs : Def[A]): Exp[A] = {
    try {
      mirror(rhs, self.asInstanceOf[Transformer])(mtype(sym.tp),mpos(sym.pos)) // cast needed why?
    }
    //TODO: HACK -- should not catch errors
    catch {
      case e if e.toString contains "don't know how to mirror" =>
        printerr("error: " + e.getMessage)
        sym
      case e: Throwable =>
        printerr("error: exception during mirroring of "+rhs+": "+ e)
        e.printStackTrace;
        sym
    }
  }
}

trait RecursiveTransformer extends ForwardTransformer { self =>
  import IR._

  override def runOnce[A:Manifest](s: Block[A]): Block[A] = transformBlock(s)

  def transformDef[A](lhs: Sym[A], rhs: Def[A]): Option[() => Def[A]] = None

  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = {
    for (sym <- recursive) {
      subst += (sym -> fresh(mtype(sym.tp)))
    }
    super.traverseStmsInBlock(stms)
  }

  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(s, rhs) => transformDef(s, rhs) match {
      case Some(rhsThunk) =>
        val s2 = subst.get(s) match {
          case Some(s2@Sym(_)) => assert(recursive.contains(s)); s2
          case _ => assert(!recursive.contains(s)); fresh(mtype(s.tp))
        }
        createDefinition(s2, rhsThunk())
        s2
      case None => subst.get(s) match {
        case Some(s2@Sym(_)) =>
          assert(recursive.contains(s))
          createDefinition(s2, Def.unapply(self_mirror(s, rhs)).get)
          s2
        case None =>
          assert(!recursive.contains(s))
          super.transformStm(stm)
      }
    }
    case _ => super.transformStm(stm)
  }
}

trait IterativeTransformer extends ForwardTransformer with IterativeTraversal {
  import IR._
  // TODO: Anything needed here? Does this need to be its own trait?
}

trait WorklistTransformer extends IterativeTransformer {
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

  override def hasConverged = runs > 0 && nextSubst.isEmpty

  def isDone = nextSubst.isEmpty // Needed for LMS tests

  override def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    subst = Map.empty
    curSubst = nextSubst
    nextSubst = Map.empty
    transformBlock(s)
  }

  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(sym, rhs) =>
      curSubst.get(sym) match {
        case Some(replace) =>
          printdbg("install replacement for " + sym)
          //val b = reifyEffects(replace())
          //reflectBlock(b)
          replace()
        case None =>
          super.transformStm(stm)
      }
  }
}