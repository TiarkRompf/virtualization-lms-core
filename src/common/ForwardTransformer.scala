package scala.virtualization.lms
package common

import scala.reflect.SourceContext
import scala.collection.{immutable,mutable}

import internal.{AbstractSubstTransformer, Traversal, IterativeTraversal}
import internal.{FatExpressions, Effects, Transforming}

trait ForwardTransformer extends AbstractSubstTransformer with Traversal { self =>
  val IR: FatExpressions with EffectExp
  import IR._

  override def hasContext = true

  override def processBlock[A:Manifest](xs: Block[A]): Block[A] = transformBlock(xs)

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
      if (sym != replace)
        register(sym -> replace)  // record substitution only if result is different

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
    case TP(sym,rhs) =>
      self_mirror(sym, rhs)
  }

  // TODO: Should be removed eventually
  def self_mirror[A](sym: Sym[A], rhs : Def[A]): Exp[A] = {
    //TODO: HACK -- should not catch errors
    try {
      val sym2 = mirror(rhs, self.asInstanceOf[Transformer])(mtype(sym.tp),mpos(sym.pos))
      setProps(sym2, mirror(getProps(sym), self.asInstanceOf[Transformer]))
      (sym2)
    }
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

  def transformDef[A](lhs: Sym[A], rhs: Def[A]): Option[() => Def[A]] = None

  override def traverseStmsInBlock[A](stms: Seq[Stm]): Unit = {
    for (sym <- recursive) {
      register(sym -> fresh(mtype(sym.tp)))
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

/**
 * Delite transformers are run in a fixpoint fashion, but with a limited number of iterations.
 * At the beginning of each iteration, the info string is printed to the log.
 */
// TODO: Unify this with IterativeTraversal
trait FixpointTransformer extends ForwardTransformer {
  import IR._
  var runs = 0
  def getInfoString: String
  def isDone: Boolean

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    runs += 1
    super.runOnce(b)
  }

  override def run[A:Manifest](s: Block[A]): Block[A] = {
    var blk = s
    while (!isDone) {
      blk = runOnce(blk)
    }
    blk
  }
}

/**
 * Skip statements that don't have symbols which need substitution, unless they contain
 * blocks (need to recurse into blocks).
 */
trait PreservingFixpointTransformer extends FixpointTransformer {
  import IR._

  // Implement optimization suggested in ForwardTransformer:
  // optimization from MirrorRetainBlockTransformer in TestMiscTransform
  // we want to skip those statements that don't have symbols that need substitution
  // however we need to recurse into any blocks
  // Also need to mirror all effects because otherwise they won't be reified
  override def transformStm(stm: Stm): Exp[Any] = {
    stm match {
    case TP(sym, rhs@Reflect(_, _, _)) =>
      self_mirror(sym, rhs)
    case TP(sym, rhs) if (needsSubst(rhs) || needsRecursion(rhs)) =>
      self_mirror(sym, rhs)
    case TP(sym, rhs) => // no mirroring, preserve statements
      if (!globalDefs.contains(stm))
        reflectSubGraph(List(stm))
      sym
  }}

  def needsSubst(e: Any) = (syms(e) ++ boundSyms(e)).exists(subst contains _)
  def needsRecursion(e: Any) = !blocks(e).isEmpty || hasFuncs(e)
  def hasFuncs(e: Any): Boolean = e match {
    case _: Function0[_] | _: Function1[_,_] | _: Function2[_,_,_] | _: Function3[_,_,_,_] |
         _: Function4[_,_,_,_,_] | _: Function5[_,_,_,_,_,_] | _: Function6[_,_,_,_,_,_,_] |
         _: Function7[_,_,_,_,_,_,_,_] | _: Function8[_,_,_,_,_,_,_,_,_] | _: Function9[_,_,_,_,_,_,_,_,_,_] |
         _: Function10[_,_,_,_,_,_,_,_,_,_,_] | _: Function11[_,_,_,_,_,_,_,_,_,_,_,_] |
         _: Function12[_,_,_,_,_,_,_,_,_,_,_,_,_] | _: Function13[_,_,_,_,_,_,_,_,_,_,_,_,_,_] |
         _: Function14[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] | _: Function15[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] |
         _: Function16[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] | _: Function17[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] |
         _: Function18[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] |
         _: Function19[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] |
         _: Function20[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] |
         _: Function21[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] |
         _: Function22[_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_,_] => true
    case p: Product => p.productIterator.exists(hasFuncs(_))
    case _ => false
  }
}


trait WorklistTransformer extends FixpointTransformer {
  import IR._

  override def getInfoString = nextSubst.toString

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

  //override def hasConverged = runs > 0 && nextSubst.isEmpty
  def isDone = runs > 0 && nextSubst.isEmpty // Needed for LMS tests

  override def processBlock[A:Manifest](s: Block[A]): Block[A] = {
    resetSubst()
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
