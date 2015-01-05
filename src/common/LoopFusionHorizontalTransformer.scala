package scala.virtualization.lms
package common

import util.GraphUtil
import scala.collection.mutable.{HashMap, HashSet}

// TODO document interface
trait LoopFusionHorizontalTransformer extends PreservingFixpointTransformer { 
  val IR: LoopFusionCore
  import IR.{__newVar => _, _}

  // TODO dedup
  type EffectTuple = Option[(Summary, List[Exp[Any]])]
  object LoopOrReflectedLoop {
    def unapply(a: Def[Any]): Option[(AbstractLoop[_], EffectTuple)] = a match {
      case Reflect(loop: AbstractLoop[_], summ, deps) => Some((loop, Some((summ, deps))))
      case loop: AbstractLoop[_] => Some((loop, None))
      case _ => None
    }
  }
  object IfOrReflectedIf {
    def unapply(a: Def[Any]): Option[(AbstractIfThenElse[_], EffectTuple)] = a match {
      case Reflect(ite: AbstractIfThenElse[_], summ, deps) => Some((ite, Some((summ, deps))))
      case ite: AbstractIfThenElse[_] => Some((ite, None))
      case _ => None
    }
  }
  def getFusedSyms(c: CanBeFused, effectful: Boolean): Option[(List[Sym[Any]], Boolean)] = {
    var setToFuseEffectful = effectful
    val setToFuse = c.getFusedSet match {
      case None => None
      case Some(fusedDefs) =>
        // ignore fat defs, they're added by fattening, but are already
        // present in their slim forms
        val defs = fusedDefs.filter({ case d: Def[_] => true case _ => false })
        Some(defs.map({ d => 
          globalDefs.collect({ 
              case TP(fusedSym, `d`) => fusedSym
              case TP(fusedSym, Reflect(`d`, _, _)) => setToFuseEffectful = true; fusedSym
          }) match {
            case Nil => sys.error("(HFT) No statements/symbols found for definition. Def: " + d)
            case fusedSym :: Nil => fusedSym
            case list => sys.error("(HFT) Multiple statements/symbols found for definition. Def: " + d + ", syms: " + list)
          }
        }))
    }
    setToFuse.map((_, setToFuseEffectful))
  }

  abstract class FusedSet {
    val syms: List[Sym[Any]]
    val setIndex: Int
    val hasEffects: Boolean
  }
  /** A set of horizontally fused loops.
    * @param shape common shape @param index common index
    * @param syms loop symbols that need to be a part of the set (also loops
    * that haven't been processed yet, but are vertically fused with one of the
    * loops in the set)
    * @param setIndex index of this fusedLoopSet in the FusionScope instance owning it
    * @param innerScope the FusionScope instance used for fusing all inner scopes of the
    * loops in the set. Need to use same instance for each so that we can fuse
    * across (they're different scopes in this schedule, but will be in the
    * same scope of the fat fused loop)
    * @param hasEffects true if the set contains an effectful loop (at most one), it then
    * cannot be fused with any other effectful loops/sets.
    */
  case class FusedLoopSet(shape: Exp[Int], index: Sym[Int], syms: List[Sym[Any]], setIndex: Int,
      innerScope: FusionScope, hasEffects: Boolean) extends FusedSet {
    def addSyms(newSyms: List[Sym[Any]], effectful: Boolean) = {
      assert(!(effectful && hasEffects), "(FTO) ERROR: cannot fuse two effectful sets")
      FusedLoopSet(shape, index, syms ++ newSyms, setIndex, innerScope, hasEffects || effectful)
    }
    override def toString = "FusedLoopSet(shape = " + shape + ", indexSym = " + index + ", loopSyms = " + syms + ")"
  }
  /** A set of horizontally fused Ifs. */
  case class FusedIfSet(cond: Exp[Boolean], syms: List[Sym[Any]], setIndex: Int,
      thenInnerScope: FusionScope, elseInnerScope: FusionScope, hasEffects: Boolean) extends FusedSet {
    def addSym(newSym: Sym[Any], effectful: Boolean) = {
      assert(!(effectful && hasEffects), "(FTO) ERROR: cannot fuse two effectful sets")
      FusedIfSet(cond, syms ++ List(newSym), setIndex, thenInnerScope, elseInnerScope, hasEffects || effectful)
    }
    override def toString = "FusedIfSet(cond = " + cond + ", ifSyms = " + syms + ")"
  }

  /** Fusion sets for a particular fusion scope (can be a combination of several inner
    * scopes if the outer loops have been fused). */
  class FusionScope {
    AllFusionScopes.add(this)
    // All syms here are original syms
    private var loopSets: List[FusedLoopSet] = Nil
    private var ifSets: List[FusedIfSet] = Nil
    // Since we want to prepend new loopSets but still have stable indices, index from back
    private def getLoopSet(setIndex: Int) = loopSets(loopSets.length - 1 - setIndex)
    private def getIfSet(setIndex: Int) = ifSets(ifSets.length - 1 - setIndex)

    // TODO lazy HashMaps?
    // Lookup mandatory fusion set by symbol (sym is vertically fused with a loop in the set)
    private val sym2loopSet = new HashMap[Sym[Any], Int]
    private val sym2ifSet = new HashMap[Sym[Any], Int]
    // Lookup fusion set candidates by shape, need to check independence
    private val shape2loopSets = new HashMap[Exp[Int], List[Int]]
    private val cond2ifSets = new HashMap[Exp[Boolean], List[Int]]

    // realLoopSets(i) contains the symbols from loopSets(i) that were actually fused (not DCE'd)
    // these are new (transformed) syms
    // It also contains one loop of the set so that each loop added to the set can be
    // updated as fused with this one.
    private var realLoopSets: List[(List[Sym[Any]], Option[CanBeFused])] = Nil
    private var realIfSets: List[(List[Sym[Any]], Option[CanBeFused])] = Nil

    def getLoopSet(sym: Sym[Any]): Option[FusedLoopSet] = sym2loopSet.get(sym).map(getLoopSet(_))
    def getIfSet(sym: Sym[Any]): Option[FusedIfSet] = sym2ifSet.get(sym).map(getIfSet(_))
    def getByShape(shape: Exp[Int]): List[FusedLoopSet] = shape2loopSets.get(shape).getOrElse(Nil).map(getLoopSet(_))
    def getByCond(cond: Exp[Boolean]): List[FusedIfSet] = cond2ifSets.get(cond).getOrElse(Nil).map(getIfSet(_))
    def getAllFusedLoops(syms: List[Sym[Any]]): List[Sym[Any]] = (syms ++ syms.flatMap(getLoopSet(_).map(_.syms).getOrElse(Nil)))
    def getAllFusedIfs(syms: List[Sym[Any]]): List[Sym[Any]] = (syms ++ syms.flatMap(getIfSet(_).map(_.syms).getOrElse(Nil)))
    
    // Start a new fusion set
    def recordNewLoop(shape: Exp[Int], index: Sym[Int], syms: List[Sym[Any]], effectful: Boolean) = {
      val setIndex = loopSets.length
      val set = FusedLoopSet(shape, index, syms, setIndex, new FusionScope, effectful)
      loopSets = set :: loopSets
      realLoopSets ::= (Nil, None)
      val indexList = setIndex :: shape2loopSets.get(shape).getOrElse(Nil)
      shape2loopSets.put(shape, indexList)
      set.syms.foreach({ otherSym => sym2loopSet.put(otherSym, setIndex) match {
        case Some(old) => sys.error("FusedLoopSet already had a set for symbol " + otherSym + ": " + old + " = " 
          + getLoopSet(old) + " instead of new " + set)
        case None =>
      }})
      set.innerScope
    }
    def recordNewIf(cond: Exp[Boolean], syms: List[Sym[Any]], effectful: Boolean) = {
      val setIndex = ifSets.length
      val set = FusedIfSet(cond, syms, setIndex, new FusionScope, new FusionScope, effectful)
      ifSets = set :: ifSets
      realIfSets ::= (Nil, None)
      val indexList = setIndex :: cond2ifSets.get(cond).getOrElse(Nil)
      cond2ifSets.put(cond, indexList)
      set.syms.foreach({ otherSym => sym2ifSet.put(otherSym, setIndex) match {
        case Some(old) => sys.error("FusedIfSet already had a set for symbol " + otherSym + ": " + old + " = " 
          + getIfSet(old) + " instead of new " + set)
        case None =>
      }})
      (set.thenInnerScope, set.elseInnerScope)
    }

    // Add the syms to the existing fusion set
    def recordAddLoop(fusedLoopSet: FusedLoopSet, addedSyms: List[Sym[Any]], effectful: Boolean) = {
      val setIndex = fusedLoopSet.setIndex
      addedSyms.foreach(sym2loopSet.put(_, setIndex))
      loopSets = loopSets.updated(loopSets.length - 1 - setIndex, fusedLoopSet.addSyms(addedSyms, effectful))
      fusedLoopSet.innerScope
    }
    def recordAddIf(fusedIfSet: FusedIfSet, addedSym: Sym[Any], effectful: Boolean) = {
      val setIndex = fusedIfSet.setIndex
      sym2ifSet.put(addedSym, setIndex)
      ifSets = ifSets.updated(ifSets.length - 1 - setIndex, fusedIfSet.addSym(addedSym, effectful))
      (fusedIfSet.thenInnerScope, fusedIfSet.elseInnerScope)
    }

    // Record actual fusion resulting in the newSym transformed loop
    def recordRealLoop(sym: Sym[Any], newSym: Exp[Any]): Unit = {
      val setIndex = sym2loopSet(sym)
      val substSym = newSym match { case s@Sym(_) => s case _ => sym }
      val listIndex = realLoopSets.length - 1 - setIndex
      val (oldSet, oldCanBeFused) = realLoopSets(listIndex)

      val newCanBeFused = substSym match {
        case Def(EatReflect(c: CanBeFused)) => oldCanBeFused match {
          case Some(existing) => c.registerFusion(existing); oldCanBeFused
          case None => Some(c)
        }
        case _ => sys.error("Horizontal fusion with something that isn't a CanBeFused: " + substSym +
          " = " + findDefinition(substSym))
      }
      realLoopSets = realLoopSets.updated(listIndex, (substSym :: oldSet, newCanBeFused))
    }
    def recordRealIf(sym: Sym[Any], newSym: Exp[Any]): Unit = {
      val setIndex = sym2ifSet(sym)
      val substSym = newSym match { case s@Sym(_) => s case _ => sym }
      val listIndex = realIfSets.length - 1 - setIndex
      val (oldSet, oldCanBeFused) = realIfSets(listIndex)

      val newCanBeFused = substSym match {
        case Def(EatReflect(c: CanBeFused)) => oldCanBeFused match {
          case Some(existing) => c.registerFusion(existing); oldCanBeFused
          case None => Some(c)
        }
        case _ => sys.error("Horizontal fusion with something that isn't a CanBeFused: " + substSym +
          " = " + findDefinition(substSym))
      }
      realIfSets = realIfSets.updated(listIndex, (substSym :: oldSet, newCanBeFused))
    }
    def getRealLoops = realLoopSets.unzip._1.filter(_.length > 1).map(_.reverse.distinct)

    override def toString() = "FusionScope(" + loopSets.mkString("\n") + ")"

    /** See remapIndexIfFixedLength. */
    var fixedLengthIndices: Option[HashMap[Sym[Int], Sym[Any]]] = None
  }

  /** Records all fusion scopes and loads correct scope for
    * reflecting inner blocks. */
  object AllFusionScopes {
    private var allFusionScope: List[FusionScope] = Nil
    def add(f: FusionScope) = allFusionScope ::= f
    def get: List[List[Sym[Any]]] = allFusionScope.flatMap(_.getRealLoops)

    // Record inner scope that should be used before mirroring blocks
    private val blockToFused = new HashMap[Block[Any], FusionScope]()
    def set(blocks: List[Block[Any]], fused: FusionScope) = blocks.foreach { block =>
      blockToFused += (block -> fused)
    }
    def get(block: Block[Any]) = blockToFused.get(block).getOrElse(new FusionScope)
    // Remove entries after use to keep map small
    def remove(blocks: List[Block[Any]]) = blocks foreach { block =>
      blockToFused.remove(block)
    }
  }

  // --- per scope datastructures ----
  var current = new FusionScope
  
  // indented printing to show scopes
  var indent: Int = -2
  def printdbg(x: => Any) { if (verbosity >= 2) System.err.println(" " * indent + x) }
  def printlog(x: => Any) { if (verbosity >= 1) System.err.println(" " * indent + x) }

  // FixPointTransformer methods, horizontal fusion should only run once
  def getInfoString = "LoopFusionHorizontalTransformer only runs once"
  var hasRunOnce = false
  def isDone = hasRunOnce
  def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    val newBlock = if (shouldDoFusion) transformBlock(s) else s
    hasRunOnce = true
    newBlock
  }

  // Set correct current fusion scope
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    val old = current
    current = AllFusionScopes.get(block)
    indent += 2
    val res = super.reflectBlock(block)
    indent -= 2
    current = old
    res
  }

  /** Some DSLs use while-loops, so two loops in the same scope with the same
    * index variable cause an error. Vertical fusion pro-actively remaps
    * indices to be the same for all loops with the same fixed length in a
    * scope. The horizontal transformer re-uniquifies them if the loops haven't
    * been fused after all. The HashMap is created lazily if really needed in
    * the current scope and goes from loop index to one loop with that index,
    * any other loops with that index need to be in the same fusion set. */
  def remapIndexIfFixedLength(loopSym: Sym[Any], loop: AbstractLoop[_]): Option[Sym[Int]] = {
    val oldIndex = loop.v
    (hasFixedOutLength(loop), current.fixedLengthIndices) match {
      case (false, _) => None
      case (true, None) =>
        current.fixedLengthIndices = Some(new HashMap[Sym[Int], FusedLoopSet])
        current.fixedLengthIndices.get += (oldIndex -> loopSym)
        None
      case (true, Some(fixedLengthIndicesVal)) => fixedLengthIndicesVal.get(oldIndex) match {
          case None => 
            fixedLengthIndicesVal += (oldIndex -> loopSym)
            None
          case Some(otherLoop) if (current.getLoopSet(loopSym) == current.getLoopSet(otherLoop)) =>
              None
          case _ =>
              val newIndex = fresh[Int]
              subst += (oldIndex -> newIndex)
              fixedLengthIndicesVal += (newIndex -> loopSym)
              Some(newIndex)
      }    
    }
  }

  def hasFixedOutLength(loop: AbstractLoop[_]): Boolean = loop match {
    case FixedDomain(Const(len)) => true
    case FixedDomain(domainSym: Sym[Int @unchecked]) => subst.get(domainSym) match {
      case Some(Const(_)) => true
      case Some(sSym: Sym[Int @unchecked]) => !isWritableSym(sSym)
      case _ => !isWritableSym(domainSym)
    }
    case _ => false
  }


  /* The transformer: First find HFT set for the current type of CanBeFused (loop or if)
   * 1. If CanBeFused already registered in a HFT set, use that set.
   * 2. Get CanBeFused set (maybe singleton if no previous transformers fused it)
   *  a) try to fuse set with existing HFT set
   *    - same cond for ifs, same shape for loops
   *    - mutually independent
   *  b) start new HFT set
   * 3. Fusion:
   *  a) register fusion in CanBeFused for future transformers and CombineTTPScheduling
   *  b) for loops, if necessary remap loop index variable so all loops have same index
         for ifs, no substitutions, so symbols preserved unless effectful
   *  c) use combined inner scopes to process bodies of fused loops and if/else branches
   *     of fused ifs
   */
  override def transformStm(stm: Stm): Exp[Any] = {
    val transfStm = stm match {

      // Fusion for loops remaps loop index and uses one combined scope for all bodies
      case TP(sym, LoopOrReflectedLoop(loop, effects)) => 
        val (innerScope, checkIndex) = current.getLoopSet(sym) match {

          // 1. CanBeFused already registered in existing horizontal transformer set
          case Some(horizontal) => 
            printlog("(HFT) Fusing " + sym + " with containing fusion set " + horizontal)
            assert(loop.size == horizontal.shape, "Error: HFT with different shapes")
            val checkIndex = fuse(sym, loop.v, horizontal.index)
            (horizontal.innerScope, checkIndex)

          // 2. Get CanBeFused set (maybe singleton if no previous transformers fused it)
          case None => 
            val (setToFuse, setToFuseEffectful) = 
                getFusedSyms(loop, effects.isDefined).getOrElse((List(sym), effects.isDefined))
            val existing = current.getByShape(loop.size)
              .filter({ candidate => checkIndep(sym, candidate, setToFuse) })
              .filter({ candidate => checkEffects(sym, candidate, setToFuse, setToFuseEffectful) })
              .headOption
            existing match {
              // 2.a) fuse set with existing HFT set
              case Some(fusedLoopSet) =>
                printlog("(HFT) Fusing " + sym + " with fusion set " + fusedLoopSet)
                assert(loop.size == fusedLoopSet.shape, "Error: HFT with different shapes 2")
                val checkIndex = fuse(sym, loop.v, fusedLoopSet.index)
                (current.recordAddLoop(fusedLoopSet, setToFuse, setToFuseEffectful), checkIndex)

              // 2.b) start new HFT set
              case None =>
                printdbg("(HFT) Recording " + sym + ", no fusion")
                val checkIndex = remapIndexIfFixedLength(sym, loop)
                checkIndex.foreach({ sym: Sym[Int] => printdbg("(HFT) - remapping index to unique: " + loop.v + " -> " + sym) })
                (current.recordNewLoop(loop.size, checkIndex.getOrElse(loop.v), setToFuse, setToFuseEffectful), checkIndex)
            }
        }
        
        // 3. Fusion: set correct inner scope for reflecting body
        AllFusionScopes.set(blocks(loop), innerScope)
        val superTransformedStm = super.transformStm(stm)
        AllFusionScopes.remove(blocks(loop))

        // TODO make log warnings?
        checkIndex match {
          case Some(index) =>
            if (superTransformedStm == sym)
              sys.error("(HFT) ERROR: loop index remapping was not successful, aborting fusion of " + stm + ", mirroring returned the same loop: " + superTransformedStm)
            else {
              superTransformedStm match {
                case Def(LoopOrReflectedLoop(loop, _)) =>
                  if (loop.v != index) {
                    sys.error("(HFT) ERROR: loop index remapping to " + index + " was not successful, aborting fusion of " + stm + ", mirroring returned: " + loop)
                  } else {
                    // book keeping
                    current.recordRealLoop(sym, superTransformedStm)
                  }
                case _ => sys.error("(HFT) ERROR: loop index remapping was not successful, aborting fusion of " + stm +
                  ", mirroring returned something that isn't a loop: " + superTransformedStm + " = " + findDefinition(superTransformedStm.asInstanceOf[Sym[Any]]))
              }
            }
          case None => current.recordRealLoop(sym, superTransformedStm)
        }

        // don't want to change other indices, TODO reset to old (see fuse)
        subst -= loop.v
        if (superTransformedStm != sym)
          printdbg("(HFT) - new loop symbol: " + sym + " -> " + superTransformedStm)
        Some(superTransformedStm)

      // Fusion for ifs doesn't remap anything, but combines the inner scopes per branch
      case TP(sym, IfOrReflectedIf(ifthenelse, effects)) => 

        val (thenInnerScope, elseInnerScope) = current.getIfSet(sym) match {
            
          // 1. CanBeFused already registered in existing horizontal transformer set
          case Some(horizontal) =>
            assert(ifthenelse.cond == horizontal.cond, "Error: HFT found ifs with different conds")
            printlog("(HFT) Fusing " + sym + " with containing fusion set " + horizontal)
            (horizontal.thenInnerScope, horizontal.elseInnerScope)

          // 2. Get CanBeFused set (maybe singleton if no previous transformers fused it)
          case None => 
            val (setToFuse, setToFuseEffectful) =  
                getFusedSyms(ifthenelse, effects.isDefined).getOrElse((List(sym), effects.isDefined))

            val existing = current.getByCond(ifthenelse.cond)
              .filter({ candidate => checkIndep(sym, candidate, setToFuse) })
              .filter({ candidate => checkEffects(sym, candidate, setToFuse, setToFuseEffectful) })
              .headOption
            
            existing match {
              // 2.a) fuse set with existing HFT set
              case Some(fusedIfSet) =>
                printlog("(HFT) Fusing " + sym + " with fusion set " + fusedIfSet)
                current.recordAddIf(fusedIfSet, sym, setToFuseEffectful)

              // 2.b) start new HFT set
              case None =>
                printdbg("(HFT) Recording if-sym " + sym + ", no fusion")
                current.recordNewIf(ifthenelse.cond, setToFuse, setToFuseEffectful)
            }
        }

        // 3. Fusion: set correct inner scopes for reflecting each branch
        AllFusionScopes.set(List(ifthenelse.thenp), thenInnerScope)
        AllFusionScopes.set(List(ifthenelse.elsep), elseInnerScope)
        val superTransformedStm = super.transformStm(stm)
        AllFusionScopes.remove(List(ifthenelse.thenp, ifthenelse.elsep))

        // book keeping
        current.recordRealIf(sym, superTransformedStm)
        if (superTransformedStm != sym)
          printdbg("(HFT) - new if symbol: " + sym + " -> " + superTransformedStm)
        Some(superTransformedStm)

      case _ => None
    }

    transfStm.getOrElse(super.transformStm(stm))
  }

  /** Adds a substitution from the old to the new index. */
  def fuse(sym: Sym[Any], oldIndex: Sym[Int], newIndex: Sym[Int]) = {
    if (oldIndex == newIndex) {
      printdbg("(HFT) - already using same index " + oldIndex)
      None
    } else {
      printdbg("(HFT) - remapping index: " + oldIndex + " -> " + newIndex)
      subst.get(oldIndex) match {
        case Some(`newIndex`) => // already present in subst
        
        // This should never happen, because it means that an outer loop has
        // the same index
        case Some(existingNew) => sys.error("(HFT) Error: existing remap to " + existingNew + 
            " encountered when fusing " + sym + " by remapping oldIndex " + oldIndex + 
            " to newIndex " + newIndex)
        case None => // new substitution
      }
      subst += (oldIndex -> newIndex)
      Some(newIndex)
    }
  }

  /** Returns true if the existing set and the setToFuse (containing sym)
    * are mutually independent and thus safe to fuse. */
  def checkIndep(sym: Sym[Any], existing: FusedSet, setToFuse: List[Sym[Any]]): Boolean = {
    val existingSet = existing.syms

    // traverse the statements (+fusion sets) needed for the loop
    // and check there are no deps
    // throws exception to stop traversal as soon as dep found
    case class DependencyException(dependsOn: Either[Sym[Any],Sym[Any]]) extends Exception
    try {
      // check both ways - each has to be indep of other
      GraphUtil.stronglyConnectedComponents(setToFuse, { sym: Sym[Any] => 
        findDefinition(sym) match {
          case Some(d) => 
            val next = current.getAllFusedLoops(syms(d.rhs))
            next.find(existingSet.contains(_)).map({ t => throw DependencyException(Left(t)) })
            next
          case None => List()
        }
      })
      // this is necessary, see fusion30 test for example
      GraphUtil.stronglyConnectedComponents(existingSet, { sym: Sym[Any] => 
        findDefinition(sym) match {
          case Some(d) => 
            val next = current.getAllFusedLoops(syms(d.rhs))
            next.find(setToFuse.contains(_)).map({ t => throw DependencyException(Right(t)) })
            next
          case None => List()
        }
      })
      true
    } catch {
      case DependencyException(dependsOn) => 
        val setS = if (setToFuse.length > 1) " and its set (" + setToFuse + ")" else ""
        val msg = "(HFT) The candidate " + sym + setS + " cannot be fused with the existing " + existing + " because "
        printdbg(msg + (dependsOn match {
          case Left(existing) => "the candidate set depends on " + existing
          case Right(toFuse) => "the existing set depends on "  + toFuse
        }))
        false
    }
  }

  def checkEffects(sym: Sym[Any], existing: FusedSet, setToFuse: List[Sym[Any]],
      setToFuseEffectful: Boolean): Boolean = {
    // TODO what about order of effects between loops?
    val effectsOk = !(setToFuseEffectful && existing.hasEffects) 
    if (!effectsOk) {
      val setS = if (setToFuse.length > 1) " and its set (" + setToFuse + ")" else ""
      printdbg("(HFT) The candidate " + sym + setS + " cannot be fused with the existing " + existing + " because both are effectful.")
    }
    effectsOk
  }
}