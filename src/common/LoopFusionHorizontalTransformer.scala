package scala.virtualization.lms
package common

import util.GraphUtil
import scala.collection.mutable.{HashMap, HashSet}

// TODO document interface
trait LoopFusionHorizontalTransformer extends PreservingForwardTransformer { 
  val IR: LoopFusionCore2
  import IR.{__newVar => _, _}

  /** Sets of vertically fused loops must be horizontally fused
    * if they have survived DCE, otherwise we're duplicating work. */
  def setVerticallyFusedSyms(t: (HashMap[Sym[Any], Int], List[List[Sym[Any]]])) = 
    VerticallyFusedSyms.setFusedSyms(t._1, t._2)
  object VerticallyFusedSyms {
    private var fusedSyms: HashMap[Sym[Any], Int] = HashMap[Sym[Any], Int]()
    private var fusedSymsSets: List[List[Sym[Any]]] = Nil

    def setFusedSyms(fuSyms: HashMap[Sym[Any], Int], fuSymsSets: List[List[Sym[Any]]]) = {
      fusedSyms = fuSyms
      fusedSymsSets = fuSymsSets
    }
    def getSet(sym: Sym[Any]): Option[List[Sym[Any]]] = {
      fusedSyms.get(sym)
        .map({ index => fusedSymsSets(fusedSymsSets.length - 1 - index) })
        .flatMap({ set => if (set.size == 1) None else Some(set) })
    }
  }

  /** A set of horizontally fused loops.
    * @param shape common shape @param index common index
    * @param syms loop symbols that need to be a part of the set (also loops
    * that haven't been processed yet, but are vertically fused with one of the
    * loops in the set)
    * @param setIndex index of this fusedSet in the FusionScope instance owning it
    * @param innerScope the FusionScope instance used for fusing all inner scopes of the
    * loops in the set. Need to use same instance for each so that we can fuse
    * across (they're different scopes in this schedule, but will be in the
    * same scope of the fat fused loop)
    */
  case class FusedSet(shape: Exp[Int], index: Sym[Int], syms: List[Sym[Any]], setIndex: Int, innerScope: FusionScope) {
    def addSyms(newSyms: List[Sym[Any]]) = {
      FusedSet(shape, index, syms ++ newSyms, setIndex, innerScope)
    }
    override def toString = "FusedSet(shape = " + shape + ", indexSym = " + index + ", loopSyms = " + syms + ")"
  }

  /** Fusion sets for a particular fusion scope (can be a combination of several inner
    * scopes if the outer loops have been fused). */
  class FusionScope {
    AllFusionScopes.add(this)
    // All syms here are original syms
    private var sets: List[FusedSet] = Nil
    // Since we want to prepend new sets but still have stable indices, index from back
    private def get(setIndex: Int) = sets(sets.length - 1 - setIndex)

    // Lookup mandatory fusion set by symbol (sym is vertically fused with a loop in the set)
    private val sym2set = new HashMap[Sym[Any], Int]
    // Lookup fusion set candidates by shape, need to check independence
    private val shape2sets = new HashMap[Exp[Int], List[Int]]

    // realSets(i) contains the symbols from sets(i) that were actually fused (not DCE'd)
    // this are new (transformed) syms
    private var realSets: List[List[Sym[Any]]] = Nil

    def contains(sym: Sym[Any]): Boolean = sym2set.contains(sym)
    def apply(sym: Sym[Any]): FusedSet = get(sym2set(sym))
    def get(sym: Sym[Any]): Option[FusedSet] = sym2set.get(sym).map(get(_))
    def getByShape(shape: Exp[Int]): List[FusedSet] = shape2sets.get(shape).getOrElse(Nil).map(get(_))
    
    // Start a new fusion set
    def recordNew(sym: Sym[Any], shape: Exp[Int], index: Sym[Int], syms: List[Sym[Any]]) = {
      val setIndex = sets.length
      val set = FusedSet(shape, index, syms, setIndex, new FusionScope)
      sets = set :: sets
      realSets ::= Nil
      val indexList = setIndex :: shape2sets.get(set.shape).getOrElse(Nil)
      shape2sets.put(set.shape, indexList)
      set.syms.foreach({ otherSym => sym2set.put(otherSym, setIndex) match {
        case Some(old) => sys.error("FusedSet already had a set for symbol " + otherSym + ": " + old + " = " 
          + get(old) + " instead of new " + set)
        case None =>
      }})
      set.innerScope
    }

    // Add the syms to the existing fusion set
    def recordAdd(fusedSet: FusedSet, addedSyms: List[Sym[Any]]) = {
      val setIndex = fusedSet.setIndex
      addedSyms.foreach(sym2set.put(_, setIndex))
      sets = sets.updated(sets.length - 1 - setIndex, fusedSet.addSyms(addedSyms))
      get(setIndex).innerScope
    }

    // Record actual fusion resulting in the newSym transformed loop
    def recordReal(sym: Sym[Any], newSym: Exp[Any]): Unit = {
      val setIndex = sym2set(sym)
      val substSym = newSym match { case s@Sym(_) => s case _ => sym }
      val listIndex = realSets.length - 1 - setIndex
      realSets = realSets.updated(listIndex, substSym :: realSets(listIndex))
    }
    def getReal = realSets.filter(_.length > 1).map(_.reverse)

    override def toString() = "FusionScope(" + sets.mkString("\n") + ")"
  }

  /** Records all fusion scopes and loads correct scope for
    * reflecting inner blocks. */
  object AllFusionScopes {
    private var allFusionScope: List[FusionScope] = Nil
    def add(f: FusionScope) = allFusionScope ::= f
    def get: List[List[Sym[Any]]] = allFusionScope.flatMap(_.getReal)

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

  var current = new FusionScope

  // Set correct current fusion scope
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    val old = current
    current = AllFusionScopes.get(block)
    val res = super.reflectBlock(block)
    current = old
    res
  }

  /* The transformer: fusion sets preference is:
   * 1. check if loop contained in existing (because it was vertically fused with existing)
   * 2. check if there's an existing set with correct shape and no dependencies
   * 3. start a new fusion set
   */
  override def transformStm(stm: Stm): Exp[Any] = {
    val transfStm = stm match {
      case TP(sym, loop: AbstractLoop[_]) => 
        // fuse with existing set if one found, otherwise start new set
        // fusion just means remapping the loop index to the index used by the set
        // calculate innerScope to be used for transforming the loop body
        val innerScope = current.get(sym) match {
          case Some(horizontal) => // case 1. loop contained in existing
            printlog("(HFT) Fusing " + sym + " with containing fusion set " + horizontal)
            assert(loop.size == horizontal.shape, "Error: HFT with different shapes")
            fuse(sym, loop.v, horizontal.index)
            horizontal.innerScope

          case None => 
            val setToFuse = VerticallyFusedSyms.getSet(sym).getOrElse(List(sym))
            val existing = current.getByShape(loop.size)
              .filter({ candidate => checkIndep(sym, candidate, setToFuse) })
              .headOption
            existing match {
              case Some(fusedSet) => // case 2. compatible existing set
                printlog("(HFT) Fusing " + sym + " with fusion set " + fusedSet)
                assert(loop.size == fusedSet.shape, "Error: HFT with different shapes 2")
                fuse(sym, loop.v, fusedSet.index)
                current.recordAdd(fusedSet, setToFuse)

              case None => // case 3. start a new fusion set
                printdbg("(HFT) Recording " + sym + ", no fusion")
                current.recordNew(sym, loop.size, loop.v, setToFuse)
            }
        }
        
        // Do the actual transformation with the correct innerScopes
        // for reflecting the loop body
        AllFusionScopes.set(blocks(loop), innerScope)
        val superTransformedStm = super.transformStm(stm)
        AllFusionScopes.remove(blocks(loop))

        // book keeping
        current.recordReal(sym, superTransformedStm)
        // don't want to change other indices, TODO reset to old (see fuse)
        subst -= loop.v

        if (superTransformedStm != sym) {
          printdbg("(HFT) - new loop symbol: " + sym + " -> " + superTransformedStm)
        }

        Some(superTransformedStm)

      case _ => None
    }

    transfStm.getOrElse(super.transformStm(stm))
  }

  /** Adds a substitution from the old to the new index. */
  def fuse(sym: Sym[Any], oldIndex: Sym[Int], newIndex: Sym[Int]) = {
    if (oldIndex == newIndex) {
      printdbg("(HFT) - already using same index " + oldIndex)   
    } else {
      printdbg("(HFT) - remapping index: " + oldIndex + " -> " + newIndex)
      subst.get(oldIndex) match {
        case Some(`newIndex`) => // already present in subst
        
        // TODO once we implement multiple potential producers mapped to same index
        // we should return the existing index so it can be reset after we've
        // transformed this loop
        case Some(existingNew) => sys.error("(HFT) Error: existing remap to " + existingNew + 
            " encountered when fusing " + sym + " by remapping oldIndex " + oldIndex + 
            " to newIndex " + newIndex)
        case None => // new substitution
      }
      subst += (oldIndex -> newIndex)
    }
  }

  /** Returns true if the existing set and the setToFuse (containing sym)
    * are mutually independent and thus safe to fuse. */
  def checkIndep(sym: Sym[Any], existing: FusedSet, setToFuse: List[Sym[Any]]): Boolean = {
    // TODO also check effects and respect order of effects between loops
    val existingSet = existing.syms

    // traverse the statements needed for the loop and check there are no deps
    // throws exception to stop traversal as soon as dep found
    case class DependencyException(dependsOn: Either[Sym[Any],Sym[Any]]) extends Exception
    try {
      // check both ways - each has to be indep of other
      GraphUtil.stronglyConnectedComponents(setToFuse, { sym: Sym[Any] => 
        findDefinition(sym) match {
          case Some(d) => 
            val next = syms(d.rhs)
            val taboo = next.collectFirst({ case x if existingSet.contains(x) => x })
            if (taboo.isDefined) {
              throw DependencyException(Left(taboo.get))
            }
            next
          case None => List()
        }
      })
      // this is necessary, see fusion30 test for example
      GraphUtil.stronglyConnectedComponents(existingSet, { sym: Sym[Any] => 
        findDefinition(sym) match {
          case Some(d) => 
            val next = syms(d.rhs)
            val taboo = next.collectFirst({ case x if setToFuse.contains(x) => x })
            if (taboo.isDefined) {
              throw DependencyException(Right(taboo.get))
            }
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
}