package scala.virtualization.lms
package common

import util.GraphUtil
import scala.collection.mutable.{HashMap, HashSet}


trait LoopFusionVerticalTransformer extends PreservingFixpointTransformer { 

  /* General comments:
   * - The name prefixes p and c are used for producer and consumer entities
   * - Exact scope means the innermost outer scope, so two loops are only in the
   *   same exact scope when the innermost scope that contains them is the same
   * - The shape of a loop is the number of iterations, the length is the length
   *   of the output collection. So shape can be thought of as the input size
   *   and length as output size, or shape is the size of the function domain and
   *   length is the size of the function range/image. For a map, shape = range.
   *   For a filter, shape >= range. For a flatmap, it could be >, = or <.
   */

  val IR: LoopFusionCore
  import IR.{__newVar => _, _}

  // ----- global datastructures -----

  /** Debugging flag to print all information of the transformer. */
  val printAllTransformations = false

  /** One object to record all fusion decisions. */
  object FusedSyms {

    /** A set of fused loops, at most one effectful. */
    class FSet(private var effectful: Boolean, set: HashSet[Sym[Any]]) {
      def add(s: Sym[Any]) = set.add(s)
      def addEffect(): Unit = { effectful = true }
      def foreach(f: Sym[Any] => Unit) = set.foreach(f)
      def getSet = set.toSet
      def getSubstList(map: Map[Exp[Any], Exp[Any]]) = {
        set.map({ sym => getSubstSym(sym, map) }).toList
      }
      def getEffects = effectful
      override def toString = "FSet(set = " + set + ", effectful = " + effectful + ")"
    }

    private val fusedSyms = new HashMap[Sym[Any], FSet]
    /** Keeps track of pre- and post-transformation expressions (usually symbols). */
    private val fusedSubst = new HashMap[Exp[Any], Exp[Any]]
    
    /** Records symbols that have been transformed even though they weren't
      * fused, for example when the loop body has seen transformations or the
      * loop size is replaced with a constant. */
    def recordRemapped(oldSym: Sym[Any], newExp: Exp[Any]) = newExp match { 
      case newSym@Sym(_) => 
        fusedSubst.put(oldSym, newSym)
        fusedSyms.get(oldSym).map({ set =>
          fusedSyms.put(newSym, set)
          set.add(newSym)
        })  
      case _ =>
    }

    /** Records fusion between one or several producers and a consumer, merges
      * their respective FSets and updates CanBeFused information. */
    def recordFused(pSym: Sym[Any], cSym: Sym[Any], newCSym: Sym[Any], otherProds: List[OldNewSyms],
        effectful: Boolean) = {
      val set = fusedSyms.get(pSym) match {
        case Some(fset) => if (effectful) fset.addEffect(); fset
        case None =>
          val fset = new FSet(effectful, HashSet(pSym))
          fusedSyms.put(pSym, fset)
          fset
      }
      set.add(cSym); set.add(newCSym)
      fusedSyms.put(cSym, set); fusedSyms.put(newCSym, set)
      fusedSubst.put(cSym, newCSym)
      newCSym match { case s@Sym(_) => fusedSyms.put(s, set) case _ => }
      otherProds.foreach({ prod => 
        ((fusedSyms.get(prod._2) match {
          case Some(`set`) => Set()
          case Some(pSet) => pSet.getSet
          case None => Set()
        }) ++ Set(prod._1, prod._2)).foreach({ pSym =>
          set.add(pSym)
          fusedSyms.put(pSym, set)
        })
      })
      // Update CanBeFused information for next transformers
      pSym match {
        case Def(EatReflect(pCBF: CanBeFused)) => cSym match {
          case Def(EatReflect(cCBF: CanBeFused)) => 
            pCBF.registerFusion(cCBF)
            def addToProd(s: Sym[Any]) = s match {
              case Def(EatReflect(c: CanBeFused)) => pCBF.registerFusion(c)
              case _ => printdbg("(VFT) warning: cannot register as fused because " + s + " isn't a CanBeFused: " + s + " = " + findDefinition(s))
            }
            val (oldOther, newOther) = otherProds.unzip
            oldOther.foreach(addToProd)
            newOther.foreach(addToProd)
            addToProd(newCSym)
                        
          case _ => sys.error("(VFT) Error when recording vertical fusion: consumer isn't CanBeFused: " + cSym + " = " + findDefinition(cSym))
        }
        case _ => sys.error("(VFT) Error when recording vertical fusion: producer isn't CanBeFused: " + pSym + " = " + findDefinition(pSym))
      }
    }

    /** Returns the fusion set of a symbol if it exists. */
    def getSet(pSym: Sym[Any]): Option[FSet] = fusedSyms.get(pSym)

    /** Returns all the fused symbols with the set they belong to and a boolean
      * to indicate if the set is effectful. This is a big map to be used for
      * debugging purposes only. */
    def getFusedSyms(): HashMap[Sym[Any], (List[Sym[Any]], Boolean)] = {
      val fusedSubstMap = fusedSubst.toMap
      val newMap = new HashMap[Sym[Any], (List[Sym[Any]], Boolean)]()
      val setSet: Set[FSet] = fusedSyms.values.toSet

      setSet.map({ fset => 
          (fset.getSubstList(fusedSubstMap), fset.getEffects)
        }).filter( _._1.length > 1 ).foreach({ symList =>
          symList._1.foreach({ sym => newMap.put(sym, symList) })
        })
      newMap
    }
  }

  // extension to the ForwardTransformer: only substitute the given expression once
  var onceSubst = scala.collection.immutable.Map.empty[Exp[Any], Exp[Any]]
  override def apply[A](x: Exp[A]): Exp[A] = onceSubst.get(x) match { 
    case Some(y) if y != x => onceSubst -= x; apply(y.asInstanceOf[Exp[A]])
    case _ => super.apply(x)
  }

  // loops with fixed lengths, collection.length will be replaced with the expression
  val loopsToFixedOutLengths = new HashMap[Sym[Any], Exp[Int]]
  def recordFixedOutLengths(sym: Sym[Any], loop: AbstractLoop[_]): Option[Exp[Int]] = {
    val replacement: Option[Exp[Int]] = loop match {
      case FixedDomain(Const(len)) => Some(Const(len))
      case FixedDomain(domainSym: Sym[Int @unchecked]) => subst.get(domainSym) match {
        case Some(Const(len: Int))                          => Some(Const(len))
        case Some(sSym: Sym[Int @unchecked]) if (!isWritableSym(sSym)) => Some(sSym)
        case _ if (!isWritableSym(domainSym))               => Some(domainSym)
        case _                                              => None
      }
      case _ => None
    }
    replacement.foreach({ rep => loopsToFixedOutLengths += (sym -> rep) })
    replacement
  }

  /* SimpleIndex replacements:
   * The indexing replacements of the form consumer.at(index) -> exp cannot be
   * entered into the transformer's substitution map at the time when the
   * consumer loop is transformed (it makes the transformer fail with a
   * duplicate substitution error when it gets to the indexing statement).
   * So just register replacements in this map and look them up when the
   * transformer reaches the statement. */

  type SISyms = (Sym[Any], Sym[Int])
  val simpleIndexReplacements = new HashMap[SISyms, Exp[Any]]
  def addSimpleIndexReplacement(siSyms: SISyms, exp: Exp[Any]) =
    simpleIndexReplacements += (siSyms -> getBlockResult(Block(exp)))
  def removeSimpleIndexReplacement(cSym: Sym[Any], indexSym: Sym[Int]) =
    simpleIndexReplacements -= ((cSym, indexSym))
  def getSimpleIndexReplacements(cSym: Sym[Any], indexSym: Sym[Int]) =
    simpleIndexReplacements.get((cSym, indexSym))


  // ----- per scope datastructures -----
  
  // indented printing to show scopes
  var indent = -2
  def printdbg(x: => Any) { if (verbosity >= 2) System.err.println(" " * indent + x) }
  def printlog(x: => Any) { if (verbosity >= 1) System.err.println(" " * indent + x) }

  /** List of the loops in the same exact scope (level&scope)
    * e.g. l1 = for (...) { l2 = for (...) {...}; l3 = for (...) {...} }
    * l2 and l3 are in the same scope and level, but l1 and l2 are not because
    * even though they share a common outer scope, their innermost outer scopes
    * aren't the same. */
  var seenLoops = HashSet[Sym[Any]]()
  
  /** All unfused loops with fixed length of the output collection in same
    * exact scope have same index so they can be fused as multiple producers
    * with one consumer. The HashMap is created lazily if really needed in
    * current scope. */
  var fixedLengthIndex: Option[HashMap[Exp[Int], Sym[Int]]] = None
  def remapIndexIfFixedLength(shape: Option[Exp[Int]], oldIndex: Sym[Int]): Boolean = shape match {
    case Some(shape) => 
      if (!fixedLengthIndex.isDefined)
        fixedLengthIndex = Some(new HashMap[Exp[Int], Sym[Int]])
      fixedLengthIndex.get.get(shape) match {
        case Some(`oldIndex`) | None => fixedLengthIndex.get += (shape -> oldIndex); false
        case Some(newIndex) => subst += (oldIndex -> newIndex); true
      }
    case None => false
  }


  // ----- fusion information classes -----

  /** Fusion distinguishes between three relationships that can connect a
    * consumer to a producer. In all cases the consumer accesses the producer
    * only at the current index. But the shape of the consumer loop can refer
    * more or less directly to the producer: 
    * - RealProd: shape of consumer loop is prod.length
    * - ReconstrProd: shape of consumer is an expression/constant that equals
    *   the length of the producer output collection
    * - PartialProd: cannot conclude any of the above, so the consumer might
    *   only iterate over part of the producer collection
    *
    * In case there are several producers, the (unique if any) real producer is
    * prioritized, and then possible reconstructed producers are taken into
    * account. Partial producers currently aren't fused at all, would need
    * callback or flags for programmer to ask for it (may cause problems with
    * horizontal fusion, change exception behavior, only partially evaluate the
    * producer etc.)
    */
  sealed abstract class ProdType { val intVal: Int; val shortName: String }
  case class RealProd extends ProdType { val intVal = 1; val shortName = "real" }
  case class ReconstrProd extends ProdType { val intVal = 2; val shortName = "reconstructed" }
  case class PartialProd extends ProdType { val intVal = 3; val shortName = "partial" }


  /* The following are the supported producers:
   * - Empty, Singleton(x), IfThenElse, MultiCollect (=FlatMap)
   * The following are the supported consumers:
   * - MultiCollect (=FlatMap), For/Foreach, Reduce
   *
   * But not all combinations can be fused. For each consumer, the transformer
   * computes its FusionInfo, which is represented in the following hierarchy:
   *
   * FusionInfo: abstract super class
   * |- FusionInfo1to1: 1 producer and current consumer fused
   *    |- Empty_Mc
   *    |- Empty_For
   *    xx Emtpy_Red  xx  cannot be fused: behavior depends on implementation
   *    |- Single_McFor
   *    xx Single_Red xx  cannot be fused: behavior depends on implementation
   *    |- IfThenElseOneEmpty_Any: only fused if at least one branch is Empty
   *    xx IfThenElse_Any xx   otherwise not fused, results in code explosion
   *    |- Mc_McForRed
   *    |- Mcsingle_McFor (extractor only, matches map=MultiCollect(Singleton))
   *    |- InnerMc_Red: reduce consumer needs special treatment in recursion
   * |- FusionInfoManyto1: many producers and current consumer fused
   *    |- ManyMcsingle_McFor: only map producers fused as multiples
   * |- NoFusionInfo: no fusion possible for current consumer
   */

  sealed abstract class FusionInfo(cSym: Sym[Any], notFused: List[(Sym[Any], String)]) {
    def printLogBefore: Unit
    def printLogAfter(result: FusionOutcome): Unit
    def getProds: List[Sym[Any]]
    def setNotFused(list: List[(Sym[Any], String)])
  }

  // FusionInfo super classes with extractor objects

  object FusionInfo1to1 {
    def unapply(o: Any): Option[(Sym[Any], Sym[Any], List[(Sym[Any], String)])] = o match {
      case x: FusionInfo1to1 => Some((x.pSym, x.cSym, x.notFused))
      case _ => None
    } 
  }
  sealed abstract class FusionInfo1to1(val pSym: Sym[Any], val cSym: Sym[Any], var notFused: List[(Sym[Any], String)])
      extends FusionInfo(cSym, notFused) {
    override def printLogBefore = {
      notFused.foreach({ case (pSym, reason) => printlog("(VFT) Not fused prod " + pSym + " with cons " + cSym + " because " + reason) })
      printlog("(VFT) Fusing prod " + pSym + " with cons " + cSym + ". Type: " + this)
    }
    override def printLogAfter(result: FusionOutcome) = (result: @unchecked) match {
      case FusionResult(fusedSym) => printlog("(VFT) Fused prod " + pSym + " with cons " + cSym + ", fused sym: " + fusedSym)
    }
    override def getProds = List(pSym)
    override def setNotFused(list: List[(Sym[Any], String)]) = notFused = list
  }

  object FusionInfoManyto1 {
    def unapply(o: Any): Option[(Sym[Any], List[Sym[Any]], Sym[Any], List[(Sym[Any], String)])] = o match {
      case x: FusionInfoManyto1 => Some((x.pSym, x.otherProds, x.cSym, x.notFused))
      case _ => None
    } 
  }
  sealed abstract class FusionInfoManyto1(val pSym: Sym[Any], val otherProds: List[Sym[Any]], val cSym: Sym[Any],
      val notFused: List[(Sym[Any], String)]) extends FusionInfo(cSym, notFused) {
    override def printLogBefore = {
      notFused.foreach({ case (pSym, reason) => printlog("(VFT) Not fused prod " + pSym + " with cons " + cSym + " because " + reason) })
      printlog("(VFT) Fusing prod " + pSym + " with cons " + cSym + " and other prods: " + otherProds + ". Type: " + this)
    }
    override def printLogAfter(result: FusionOutcome) = (result: @unchecked) match {
      case FusionResult(fusedSym) => printlog("(VFT) Fused prod " + pSym + " and other prods " + otherProds + " with cons " + cSym + ", fused sym: " + fusedSym)
    }
    override def getProds = pSym :: otherProds
    override def setNotFused(list: List[(Sym[Any], String)]) = sys.error("(VFT) Error: FusionInfoManyto1.notFused is not mutable")
  }

  case class NoFusionInfo(cSym: Sym[Any], cLoop: Either[Def[Any],AbstractLoop[_]], notFused: List[(Sym[Any], String)])
      extends FusionInfo(cSym, notFused) {
    override def printLogBefore = {
      notFused.foreach({ case (pSym, reason) => printlog("(VFT) Not fused prod " + pSym + " with cons " + cSym + " because " + reason) })
      printlog("(VFT) No producers found for cons " + cSym)
    }
    override def printLogAfter(result: FusionOutcome) = (result: @unchecked) match {
      case NoFusionRemapped(remappedSym, remapReason) =>
        if (remappedSym != cSym)
          printlog("(VFT) Cons " + cSym + " not fused but mirrored to " + remappedSym
            + " because " + remapReason)
    }
    override def getProds = Nil
    override def setNotFused(list: List[(Sym[Any], String)]) = sys.error("(VFT) Error: NoFusionInfo.notFused is not mutable")
  }

  // concrete FusionInfo classes

  case class Empty_Mc(empty: Exp[Any],
    override val pSym: Sym[Any], override val cSym: Sym[Any]) extends FusionInfo1to1(pSym, cSym, Nil)

  case class Empty_For(override val pSym: Sym[Any], override val cSym: Sym[Any]) 
    extends FusionInfo1to1(pSym, cSym, Nil)

  case class Single_McFor(cIndex: Sym[Int], siSyms: SISyms, pInner: Exp[Any], cInner: Exp[Any],
      override val pSym: Sym[Any], override val cSym: Sym[Any]) extends FusionInfo1to1(pSym, cSym, Nil)
  
  /** IfThenElse producers are only fused if one branch is the empty collection
    * and disappears when fusing with the consumer, otherwise pushing the
    * consumer into both branches causes code duplication and potentially
    * explosion (but better runtime). In future work this could be done if
    * explicitly requested through annotations, callbacks or autotuner/debugger
    * hooks. */
  case class IfThenElseOneEmpty_Any(cond: Exp[Boolean], ifFusionInfo: FusionInfo, 
      elseFusionInfo: FusionInfo, typeSym: Sym[Any],
    override val pSym: Sym[Any], override val cSym: Sym[Any]) extends FusionInfo1to1(pSym, cSym, Nil)

  case class Mc_McForRed(cIndex: Sym[Int], pIndex: Sym[Int],
      cShape: Exp[Int], pShape: Exp[Int], innerFusionInfo: FusionInfo, 
      cInner: Exp[Any], pInner: Exp[Any],
      override val pSym: Sym[Any], override val cSym: Sym[Any]) 
    extends FusionInfo1to1(pSym, cSym, Nil)

  /** Extractor to match on producers that could be fused as one of multiple
    * producers. */
  object Mcsingle_McFor {
    def unapply(a: FusionInfo) = a match {
      case Mc_McForRed(cIndex, pIndex,_,_, Single_McFor(_,siSyms,singletonInner,_,_,_), _, _, pSym, cSym) =>
        Some((cIndex, pIndex, siSyms, singletonInner, pSym, cSym))
      case _ => None
    } 
  }

  /** Special case for the recursive inner fusion of MultiCollect and Reduce */
  case class InnerMc_Red(cIndex: Sym[Int], pIndex: Sym[Int],
      innerFusionInfo: FusionInfo, cInner: Sym[Any], pInner: Exp[Any], pLoop: Def[Any],
      override val pSym: Sym[Any], override val cSym: Sym[Any])
    extends FusionInfo1to1(pSym, cSym, Nil)

  /** Multiple producers can only be fused if they are MultiCollect(singleton),
    * aka. map nodes. They can be fused because their index variables have been
    * remapped to be the same since they have the same output length. */
  case class ManyMcsingle_McFor(cIndex: Sym[Int], pIndex: Sym[Int],
      val siSyms: SISyms, pInner: Exp[Any], val oProds: List[(SISyms, Exp[Any])], 
      override val pSym: Sym[Any], override val cSym: Sym[Any],
      override val notFused: List[(Sym[Any], String)])
    extends FusionInfoManyto1(pSym, oProds.unzip._1.unzip._1, cSym, notFused)


  // ----- fusion result classes -----

  /** FusionOutcomes contain the transformed expression for bookkeeping in
    * transformLoop and doFusion. They are also passed back to the FusionInfo
    * classes for logging. */
  sealed abstract class FusionOutcome(val transformed: Exp[Any])
  /** The consumer was transformed to the given fused loop. */
  case class FusionResult(fusedSym: Exp[Any]) extends FusionOutcome(fusedSym)
  /** The consumer wasn't fused, but still remapped to a new expression for the
    * given reason. */
  case class NoFusionRemapped(remappedSym: Exp[Any], remapReason: String) extends FusionOutcome(remappedSym)


  // ----- transformer methods -----

  // FixPointTransformer methods
  // Future work: run vertical fusion as fixpoint, for now only run once
  def getInfoString = "LoopFusionVerticalTransformer only runs once, TODO fixpoint"
  var hasRunOnce = false
  def isDone = hasRunOnce
  def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    val newBlock = if (shouldDoFusion) transformBlock(s) else s
    hasRunOnce = true
    newBlock
  }

  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    // save per scope datastructures
    val saveSeenLoops = seenLoops
    seenLoops = HashSet[Sym[Any]]()
    val saveFixedLengthIndex = fixedLengthIndex
    fixedLengthIndex = None
    indent += 2

    // check whether toplevel block result symbol
    // needs substitution, so we can change full blocks
    val blockRes = getBlockResultFull(block)
    val res = apply(blockRes) match {
      case `blockRes` => super.reflectBlock(block)
      case newBlock => newBlock
    }

    // restore per scope datastructures
    indent -= 2
    seenLoops = saveSeenLoops
    fixedLengthIndex = saveFixedLengthIndex
    res
  }

  override def transformStm(stm: Stm): Exp[Any] = { 
    if (printAllTransformations) println("--transforming: " + stmShort(stm))
    
    val transformedSym = stm match {

      /* Currently already fused loops aren't considered for vertical fusion.
       * But this is a prerequisite for running the vertical transformer in a 
       * fixpoint fashion. TODO: think about which cases are safe to fuse:
       * - as producer: just need to keep track of producer set, seems ok?
       * - as consumer: tricky when fused loop has different shape, which would
       *   mean removing it from CBF set... What takes precedence?
       * NOTE: We don't need to keep track of combined scopes (bodies of two
       * pre-fused loops that will become one body of the fat TTP) because it's
       * not possible for a producer to be in one scope and the consumer in the
       * parallel one, since the name of the producer would be out of scope for
       * the consumer. */
      case TP(sym, EatReflect(cbf: CanBeFused)) if cbf.getFusedSetID.isDefined =>
        printdbg("(VFT) Not considering " + stm + " because it's already fused, CanBeFused.setID=" + cbf.getFusedSetID.get)
        super.transformStm(stm)
     
      /* SimpleIndex statement substitutions cannot be entered when the
       * containing consumer loop is analyzed, otherwise we get a duplicate
       * substitution error here. Therefore look it up in a separate map. */
      case TP(_, SimpleIndex(pSym@Sym(_), cIndex@Sym(_))) => subst.get(pSym)
        .flatMap({ case newPSym@Sym(_) => getSimpleIndexReplacements(newPSym, cIndex) })
        .orElse(getSimpleIndexReplacements(pSym, cIndex))
        .getOrElse(super.transformStm(stm))

      /* The transformer replaces expressions of the type collection.length by
       * their constant values if possible. It's better to do this
       * optimization in the transformer rather than the DSL, because it gives
       * the transformer information about which was the real producer in the
       * expression written by the DSL user. */
      case tp@TP(_, SimpleDomain(sym@Sym(_))) =>
        loopsToFixedOutLengths.get(sym).map({ fixedSym => 
          printdbg("(VFT) Replaced " + tp + " with fixed length: " + fixedSym)
          fixedSym
        }).getOrElse(super.transformStm(stm))

      /* Transform loops - they might be a producer, consumer or both. */
      case TP(sym, LoopOrReflectedLoop(loop, effects)) =>
        transformLoop(stm, sym, loop, effects)

      /* Track other types of producers. */
      case TP(_, IfThenElse(_,_,_)) | TP(_, SingletonColl(_)) | TP(_, EmptyColl()) => 
        seenLoops += stm.lhs.head
        super.transformStm(stm)

      case _ => super.transformStm(stm)
    }

    // Debugging output
    if (printAllTransformations) {
      if (transformedSym == stm.lhs()(0)) {
        println("--identical " + transformedSym)
      } else {
        transformedSym match {
          case s@Sym(_) => 
            println("--transformed " + stmShort(stm) + " to ")
            println("----" + s + " = " + findDefinition(s))
          case c =>
            println("--transformed " + stmShort(stm) + " to " + c)
        }
      }
    }

    transformedSym
  }

  /** The loop transformation is split into two parts: 
    * - {@code findProducers}: collects all the information necessary to
    *   execute the fusion, using recursion to deconstruct the problem.
    * - {@code doFusion}: executes the fusion through mirroring, this function
    *   thus already has all the information and won't abort and roll back. */
  def transformLoop[A:Manifest, B](stm: Stm, sym: Sym[A], loop: AbstractLoop[B], 
      effects: LoopEffects): Exp[Any] = {

    printlog("")
    seenLoops += sym
    val (fusionInfo, effectful) = findProducers(sym, loop, loop.size, loop.v, loop.body, effects)
    fusionInfo.printLogBefore

    val fusionOutcome = doFusion(fusionInfo, stm, Right(true), effectful)
    fusionInfo.printLogAfter(fusionOutcome)
    findDefinitionExp(fusionOutcome.transformed) match {
      case Some(TP(newSym, LoopOrReflectedLoop((loop, _)))) => 
        if (newSym != sym) {
          seenLoops += newSym
          // Register new length for old symbol, because the transformer will
          // see sym.length and not newSym.length
          recordFixedOutLengths(sym, loop)
        }
      case _ =>
    }
    fusionOutcome.transformed
  }


  // ----- fusion functions -----

  /** Finds all possible producers for the given consumer loop. Candidates need
    * to satisfy all of the following conditions:
    * - consumer body contains access of the form candidate.at(cIndex)
    * - candidate is in same exact scope as consumer
    * - consumer goes over full range of candidate (either real or
    *   reconstructed prod type, but no partial)
    * - the consumer body can't use its index other than to access the
    *   candidate unless the candidate is a map (multicollect(singleton)),
    *   because the fused loop will use the producer index. So for a filter
    *   producer, if the first element is removed, the second element will have
    *   index 1 in the fused loop instead of index 0 for the consumer
    * - the consumer body cannot depend on the producer other than the access
    *   at the current index, this is computed by the {@code depsOK} helper
    * - at most one loop in a fusion set can be effectful, and some additional
    *   restrictions apply. This is computed by {@code effectsOk}
    *
    * The list of all candidates is then passed to {@code combineProducers} to
    * retain only the ones that are compatible if there are multiple producers.
    */
  def findProducers[A:Manifest](cSym: Sym[A], cLoop: AbstractLoop[_], cShape: Exp[Int],
      cIndex: Sym[Int], cBody: Def[A], cEffects: LoopEffects): (FusionInfo, Boolean) = {

    /** Get all the statements of the consumer loop body, which means all
     * statements that depend on the consumer loop index variable (otherwise
     * they woudn't be scheduled into the loop body). */
    val cIndexStms = getFatDependentStuff(initialDefs)(List(cIndex))

    var listOfNotFused: List[(Sym[Any], String)] = Nil
    
    val producers = cIndexStms.collect({
      case TP(_, SimpleIndex(pSym@Sym(_), `cIndex`)) => pSym 
    }).filter({ pSym => 
      if (!seenLoops.contains(pSym)) {
        listOfNotFused ::= (pSym, "not in same exact scope"); false
      } else true
    }).map({ case pSym => 
      val (prodType, msg) = consumerGoesOverRangeOfProducer(cShape, pSym)
      prodType match {
        case PartialProd() => 
          listOfNotFused ::= (pSym, "consumer loop might not iterate over full range of producer collection")
          // Future work: could have callback, but now not shape of prod, only for SingleCollects?
          // Problem when shape of consumer replaced with shape of producer...
          // Keep exceptions when iterating too far, evaluate everything when
          // iterating not far enough, can't fuse horizontally etc.
        case _ =>
      }
      (pSym, prodType)
    }).filter({ case (_, PartialProd()) => false case _ => true
    }).sortWith({ (a, b) => a._2.intVal < b._2.intVal
    }).map({ case (pSym, prodType) => (pSym, getSubstSym(pSym), prodType)
    }).filter({ case (pSym, newPSym, _) =>
      // Future work: could lift this restriction on index use by introducing a
      // mutable index variable that keeps track of the consumer index
      val indexIsTaboo = newPSym match {
        case Def(LoopOrReflectedLoop(pLoop, _)) => pLoop.body match {
          case MultiCollect(Def(SingletonColl(_))) => false
          case _ => true
        }
        case _ => true
      }
      depsOK(cSym, cIndex, indexIsTaboo, cBody, cEffects, pSym, newPSym) match {
        case None => true
        case Some(msg) => listOfNotFused ::= (pSym, msg); false
      }
    }).map({ case (pSym, newPSym, prodType) => (pSym, newPSym, prodType, effectsOk(cEffects, pSym, newPSym))
    }).filter({ case (pSym, _, _, effectful) => effectful match {
        case Left(msg) => listOfNotFused ::= (pSym, "of effects: " + msg); false
        case _ => true
      }
    }).map({ case (pSym, newPSym, prodType, effectful) =>
      (pSym, newPSym, prodType, effectful.right.get)
    })

    combineProducers(cSym, Right(cLoop), producers, listOfNotFused, None, None, None)
  }

  /** Prunes the producer candidates and creates the FusionInfo containing all
    * compatible fusions. */
  def combineProducers[A:Manifest](cSym: Sym[A], cDef: Either[Def[A], AbstractLoop[_]],
      prodCandidates: List[(Sym[Any], Sym[Any], ProdType, Boolean)],
      notFused: List[(Sym[Any], String)], outerIndex: Option[Sym[Int]],
      outerSISyms: Option[SISyms], remapReduceToMc: Option[Sym[Any]]): (FusionInfo, Boolean) = {
    
    /** Remember not fused producers & reason for debugging output. */
    var listOfNotFused: List[(Sym[Any], String)] = notFused

    // Process the different types of producers and their combinations with
    // consumers.

    def processEmptyProducer(empty: Def[Any], newPSym: Sym[Any]) = (cDef, remapReduceToMc.isDefined) match {

      // empty + Mc -> empty of consumer type
      case MatchMC(cInner, cShape, cIndex) => (empty, cInner, outerIndex) match {
          case EmptyCollNewEmpty(newEmpty) => Right(Empty_Mc(newEmpty, newPSym, cSym))
          case _ => Left("empty producer not fused with MC because no EmptyCollNewEmpty constructor found")
        }

      // empty + For/foreach -> Unit
      case (MatchLoop(For(_),_,_), _) => Right(Empty_For(newPSym, cSym))

      // empty + Reduce: don't fuse, maintain dsl specific error
      case (MatchLoop(Reduce(_),_,_), false) => 
        Left("empty producer not fused with reduce because need to maintain implementation-specific error")

      case _ => Left("empty producer not fused with unknown consumer (have you overridden the LoopFusionExtractors?):" + cSym)
    }

    def processSingletonProducer(pInner: Exp[Any], oldPSym: Sym[Any], newPSym: Sym[Any]) = {
      (cDef, remapReduceToMc.isDefined) match {

        // singleton + MC or For: replace loop by body with singleton element
        case MatchMcFor(cInner, cShape, cIndex) =>
          val siSyms = outerSISyms.getOrElse((oldPSym, cIndex))
          Right(Single_McFor(cIndex, siSyms, pInner, cInner, newPSym, cSym))

        // singleton + Reduce: don't fuse, maintain dsl specific behavior
        case (MatchLoop(Reduce(_),_,_), false) => 
          Left("singleton producer not fused with reduce because need to maintain implementation-specific behavior")

        case _ => Left("singleton producer not fused with unknown consumer (have you overridden the LoopFusionExtractors?):" + cSym)
      }    
    }

    def processIteProducer(cond: Exp[Boolean], ifSym: Sym[Any], elseSym: Sym[Any], oldPSym: Sym[Any], newPSym: Sym[Any]) = {
      ((ifSym, elseSym) match {
        case (Def(EmptyColl()), _) => Some((true, ifSym, elseSym))
        case (_, Def(EmptyColl())) => Some((false, elseSym, ifSym))
        case _ => None
      }) match {
        case Some((emptyFirst, empty, other)) => 
          combineProducers(cSym, cDef, List((oldPSym, empty, RealProd(), false)),
              Nil, outerIndex, outerSISyms, remapReduceToMc)._1 match {
            
            case NoFusionInfo(_,_,_) => Left("if-then-else producer not fused because empty branch cannot be fused with consumer (don't want to duplicate consumer code)")
            
            case emptyFusionInfo =>
              combineProducers(cSym, cDef,
                  List((oldPSym, other, RealProd(), false)), Nil, outerIndex, outerSISyms, remapReduceToMc)._1 match {
                case NoFusionInfo(_,_,_) => Left("if-then-else producer not fused because non-empty branch cannot be fused with consumer")
                case otherFusionInfo => 
                  if (emptyFirst) Right(IfThenElseOneEmpty_Any(cond, emptyFusionInfo, otherFusionInfo, 
                    remapReduceToMc.getOrElse(cSym), newPSym, cSym))
                  else Right(IfThenElseOneEmpty_Any(cond, otherFusionInfo, emptyFusionInfo,
                    remapReduceToMc.getOrElse(cSym), newPSym, cSym))
              }
          }
        case None => Left("if-then-else producer only fused if one branch is empty collection (have you overridden the LoopFusionExtractors?)")
      }
    }

    def processMcProducer(pInner: Sym[Any], pLoop: AbstractLoop[_], oldPSym: Sym[Any], newPSym: Sym[Any]) = {
      cDef match {
        // SL(ps, pi, Mc(pinner(pi))) + SL(prod.len, ci, Mc(cinner(prod.at(ci))))
        // -> SL(ps, pi, Mc(pinner(pi) + SL(pinner.len, ci, Mc(cinner(pinner.at(ci))))))

        // SL(ps, pi, Mc(pinner(pi))) + SL(prod.len, ci, Foreach(cInner(prod.at(ci))))
        // -> SL(ps, pi, Foreach(pinner + SL(pinner.len, ci, Foreach(cInner(pinner.at(ci))))

        // SL(ps, pi, Mc(pinner(pi))) + SL(prod.len, ci, Reduce(cInner(prod.at(ci))))
        // -> SL(ps, pi, Reduce(pinner(pi) + SL(pinner.len, ci, Mc(cInner(pinner.at(ci))))))
        
        case MatchMcForReduce(cInner@Sym(_), cShape, cIndex) =>
          val nextRemapReduceToMc = cDef match {
            case MatchLoop(Reduce(_), _, _) => Some(cInner)
            case _ => None
          }

          val innerFusionInfo = combineProducers(cSym, cDef, 
            List((oldPSym, pInner, RealProd(), false)), Nil, Some(pLoop.v), Some(oldPSym, cIndex), nextRemapReduceToMc)._1
          innerFusionInfo match {
            case NoFusionInfo(_,_,_) => // TODO here use remapReduceToMc
              Left("multicollect producer's inner collection cannot be fused with consumer")
            case Empty_For(_,_) => Right(Empty_For(newPSym, cSym))
            case _ => 
              if (!remapReduceToMc.isDefined)
                Right(Mc_McForRed(cIndex, pLoop.v, cShape, pLoop.size, 
                  innerFusionInfo, cInner, pInner, newPSym, cSym))
              else
                Right(InnerMc_Red(cIndex, pLoop.v, innerFusionInfo, cInner, pInner, pLoop, newPSym, cSym))
          }

        case _ => Left("don't know how to fuse MultiCollect producer with consumer of type " + cDef)
      }
    }

    /** Check all candidates and retain all that could be fused. */
    val producers: List[(FusionInfo1to1, ProdType, Boolean)] = prodCandidates.map({ 
      case (oldPSym, newPSym, prodType, effectful) => 
        val fusionInfo: Either[String, FusionInfo1to1] = newPSym match {

          case Def(empty@EmptyColl()) => 
            processEmptyProducer(empty, newPSym)

          case Def(SingletonColl(pInner)) => 
            processSingletonProducer(pInner, oldPSym, newPSym)

          case Def(IfThenElse(cond, Block(ifSym@Sym(_)), Block(elseSym@Sym(_)))) =>
            processIteProducer(cond, ifSym, elseSym, oldPSym, newPSym)

          case Def(LoopOrReflectedLoop(pLoop, _)) => pLoop.body match {
            case MultiCollect(pInner@Sym(_)) => 
              processMcProducer(pInner, pLoop, oldPSym, newPSym)
            case _ => Left("unknown elem in loop producer (have you overridden the LoopFusionExtractors?): " + pLoop.body)
          }

          case _ => Left("unknown producer collection (have you overridden the LoopFusionExtractors?): " + newPSym)
        }
        fusionInfo match {
          case Left(msg) => listOfNotFused ::= (oldPSym, msg); None
          case Right(fusionInfo) => Some((fusionInfo, prodType, effectful))
        }
    }).collect({ case Some(tuple) => tuple })

    /** Helper function to log not-chosen producers when multiple producers are
      * available. */
    def addNotFused(notFused: List[FusionInfo], fusedProd: Sym[Any], 
        fusedPType: ProdType, details: String, fInfo: Option[FusionInfo] = None) = {
      notFused.flatMap(_.getProds).foreach({ notFusedProd =>
        val msg = "the consumer is already being fused with the " + fusedPType.shortName + 
          " producer " + fusedProd + ". Can only fuse multiple producers if all are " + 
          "MultiCollect(Singleton) and have the same fixed shape " +
          "and thus have been mirrored to use the same index symbol. " + details
        listOfNotFused ::= ((notFusedProd, msg))
      })
      fInfo.foreach(_.setNotFused(listOfNotFused))
    }

    /** Prune the list of producers if there are more than one and either
      * create a ManyMcsingle_McFor node for multiple fusion or retain one
      * node.
      * Note that the real producer (if any) always has precedence because
      * that's the fusion that the user expects.
      * Note that some fusion decisions prevent fusions further down the road.
      * However, this problem is basically graph coloring on a partially
      * ordered set, and the greedy algorithm is optimal if the traversal is
      * done in topological order. This is the case because the transformer
      * sees the nodes as sorted by the scheduler. */
    def combineMultipleProducers(mcsm: FusionInfo, firstPType: ProdType, eff: Boolean, list: List[(FusionInfo1to1, ProdType, Boolean)]) = {
      mcsm match {
        case Mcsingle_McFor(cIndex,pIndex,siSyms,pInner,newPSym,cSym) => 
          val canFuseRight = list.map({
            case (fi@Mcsingle_McFor(_,`pIndex`,osiSyms,opInner,_,_), _, otherEff) => Right((fi, (osiSyms, opInner, otherEff)))
            case other => Left(other)
          })
          addNotFused(canFuseRight.collect({ case Left(notFused) => notFused._1 }), siSyms._1, firstPType,
            "(Current producer isn't Mc(singleton) with correct index)")
          val otherProdsEffectful = canFuseRight.collect({ case Right(toFuse) => toFuse })

          val (otherProds, combinedEffect) = if (eff) {
            (otherProdsEffectful, true)
          } else {
            
            val impureIndex = otherProdsEffectful.indexWhere({ case (_,(_,_,testEff)) =>  testEff })
            if (impureIndex == -1) {
              (otherProdsEffectful, false) 
            } else {
              val impureProd = otherProdsEffectful(impureIndex)._2
              val (pureTail, impureTail) = otherProdsEffectful.drop(impureIndex + 1).partition(_._2._3)
              addNotFused(impureTail.map(_._1), siSyms._1, firstPType,
                "(Current producer is effectful, but consumer already fused with effectful prod " + impureProd._1._1 + ")")
              (otherProdsEffectful.take(impureIndex + 1) ++ pureTail, true)
            }
          }
          val otherProds2 = otherProds.map(_._2).map({ t => (t._1, t._2) })
          if (!otherProds.isEmpty)
            (ManyMcsingle_McFor(cIndex, pIndex, siSyms, pInner, otherProds2, newPSym, cSym, listOfNotFused), combinedEffect)
          else
            (mcsm, eff)
        case err => sys.error("(VFT) Error: combineMultipleProducers: first isn't Mcsingle_McFor: " + err)
          // With current taboo index restriction can only have Mcsingle_McFor
          // because of index use, for any other type of producer the consumer
          // body can't use the index to access other producers.
      }
    }

    val result: (FusionInfo, Boolean) = producers match {
      case Nil => (NoFusionInfo(cSym, cDef, listOfNotFused), false)
      
      case prod :: Nil => prod._1.notFused = listOfNotFused; (prod._1, prod._3)

      case (mcsm@Mcsingle_McFor(cIndex,pIndex,siSyms,pInner,newPSym,cSym), firstPType, eff) :: list => 
        combineMultipleProducers(mcsm, firstPType, eff, list)

      case (first, firstPType, firstEff) :: list => 
        val fusedProd: Sym[Any] = first.getProds.head
        addNotFused(list.map(_._1), fusedProd, firstPType, "(First fused producer " + fusedProd + 
          " is not a Mc(Singleton)).", Some(first))
        (first, firstEff)
    }
    // println("===combineProducers(cSym=" + cSym + ", cDef=" + cDef + ", prodCandidates=" + prodCandidates + 
    //   ", notFused=" + notFused + ", outerIndex=" + outerIndex + "/n    -> " + result)
    result
  }

  /** This function recursively executes the fusion according to the previously
    * collected FusionInfo. In many cases it just adds the necessary
    * substitutions and calls the transformer to mirror everything.
    * @param recordFusion Right(true) means record regularly (between producer
    * and consumer from fusionInfo), Right(false) means don't record fusion, 
    * Left(prod) means record fusion between given producer and the consumer. */
  def doFusion(fusionInfo: FusionInfo, cStm: Stm, recordFusion: Either[Exp[Any],Boolean], effectful: Boolean): FusionOutcome = {
    var overrideRecordFusion: Option[Either[Exp[Any],Boolean]] = None
    val fusionOutcome = fusionInfo match {
      case NoFusionInfo(sym, Right(loop), _) if (remapIndexIfFixedLength(recordFixedOutLengths(sym, loop), loop.v)) =>
          NoFusionRemapped(super.transformStm(cStm), "of fixed shape")

      case NoFusionInfo(sym, _, _) => 
        NoFusionRemapped(super.transformStm(cStm), "of previous substitutions or effects")

      case Empty_Mc(empty, _, _) => FusionResult(empty)
      case Empty_For(_,_) => FusionResult(Const(()))

      case Single_McFor(cIndex, siSyms, pInner, cInner, _, _) => 
        if (!subst.contains(cIndex))
          subst += (cIndex -> Const(0))
        addSimpleIndexReplacement(siSyms, pInner)
        // fused is not cons=Mc(cInner), but cInner, so don't record as fused with singleton (different shapes)
        overrideRecordFusion = Some(Right(false))
        FusionResult(reflectBlock(Block(cInner)))

      case IfThenElseOneEmpty_Any(cond, thenFusionInfo, elseFusionInfo, typeSym, pSym, cSym) =>
        val newCond = reflectBlock(Block(cond))
        // __ifThenElse does reify
        FusionResult(__ifThenElse(newCond, doFusion(thenFusionInfo, cStm, Right(false), false).transformed, 
          doFusion(elseFusionInfo, cStm, Right(false), false).transformed)(mtype(typeSym.tp), mpos(cSym.pos)))

      case Mc_McForRed(cIndex, pIndex, cShape, pShape, innerFusionInfo, cInner, pInner, _, _) =>
        subst += (cIndex -> pIndex)
        val innerFused = reifyEffects(doFusion(innerFusionInfo, cStm, Left(pInner), false).transformed)(mtype(cInner.tp))
        onceSubst += (cShape -> pShape)
        onceSubst += (cInner -> getBlockResultFull(innerFused))
        subst += (cIndex -> pIndex)
        FusionResult(super.transformStm(cStm)) 

      case InnerMc_Red(cIndex, pIndex, innerFusionInfo, cInner, pInner, pLoop, _, _) =>
        subst += (cIndex -> pIndex)
        val innerFused = reifyEffects(doFusion(innerFusionInfo, cStm, Left(pInner), false).transformed)(mtype(cInner.tp))
        onceSubst += (pInner -> getBlockResultFull(innerFused))
        FusionResult(self_mirror(cInner, pLoop))

      case ManyMcsingle_McFor(cIndex, pIndex, siSyms, pInner, otherProds, _,_,_) =>
        subst += (cIndex -> pIndex) // otherProds have same pIndex
        // onceSubst += (cShape -> Const(1)) // for reduce need loop?
        addSimpleIndexReplacement(siSyms, pInner)
        otherProds.foreach({ case (osiSyms, opInner) => addSimpleIndexReplacement(osiSyms, opInner) })
        FusionResult(super.transformStm(cStm))

      case err => sys.error("(VFT) Error: Unknown FusionInfo in doFusion: " + err)
    }
    // println("=== done fusion: " + fusionInfo + "\n     -> " + fusionOutcome)
    recordOutcome(overrideRecordFusion.getOrElse(recordFusion), fusionInfo, fusionOutcome, effectful)
    fusionOutcome
  }

  /** Track the outcome of the fusion.
    * @param recordFusion Right(true) means regular tracking of producer and
    * consumer as found in fusionInfo and fusionOutcome. Right(false) means no
    * tracking at all. Left(symbol) means that the given symbol is recorded
    * instead of the producer, to be used in the case of recursion where the
    * inner collection of the producer should be used. */
  def recordOutcome(recordFusion: Either[Exp[Any],Boolean], fusionInfo: FusionInfo,
    fusionOutcome: FusionOutcome, effectful: Boolean) = {
    recordFusion match {
      case Right(true) => (fusionInfo, fusionOutcome) match {
        case (NoFusionInfo(cons, _, _), NoFusionRemapped(newSym, _)) =>
          FusedSyms.recordRemapped(cons, newSym)

        case (FusionInfo1to1(prod, cons, _), FusionResult(fusedSym@Sym(_))) => 
          FusedSyms.recordFused(prod, cons, fusedSym, Nil, effectful)
        
        case (FusionInfoManyto1(prod, otherProds, cons, _), FusionResult(fusedSym@Sym(_))) => 
          FusedSyms.recordFused(prod, cons, fusedSym, 
            otherProds.map({ old => (old, getSubstSym(old)) }), effectful)
        
        case (_, FusionResult(Const(_))) => // don't record anything

        case err => sys.error("(VFT) Error: recordOutcome unknown right case: " + err)
      }
      case Right(false) => // don't record anything

      case Left(innerPSym@Sym(_)) => fusionOutcome match {
        case FusionResult(fusedSym@Sym(_)) => 
          FusedSyms.recordFused(innerPSym, fusedSym, fusedSym, Nil, effectful)

        case err => sys.error("(VFT) Error: recordOutcome unknown left case: " + err)
      }
      case Left(Const(_)) => // don't record anything
      case err => sys.error("(VFT) Error: recordOutcome unknown case: " + err)
    }
  }


  // ----- fusion checks -----

  /** Calculates the producer type. CShape and pSym are originals, this
    * function will check their substitutions too.
    * @return the ProdType and debugging output. */
  def consumerGoesOverRangeOfProducer(cShape: Exp[Int], pSym: Sym[Any]): (ProdType, String) = {
    var cShapeSyms: List[Sym[Any]] = Nil
    var cConsts: List[Const[Any]] = Nil

    (cShape :: (subst.get(cShape).map(List(_)).getOrElse(Nil))).foreach({
      case sym@Sym(_) => cShapeSyms ::= sym
      case c@Const(l: Int) => cConsts ::= c
    })
    val pSyms = subst.get(pSym) match {
      case Some(sym@Sym(_)) => sym :: List(pSym)
      case _ => List(pSym) // don't know what to do with array constants
    }

    cShapeSyms.collect({ 
      case Def(SimpleDomain(sym)) if pSyms.contains(sym) => 
        return (RealProd(), "real producer") // real producer
    })

    val (pShapeSyms, pConsts) = pSyms.map(loopsToFixedOutLengths.get(_))
      .filter(_.isDefined).map(_.get)
      .partition({ case Sym(_) => true case _ => false })

    cConsts.intersect(pConsts).foreach({ c => return (ReconstrProd(), "same constant: " + c) })
    cShapeSyms.intersect(pShapeSyms).foreach({ s => return (ReconstrProd(), "same symbol: " + s) })

    return (PartialProd(), "cConsts: " + cConsts + ", pConsts: " + pConsts + 
        ", cShapeSyms: " + cShapeSyms + ", pShapeSyms: " + pShapeSyms)
  }

  /** Returns None if the consumer is independent of the producer apart from
    * the SimpleIndex access and the index use is ok. Returns Some(msg)
    * otherwise with the reason why the producer isn't taken. */
  def depsOK[A](cSym: Sym[A], cIndex: Sym[Int], indexIsTaboo: Boolean, cBody: Def[A], 
      cEffects: LoopEffects, pSym: Sym[Any], newPSym: Sym[Any]): Option[String] = {

    val prodFusedSyms = FusedSyms.getSet(pSym).map(_.getSet)
      .getOrElse(Set(pSym, getSubstSym(pSym)))
    val consFusedSyms = syms(cBody) ++ 
      cEffects.map(_._2.collect({ case s@Sym(_) => s })).getOrElse(List[Sym[Any]]())

    def substEqu(other: Exp[Any], pSym: Sym[Any]) = 
      ((other == pSym) || (subst.get(other)).map(_ == pSym).getOrElse(false))

    // traverse the statements needed for the loop and check there are no deps
    GraphUtil.stronglyConnectedComponents(consFusedSyms, { sym: Sym[Any] => 
      // TODO don't want to do full traversal every time, could cache deps of seen loops?
      // But then would have to do full traversal, not stop early?
      // Could also build full deps graph with separate analysis pass?
      sym match {
        case Def(SimpleIndex(other, `cIndex`)) if substEqu(other, pSym) => 
          List() // SimpleIndex will be substituted, so don't count dependency on index or producer 
        case Def(d) => 
          val next = getNewSyms(syms(d).flatMap({ sym => 
            FusedSyms.getSet(sym).map(_.getSet).getOrElse(Set(sym))
          }))

          next.collectFirst({ 
            case x if prodFusedSyms.contains(x) => return Some("consumer depends on producer through " + x)
            case `cIndex` if (indexIsTaboo && !ignoreIndex(d, cIndex)) => return Some("consumer uses index in " + d + ". " + 
              "Override ignoreIndex if the index value doesn't matter (e.g. the index is just used " +
              "to keep a node inside the loop)")
          })
          next
        case _ => List()
      }
    })
    None
  }

  /** Producer and consumer can be fused only if at most one is effectful. If
    * the producer is effectful, additionally the effects need to be separate
    * from the result value so that the result computation can be fused into
    * the consumer without duplicating the effects (which would not be
    * deduplicated by cse). In the current model, since all producers contain a
    * collection block, effectful producers cannot be fused because the result
    * value contains the effects from the block.
    *
    * @return If fusion is ok, {@code Right(effectful)} is returned, else
    *         {@code Left(error message)}. */
  def effectsOk(cEffects: LoopEffects, pSym: Sym[Any], newPSym: Sym[Any]): Either[String, Boolean] = {

    val pEffects = newPSym match {
      case Def(Reflect(_, summ, deps)) => Some((summ, deps))
      case _ => None
    }

    (cEffects.isDefined, pEffects.isDefined) match {
      case (false, false) => Right(false)
      case (true, true) => Left("effectful consumer cannot be fused with effectful producer")
      case (true, false) => 
        if (FusedSyms.getSet(pSym).map(_.getEffects).getOrElse(false))
          Left("effectful consumer cannot be fused with producer that has " +
            "been fused with another effectful loop")
        else
          Right(true)
      case (false, true) =>
        Left("effectful producer cannot be fused")

        /* TODO: with map being translated to MultiCollect(singleton(block)),
         * the loop body will be effectful if anything in the singleton block
         * is... In theory could fuse if producer is treated like
         * MC(singleton(blockWithoutEffects)), but how to filter? Will also
         * create problems because horizontal fusion doesn't combine two
         * singletons? Following is the old code to check that effects and
         * value of the producer block are separate. */

        // val pIndexStms = getFatDependentStuff(initialDefs)(List(prod.v))
        // val resSym = ProducerResultBlock.unapply(prod.body) match {
        //   case Some(Def(Reify(resSym@Sym(_), _, _))) => resSym
        //   case _ => sys.error("TODO what if no result block? " + prod.body)
        // }
        // GraphUtil.stronglyConnectedComponents(List(resSym), { sym: Sym[Any] => sym match {
        //   case Def(Reflect(_, _, _)) => return Left("effectful producers can only be fused if " +
        //     "the effects are separate from the value of the loop body, but the loop block result depends on " +
        //     findDefinition(sym).get)
        //   case Def(d) => getNewSyms(syms(d).intersect(pIndexStms))
        //   case _ => List()
        // }})
        // Right(true)
    }
  }


  // ----- extractors/helpers -----

  type OldNewSyms = (Sym[Any], Sym[Any])
  type LoopEffects = Option[(Summary, List[Exp[Any]])]
  object LoopOrReflectedLoop {
    def unapply(a: Def[Any]): Option[(AbstractLoop[_], LoopEffects)] = a match {
      case Reflect(loop: AbstractLoop[_], summ, deps) => Some((loop, Some((summ, deps))))
      case loop: AbstractLoop[_] => Some((loop, None))
      case _ => None
    }
  }
  object MatchLoop {
    def unapply(a: Any): Option[(Def[Any], Exp[Int], Sym[Int])] = a match {
      case Right(cLoop: AbstractLoop[_]) => Some((cLoop.body, cLoop.size, cLoop.v))
      case _ => None
    }
  }

  object MatchMC {
    def unapply(a: Any): Option[(Exp[Any], Exp[Int], Sym[Int])] = a match {
      case (Right(cLoop: AbstractLoop[_]), remapReduceToMc: Boolean) => cLoop.body match {
        case MultiCollect(cInner) => Some((cInner, cLoop.size, cLoop.v))
        case Reduce(cInner) if remapReduceToMc => Some((cInner, cLoop.size, cLoop.v))
        case _ => None
      }
      case _ => None
    }
  }

  object MatchMcFor {
    def unapply(a: Any): Option[(Exp[Any], Exp[Int], Sym[Int])] = a match {
      case MatchMC(cInner, cSize, cIndex) => Some((cInner, cSize, cIndex))
      case (Right(cLoop: AbstractLoop[_]), remapReduceToMc) => cLoop.body match {
        case For(cInner) => Some((cInner, cLoop.size, cLoop.v))
        case _ => None
      }
      case _ => None
    }    
  }

  object MatchMcForReduce {
    def unapply(a: Any): Option[(Exp[Any], Exp[Int], Sym[Int])] = a match {
      case Right(cLoop: AbstractLoop[_]) => (cLoop.body match {
        case MultiCollect(cInner) => Some(cInner)
        case For(cInner) => Some(cInner)
        case Reduce(cInner) => Some(cInner)
        case _ => None
      }).map({t => (t, cLoop.size, cLoop.v)})
      case _ => None
    }    
  }

  def stmShort(stm: Stm) = stm match {
    case TP(s@Sym(_), Reflect(loop: AbstractLoop[_], _, _)) => "TP(" + s + ",Reflect(" + loop + ", ...))"
    case _ => stm.toString
  }
  def getSubstSym(sym: Sym[Any]): Sym[Any] = subst.get(sym) match {
    case Some(s@Sym(_)) => s
    case _ => sym
  }
  def getSubstSym(sym: Sym[Any], subst2: Map[Exp[Any], Exp[Any]] = subst): Sym[Any] = subst2.get(sym) match {
    case Some(s@Sym(_)) => s
    case _ => sym
  }
  def findDefinitionExp(exp: Exp[Any]): Option[Stm] = exp match {
    case s@Sym(_) => findDefinition(s)
    case _ => None
  }
  def getNewSyms(syms: List[Sym[Any]]) = syms.flatMap(n => subst.get(n) match { 
    // use the new expressions
    case Some(nn@Sym(_)) => List(nn)
    case Some(Const(_)) => List()
    case _ => List(n)
  })
}
