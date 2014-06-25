package scala.virtualization.lms
package common

import util.GraphUtil
import scala.collection.mutable.{HashMap, HashSet}


        // TODO asserts?


trait LoopFusionVerticalTransformer extends PreservingForwardTransformer { 
  val IR: LoopFusionCore
  import IR.{__newVar => _, _}

  // ----- global datastructures -----

  /** Records all fusion decisions. */
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
    private val fusedSubst = new HashMap[Exp[Any], Exp[Any]]
    
    def recordRemapped(oldSym: Sym[Any], newExp: Exp[Any]) = newExp match { 
      case newSym@Sym(_) => 
        fusedSubst.put(oldSym, newSym)
        fusedSyms.get(oldSym).map({ set =>
          fusedSyms.put(newSym, set)
          set.add(newSym)
        })  
      case _ =>
    }
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

    def getSet(pSym: Sym[Any]): Option[FSet] = fusedSyms.get(pSym)

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

  // Indexing statements replacement: consumer.at(index) -> exp
  // (can't register before because of duplicate substitution error).
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

  // List of the loops on the same level&scope
  var seenLoops = HashSet[Sym[Any]]()
  
  // All unfused loops with fixed length of the output collection in same exact scope
  // have same index so they can be fused as multiple producers with one consumer
  // Lazily created HashMap if really needed in current scope
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

  /** RealProd: shape of consumer loop is prod.length
   *  ReconstrProd: length of producer output collection equals shape of consumer loop
   *  PartialProd: cannot conclude any of the above, consumer might only
   *    iterate over part of the producer collection
   */
  sealed abstract class ProdType { val intVal: Int; val shortName: String }
  case class RealProd extends ProdType { val intVal = 1; val shortName = "real" }
  case class ReconstrProd extends ProdType { val intVal = 2; val shortName = "reconstructed" }
  case class PartialProd extends ProdType { val intVal = 3; val shortName = "partial" }

  
  sealed abstract class FusionInfo(cSym: Sym[Any], notFused: List[(Sym[Any], String)]) {
    def printLogBefore: Unit
    def printLogAfter(result: FusionOutcome): Unit
    def getProds: List[Sym[Any]]
    def setNotFused(list: List[(Sym[Any], String)])
  }

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
    override def setNotFused(list: List[(Sym[Any], String)]) = sys.error("FusionInfoManyto1.notFused is not mutable")
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
    override def setNotFused(list: List[(Sym[Any], String)]) = sys.error("NoFusionInfo.notFused is not mutable")
  }

  case class Single_MC(cIndex: Sym[Int], siSyms: SISyms, pInner: Exp[Any], cInner: Exp[Any],
      override val pSym: Sym[Any], override val cSym: Sym[Any]) extends FusionInfo1to1(pSym, cSym, Nil)
  case class Single_Single(siSyms: SISyms, pInner: Exp[Any], 
      override val pSym: Sym[Any], override val cSym: Sym[Any]) extends FusionInfo1to1(pSym, cSym, Nil)
  
  case class Empty_MCSingle(empty: Exp[Any],
    override val pSym: Sym[Any], override val cSym: Sym[Any]) extends FusionInfo1to1(pSym, cSym, Nil)
  case class Empty_Foreach(override val pSym: Sym[Any], override val cSym: Sym[Any]) 
    extends FusionInfo1to1(pSym, cSym, Nil)

  case class IfThenElseOneEmpty_Any(cond: Exp[Boolean], ifFusionInfo: FusionInfo, 
      elseFusionInfo: FusionInfo, typeSym: Sym[Any],
    override val pSym: Sym[Any], override val cSym: Sym[Any]) extends FusionInfo1to1(pSym, cSym, Nil)

  case class Mc_McForlike(cIndex: Sym[Int], pIndex: Sym[Int],
      cShape: Exp[Int], pShape: Exp[Int], innerFusionInfo: FusionInfo, 
      cInner: Exp[Any], pInner: Exp[Any],
      override val pSym: Sym[Any], override val cSym: Sym[Any]) 
    extends FusionInfo1to1(pSym, cSym, Nil)

  case class InnerMC_Red(cIndex: Sym[Int], pIndex: Sym[Int],
      innerFusionInfo: FusionInfo, cInner: Sym[Any], pInner: Exp[Any], pLoop: Def[Any],
      override val pSym: Sym[Any], override val cSym: Sym[Any])
    extends FusionInfo1to1(pSym, cSym, Nil)

  case class Mc_Single(val siSyms: SISyms, pInner: Exp[Any], cInner: Exp[Any], pLoop: Def[Any],
      override val pSym: Sym[Any], override val cSym: Sym[Any])
    extends FusionInfo1to1(pSym, cSym, Nil)

  object Mcsingle_Multi {
    def unapply(a: FusionInfo) = a match {
      case Mc_McForlike(cIndex, pIndex,_,_, Single_MC(_,siSyms,singletonInner,_,_,_), _, _, pSym, cSym) =>
        Some((cIndex, pIndex, siSyms, singletonInner, pSym, cSym))
      case _ => None
    } 
  }
  case class ManyMcsingle_MultiFor(cIndex: Sym[Int], pIndex: Sym[Int],
      val siSyms: SISyms, pInner: Exp[Any], val oProds: List[(SISyms, Exp[Any])], 
      override val pSym: Sym[Any], override val cSym: Sym[Any],
      override val notFused: List[(Sym[Any], String)])
    extends FusionInfoManyto1(pSym, oProds.unzip._1.unzip._1, cSym, notFused)


  // ----- fusion result classes -----

  sealed abstract class FusionOutcome(val transformed: Exp[Any])
  case class FusionResult(fusedSym: Exp[Any]) extends FusionOutcome(fusedSym)
  case class NoFusionRemapped(remappedSym: Exp[Any], remapReason: String) extends FusionOutcome(remappedSym)


  // ----- transformer methods -----

  // FixPointTransformer methods, TODO run vertical fusion as fixpoint, for now only run once
  def getInfoString = "LoopFusionVerticalTransformer only runs once, TODO fixpoint"
  var hasRunOnce = false
  def isDone = hasRunOnce
  def runOnce[A:Manifest](s: Block[A]): Block[A] = {
    val newBlock = transformBlock(s)
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

  val printAllTransformations = false
  override def transformStm(stm: Stm): Exp[Any] = { 
    if (printAllTransformations) println("--transforming: " + stmShort(stm))
    
    val transformedSym = stm match {
      case TP(_, SimpleIndex(pSym@Sym(_), cIndex@Sym(_))) => subst.get(pSym)
        .flatMap({ case newPSym@Sym(_) => getSimpleIndexReplacements(newPSym, cIndex) })
        .orElse(getSimpleIndexReplacements(pSym, cIndex))
        .getOrElse(super.transformStm(stm))

      case TP(sym, LoopOrReflectedLoop(loop, effects)) =>
        transformLoop(stm, sym, loop, effects)

      case tp@TP(_, SimpleDomain(sym@Sym(_))) =>
        loopsToFixedOutLengths.get(sym).map({ fixedSym => 
          printdbg("(VFT) Replaced " + tp + " with fixed length: " + fixedSym)
          fixedSym
        }).getOrElse(super.transformStm(stm))

        // TODO remove?
      case TP(sym, IfThenElse(_,_,_)) => seenLoops += sym
        super.transformStm(stm)
      case TP(sym, SingletonColl(_)) => seenLoops += sym
        super.transformStm(stm)
      case TP(sym, EmptyColl()) => seenLoops += sym
        super.transformStm(stm)

      case _ => super.transformStm(stm)
    }

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
          recordFixedOutLengths(sym, loop)
        }
      case _ =>
    }
    fusionOutcome.transformed
  }


  // ----- fusion functions -----

  def findProducers[A:Manifest](cSym: Sym[A], cLoop: AbstractLoop[_], cShape: Exp[Int], 
      cIndex: Sym[Int], cBody: Def[A], cEffects: LoopEffects): (FusionInfo, Boolean) = {
    // TODO other, multiple prods, deps, index use
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
          // TODO could have callback, but now not shape of prod, only for SingleCollects?
          // problem when shape of consumer replaced with shape of producer
          // fusePartialConsumers(pSym, findDefinition(pSym), cSym, findDefinition(cSym))
        case _ =>
      }
      (pSym, prodType)
    }).filter({ case (_, PartialProd()) => false case _ => true
    }).sortWith({ (a, b) => a._2.intVal < b._2.intVal
    }).map({ case (pSym, prodType) => (pSym, getSubstSym(pSym), prodType)
    }).filter({ case (pSym, newPSym, _) =>
      val indexIsTaboo = newPSym match {
        case Def(LoopOrReflectedLoop(pLoop, _)) => pLoop.body match {
          case MultiCollect(Def(SingletonColl(_))) => false
          case _ => true
        }
        case _ => true
      }
      notIndep(cSym, cIndex, indexIsTaboo, cBody, cEffects, pSym, newPSym) match {
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

    combineProducers(cSym, Right(cLoop), cEffects, producers, listOfNotFused, None, None, None)
  }

  def combineProducers[A:Manifest](cSym: Sym[A], cDef: Either[Def[A], AbstractLoop[_]], 
      cEffects: LoopEffects, prodCandidates: List[(Sym[Any], Sym[Any], ProdType, Boolean)],
      notFused: List[(Sym[Any], String)], outerIndex: Option[Sym[Int]],
      outerSISyms: Option[SISyms], remapReduceToMC: Option[Sym[Any]]): (FusionInfo, Boolean) = {
    // TODO invariants: cshape doesn't survive fusion, cIndex on outermost level becomes pindex?
    // TODO do something with effects, what about recursive calls?
    var listOfNotFused: List[(Sym[Any], String)] = notFused

    val producers: List[(FusionInfo1to1, ProdType, Boolean)] = prodCandidates.flatMap({ case (oldPSym, newPSym, prodType, effectful) => 
      val fusionInfo: Option[FusionInfo1to1] = (oldPSym, newPSym) match {
        case (_, Def(LoopOrReflectedLoop(pLoop, _))) => pLoop.body match {
          
          case MultiCollect(pInner@Sym(_)) => cDef match {
            // SL(ps, pi, MC(pinner(pi))) + SL(prod.len, ci, MC(cinner(prod.at(ci))))
            // -> SL(ps, pi, MC(pinner(pi) + SL(pinner.len, ci, MC(cinner(pinner.at(ci))))))

            // SL(ps, pi, MC(pinner(pi))) + SL(prod.len, ci, Foreach(cInner(prod.at(ci))))
            // -> SL(ps, pi, Foreach(pinner + SL(pinner.len, ci, Foreach(cInner(pinner.at(ci))))

            // SL(ps, pi, MC(pinner(pi))) + SL(prod.len, ci, Reduce(cInner(prod.at(ci))))
            // -> SL(ps, pi, Reduce(pinner(pi) + SL(pinner.len, ci, MC(cInner(pinner.at(ci))))))
            case MatchLoop(MultiCollect(Sym(_)), _, _) | MatchLoop(ForLike(Sym(_),_), _, _) =>
              val (cInner, cShape, cIndex, nextTreatReduceAsMc) = cDef match {
                case MatchLoop(MultiCollect(cInner@Sym(_)),  cShape, cIndex) => (cInner, cShape, cIndex, false)
                case MatchLoop(ForLike(cInner@Sym(_),true),  cShape, cIndex) => (cInner, cShape, cIndex, false)
                case MatchLoop(ForLike(cInner@Sym(_),false), cShape, cIndex) => (cInner, cShape, cIndex, true)
              }
              val nextRemapReduceToMC = if (nextTreatReduceAsMc) Some(cInner) else None

              val innerFusionInfo = combineProducers(cSym, cDef, cEffects, 
                List((oldPSym, pInner, RealProd(), false)), Nil, Some(pLoop.v), Some(oldPSym, cIndex), nextRemapReduceToMC)._1
              innerFusionInfo match {
                case NoFusionInfo(_,_,_) => // TODO here use remapReduceToMC
                  listOfNotFused ::= (oldPSym, "multicollect producer's inner collection cannot be fused with consumer"); None
                case Empty_Foreach(_,_) => Some(Empty_Foreach(newPSym, cSym))
                case _ => 
                  if (!remapReduceToMC.isDefined)
                    Some(Mc_McForlike(cIndex, pLoop.v, cShape, pLoop.size, 
                      innerFusionInfo, cInner, pInner, newPSym, cSym))
                  else
                    Some(InnerMC_Red(cIndex, pLoop.v, innerFusionInfo, cInner, pInner, pLoop, newPSym, cSym))
              }

            case _ => listOfNotFused ::= (oldPSym, "don't know how to fuse MultiCollect producer with consumer of type " + cDef); None
          }

          case _ => listOfNotFused ::= (oldPSym, "unknown elem in loop producer (have you overriden the LoopFusionExtractors?): " + pLoop.body); None
        }

        // TODO change: fused only as inner functions, or callback
        // Generally cannot fuse IfThenElse producers because they can't be vertically fused, so
        // would result in code duplication. Therefore not added to seenLoops, so not recognized
        // as producers in findProducers. But here it's inner loop of MC being fused.

        case (_, Def(IfThenElse(cond, Block(ifSym@Sym(_)), Block(elseSym@Sym(_))))) =>
          // TODO think about constants? but array type... fuse if result is empty/unit?
          ((ifSym, elseSym) match {
            case (Def(EmptyColl()), _) => Some((true, ifSym, elseSym))
            case (_, Def(EmptyColl())) => Some((false, elseSym, ifSym))
            case _ => None
          }) match {
            case Some((emptyFirst, empty, other)) => 
              combineProducers(cSym, cDef, cEffects, List((oldPSym, empty, RealProd(), false)),
                  Nil, outerIndex, outerSISyms, remapReduceToMC)._1 match {
                
                case NoFusionInfo(_,_,_) => listOfNotFused ::= (oldPSym, "if-then-else not fused because empty branch cannot be fused with consumer (don't want to duplicate consumer code)"); None
                
                case emptyFusionInfo =>
                  val otherFusionInfo = combineProducers(cSym, cDef, cEffects,
                      List((oldPSym, other, RealProd(), false)), Nil, outerIndex, outerSISyms, remapReduceToMC)._1 match {
                    case NoFusionInfo(_,_,_) => sys.error("TODO ifthenelse not fused")
                    case oFusionInfo => oFusionInfo
                  }

                  if (emptyFirst)
                    Some(IfThenElseOneEmpty_Any(cond, emptyFusionInfo, otherFusionInfo, 
                      remapReduceToMC.getOrElse(cSym), newPSym, cSym))
                  else 
                    Some(IfThenElseOneEmpty_Any(cond, otherFusionInfo, emptyFusionInfo,
                      remapReduceToMC.getOrElse(cSym), newPSym, cSym))
              }
            case None => listOfNotFused ::= (oldPSym, "if-then-else producer only fused if one branch is empty collection"); None
          }

        case (_, Def(empty@EmptyColl())) => (cDef, remapReduceToMC.isDefined) match {
          // empty + MC -> empty of consumer type
          case (MatchLoop(MultiCollect(_),_,_),_) | (MatchLoop(ForLike(_,false),_,_), true) =>
            val (cInner, cShape, cIndex) = cDef match {
              case MatchLoop(MultiCollect(cInner), cShape, cIndex) => (cInner, cShape, cIndex)
              case MatchLoop(ForLike(cInner, _),   cShape, cIndex) => (cInner, cShape, cIndex)
            }
            (empty, cInner, outerIndex) match {
              case EmptyCollNewEmpty(newEmpty) => Some(Empty_MCSingle(newEmpty, newPSym, cSym))
              case _ => sys.error("TODO no empty")
            }

          // empty + For/foreach -> Unit
          case (MatchLoop(ForLike(_,true),_,_), _) => Some(Empty_Foreach(newPSym, cSym))

          // empty + reduce: don't fuse, maintain dsl specific error
          case (MatchLoop(ForLike(_,false),_,_), false) => 
            listOfNotFused ::= (oldPSym, "empty producer not fused with reduce because need to maintain implementation-specific error"); None

          case _ => sys.error("TODO findProducers 5 cons: " + cSym)
        }

        case (_, Def(SingletonColl(pInner))) => (cDef, remapReduceToMC.isDefined) match {

          case (MatchLoop(MultiCollect(_),_,_),_) | (MatchLoop(ForLike(_,false),_,_), true) |
              (MatchLoop(ForLike(_,true),_,_), _) =>
            val (cInner, cShape, cIndex) = cDef match {
              case MatchLoop(MultiCollect(cInner), cShape, cIndex) => (cInner, cShape, cIndex)
              case MatchLoop(ForLike(cInner, _),   cShape, cIndex) => (cInner, cShape, cIndex)
            }
            val siSyms = outerSISyms.getOrElse((oldPSym, cIndex))
            Some(Single_MC(cIndex, siSyms, pInner, cInner, newPSym, cSym))

          case (Left(SingletonColl(cInner)),_) if outerSISyms.isDefined => 
            Some(Single_Single(outerSISyms.get, pInner, newPSym, cSym))              

          case _ => sys.error("TODO findProducers 3 cons: " + cSym + ", cDef: " + cDef)
        }

        case _ => sys.error("TODO findProducers prod: " + newPSym)
      }
      fusionInfo.map({ fi => List((fi, prodType, effectful)) }).getOrElse(Nil)
    })

    /** Helper function to log not-chosen producers when multiple producers are available. */
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

    /** Helper function to create ManyMcsingle_MultiFor and log non-chosen multiple producers. */
    def combineMultipleProducers(mcsm: FusionInfo, firstPType: ProdType, eff: Boolean, list: List[(FusionInfo1to1, ProdType, Boolean)]) = {
      mcsm match {
        case Mcsingle_Multi(cIndex,pIndex,siSyms,pInner,newPSym,cSym) => 
          val canFuseRight = list.map({
            case (fi@Mcsingle_Multi(_,`pIndex`,osiSyms,opInner,_,_), _, otherEff) => Right((fi, (osiSyms, opInner, otherEff)))
            case other => Left(other)
          })
          addNotFused(canFuseRight.collect({ case Left(notFused) => notFused._1 }), siSyms._1, firstPType,
            "(Current producer isn't MC(singleton) with correct index)")
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
            (ManyMcsingle_MultiFor(cIndex, pIndex, siSyms, pInner, otherProds2, newPSym, cSym, listOfNotFused), combinedEffect)
          else
            (mcsm, eff)
      }
    }

    val result: (FusionInfo, Boolean) = producers match {
      case Nil => (NoFusionInfo(cSym, cDef, listOfNotFused), false)
      
      case prod :: Nil => prod._1.notFused = listOfNotFused; (prod._1, prod._3)

      case (mcsm@Mcsingle_Multi(cIndex,pIndex,siSyms,pInner,newPSym,cSym), firstPType, eff) :: list => 
        combineMultipleProducers(mcsm, firstPType, eff, list)

      case (first, firstPType, firstEff) :: list => 
        val fusedProd: Sym[Any] = first.getProds.head
        addNotFused(list.map(_._1), fusedProd, firstPType, "(First fused producer " + fusedProd + 
          " is not a MC(Singleton)).", Some(first))
        (first, firstEff)
    }
    // println("===combineProducers(cSym=" + cSym + ", cDef=" + cDef + ", prodCandidates=" + prodCandidates + 
    //   ", notFused=" + notFused + ", outerIndex=" + outerIndex + "/n    -> " + result)
    result
  }

  def doFusion(fusionInfo: FusionInfo, cStm: Stm, recordFusion: Either[Exp[Any],Boolean], effectful: Boolean): FusionOutcome = {
    var overrideRecordFusion: Option[Either[Exp[Any],Boolean]] = None
    val fusionOutcome = fusionInfo match {
      case NoFusionInfo(sym, Right(loop), _) if (remapIndexIfFixedLength(recordFixedOutLengths(sym, loop), loop.v)) =>
          NoFusionRemapped(super.transformStm(cStm), "of fixed shape")

      case NoFusionInfo(sym, _, _) => 
        NoFusionRemapped(super.transformStm(cStm), "of previous substitutions or effects")

      case ManyMcsingle_MultiFor(cIndex, pIndex, siSyms, pInner, otherProds, _,_,_) =>
        subst += (cIndex -> pIndex) // otherProds have same pIndex
        // onceSubst += (cShape -> Const(1)) // for reduce need loop?
        addSimpleIndexReplacement(siSyms, pInner)
        otherProds.foreach({ case (osiSyms, opInner) => addSimpleIndexReplacement(osiSyms, opInner) })
        FusionResult(super.transformStm(cStm))

      case Single_MC(cIndex, siSyms, pInner, cInner, _, _) => 
        if (!subst.contains(cIndex))
          subst += (cIndex -> Const(0))
        addSimpleIndexReplacement(siSyms, pInner)
        // fused is not cons=MC(cInner), but cInner, so don't record as fused with singleton (different shapes)
        overrideRecordFusion = Some(Right(false))
        FusionResult(reflectBlock(Block(cInner)))

      case Empty_MCSingle(empty, _, _) => FusionResult(empty)

      case Empty_Foreach(_,_) => FusionResult(Const(()))

      case IfThenElseOneEmpty_Any(cond, thenFusionInfo, elseFusionInfo, typeSym, pSym, cSym) =>
        val newCond = reflectBlock(Block(cond))
        // __ifThenElse does reify
        FusionResult(__ifThenElse(newCond, doFusion(thenFusionInfo, cStm, Right(false), false).transformed, 
          doFusion(elseFusionInfo, cStm, Right(false), false).transformed)(mtype(typeSym.tp), mpos(cSym.pos)))

      case Mc_McForlike(cIndex, pIndex, cShape, pShape, innerFusionInfo, cInner, pInner, _, _) =>
        subst += (cIndex -> pIndex)
        val innerFused = reifyEffects(doFusion(innerFusionInfo, cStm, Left(pInner), false).transformed)(mtype(cInner.tp))
        onceSubst += (cShape -> pShape)
        onceSubst += (cInner -> getBlockResultFull(innerFused))
        subst += (cIndex -> pIndex)
        FusionResult(super.transformStm(cStm)) 

      case InnerMC_Red(cIndex, pIndex, innerFusionInfo, cInner, pInner, pLoop, _, _) =>
        subst += (cIndex -> pIndex)
        val innerFused = reifyEffects(doFusion(innerFusionInfo, cStm, Left(pInner), false).transformed)(mtype(cInner.tp))
        onceSubst += (pInner -> getBlockResultFull(innerFused))
        FusionResult(self_mirror(cInner, pLoop))

      case Mc_Single(siSyms, pInner, cInner, pLoop, _, cSym) =>
        addSimpleIndexReplacement(siSyms, pInner)
        onceSubst += (pInner -> cInner)
        FusionResult(self_mirror(cSym, pLoop))

      case Single_Single(siSyms, pInner, _, _) =>
        addSimpleIndexReplacement(siSyms, pInner)
        FusionResult(super.transformStm(cStm))

      case _ => sys.error("TODO doFusion")
    }
    // println("=== done fusion: " + fusionInfo + "\n     -> " + fusionOutcome)
    overrideRecordFusion.getOrElse(recordFusion) match {
      case Right(true) => (fusionInfo, fusionOutcome) match {
        case (NoFusionInfo(cons, _, _), NoFusionRemapped(newSym, _)) =>
          FusedSyms.recordRemapped(cons, newSym)

        case (FusionInfo1to1(prod, cons, _), FusionResult(fusedSym@Sym(_))) => 
          FusedSyms.recordFused(prod, cons, fusedSym, Nil, effectful)
        
        case (FusionInfoManyto1(prod, otherProds, cons, _), FusionResult(fusedSym@Sym(_))) => 
          FusedSyms.recordFused(prod, cons, fusedSym, 
            otherProds.map({ old => (old, getSubstSym(old)) }), effectful)
        
        case (_, FusionResult(Const(_))) => 
          // Do nothing, constants not relevant for further vert. fusion decisions or horizontal

        case err => sys.error("doFusion unknown right case: " + err)
      }
      case Left(innerPSym@Sym(_)) => fusionOutcome match {
        case FusionResult(fusedSym@Sym(_)) => 
          FusedSyms.recordFused(innerPSym, fusedSym, fusedSym, Nil, effectful)
        case err => sys.error("doFusion unknown left case: " + err)
      }
      case _ =>
    }
    fusionOutcome
  }


  // ----- fusion checks -----

  /** cShape and pSym are originals, this function will check their substitutions too.
   *  Returns the ProdType and debugging output. */
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

  def notIndep[A](cSym: Sym[A], cIndex: Sym[Int], indexIsTaboo: Boolean, cBody: Def[A],
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

  /** Producer and consumer can be fused only if at most one is effectful. If the producer is
   *  effectful, additionally the effects need to be separate from the result value so that the result
   *  computation can be fused into the consumer without duplicating the effects (which would
   *  not be deduplicated by cse).
   *  If fusion is ok, Right(effectful) is returned, else Left(error message).
   */
  def effectsOk(cEffects: LoopEffects, pSym: Sym[Any], newPSym: Sym[Any]): Either[String, Boolean] = {

    val (prod, pEffects) = newPSym match {
      case Def(LoopOrReflectedLoop(prod, pEffects)) => (prod, pEffects)
      case _ => return Left("no loop definition found for producer " + newPSym)
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
        // check pEffects are separate from producer value
        val pIndexStms = getFatDependentStuff(initialDefs)(List(prod.v))
        val resSym = ProducerResultBlock.unapply(prod.body) match {
          case Some(Def(Reify(resSym@Sym(_), _, _))) => resSym
          case _ => sys.error("TODO what if no result block? " + prod.body)
        }

        GraphUtil.stronglyConnectedComponents(List(resSym), { sym: Sym[Any] => sym match {
          case Def(Reflect(_, _, _)) => return Left("effectful producers can only be fused if " +
            "the effects are separate from the value of the loop body, but the loop block result depends on " +
            findDefinition(sym).get)
          case Def(d) => getNewSyms(syms(d).intersect(pIndexStms))
          case _ => List()
        }})
        Right(true)
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
  def stmShort(stm: Stm) = stm match {
    case TP(s@Sym(_), Reflect(loop: AbstractLoop[_], _, _)) => "TP(" + s + ",Reflect(" + loop + ", ...))"
    case _ => stm.toString
  }
  def getSubstSym(sym: Sym[Any]): Sym[Any] = subst.get(sym) match {
    case Some(s@Sym(_)) => s
    case _ => sym
  }
  def getSubstSym(sym: Sym[Any],
      subst2: Map[Exp[Any], Exp[Any]] = subst): Sym[Any] = subst2.get(sym) match {
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

// ==============================
// ========= ARCHIVE ============
// ==============================

/*  
  def transformLoop[A:Manifest, B](stm: Stm, sym: Sym[A], loop: AbstractLoop[B], 
      effects: LoopEffects): Exp[Any] = {
    
    printlog("")
    val (prod, reconstrProds, combinedEffect) = 
      findProducers(sym, loop.size, loop.v, loop.body, effects)
    
    def printProd(p: OldNewSyms): String = p._2 + (if (p._1 == p._2) "" else " (was " + p._1 + ")")

    val msg = if (prod.isDefined) {
      "(VFT) Fusing consumer " + stmShort(stm) + " with real producer: " + printProd(prod.get) +
        (if (reconstrProds.isEmpty) ""
        else " and then with reconstructed producers: " + reconstrProds.map(printProd(_)))

    } else if (!reconstrProds.isEmpty) {
      "(VFT) Fusing consumer " + stmShort(stm) + " with reconstructed producer: " + printProd(reconstrProds(0)) +
        (if (reconstrProds.tail.isEmpty) ""
        else " and then with: " + reconstrProds.tail.map(printProd(_)))
    } else ""
    if (msg.length > 0) {
      printlog(msg)
      if (combinedEffect.isDefined)
        printlog("(VFT) Combined effect of fused loop is: " + combinedEffect.get)
    }

    val allProds = prod.map(List(_)).getOrElse(List()) ++ reconstrProds

    val resultLoop = allProds.headOption
      .flatMap({t => findDefinition(t._2)})
      .map({ case TP(s, LoopOrReflectedLoop(p, _)) =>
        val otherProds = allProds.tail
        val f = fuseProdAndCons(s, p, p.size, p.v, p.body, sym, loop, loop.size, loop.v, loop.body, otherProds, stm, combinedEffect)
        f match { 
          case fSym@Sym(_) => FusedSyms.recordFused(s, sym, fSym, otherProds, combinedEffect) 
          case _ => sys.error("(VFT) ERROR non-sym") }
        f
      })     

    seenLoops += sym

    resultLoop match {
      case Some(newLoop@Sym(_)) =>  // fused
        recordFixedOutLengths(sym, loop)

        newLoop
      case _ => // not fused
        val remapped = remapIndexIfFixedLength(recordFixedOutLengths(sym, loop), loop.v)
        val superTransformed = super.transformStm(stm)
        val origSym = stm.lhs()(0)
        // loop can be transformed because of subst from outer loop
        val changed = (superTransformed != origSym)
        if (changed) {
          superTransformed match {
            case s@Sym(_) => findDefinition(s) match {
              case Some(TP(newSym@Sym(_), LoopOrReflectedLoop(loop, _))) => 
                seenLoops += newSym
                recordFixedOutLengths(newSym, loop)
                FusedSyms.recordRemapped(sym, newSym)
              case _ => sys.error("ERROR VFT matching error")
            }
            case _ => sys.error("ERROR VFT matching error")
          }
        }
        if (remapped) {
          printdbg("(VFT) No producers found for " + stmShort(stm) + ", remapping to " + superTransformed + " because of fixed shape")
        } else if (changed) {
          printdbg("(VFT) No producers found for " + stmShort(stm) + ", changed to " + superTransformed + " because of existing substitutions or to reflect effects")
        } else {
          printdbg("(VFT) No producers found for " + stmShort(stm))
        }
        superTransformed
    }
  }

  def fuseProdAndCons[P: Manifest, C: Manifest](
      pSym: Sym[P], pLoop: Def[P], pSize: Exp[Int], pIndex: Sym[Int], pBody: Def[P], 
      cSym: Sym[C], cLoop: Def[C], cSize: Exp[Int], cIndex: Sym[Int], cBody: Def[C],
      otherProds: List[OldNewSyms],
      stm: Stm, effects: LoopEffects,
      outerMulti: Option[(Sym[Any], Sym[Int])] = None): Exp[Any] = {
    assert(otherProds.isEmpty || SimpleCollect.unapply(pBody).isDefined, 
        "(VFT) Error: cannot have multiple producers unless they're SimpleCollects")
    pBody match {
      // applies to all fusion types
      case ResultBlock(pRes) =>
        printdbg("(VFT) General fusion: remap index to " + pIndex + ", SimpleIndex to " + pRes + ".")
        subst += (cIndex -> pIndex)
        addSimpleIndexReplacement(pSym, cIndex, pRes)
      case _ => return fusionError("General fusion failed, no ResultBlock extractor for producer.", 
        cSym, cLoop, pSym, cIndex, stm)
    }
    val res = pBody match { // return None to use super.transform, or create fused consumer yourself
      case SimpleCollect(_) => 
        if (otherProds.isEmpty) {
          printdbg("(VFT) SimpleCollect+Any fusion: nothing more to do.")
        } else {
          printdbg("(VFT) SimpleCollect+Any fusion: nothing more to do for first producer, " +
            "fuse with other producers: " + otherProds.map(_._2))
          // Don't need to check that sets are independent, because if setA and setB
          // contained producer pA and pB, but pB depended on something in setA, then
          // pA wouldn't be a producer, because the consumer depends on setA through more
          // than just the SimpleIndex(pA, index).
          otherProds.map({t => findDefinition(t._2).get})
            .foreach({ case TP(otherPSym, LoopOrReflectedLoop(otherP, _)) => otherP.body match {
              case ResultBlock(otherPRes) =>
                printdbg("(VFT) Multiple fusion: remap SimpleIndex(" + otherPSym + ") to " + otherPRes + ".")
                addSimpleIndexReplacements(otherPSym, cIndex, otherPRes)
              case _ => 
                otherProds.foreach(t => removeSimpleIndexReplacements(t._2, cIndex))
                return fusionError("Multiple producer fusion failed, no ResultBlock extractor for producer " + otherPSym, 
                  cSym, cLoop, pSym, cIndex, stm)
            }})     
        }
        None

      case SimpleCollectIf(pRes,pConds) =>
        var res = cBody match {
          case SimpleCollect(cRes) =>
            printdbg("(VFT) SimpleCollectIf+SimpleCollect fusion: use producer loop with consumer body.")
            onceSubst += (pRes -> cRes)
            Some(self_mirror(cSym, wrapInReflect(pLoop, effects)))

          case SimpleCollectIf(_,cConds) =>
            printdbg("(VFT) SimpleCollectIf+SimpleCollectIf fusion: add producer conditions to consumer loop.")
            // If the condition also contains SimpleIndex, need to reflect full block first
            val transformedCCond = transformExpAsBlock(cConds.head)
            // substitute first consumer condition with AND(it, producer conditions)
            val fusedCond = pConds.foldLeft(transformedCCond)(boolean_and(_, _))
            subst += (cConds.head -> fusedCond)
            None
          case MultiCollect(cRes, Some(emptyArray)) =>
            printdbg("(VFT) SimpleCollectIf+MultiCollect fusion: add if-then-else with empty array to consumer.")
            onceSubst += (cRes -> getIfThenElse(pConds, cRes, emptyArray()))
            None
          case Reduce(cRes, Some(neutral), _) =>
            printdbg("(VFT) SimpleCollectIf+Reduce fusion: add if-then-else with neutral element to consumer.")
            onceSubst += (cRes -> getIfThenElse(pConds, cRes, neutral))
            None
        }
        res

      case MultiCollect(pRes@Sym(_),_) =>
        printdbg("(VFT) MultiCollect+Any fusion: fuse consumer with inner array of producer.")

        cBody match {
          case Reduce(_, _, Some(false)) | Reduce(_, _, None) => 
            return fusionError("MultiCollect+Reduce fusion failed, reduce not associative.",
              cSym, cLoop, pSym, cIndex, stm)
          case _ =>
        }

        val innerFused = findDefinition(pRes) match {
          case Some(TP(innerS@Sym(_), LoopOrReflectedLoop(innerL, _))) => 
            val innerRes = innerL.body match {
              case ResultBlock(r) => r
              case _ => return fusionError("MultiCollect+Any fusion failed, no ResultBlock extractor for inner.",
                cSym, cLoop, pSym, cIndex, stm)
            }
            val outerMultiSI = outerMulti.getOrElse((pSym, cIndex))
            addSimpleIndexReplacements(outerMultiSI._1, outerMultiSI._2, innerRes)

            // Note: we cannot have multiple producer fusion here because the consumer
            // - cannot consume multiple producers on its original scope because the
            //   MC doesn't have a fixed output size (multiple producers are all SCs)
            // - cannot consume multiple producers on its new scope inside the MC because
            //   the loops in that scope weren't visible to the consumer before

  // TODO what about effects in MC? stm and None here not correct????

            val innerFused = fuseProdAndCons(innerS, innerL, innerL.size, innerL.v, innerL.body,
              cSym, cLoop, cSize, cIndex, cBody, Nil, stm, None, Some(outerMultiSI))
            (innerS, innerFused) match {
              case (prod@Sym(_), cons@Sym(_)) => FusedSyms.recordFused(prod, cons, cons, Nil, None)
              case _ =>
            }
            innerFused
          case _ => return fusionError("MultiCollect+Any fusion failed, inner loop not recognized.",
              cSym, cLoop, pSym, cIndex, stm)
        }

        cBody match {
          case Reduce(cRes, _, _) => 
            subst += (cIndex -> pIndex)
            onceSubst += (cRes -> innerFused) 
            None
          case _ =>
            onceSubst += (pRes -> innerFused) 
            Some(self_mirror(cSym, wrapInReflect(pLoop, effects)))
        }        

      case _ => 
        return fusionError("Fusion failed, unknown producer type: " + pBody,
          cSym, cLoop, pSym, cIndex, stm)
    }

    (res match {
      case Some(s) => s
      case None => 
        onceSubst += (cSize -> pSize)
        super.transformStm(TP(cSym, wrapInReflect(cLoop, effects)))
    }) match {
      case newSym@Sym(_) =>
        printlog(""); printlog("(VFT) Finished fusion of " + 
          (if (otherProds.isEmpty) "prod: " + pSym else "prods: " + (pSym :: otherProds.map(_._2))) +
          " and cons: " + cSym + ", the resulting fused loop is " + newSym)
        newSym
      case exp => exp
    }
  }

  // TODO can we really continue? should propagate error to avoid registration of non-fused?
  def fusionError(msg: String, cSym: Sym[Any], cLoop: Def[Any], pSym: Sym[Any], cIndex: Sym[Int], stm: Stm): Exp[Any] = {
    printlog("(VFT) Error: " + msg)
    subst -= cIndex
    removeSimpleIndexReplacements(pSym, cIndex)
    super.transformStm(stm)
  }

  // TODO drop effects if not original?
  def wrapInReflect(loop: Def[Any], effects: LoopEffects): Def[Any] = effects match {
    case None => loop
    case Some((summ, deps)) => loop match {
      case Reflect(_, otherSumm, otherDeps) => 
        assert(summ == otherSumm && deps == otherDeps, "(VFT) Error wrong effects")
        loop
      case _ => Reflect(loop, summ, deps)
    }
  }

  // transformation helpers

  def transformExpAsBlock[T](e: Exp[T]): Exp[T] = e match {
    case c: Const[T @unchecked] => c
    case s: Sym[T @unchecked] => reflectBlock(Block(s))
    case _ => e
  }
  
  def getIfThenElse[T:Manifest](pConds: List[Exp[Boolean]], cRes: Exp[T], elseExp: Exp[T]): Exp[T] = {
    val tCRes = transformExpAsBlock(cRes) // first transform the body
    val c = pConds.reduce(boolean_and(_, _))
    val ite = __ifThenElse(c, tCRes, elseExp)(mtype(tCRes.tp), mpos(tCRes.pos))              
    reflectBlock(Block(ite))
  }

  // finding producer helpers

  /** Returns the real producer (if any), the list of reconstructed producers (if any)
   *  and the resulting effects of the fused loop (currently only fuse loops if at most
   *  one is effectful).
   */
  def findProducers[A](cSym: Sym[A], cShape: Exp[Int], cIndex: Sym[Int], cBody: Def[A],
      cEffects: LoopEffects): (Option[OldNewSyms], List[OldNewSyms], LoopEffects) = {
    val cIndexStms = getFatDependentStuff(initialDefs)(List(cIndex))
    
    val allProds = cIndexStms.collect({
      case TP(_, SimpleIndex(pSym@Sym(_), `cIndex`)) => pSym 
    }).map({ pSym => 
      // Scope can change as a result of fusion
      // A) if fusion removes dep on index -> move to outer scope TODO testFusionTransform49
      // B) if outer loops fused inner loops are now in same scope
      // +) can fuse successive inner loops TODO testFusionTransform50
      val sameScope = seenLoops.contains(pSym)
      def notSameScope() = {
        printdbg("(VFT) " + cSym + " not fused with " + pSym + " because not in same level/scope.")
        (false, pSym)
      }
      if (sameScope) {
        (true, pSym)
      } else {
        findDefinition(pSym).flatMap({
          case tp@TP(_, SimpleIndex(otherPSym@Sym(_), otherIndex@Sym(_))) if (otherIndex != cIndex) => 
            getSimpleIndexReplacements(getSubstSym(otherPSym), otherIndex) match {
              case Some(fusedSym@Sym(_)) => 
                printdbg("(VFT) Successive simpleIndex fusion of outer: SimpleIndex(" + pSym + ", " + cIndex + 
                  ") where inner has already been fused: " + tp + " -> " + fusedSym)
                Some((true, fusedSym))
              case _ => 
                None
            }
          case tp =>
            None
        }).getOrElse(notSameScope())
      }
    }).filter(_._1).map(_._2).map({ pSym => 
      (pSym,getSubstSym(pSym))
    }).map({ case (pSym, newPSym) => (pSym, newPSym, consumerGoesOverRangeOfProducer(cShape, pSym))
    }).filter({ case (pSym, newPSym, range) =>
      if (range._1 < 0) {
        printdbg("(VFT) " + cSym + " not fused with " + pSym + " because not same range (" + range._2 + ").")
      }
      range._1 > 0
    }).map({ case (pSym, newPSym, range) =>
      (pSym, newPSym, range._1)
    }).map({ case (pSym, newPSym, range) =>
      val (msg, indep, pEffect) = isIndepConsumer(cSym, cIndex, cBody, cEffects, pSym, newPSym)
      (pSym, newPSym, range, pEffect, msg, indep)
    }).filter({ case (pSym, newPSym, range, pEffect, msg, indep) =>
      if (indep < 0) {
        printdbg("(VFT) " + cSym + " not fused with " + pSym + " because not indep (" + msg + ").")
      }
      indep > 0
    }).sortWith({ 
      (a, b) => a._3 < b._3
    })

    def isPure(effect: LoopEffects): Boolean = !effect.isDefined || mustPure(effect.get._1)
    
    val (prodSyms, combinedEffect) = if (cEffects.isDefined) {
      (allProds, cEffects)
    } else {
      val impureIndex = allProds.indexWhere({ case (_,_,_,pEffect,_,_) => pEffect.isDefined })
      if (impureIndex == -1) {
        (allProds, None) 
      } else {
        val impureProd = allProds(impureIndex)
        val pureTail = allProds.drop(impureIndex + 1).filter({ 
          case (pSym, newPSym, range, pEffect, msg, indep) => 
            val pure = !pEffect.isDefined
            if (!pure) {
              printdbg("(VFT) " + cSym + " not fused with " + pSym + " because " + pSym + " is effectful and "
               + cSym + " is already being fused with other effectful producer " + impureProd._1)
            }
            pure
        })
        (allProds.take(impureIndex + 1) ++ pureTail, impureProd._4)
      }
    }


    val t2 = if (prodSyms.isEmpty) {
      (None, Nil)
    } else if (prodSyms(0)._3 == 1) {
      (Some((prodSyms(0)._1, prodSyms(0)._2)), prodSyms.tail.map(t => (t._1, t._2)))
    } else {
      (None, prodSyms.map(t => (t._1, t._2)))
    }
    (t2._1, t2._2, combinedEffect)
  }

  /** cShape and pSym are originals, this function will check their substitutions too.
   *  Returns = 1 if shape of consumer is prod.length (real producer),
   *  returns > 1 if same range (reconstructed producer),
   *  returns < 0 if not same range, and the string contains debugging output.
   */
  def consumerGoesOverRangeOfProducer(cShape: Exp[Int], pSym: Sym[Any]): (Int, String) = {

    var cShapeSyms: List[Sym[Any]] = Nil
    var cConsts: List[Const[Any]] = Nil

    cShape match {
      case sym@Sym(_) => cShapeSyms ::= sym
      case c@Const(l: Int) => cConsts ::= c
    }
    subst.get(cShape).foreach({
      case sym@Sym(_) => cShapeSyms ::= sym
      case c@Const(l: Int) => cConsts ::= c
    })

    val pSyms = subst.get(pSym) match {
      case Some(sym@Sym(_)) => sym :: List(pSym)
      case _ => List(pSym) // don't know what to do with array constants
    }

    cShapeSyms.foreach({ 
      findDefinition(_).foreach({
        case TP(_, SimpleDomain(sym)) if pSyms.contains(sym) => 
          return (1, "real producer") // real producer
        case _ =>  
      }) 
    })

    val (pShapeSyms, pConsts) = pSyms.map(loopsToFixedOutLengths.get(_))
      .filter(_.isDefined).map(_.get)
      .partition({ case Sym(_) => true case _ => false })

    cConsts.intersect(pConsts).foreach({ c => return (5, "same constant: " + c) })
    cShapeSyms.intersect(pShapeSyms).foreach({ s => return (4, "same symbol: " + s) })

    return (-1, "cConsts: " + cConsts + ", pConsts: " + pConsts + 
        ", cShapeSyms: " + cShapeSyms + ", pShapeSyms: " + pShapeSyms)
  }

  /** Returns > 0 if consumer can be fused with producer, < 0 and an error message otherwise.
   *  In the first case the effects of the producer are also returned.
   */
  def isIndepConsumer[A](cSym: Sym[A], cIndex: Sym[Int], cBody: Def[A], cEffects: LoopEffects,
      pSym: Sym[Any], newPSym: Sym[Any]): (String, Int, LoopEffects) = {
    // Note: no need to check that shape doesn't use any dependencies because it can
    // only have the value of the size of the producer (either SimpleDomain, same constant
    // or same immutable expression).

    val (prod, pEffects) = findDefinition(newPSym) match {
      case Some(TP(`newPSym`, LoopOrReflectedLoop(prod, pEffects))) => (prod, pEffects)
      case d => 
        return ("unknown producer loop " + newPSym + ": " + d, -2, None)
    }

    if (pEffects.isDefined && cEffects.isDefined) {
      return ("effectful consumer cannot be fused with effectful producer", -6, None)
    }

    // SimpleCollect producers result in 1-1 mapping of producer and consumer indices,
    // so consumer can use the index. But when fusing with other collects the consumer
    // cannot depend on index except in SimpleIndex(prod, index).
    val noIndexUse = prod.body match {
      case SimpleCollect(_) => false
      case SimpleCollectIf(_, _) | MultiCollect(_, _) => true
      case b => 
        return ("unknown producer loop type of " + prod + ": " + b, -3, None)
    }

    val (prodFusedSyms, prodEffects) = FusedSyms.getSet(pSym) match {
      case Some(fset) => 
        if (pEffects.isDefined)
          assert(pEffects == fset.getEffects, "(VTO) Error wrong set effects")
        (fset.getSet, fset.getEffects)
      case None => (Set(pSym, getSubstSym(pSym)), pEffects)
    }

    if (prodEffects.isDefined && cEffects.isDefined) {
      return ("effectful consumer cannot be fused with producer that has been fused with another effectful loop", -7, None)
    }

    // No need to check prodEffects (from set), because previous fusion already checked
    // only need to check pEffects of current producer
    val prodEffOk = prodEffectsOk(prod, pEffects, pSym, newPSym)
    if (prodEffOk._2 < 0)
      return (prodEffOk._1, prodEffOk._2, None)


    val consFusedSyms = syms(cBody) ++ cEffects.map(_._2.collect({ case s@Sym(_) => s })).getOrElse(List[Sym[Any]]())
    def substEqu(other: Exp[Any], pSym: Sym[Any]) = ((other == pSym) || (subst.get(other) match {
      case Some(`pSym`) => true
      case _ => false
    }))
    // traverse the statements needed for the loop and check there are no deps
    // Exception to stop traversal as soon as dep found
    case class DependencyException(message: String, value: Int) extends Exception(message)
    try {
      // TODO don't want to do full traversal every time, could cache deps of seen loops?
      // But then would have to do full traversal, not stop early?
      // TODO only do one traversal per consumer (check all producers)?
      GraphUtil.stronglyConnectedComponents(consFusedSyms, { sym: Sym[Any] => 
        findDefinition(sym) match {
          case Some(TP(_, SimpleIndex(other, `cIndex`))) if substEqu(other, pSym) => 
            List() // SimpleIndex will be substituted, so don't count dependency on index or producer 
          case Some(d) => 
            val next = syms(d.rhs).flatMap({ sym => 
                FusedSyms.getSet(sym).map(_.getSet).getOrElse(Set(sym))
              }).flatMap(n => subst.get(n) match { // use the new expressions
                case Some(nn@Sym(_)) => List(nn)
                case Some(Const(_)) => List()
                case _ => List(n)
              })

            if (noIndexUse && next.contains(cIndex)) {
              throw DependencyException("consumer uses index", -4)
            }
            val taboo = next.collectFirst({ case x if prodFusedSyms.contains(x) => x })
            if (taboo.isDefined) {
              throw DependencyException("consumer depends on " + taboo.get, -5)
            }
            next
          case None => List()
        }
      })

      ("", 1, prodEffects)
    } catch {
      case DependencyException(msg, retValue) => (msg, retValue, None)
    }
  }

  /** Returns > 0 if consumer can be fused with producer, < 0 and an error message otherwise.
   */
  def prodEffectsOk(prod: AbstractLoop[_], pEffects: LoopEffects, pSym: Sym[Any], newPSym: Sym[Any]): (String, Int) = {
    if (!pEffects.isDefined) {
      return ("", 1)
    }

    val pIndexStms = getFatDependentStuff(initialDefs)(List(prod.v))
    val resSym = prod.body match {
      case ResultBlock(reifySym@Sym(_)) => findDefinition(reifySym) match {
        case Some(TP(_, Reify(resSym@Sym(_), _, _))) => resSym
        case _ => return ("", 1) 
        // if prod is cons that has been fused with effectful prod,
        // then it has effects (wrapped in reflect) but no reify
        // don't need to check it since its effects all come from a
        // producer that has been checked
      }
      case _ => return ("error: producer result block not found", -10)
    }

    // TODO dedup?
    case class DependencyException(message: String, value: Int) extends Exception(message)
    try {
      GraphUtil.stronglyConnectedComponents(List(resSym), { sym: Sym[Any] => 
        findDefinition(sym) match {
          case Some(tp@TP(_, Reflect(_, _, _))) => 
            throw DependencyException("effectful producers can only be fused if " +
                "the effects are separate from the value of the loop body, but the " + 
                "loop block result depends on " + tp, -12)
          case Some(d) => 
            syms(d.rhs).intersect(pIndexStms).flatMap(n => subst.get(n) match { // use the new expressions
              case Some(nn@Sym(_)) => List(nn)
              case Some(Const(_)) => List()
              case _ => List(n)
            })
          case None => List()
        }
      })

      ("", 1)
    } catch {
      case DependencyException(msg, retValue) => (msg, retValue)
    }
  } */
}