package scala.virtualization.lms
package common

import util.GraphUtil
import scala.collection.mutable.{HashMap, HashSet}


        // TODO asserts?


trait LoopFusionVerticalTransformer extends PreservingForwardTransformer { 
  val IR: LoopFusionCore2
  import IR.{__newVar => _, _}

  // ---- global datastructures ----

  /** Records all fusion decisions. */
  object FusedSyms {

    /** A set of fused loops, at most one effectful. */
    class FSet(private var effects: LoopEffects, set: HashSet[Sym[Any]]) {
      def add(s: Sym[Any]) = set.add(s)
      def addEffect(e: LoopEffects): Unit = {
        assert(!(effects.isDefined && e.isDefined && effects != e), "(VFT) ERROR: Can't fuse effects!")
        e match { case Some(_) => this.effects = e case _ => }
      }
      def foreach(f: Sym[Any] => Unit) = set.foreach(f)
      def getSet = set.toSet
      def getSubstList(map: Map[Exp[Any], Exp[Any]]) = {
        set.map({ sym => getSubstSym(sym, map) }).toList
      }
      def getEffects = effects
      override def toString = "FSet(set = " + set + ", effects = " + effects + ")"
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
        combinedEffect: LoopEffects) = {
      val set = fusedSyms.get(pSym) match {
        case Some(fset) => fset.addEffect(combinedEffect); fset
        case None =>
          val fset = new FSet(combinedEffect, HashSet(pSym))
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
    }

    def getSet(pSym: Sym[Any]): Option[FSet] = fusedSyms.get(pSym)

    def getFusedSyms(): HashMap[Sym[Any], (List[Sym[Any]], Boolean)] = {
      val fusedSubstMap = fusedSubst.toMap
      val newMap = new HashMap[Sym[Any], (List[Sym[Any]], Boolean)]()
      val setSet: Set[FSet] = fusedSyms.values.toSet

      setSet.map({ fset => 
          (fset.getSubstList(fusedSubstMap), fset.getEffects.isDefined)
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
  val simpleIndexReplacements = new HashMap[(Sym[Any], Sym[Any]), Exp[Any]]
  def addSimpleIndexReplacement(cSym: Sym[Any], indexSym: Sym[Any], exp: Exp[Any]) =
    simpleIndexReplacements += ((cSym, indexSym) -> getBlockResult(Block(exp)))
  def removeSimpleIndexReplacement(cSym: Sym[Any], indexSym: Sym[Any]) =
    simpleIndexReplacements -= ((cSym, indexSym))
  def getSimpleIndexReplacements(cSym: Sym[Any], indexSym: Sym[Any]) =
    simpleIndexReplacements.get((cSym, indexSym))


  // --- extractors/helpers ---

  type OldNewSyms = (Sym[Any], Sym[Any])
  type LoopEffects = Option[(Summary, List[Exp[Any]])]
  object LoopOrReflectedLoop {
    def unapply(a: Def[Any]): Option[(AbstractLoop[_], LoopEffects)] = a match {
      case Reflect(loop: AbstractLoop[_], summ, deps) => Some((loop, Some((summ, deps))))
      case loop: AbstractLoop[_] => Some((loop, None))
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


  // --- per scope datastructures ----
  
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

  // --- transformation ---

  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    // save per scope datastructures
    val saveSeenLoops = seenLoops
    seenLoops = HashSet[Sym[Any]]()
    val saveFixedLengthIndex = fixedLengthIndex
    fixedLengthIndex = None
    indent += 2

    // also check whether toplevel block result symbol
    // needs substitution, so we can change full blocks
    val newBlock = Block.unapply(block).map({sym => Block(apply(sym))}).getOrElse(block)
    val res = super.reflectBlock(newBlock)

    // restore per scope datastructures
    indent -= 2
    seenLoops = saveSeenLoops
    fixedLengthIndex = saveFixedLengthIndex
    res
  }


  override def transformStm(stm: Stm): Exp[Any] = { 
    // println("--transforming: " + stmShort(stm))
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

      case _ => super.transformStm(stm)
    }
    // if (transformedSym == stm.lhs()(0)) {
    //   println("--identical " + transformedSym)
    // } else {
    //   transformedSym match {
    //     case s@Sym(_) => 
    //       println("--transformed " + stmShort(stm) + " to ")
    //       println("----" + s + " = " + findDefinition(s))
    //     case c =>
    //       println("--transformed " + stmShort(stm) + " to " + c)
    //   }
    // }
    transformedSym
  }

  sealed abstract class FusionInfo(cons: Sym[Any], notFused: List[(Sym[Any], String)]) {
    def printLogBefore: Unit
    def printLogAfter(result: FusionOutcome): Unit
    def getProds: List[Sym[Any]]
    def setNotFused(list: List[(Sym[Any], String)])
  }
  object FusionInfo1to1 {
    def unapply(o: Any): Option[(Sym[Any], Sym[Any], List[(Sym[Any], String)])] = o match {
      case x: FusionInfo1to1 => Some((x.prod, x.cons, x.notFused))
      case _ => None
    } 
  }
  sealed abstract class FusionInfo1to1(val prod: Sym[Any], val cons: Sym[Any], var notFused: List[(Sym[Any], String)])
      extends FusionInfo(cons, notFused) {
    override def printLogBefore = {
      notFused.foreach({ case (prod, reason) => printlog("(VFT) Not fused prod " + prod + " with cons " + cons + " because " + reason) })
      printlog("(VFT) Fusing prod " + prod + " with cons " + cons + ". Type: " + this)
    }
    override def printLogAfter(result: FusionOutcome) = (result: @unchecked) match {
      case FusionResult(fusedSym) => printlog("(VFT) Fused prod " + prod + " with cons " + cons + ", fused sym: " + fusedSym)
    }
    override def getProds = List(prod)
    override def setNotFused(list: List[(Sym[Any], String)]) = notFused = list
  }
  object FusionInfoManyto1 {
    def unapply(o: Any): Option[(Sym[Any], List[Sym[Any]], Sym[Any], List[(Sym[Any], String)])] = o match {
      case x: FusionInfoManyto1 => Some((x.prod, x.otherProds, x.cons, x.notFused))
      case _ => None
    } 
  }
  sealed abstract class FusionInfoManyto1(val prod: Sym[Any], val otherProds: List[Sym[Any]], val cons: Sym[Any],
      val notFused: List[(Sym[Any], String)]) extends FusionInfo(cons, notFused) {
    override def printLogBefore = {
      notFused.foreach({ case (prod, reason) => printlog("(VFT) Not fused prod " + prod + " with cons " + cons + " because " + reason) })
      printlog("(VFT) Fusing prod " + prod + " with cons " + cons + " and other prods: " + otherProds + ". Type: " + this)
    }
    override def printLogAfter(result: FusionOutcome) = (result: @unchecked) match {
      case FusionResult(fusedSym) => printlog("(VFT) Fused prod " + prod + " and other prods " + otherProds + " with cons " + cons + ", fused sym: " + fusedSym)
    }
    override def getProds = prod :: otherProds
    override def setNotFused(list: List[(Sym[Any], String)]) = sys.error("FusionInfoManyto1.notFused is not mutable")
  }

  case class NoFusionInfo(cons: Sym[Any], cLoop: AbstractLoop[_], notFused: List[(Sym[Any], String)])
      extends FusionInfo(cons, notFused) {
    override def printLogBefore = {
      notFused.foreach({ case (prod, reason) => printlog("(VFT) Not fused prod " + prod + " with cons " + cons + " because " + reason) })
      printlog("(VFT) No producers found for cons " + cons)
    }
    override def printLogAfter(result: FusionOutcome) = (result: @unchecked) match {
      case NoFusionRemapped(remappedSym, remapReason) =>
        if (remappedSym != cons)
          printlog("(VFT) Cons " + cons + " not fused but mirrored to " + remappedSym
            + " because " + remapReason)
    }
    override def getProds = Nil
    override def setNotFused(list: List[(Sym[Any], String)]) = sys.error("NoFusionInfo.notFused is not mutable")
  }

  // case class Empty_McBmcBr(prod: Sym[Any], cons: Sym[Any]) extends FusionInfo1to1(prod, cons)
  // case class Empty_For(prod: Sym[Any], cons: Sym[Any]) extends FusionInfo1to1(prod, cons)
  // case class Single_McFor(prod: Sym[Any], cons: Sym[Any], 
  //   singleVal: Sym[Any], cBlock: Block[Any], cIndex: Sym[Int]) extends FusionInfo1to1(prod, cons)
  // case class Single_Bmc(prod: Sym[Any], cons: Sym[Any], 
  //   singleVal: Sym[Any], cBlock: Block[Any], cIndex: Sym[Int]) extends FusionInfo1to1(prod, cons)
  // case class Mc_Mc(prod: Sym[Any], cons: Sym[Any]) extends FusionInfo1to1(prod, cons)
  // case class Mc_For(prod: Sym[Any], cons: Sym[Any]) extends FusionInfo1to1(prod, cons)
  // case class Mcsingle_BmcBr(prod: Sym[Any], cons: Sym[Any]) extends FusionInfo1to1(prod, cons)
  // TODO case class inheritance needs override val? seriously?
  case class Mcsingle_Multi(pElem: Exp[Any], pIndex: Sym[Int], cIndex: Sym[Int], 
      override val prod: Sym[Any], oldPSym: Sym[Any], override val cons: Sym[Any])
    extends FusionInfo1to1(prod, cons, Nil)
  case class ManyMcsingle_Multi(pElem: Exp[Any], pIndex: Sym[Int], cIndex: Sym[Int], 
      override val prod: Sym[Any], oldPSym: Sym[Any], override val cons: Sym[Any],
      val oProds: List[(Exp[Any], Sym[Any])], override val notFused: List[(Sym[Any], String)])
    extends FusionInfoManyto1(prod, oProds.unzip._2, cons, notFused)

  sealed abstract class FusionOutcome(val transformed: Exp[Any])
  case class FusionResult(fusedSym: Exp[Any]) extends FusionOutcome(fusedSym)
  case class NoFusionRemapped(remappedSym: Exp[Any], remapReason: String) extends FusionOutcome(remappedSym)

  def transformLoop[A:Manifest, B](stm: Stm, sym: Sym[A], loop: AbstractLoop[B], 
      effects: LoopEffects): Exp[Any] = {

    printlog("")
    seenLoops += sym
    val fusionInfo = findProducers(sym, loop, loop.size, loop.v, loop.body, effects)
    fusionInfo.printLogBefore

    val fusionOutcome = doFusion(fusionInfo, stm)
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

  /** RealProd: shape of consumer loop is prod.length
   *  ReconstrProd: length of producer output collection equals shape of consumer loop
   *  PartialProd: cannot conclude any of the above, consumer might only
   *    iterate over part of the producer collection
   */
  object ProdType extends Enumeration {
    type ProdType = Value
    val RealProd, ReconstrProd, PartialProd = Value
  }
  import ProdType._
  def intVal(prodType: ProdType) = prodType match {
    case RealProd => 1 case ReconstrProd => 2 case PartialProd => 3
  }
  def toString(prodType: ProdType) = prodType match {
    case RealProd => "real" case ReconstrProd => "reconstructed" case PartialProd => "partial"
  }

  def findProducers[A](cSym: Sym[A], cLoop: AbstractLoop[_], cShape: Exp[Int], cIndex: Sym[Int], cBody: Def[A],
      cEffects: LoopEffects): FusionInfo = {
    // TODO check range, effects, other, multiple prods, deps, index use
    val cIndexStms = getFatDependentStuff(initialDefs)(List(cIndex))

    
    var listOfNotFused: List[(Sym[Any], String)] = Nil // TODO how to return this?
    
    val producers = cIndexStms.collect({
      case TP(_, SimpleIndex(pSym@Sym(_), `cIndex`)) => pSym 
    }).filter({ pSym => 
      if (!seenLoops.contains(pSym)) {
        listOfNotFused ::= (pSym, "not in same exact scope"); false
      } else true
    }).map({ case pSym => 
      val (prodType, msg) = consumerGoesOverRangeOfProducer(cShape, pSym)
      prodType match {
        case PartialProd => 
          listOfNotFused ::= (pSym, "consumer loop might not iterate over full range of producer collection")
          // TODO could have callback, but now not shape of prod, only for SingleCollects?
          // fusePartialConsumers(pSym, findDefinition(pSym), cSym, findDefinition(cSym))
        case _ =>
      }
      (pSym, prodType)
    }).filter({ case (_, PartialProd) => false case _ => true
    }).sortWith({ (a, b) => intVal(a._2) < intVal(b._2)
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
        case Some((sym, msg)) => listOfNotFused ::= (sym, msg); false
      }
    }).map({ case (oldPSym, newPSym, prodType) => (oldPSym, newPSym) match {
      case (_, Def(LoopOrReflectedLoop(pLoop, effects))) => pLoop.body match {
        case MultiCollect(Def(SingletonColl(pElem))) => 
          val pIndex = pLoop.v
          cBody match {
            case MultiCollect(_) => 
              (Mcsingle_Multi(pElem, pLoop.v, cIndex, newPSym, oldPSym, cSym), prodType)
      
            case _ => sys.error("TODO findProducers cons: " + cSym)
          }
        case _ => sys.error("TODO findProducers prod body: " + pLoop.body)
      }
      case _ => sys.error("TODO findProducers prod: " + newPSym)
    }})

    def addNotFused(notFused: List[(FusionInfo, ProdType)], fusedProd: Sym[Any], 
        fusedPType: ProdType, fInfo: Option[FusionInfo] = None) = {
      notFused.flatMap({ case (fi, _) => fi.getProds }).foreach({ notFusedProd =>
        val msg = "the consumer is already being fused with the " + toString(fusedPType) + 
          " producer " + fusedProd + ". Can only fuse multiple producers if all are " + 
          "MultiCollect(Singleton) and have the same fixed shape " +
          "(and thus have been mirrored to use the same index symbol)"
        listOfNotFused ::= ((notFusedProd, msg))
      })
      fInfo.foreach(_.setNotFused(listOfNotFused))
    }

    producers match {
      case Nil => NoFusionInfo(cSym, cLoop, listOfNotFused)
      case prod :: Nil => prod._1.notFused = listOfNotFused; prod._1
      case (mcsm@Mcsingle_Multi(pElem,pIndex,cIndex,newPSym, oldPSym, cSym), firstPType) :: list => 
        val canFuseRight = list.map({
          case (Mcsingle_Multi(opElem,`pIndex`,_,_,ooldPSym,_), _) => Right((opElem, ooldPSym))
          case other => Left(other)
        })
        addNotFused(canFuseRight.collect({ case Left(notFused) => notFused }), oldPSym, firstPType)
        val otherProds = canFuseRight.collect({ case Right(toFuse) => toFuse })
        ManyMcsingle_Multi(pElem, pIndex, cIndex, newPSym, oldPSym, cSym, otherProds, listOfNotFused)
      case (first, firstPType) :: list => 
        val fusedProd: Sym[Any] = first.getProds.head
        addNotFused(list, fusedProd, firstPType, Some(first))
        first
    }
  }

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
        return (RealProd, "real producer") // real producer
    })

    val (pShapeSyms, pConsts) = pSyms.map(loopsToFixedOutLengths.get(_))
      .filter(_.isDefined).map(_.get)
      .partition({ case Sym(_) => true case _ => false })

    cConsts.intersect(pConsts).foreach({ c => return (ReconstrProd, "same constant: " + c) })
    cShapeSyms.intersect(pShapeSyms).foreach({ s => return (ReconstrProd, "same symbol: " + s) })

    return (PartialProd, "cConsts: " + cConsts + ", pConsts: " + pConsts + 
        ", cShapeSyms: " + cShapeSyms + ", pShapeSyms: " + pShapeSyms)
  }

  def doFusion(fusionInfo: FusionInfo, stm: Stm): FusionOutcome = {
    val fusionOutcome = fusionInfo match {
      case NoFusionInfo(sym, loop, _) => 
        if (remapIndexIfFixedLength(recordFixedOutLengths(sym, loop), loop.v))
          NoFusionRemapped(super.transformStm(stm), "of fixed shape")
        else
          NoFusionRemapped(super.transformStm(stm), "of previous substitutions or effects")
      
      case Mcsingle_Multi(pElem, pIndex, cIndex, pSym, oldPSym, cSym) =>
        subst += (cIndex -> pIndex) // otherProds have same pIndex
        addSimpleIndexReplacement(oldPSym, cIndex, pElem)
        FusionResult(super.transformStm(stm))

      case ManyMcsingle_Multi(pElem, pIndex, cIndex, _, oldPSym, _, otherProds, _) =>
        subst += (cIndex -> pIndex) // otherProds have same pIndex
        addSimpleIndexReplacement(oldPSym, cIndex, pElem)
        otherProds.foreach({ case (opElem, ooldPSym) => 
          addSimpleIndexReplacement(ooldPSym, cIndex, opElem) })
        FusionResult(super.transformStm(stm))
      
      case _ => sys.error("TODO doFusion")
    }
    (fusionInfo, fusionOutcome) match {
      case (NoFusionInfo(cons, _, _), NoFusionRemapped(newSym, _)) =>
        FusedSyms.recordRemapped(cons, newSym)
      case (FusionInfo1to1(prod, cons, _), FusionResult(fusedSym@Sym(_))) => 
        FusedSyms.recordFused(prod, cons, fusedSym, Nil, None)
      case (FusionInfoManyto1(prod, otherProds, cons, _), FusionResult(fusedSym@Sym(_))) => 
        FusedSyms.recordFused(prod, cons, fusedSym, 
          otherProds.map({ old => (old, getSubstSym(old)) }), None)
      case err => sys.error("doFusion unknown case: " + err)
    }
    fusionOutcome
  }

  def notIndep[A](cSym: Sym[A], cIndex: Sym[Int], indexIsTaboo: Boolean, cBody: Def[A],
      cEffects: LoopEffects, pSym: Sym[Any], newPSym: Sym[Any]): Option[(Sym[Any], String)] = {
    // Note: no need to check that shape doesn't use any dependencies because it can
    // only have the value of the size of the producer (either SimpleDomain, same constant
    // or same immutable expression). TODO partial

    val prodLoop = findDefinition(newPSym) match {
      case Some(TP(`newPSym`, LoopOrReflectedLoop(loop, pEffects))) => loop
      case d => return Some((pSym, "no loop definition found: " + d))
    }
    val prodFusedSyms = FusedSyms.getSet(pSym).map(_.getSet)
      .getOrElse(Set(pSym, getSubstSym(pSym)))
    val consFusedSyms = syms(cBody) ++ cEffects.map(_._2.collect({ case s@Sym(_) => s }))
      .getOrElse(List[Sym[Any]]())

    def substEqu(other: Exp[Any], pSym: Sym[Any]) = 
      ((other == pSym) || (subst.get(other)).map(_ == pSym).getOrElse(false))

    // traverse the statements needed for the loop and check there are no deps
    // Exception to stop traversal as soon as dep found
    case class DepException(message: String) extends Exception(message)
    try {
      // TODO don't want to do full traversal every time, could cache deps of seen loops?
      // But then would have to do full traversal, not stop early?
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

            next.collectFirst({ 
              case x if prodFusedSyms.contains(x) => throw DepException("consumer depends on producer through " + x)
              case `cIndex` if indexIsTaboo => throw DepException("consumer uses index")
            })
            next
          case None => List()
        }
      })

      None
    } catch {
      case DepException(msg) => Some((pSym, msg))
    }
  }

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