package scala.virtualization.lms
package common

import util.GraphUtil
import scala.collection.mutable.{HashMap, HashSet}


// TODO documentation
trait LoopFusionVerticalTransformer extends PreservingForwardTransformer { 
  val IR: LoopFusionCore2
  import IR.{__newVar => _, _}

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

  // --- per scope datastructures ----
  
  // indented printing to show scopes
  var indent = -2
  def printdbg(x: => Any) { if (verbosity >= 2) System.err.println(" " * indent + x) }
  def printlog(x: => Any) { if (verbosity >= 1) System.err.println(" " * indent + x) }

  // List of the loops on the same level&scope
  var seenLoops = HashSet[Sym[Any]]()
  
  // All unfused loops with fixed shape have same index so they
  // can be fused as multiple producers with one consumer
  var fixedShapeToIndex = new HashMap[Exp[Int], Sym[Int]]
  def remapIndexIfFixedShape(shape: Option[Exp[Int]], oldIndex: Sym[Int]): Boolean = shape match {
    case Some(shape) => 
      fixedShapeToIndex.get(shape) match {
        case Some(`oldIndex`) | None => fixedShapeToIndex += (shape -> oldIndex); false
        case Some(newIndex) => subst += (oldIndex -> newIndex); true
      }
    case None => false
  }


  // Each scope has a fresh map, don't want to fuse loops from different levels or scopes
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    val saveSeenLoops = seenLoops
    seenLoops = HashSet[Sym[Any]]()
    val saveFixedShapeToIndex = fixedShapeToIndex
    fixedShapeToIndex = new HashMap[Exp[Int], Sym[Int]]
    indent += 2
    val newBlock = block match {  
      // also check whether toplevel block result symbol
      // needs substitution, so we can change full blocks
      case Block(sym) => Block(apply(sym))
    }
    val res = super.reflectBlock(newBlock)
    indent -= 2
    seenLoops = saveSeenLoops
    fixedShapeToIndex = saveFixedShapeToIndex
    res
  }

  // ---- global datastructures ----

  // loops with fixed lengths (SimpleCollect with constant or immutable expression shape)
  val loopsToFixedLengths = new HashMap[Sym[Any], Exp[Int]]

  // TODO think about this - input vs. output length, result of fusion etc.
  def recordFixedLengths(sym: Sym[Any], loop: AbstractLoop[_]): Option[Exp[Int]] = {
    val shape = loop.size
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
    replacement.map({ rep => loopsToFixedLengths += (sym -> rep) })
    if (replacement.isDefined && shape != replacement.get)
      subst += (shape -> replacement.get)
    replacement
  }

  // Indexing statements replacement
  // (can't register before because of duplicate substitution error).
  val simpleIndexReplacements = new HashMap[(Sym[Any], Sym[Any]), Exp[Any]]

  object FusedSyms {

    class FSet(private var effects: LoopEffects, set: HashSet[Sym[Any]]) {
      def add(s: Sym[Any]) = set.add(s)
      def addEffect(e: LoopEffects): Unit = {
        // TODO asserts?
        assert(!(effects.isDefined && e.isDefined && effects != e), "(VFT) ERROR: Can't fuse effects!")
        e match {
          case Some(_) => this.effects = e
          case None =>
        }
      }
      def foreach(f: Sym[Any] => Unit) = set.foreach(f)
      def getSet = set.toSet
      def getSubstList(map: Map[Exp[Any], Exp[Any]]) = {
        set.map({ sym => getSubstSym(sym, map) }).toList
      }
      def getEffects = effects
      override def toString = "FSet(set = " + set + ", effects: " + effects + ")"
    }

    private val fusedSyms = new HashMap[Sym[Any], FSet]
    private val fusedSubst = new HashMap[Exp[Any], Exp[Any]]
    
    def recordRemapped(oldSym: Sym[Any], newSym: Sym[Any]) = {
      fusedSubst.put(oldSym, newSym)
      fusedSyms.get(oldSym).map({ set =>
        fusedSyms.put(newSym, set)
        set.add(newSym)
      })  
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
      set.add(cSym)
      set.add(newCSym)
      fusedSyms.put(cSym, set)
      fusedSyms.put(newCSym, set)
      fusedSubst.put(cSym, newCSym)
      newCSym match {
        case s@Sym(_) => fusedSyms.put(s, set)
        case _ =>
      }
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

  var onceSubst = scala.collection.immutable.Map.empty[Exp[Any], Exp[Any]]
  override def apply[A](x: Exp[A]): Exp[A] = onceSubst.get(x) match { 
    case Some(y) if y != x => onceSubst -= x; apply(y.asInstanceOf[Exp[A]])
    case _ => super.apply(x)
  }

  override def transformStm(stm: Stm): Exp[Any] = { 
    // println("--transforming: " + stmShort(stm))
    val transformedSym = stm match {

      case TP(_, SimpleIndex(pSym@Sym(_), cIndex@Sym(_))) => 
        (subst.get(pSym) match {
          case Some(newPSym@Sym(_)) => simpleIndexReplacements.get((newPSym, cIndex))
          case _ => None
        }).orElse(simpleIndexReplacements.get((pSym, cIndex)))
        .map({ case s@Sym(_) => findDefinition(s) match {
            case Some(TP(s, Reify(innerSym, _, _))) => 
              // Don't duplicate effects from producer, since effectful
              // producer won't be DCE'd, so it will be horizontally
              // fused in.
              innerSym
            case _ => s
          }
          case c => c
        }).getOrElse(super.transformStm(stm))

      case TP(sym, Reflect(loop: AbstractLoop[_], sum, deps)) =>
        transformLoop(stm, sym, loop, Some((sum, deps)))

      case TP(sym, loop: AbstractLoop[_]) => 
        transformLoop(stm, sym, loop, None)

      case tp@TP(_, SimpleDomain(sym@Sym(_))) =>
        loopsToFixedLengths.get(sym) match {
          case Some(fixedSym) => 
            printdbg("(VFT) Replaced " + tp + " with fixed length: " + fixedSym)
            fixedSym
          case _ => super.transformStm(stm)
        }

      case _ =>
        val superTransformed = super.transformStm(stm)
//        println("+++ passed through: " + stmShort(stm) + " -> " + superTransformed)
        superTransformed
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
        recordFixedLengths(sym, loop)

        newLoop
      case _ => // not fused
        val remapped = remapIndexIfFixedShape(recordFixedLengths(sym, loop), loop.v)
        val superTransformed = super.transformStm(stm)
        val origSym = stm.lhs()(0)
        // loop can be transformed because of subst from outer loop
        val changed = (superTransformed != origSym)
        if (changed) {
          superTransformed match {
            case s@Sym(_) => findDefinition(s) match {
              case Some(TP(newSym@Sym(_), LoopOrReflectedLoop(loop, _))) => 
                seenLoops += newSym
                recordFixedLengths(newSym, loop)
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
        simpleIndexReplacements += ((pSym, cIndex) -> pRes)
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
                simpleIndexReplacements += ((otherPSym, cIndex) -> otherPRes)
              case _ => 
                otherProds.foreach(t => simpleIndexReplacements -= ((t._2, cIndex)))
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
            simpleIndexReplacements += (outerMultiSI -> innerRes)

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
    simpleIndexReplacements -= ((pSym, cIndex))
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

  def getSubstSym(sym: Sym[Any]): Sym[Any] = subst.get(sym) match {
    case Some(s@Sym(_)) => s
    case _ => sym
  }
  def getSubstSym(sym: Sym[Any],
      subst2: Map[Exp[Any], Exp[Any]] = subst): Sym[Any] = subst2.get(sym) match {
    case Some(s@Sym(_)) => s
    case _ => sym
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
            simpleIndexReplacements.get((getSubstSym(otherPSym), otherIndex)) match {
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

    val (pShapeSyms, pConsts) = pSyms.map(loopsToFixedLengths.get(_))
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
  }
}