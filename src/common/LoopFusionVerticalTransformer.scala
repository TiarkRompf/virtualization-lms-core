package scala.virtualization.lms
package common

import util.GraphUtil
import scala.collection.mutable.{HashMap, HashSet}

// TODO documentation
trait LoopFusionVerticalTransformer extends PreservingForwardTransformer { 
  val IR: LoopFusionCore2
  import IR.{__newVar => _, _}

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
    replacement.map( rep => loopsToFixedLengths += (sym -> rep) )
    if (replacement.isDefined && shape != replacement.get)
      subst += (shape -> replacement.get)
    replacement
  }

  // Indexing statements replacement
  // (can't register before because of duplicate substitution error).
  val simpleIndexReplacements = new HashMap[(Sym[Any], Sym[Any]), Exp[Any]]

  object FusedSyms {
    private val fusedSyms = new HashMap[Sym[Any], HashSet[Sym[Any]]]
    private val fusedSubst = new HashMap[Exp[Any], Exp[Any]]
    private lazy val fusedSubstMap = fusedSubst.toMap
    
    def recordRemapped(oldSym: Sym[Any], newSym: Sym[Any]) = {
      fusedSubst.put(oldSym, newSym)  
    }
    def recordFused(pSym: Sym[Any], cSym: Sym[Any], newCSym: Exp[Any], otherProds: List[(Sym[Any], Sym[Any])]) = {
      val set = fusedSyms.get(pSym) match {
        case Some(set) => set
        case None => 
          val set = HashSet(pSym)
          fusedSyms.put(pSym, set)
          set
      }
      set.add(cSym)
      fusedSyms.put(cSym, set)
      fusedSubst.put(cSym, newCSym)
      newCSym match {
        case s@Sym(_) => fusedSyms.put(s, set)
        case _ =>
      }
      otherProds.foreach { prod => 
        (fusedSyms.get(prod._2) match {
          case Some(`set`) => Set() // nothing to do, already in same set
          case Some(pSet) => pSet
          case None => Set(prod._1, prod._2)
        }).foreach({ pSym =>
          set.add(pSym)
          fusedSyms.put(pSym, set)
        })
      }
    }

    def getOldAndNew(pSym: Sym[Any]): Set[Sym[Any]] = {
      val fused = fusedSyms.get(pSym).getOrElse(HashSet(pSym))
      fused.foreach(s => fused.add(getSubstSym(s)))
      fused.toSet
    }

    def getNew(syms: List[Sym[Any]]): List[Sym[Any]] = {
      syms.flatMap(sym => fusedSyms.get(sym).getOrElse(List(sym)))
        .flatMap(n => subst.get(n) match { // use the new expressions
          case Some(nn@Sym(_)) => List(nn)
          case Some(Const(_)) => List()
          case _ => List(n)
        })
    }

    def getFusedSyms: (HashMap[Sym[Any], List[Sym[Any]]]) = {
      fusedSyms.map({ case (k,v) => 
        (getSubstSym(k, fusedSubstMap), 
          v.map(sym => getSubstSym(sym, fusedSubstMap)).toList)
      })
    }
  }

  var onceSubst = scala.collection.immutable.Map.empty[Exp[Any], Exp[Any]]
  override def apply[A](x: Exp[A]): Exp[A] = onceSubst.get(x) match { 
    case Some(y) if y != x => onceSubst -= x; apply(y.asInstanceOf[Exp[A]])
    case _ => super.apply(x)
  }

  override def transformStm(stm: Stm): Exp[Any] = { 
//    println("--transforming: " + stm)
    val transformedSym = stm match {

      case TP(_, SimpleIndex(pSym@Sym(_), cIndex@Sym(_))) => 
        (subst.get(pSym) match {
          case Some(newPSym@Sym(_)) => simpleIndexReplacements.get((newPSym, cIndex))
          case _ => None
        }).orElse(simpleIndexReplacements.get((pSym, cIndex))) 
          .getOrElse(super.transformStm(stm))

      case TP(sym, loop: AbstractLoop[_]) => // Check if we can do loop fusion
        printlog("")
        val (prod, reconstrProds) = findProducers(sym, loop.size, loop.v, loop.body)
        
        def printProd(p: (Sym[Any], Sym[Any])): String = p._2 + (if (p._1 == p._2) "" else " (was " + p._1 + ")")

        if (prod.isDefined) {
          printlog("(VFT) Fusing consumer " + stm + " with real producer: " + printProd(prod.get) +
            (if (!reconstrProds.isEmpty)
              " and then with reconstructed producers: " + reconstrProds.map(printProd(_))
            else ""))
            
        } else if (!reconstrProds.isEmpty) {
          printlog("(VFT) Fusing consumer " + stm + " with reconstructed producer: " + printProd(reconstrProds(0)) +
            (if (!reconstrProds.tail.isEmpty)
              " and then with: " + reconstrProds.tail.map(printProd(_))
            else ""))
        }
        val allProds = prod.map(List(_)).getOrElse(List()) ++ reconstrProds

        val resultLoop = allProds.headOption
          .flatMap({t => findDefinition(t._2)})
          .map({ case TP(s, p: AbstractLoop[_]) => 
            val otherProds = allProds.tail
            val f = fuse(s, p, p.size, p.v, p.body, sym, loop, loop.size, loop.v, loop.body, otherProds)
            FusedSyms.recordFused(s, sym, f, otherProds)
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
            if (remapped) {
              superTransformed match {
                case s@Sym(_) => findDefinition(s) match {
                  case Some(TP(newSym@Sym(_), loop: AbstractLoop[_])) => 
                    seenLoops += newSym
                    recordFixedLengths(newSym, loop)
                    FusedSyms.recordRemapped(sym, newSym)
                  case _ => sys.error("ERROR VFT matching error")
                }
                case _ => sys.error("ERROR VFT matching error")
              }
              printdbg("(VFT) No producers found for " + stm + ", remapping to " + superTransformed + " because of fixed shape")
            } else {
              printdbg("(VFT) No producers found for " + stm)
            }
            superTransformed
        }

      case TP(_, SimpleDomain(sym@Sym(_))) =>
        loopsToFixedLengths.getOrElse(sym, super.transformStm(stm))

      case _ =>
        super.transformStm(stm)
    }
    // if (transformedSym == stm.lhs()(0)) {
    //   println("--identical " + transformedSym)
    // } else {
    //   transformedSym match {
    //     case s@Sym(_) => 
    //       println("--transformed " + stm + " to ")
    //       println("----" + s + " = " + findDefinition(s))
    //     case c =>
    //       println("--transformed " + stm + " to " + c)
    //   }
    // }
    transformedSym
  }

  def fuse[P: Manifest, C: Manifest](
      pSym: Sym[P], pLoop: Def[P], pSize: Exp[Int], pIndex: Sym[Int], pBody: Def[P], 
      cSym: Sym[C], cLoop: Def[C], cSize: Exp[Int], cIndex: Sym[Int], cBody: Def[C],
      otherProds: List[(Sym[Any], Sym[Any])],
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
        cSym, cLoop, pSym, cIndex)
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
            .foreach({ case TP(otherPSym, otherP: AbstractLoop[_]) => otherP.body match {
              case ResultBlock(otherPRes) =>
                printdbg("(VFT) Multiple fusion: remap SimpleIndex(" + otherPSym + ") to " + otherPRes + ".")
                simpleIndexReplacements += ((otherPSym, cIndex) -> otherPRes)
              case _ => 
                otherProds.foreach(t => simpleIndexReplacements -= ((t._2, cIndex)))
                return fusionError("Multiple fusion failed, no ResultBlock extractor for producer.", 
                  cSym, cLoop, pSym, cIndex)
            }})     
        }
        None

      case SimpleCollectIf(pRes,pConds) =>
        var res = cBody match {
          case SimpleCollect(cRes) =>
            printdbg("(VFT) SimpleCollectIf+SimpleCollect fusion: use producer loop with consumer body.")
            onceSubst += (pRes -> cRes)
            Some(self_mirror(cSym, pLoop))
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
              cSym, cLoop, pSym, cIndex)
          case _ =>
        }

        val innerFused = findDefinition(pRes) match {
          case Some(TP(innerS@Sym(_), innerL: AbstractLoop[_])) => 
            val innerRes = innerL.body match {
              case ResultBlock(r) => r
              case _ => return fusionError("MultiCollect+Any fusion failed, no ResultBlock extractor for inner.",
                cSym, cLoop, pSym, cIndex)
            }
            val outerMultiSI = outerMulti.getOrElse((pSym, cIndex))
            simpleIndexReplacements += (outerMultiSI -> innerRes)

            // Note: we cannot have multiple producer fusion here because the consumer
            // - cannot consume multiple producers on its original scope because the
            //   MC doesn't have a fixed output size (multiple producers are all SCs)
            // - cannot consume multiple producers on its new scope inside the MC because
            //   the loops in that scope weren't visible to the consumer before

            val innerFused = fuse(innerS, innerL, innerL.size, innerL.v, innerL.body,
              cSym, cLoop, cSize, cIndex, cBody, Nil, Some(outerMultiSI))
            (innerS, innerFused) match {
              case (prod@Sym(_), cons@Sym(_)) => FusedSyms.recordFused(prod, cons, cons, Nil)
              case _ =>
            }
            innerFused
          case _ => return fusionError("MultiCollect+Any fusion failed, inner loop not recognized.",
              cSym, cLoop, pSym, cIndex)
        }

        cBody match {
          case Reduce(cRes, _, _) => 
            subst += (cIndex -> pIndex)
            onceSubst += (cRes -> innerFused) 
            None
          case _ =>
            onceSubst += (pRes -> innerFused) 
            Some(self_mirror(cSym, pLoop))
        }        

      case _ => 
        return fusionError("Fusion failed, unknown producer type: " + pBody,
          cSym, cLoop, pSym, cIndex)
    }

    (res match {
      case Some(s) => s
      case None => 
        onceSubst += (cSize -> pSize)
        super.transformStm(TP(cSym, cLoop))
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
  def fusionError(msg: String, cSym: Sym[Any], cLoop: Def[Any], pSym: Sym[Any], cIndex: Sym[Int]): Exp[Any] = {
    printlog("(VFT) Error: " + msg)
    subst -= cIndex
    simpleIndexReplacements -= ((pSym, cIndex))
    super.transformStm(TP(cSym, cLoop))
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

  def getSubstSym(sym: Sym[Any],
      subst2: scala.collection.immutable.Map[Exp[Any], Exp[Any]] = subst): Sym[Any] = subst2.get(sym) match {
    case Some(s@Sym(_)) => s
    case _ => sym
  }

  // finding producer helpers
  // return pSym, pSubstSym
  def findProducers[A](cSym: Sym[A], cShape: Exp[Int], cIndex: Sym[Int], cBody: Def[A]): (Option[(Sym[Any], Sym[Any])], List[(Sym[Any], Sym[Any])]) = {
    val cIndexStms = getFatDependentStuff(initialDefs)(List(cIndex))
    
    val prodSyms = cIndexStms.collect({
      case TP(_, SimpleIndex(pSym@Sym(_), `cIndex`)) => pSym 
    }).filter({ pSym => 
      val sameScope = seenLoops.contains(pSym)
      // TODO scope can change as a result of fusion if fusion removes dep on index
      if (!sameScope)
        printdbg("(VFT) " + cSym + " not fused with " + pSym + " because not in same level/scope.")
      sameScope
    }).map({ pSym => 
      (pSym,getSubstSym(pSym))
    }).map({ case (pSym, newPSym) => (pSym, newPSym, consumerGoesOverRangeOfProducer(cShape, pSym))
    }).filter({ case (pSym, newPSym, range) =>
      if (range < 0) {
        printdbg("(VFT) " + cSym + " not fused with " + pSym + " because not same range (" + range + ").")
      }
      range > 0
    }).filter({ case (pSym, newPSym, range) =>
      val (msg, indep) = isIndepConsumer(cSym, cIndex, cBody, pSym, newPSym)
      if (indep < 0) {
        printdbg("(VFT) " + cSym + " not fused with " + pSym + " because not indep (" + msg + ").")
      }
      indep > 0
    }).sortWith((a, b) => a._3 < b._3)

    // TODO EFFECTS!!!

    if (prodSyms.isEmpty) {
      (None, Nil)
    } else if (prodSyms(0)._3 == 1) {
      (Some((prodSyms(0)._1, prodSyms(0)._2)), prodSyms.tail.map(t => (t._1, t._2)))
    } else {
      (None, prodSyms.map(t => (t._1, t._2)))
    }
  }

  /** Returns 1 if shape of consumer is prod.length,
   *  returns > 0  if same range,
   *  returns < 0 otherwise.
   */
  def consumerGoesOverRangeOfProducer(cShape: Exp[Int], pSym: Sym[Any]): Int = {
    // pSym is old producer, since domain of consumer hasn't been transformed yet,
    // and fixed lengths are recorded for old producer too.
    cShape match {
      case shapeSym@Sym(_) => findDefinition(shapeSym) match {
        case Some(TP(_, SimpleDomain(`pSym`))) => 1            // real producer
        case _ => loopsToFixedLengths.get(pSym) match { 
          case Some(`shapeSym`) => 4                              // reconstr. same immutable expression
          case Some(Const(l: Int)) => subst.get(shapeSym) match {
            case Some(Const(`l`)) => 2                            // reconstr. same subst. constant
            case _ => -1
          }
          case _ => -2
        }
      }
      case Const(l: Int) => loopsToFixedLengths.get(pSym) match {
        case Some(Const(`l`)) => 3                                // reconstr. same constant
        case Some(x) => -3
        case None => -5
      }
      case _ => -4
    }
  }

  /* Returns > 0 if consumer can be fused with producer, < 0 otherwise. */
  def isIndepConsumer[A](cSym: Sym[A], cIndex: Sym[Int], cBody: Def[A], 
      pSym: Sym[Any], newPSym: Sym[Any]): (String, Int) = {
    // Note: no need to check that shape doesn't use any dependencies because it can
    // only have the value of the size of the producer (either SimpleDomain, same constant
    // or same immutable expression).

    val prod = findDefinition(newPSym) match {
      case Some(TP(`newPSym`, prod: AbstractLoop[_])) => prod
      case d => 
        return ("unknown producer loop " + newPSym + ": " + d, -2)
    }

    // SimpleCollect producers result in 1-1 mapping of producer and consumer indices,
    // so consumer can use the index. But when fusing with other collects the consumer
    // cannot depend on index except in SimpleIndex(prod, index).
    val noIndexUse = prod.body match {
      case SimpleCollect(_) => false
      case SimpleCollectIf(_, _) | MultiCollect(_, _) => true
      case b => 
        return ("unknown producer loop type of " + prod + ": " + b, -3)
    }

    val prodFusedSyms = FusedSyms.getOldAndNew(pSym)
    val consFusedSyms = syms(cBody)

    // traverse the statements needed for the loop and check there are no deps
    // Exception to stop traversal as soon as dep found
    case class DependencyException(message: String, value: Int) extends Exception(message)
    try {
      // TODO don't want to do full traversal every time, could cache deps of seen loops?
      // But then would have to do full traversal, not stop early?
      GraphUtil.stronglyConnectedComponents(consFusedSyms, { sym: Sym[Any] => 
        findDefinition(sym) match {
          case Some(TP(_, SimpleIndex(`pSym`, `cIndex`))) => List() // OK, don't add dep on pSym or index
          case Some(d) => 

            val next = FusedSyms.getNew(syms(d.rhs))
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

      ("", 1)
    } catch {
      case DependencyException(msg, retValue) => (msg, retValue)
    }
  }
}