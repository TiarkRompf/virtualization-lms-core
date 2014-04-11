package scala.virtualization.lms
package common

import util.GraphUtil
import scala.collection.mutable.{HashMap, HashSet}

// TODO documentation
trait LoopFusionVerticalTransformer extends PreservingForwardTransformer { 
  val IR: LoopFusionCore2
  import IR.{__newVar => _, _}

  // List of the loops on the same level&scope
  var seenLoops = HashSet[Sym[Any]]()

  // Each scope has a fresh map, don't want to fuse loops from different levels or scopes
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    val save = seenLoops
    seenLoops = HashSet[Sym[Any]]()
    val newBlock = block match {  
      // also check whether toplevel block result symbol
      // needs substitution, so we can change full blocks
      case Block(sym) => Block(apply(sym))
    }
    val res = super.reflectBlock(newBlock)
    seenLoops = save
    res
  }

  // loops with fixed lengths (SimpleCollect with constant or immutable expression shape)
  val loopsToFixedLengths = new HashMap[Sym[Any], Exp[Int]]

  // TODO think about this - input vs. output length, result of fusion etc.
  def recordFixedLengths(sym: Sym[Any], loop: AbstractLoop[_]) = {
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
  }

  // Indexing statements replacement
  // (can't register before because of duplicate substitution error).
  val simpleIndexReplacements = new HashMap[(Sym[Any], Sym[Any]), Exp[Any]]

  object FusedSyms {
    private val fusedSyms = new HashMap[Sym[Any], Int]
    private var fusedSymsSets: List[HashSet[Sym[Any]]] = Nil
    private val fusedSubst = new HashMap[Exp[Any], Exp[Any]]
    private lazy val fusedSubstMap = fusedSubst.toMap

    private def getSet(i: Int): HashSet[Sym[Any]] = {
      fusedSymsSets(fusedSymsSets.length - 1 - i)
    }
    
    def record(pSym: Sym[Any], cSym: Sym[Any], newCSym: Exp[Any]) = {
      val index = fusedSyms.get(pSym) match {
        case Some(pIndex) => pIndex
        case None => 
          val s = fusedSymsSets.length
          fusedSymsSets = HashSet(pSym) :: fusedSymsSets
          fusedSyms.put(pSym, s)
          s
      }
      getSet(index).add(cSym)
      fusedSubst.put(cSym, newCSym)
      newCSym match {
        case s@Sym(_) => fusedSyms.put(s, index)
        case _ =>
      }
      fusedSyms.put(cSym, index)
    }

    def getOldAndNew(pSym: Sym[Any]): Set[Sym[Any]] = {
      val fused = fusedSyms.get(pSym).map(getSet(_)).getOrElse(HashSet(pSym))
      fused.foreach(s => fused.add(getSubstSym(s)))
      fused.toSet
    }

    def getFusedSyms: (HashMap[Sym[Any], Int], List[List[Sym[Any]]]) = {
      val fusedSyms2 = fusedSyms.map({ case (k,v) => 
        (getSubstSym(k, fusedSubstMap),v) })
      val fusedSymsSets2 = fusedSymsSets.map(set => 
        set.map(sym => getSubstSym(sym, fusedSubstMap)).toList)
      (fusedSyms2, fusedSymsSets2)
    }
  }

  var onceSubst = scala.collection.immutable.Map.empty[Exp[Any], Exp[Any]]
  override def apply[A](x: Exp[A]): Exp[A] = onceSubst.get(x) match { 
    case Some(y) if y != x => onceSubst -= x; apply(y.asInstanceOf[Exp[A]])
    case _ => super.apply(x)
  }

  override def transformStm(stm: Stm): Exp[Any] = { 
    // println("--transforming: " + stm)
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

        // TODO eagerly remap producer indices to same symbol for same ranges
        // so we can then fuse multiple producers (zip)
        if (prod.isDefined) {
          printlog("(VFT) Fusing consumer " + stm + " with real producer: " + printProd(prod.get))
          if (!reconstrProds.isEmpty)
            printdbg("(VFT) TODO could then fuse with reconstructed producers: " + reconstrProds.map(printProd(_)))
        } else if (!reconstrProds.isEmpty) {
          printlog("(VFT) Fusing consumer " + stm + " with reconstructed producer: " + printProd(reconstrProds(0)))
          if (!reconstrProds.tail.isEmpty)
            printdbg("(VFT) TODO could then fuse with reconstructed producers: " + reconstrProds.tail.map(printProd(_)))
        } else {
          printdbg("(VFT) No producers found for " + stm)
        }

        val resultLoop = prod.orElse(reconstrProds.headOption)
          .flatMap({t => findDefinition(t._2)})
          .map({ case TP(s, p: AbstractLoop[_]) => 
            val f = fuse(s, p, p.size, p.v, p.body, sym, loop, loop.size, loop.v, loop.body)
            FusedSyms.record(s, sym, f)
            f
          })     

        // bookkeeping
        seenLoops += sym
        recordFixedLengths(sym, loop)

        resultLoop.getOrElse(super.transformStm(stm))

      case TP(_, SimpleDomain(sym@Sym(_))) =>
        loopsToFixedLengths.getOrElse(sym, super.transformStm(stm))

      case _ =>
        super.transformStm(stm)
    }
    // if (transformedSym == stm.lhs()(0)) {
    //   println("--identical " + transformedSym)
    // } else {
    //   println("--transformed " + stm + " to ")
    //   println("----" + transformedSym + " = " + findDefinition(transformedSym.asInstanceOf[Sym[Any]]))
    // }
    transformedSym
  }

  def fuse[P: Manifest, C: Manifest](
      pSym: Sym[P], pLoop: Def[P], pSize: Exp[Int], pIndex: Sym[Int], pBody: Def[P], 
      cSym: Sym[C], cLoop: Def[C], cSize: Exp[Int], cIndex: Sym[Int], cBody: Def[C],
      outerMulti: Option[(Sym[Any], Sym[Int])] = None): Exp[Any] = {
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
      case SimpleCollect(_) => printdbg("(VFT) SimpleCollect+Any fusion: nothing more to do."); None

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
            val innerFused = fuse(innerS, innerL, innerL.size, innerL.v, innerL.body,
              cSym, cLoop, cSize, cIndex, cBody, Some(outerMultiSI))
            (innerS, innerFused) match {
              case (prod@Sym(_), cons@Sym(_)) => FusedSyms.record(prod, cons, cons)
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
        printlog("(VFT) Finished fusion of prod: " + pSym + " and cons: " + cSym + ", the resulting fused loop is " + newSym)
        newSym
      case exp => exp
    }
  }

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

  def findProducers[A](cSym: Sym[A], cShape: Exp[Int], cIndex: Sym[Int], cBody: Def[A]): (Option[(Sym[Any], Sym[Any])], List[(Sym[Any], Sym[Any])]) = {
    val cIndexStms = getDependentStuff(cIndex)
    
    val prodSyms = cIndexStms.collect({
      case TP(_, SimpleIndex(pSym@Sym(_), `cIndex`)) => pSym 
    }).filter({ pSym => 
      val sameScope = seenLoops.contains(pSym)
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
        case _ => -3
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

    // find the consumer body
    val cBlocks = AllBlocks.unapply(cBody) match {
      case Some(l) => l.collect({ case s@Sym(_) => s })
      case None => return ("unknown consumer loop type: " + cBody, -1)
    }
    
    // traverse the statements needed for the loop and check there are no deps
    // Exception to stop traversal as soon as dep found
    case class DependencyException(message: String, value: Int) extends Exception(message)
    try {
      // TODO don't want to do full traversal every time, could cache deps of seen loops?
      // But then would have to do full traversal, not stop early?
      GraphUtil.stronglyConnectedComponents(cBlocks, { sym: Sym[Any] => 
        findDefinition(sym) match {
          case Some(TP(_, SimpleIndex(`pSym`, `cIndex`))) => List() // OK, don't add dep on pSym or index
          case Some(d) => 
            val next = syms(d.rhs).flatMap(n => subst.get(n) match { // use the new expressions
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

      ("", 1)
    } catch {
      case DependencyException(msg, retValue) => (msg, retValue)
    }
  }
}