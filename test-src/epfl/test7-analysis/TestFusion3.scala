package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.{OverloadHack, GraphUtil}
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}

trait LoopFusionExtractors extends internal.Expressions { // copied from LoopFusionOpt: LoopFusionCore trait

  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = None
  def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = None
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
  def unapplySimpleCollectIf(e: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = None
  def unapplyMultiCollect[T](a: Def[T]): Option[(Exp[T], Option[() => Exp[T]])] = None
  def unapplyReduce[T](e: Def[T]): Option[(Exp[T], Option[Exp[T]], Option[Boolean])] = None

  object SimpleIndex {
    def unapply(a: Def[Any]): Option[(Exp[Any], Exp[Int])] = unapplySimpleIndex(a)
  }

  object SimpleDomain {
    def unapply(a: Def[Int]): Option[Exp[Any]] = unapplySimpleDomain(a)
  }

  object FixedDomain {
    def unapply(a: Def[Any]): Option[Exp[Int]] = unapplyFixedDomain(a)
  }

  object SimpleCollect {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySimpleCollect(a)
  }

  object SimpleCollectIf {
    def unapply(a: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollectIf(a)
  }

  // first exp is array result, second exp is empty array expression
  object MultiCollect {
    def unapply[T](a: Def[T]): Option[(Exp[T], Option[() => Exp[T]])] = unapplyMultiCollect(a)
  }

  // first exp is reduce block, second is neutral element, third is associativity
  object Reduce {
    def unapply[T](a: Def[T]): Option[(Exp[T], Option[Exp[T]], Option[Boolean])] = unapplyReduce(a)
  }

  object ResultBlock {
    def unapply(a: Def[Any]): Option[Exp[Any]] = a match {
      case SimpleCollect(res) => Some(res)
      case SimpleCollectIf(res, _) => Some(res)
      case MultiCollect(res, _) => Some(res)
      case Reduce(res, _, _) => Some(res)
      case _ => None
    }
  }

  object AllBlocks {
    def unapply(a: Def[Any]): Option[List[Exp[Any]]] = a match {
      case SimpleCollect(res) => Some(List(res))
      case SimpleCollectIf(res, conds) => Some(res :: conds)
      case MultiCollect(res, _) => Some(List(res))
      case Reduce(res, _, _) => Some(List(res))
      case _ => None
    }
  }
}

trait ArrayLoopsExpFixes extends ArrayLoopsExp {
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleLoop(s, i, body) => 
      // First process body, otherwise shape or index could take onceSubst
      val newBody = mirrorFatDef(body, f)
      simpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], newBody)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayElem(y) => ArrayElem(reifyEffects(f.reflectBlock(y)))
    case ReduceElem(y) => ReduceElem(reifyEffects(f.reflectBlock(y)))
    case ArrayIfElem(c,y) =>
      // Cheating, need to process body first, otherwise condition takes
      // onceSubst of body if it's a common subexpression
      val body = reifyEffects(f.reflectBlock(y))
      ArrayIfElem(f.reflectBlock(Block(c)),body)
    case FlattenElem(y) => FlattenElem(reifyEffects(f.reflectBlock(y)))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]

  case class EmptyArray[T]() extends Def[Array[T]]
  def emptyArray[T:Manifest](): Rep[Array[T]] = EmptyArray[T]()
}

trait ScalaGenArrayLoopsFatFixes extends ScalaGenArrayLoopsFat {
  val IR: ArrayLoopsFatExp with ArrayLoopsExpFixes
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case e@EmptyArray() => 
      stream.println("var " + quote(sym) + " = new " + sym.tp + "(0)")
    case _ => super.emitNode(sym, rhs)
  }
}

trait ArrayLoopFusionExtractors extends ArrayLoopsExpFixes with LoopFusionExtractors {

  override def unapplySimpleIndex(e: Def[Any]) = e match {
    case ArrayIndex(a, i) => Some((a,i))
    case _ => super.unapplySimpleIndex(e)
  }

  override def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = e match {
    case ArrayLength(a) => Some(a)
    case _ => super.unapplySimpleDomain(e)
  }


  override def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = e match {
    case SimpleLoop(s, _, ArrayElem(_)) => Some(s)
    case _ => super.unapplyFixedDomain(e)
  }

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(Block(a)) => Some(a) //TODO: block??
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayIfElem(c,Block(a)) => Some((a,List(c))) //TODO: block?
    case _ => super.unapplySimpleCollectIf(e)
  }

  override def unapplyReduce[T](e: Def[T]) = e match {
    case ReduceElem(Block(a: Exp[Double])) => // ReduceElem is sum of doubles...
      Some((a.asInstanceOf[Exp[T]], Some(unit(0.0).asInstanceOf[Exp[T]]), Some(true)))
    // what about ReduceIfElem?
    case _ => super.unapplyReduce(e)
  }

  override def unapplyMultiCollect[T](e: Def[T]) = e match {
    case FlattenElem(Block(a: Exp[T])) => Some((a.asInstanceOf[Exp[T]], Some((() => {
        emptyArray()(mtype(a.tp.typeArguments(0)))
      }).asInstanceOf[() => Exp[T]])))
    case _ => super.unapplyMultiCollect(e)
  }

}

trait MyFusionProg extends NumericOps with PrimitiveOps with LiftNumeric with OrderingOps 
    with BooleanOps with ArrayLoops with Print {
  def test(x: Rep[Int]): Rep[Unit]
}

trait Impl extends MyFusionProg with NumericOpsExp with PrimitiveOpsExp with ArrayLoopsFatExp with PrintExp
    with IfThenElseFatExp with OrderingOpsExp with BooleanOpsExp with ArrayLoopFusionExtractors { self =>
  override val verbosity = 2 // 1: only printlog, 2: also printdbg
  val runner = new Runner { val p: self.type = self }
  runner.run()
}

trait Codegen extends ScalaGenNumericOps with ScalaGenPrimitiveOps
  with ScalaGenPrint with ScalaGenOrderingOps with ScalaGenIfThenElse
  with ScalaGenBooleanOps with ScalaGenArrayLoopsFatFixes { val IR: Impl }

trait FusionCodegen extends Codegen with LoopFusionTransformOpt { val IR: Impl }


trait Runner /*extends internal.ScalaCompile*/ {
  val p: Impl
  def run() = {
    val x = p.fresh[Int]
    val y = p.reifyEffects(p.test(x))

    val codegen = new Codegen { val IR: p.type = p }
    val fusionCodegen = new FusionCodegen { val IR: p.type = p }

    val graph = p.globalDefs
    println("-- full graph")
    graph foreach println

    println("\n-- before transformation")
    codegen.withStream(new PrintWriter(System.out)) {
      codegen.emitBlock(y)
    }
    
    val verticalTransf = new VerticalFusionTransformer {
      val IR: p.type = p
    }
    val horTransf = new HorizontalFusionTransformer {
      val IR: p.type = p
    }

    try {
      println("\n-- vertical transformation")

      val v = verticalTransf.transformBlock(y)
      // TODO how to transmit state more cleanly?
      val vFused = verticalTransf.FusedSyms.getFusedSyms
      println("\n(VFT) all vertically fused: " + vFused._2.mkString("\n"))

      println("\n-- after vertical transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(v)
      }

      println("\n-- horizontal transformation")
      horTransf.setVerticallyFusedSyms(vFused)
      val h = horTransf.transformBlock(v)

      val hFused = horTransf.AllFusionScopes.get
      println("\n(HFT) all horizontally fused: " + hFused.mkString("\n"))


      println("\n-- after horizontal transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(h)
      }

      println("\n-- fusion")

      fusionCodegen.setFusedSyms(hFused)
      fusionCodegen.withStream(new PrintWriter(System.out)) {
        fusionCodegen.emitBlock(h)
      }

      println("-- done")

      // TODO how to run the program? have block, not f: Exp[A] => Exp[B]
      // val test = compile({x: Int => h})
      // test(42)
    } catch {
      case ex =>
      println("error: " + ex)
    }
  }
}

/**
 * Skip statements that don't have symbols which need substitution, unless they contain
 * blocks (need to recurse into blocks).
 */
trait PreservingForwardTransformer extends ForwardTransformer {
  import IR._
  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(sym,rhs) => 
      // Implement optimization suggested in ForwardTransformer:
      // optimization from MirrorRetainBlockTransformer in TestMiscTransform
      // we want to skip those statements that don't have symbols that need substitution
      // however we need to recurse into any blocks
      if (!syms(rhs).exists(subst contains _) && blocks(rhs).isEmpty) {
        if (!globalDefs.contains(stm)) 
          reflectSubGraph(List(stm))
        sym
      } else {
        self_mirror(sym, rhs)
      }
  }
}

trait FusionTransformer extends PreservingForwardTransformer {
  import IR._
  import scala.collection.mutable.{HashMap, HashSet}

  def getSubstSym(sym: Sym[Any],
      subst2: scala.collection.immutable.Map[Exp[Any], Exp[Any]] = subst): Sym[Any] = subst2.get(sym) match {
    case Some(s@Sym(_)) => s
    case _ => sym
  }
}

// Code investigations: levelScope not exposed, has TTPs! Where do they get fattened/unfattened?
  // def traverseBlock[A](block: Block[A]): Unit = {
  //   focusBlock(block) {
  //     focusExactScope(block) { levelScope =>
  //       levelScope foreach traverseStm
  //     }
  //   }
  // }
// availableDefs: unscheduled/floating defs
// initialDefs: all defs, also includes newly mirrored ones
// innerScope: not sure, availableDefs for outermost block, empty for inner loop



trait VerticalFusionTransformer extends FusionTransformer { 
  val IR: Impl with ArrayLoopFusionExtractors
  import IR.{__newVar => _, _}
  import scala.collection.mutable.{HashMap, HashSet}

  // List of the loops on the same level&scope
  val seenLoops = HashSet[Sym[Any]]()

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
      case FixedDomain(domainSym: Sym[Int]) => subst.get(domainSym) match {
        case Some(Const(len: Int))                          => Some(Const(len))
        case Some(sSym: Sym[Int]) if (!isWritableSym(sSym)) => Some(sSym)
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
          case None => None
        }).orElse(simpleIndexReplacements.get((pSym, cIndex))) 
          .getOrElse(super.transformStm(stm))

      case TP(sym, loop: AbstractLoop[Any]) => // Check if we can do loop fusion
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

    def superRep() = { onceSubst += (cSize -> pSize); super.transformStm(TP(cSym, cLoop)) }
    res.getOrElse(superRep())
  }

  def fusionError(msg: String, cSym: Sym[Any], cLoop: Def[Any], pSym: Sym[Any], cIndex: Sym[Int]): Exp[Any] = {
    printlog("(VFT) Error: " + msg)
    subst -= cIndex
    simpleIndexReplacements -= ((pSym, cIndex))
    super.transformStm(TP(cSym, cLoop))
  }

  // transformation helpers

  def transformExpAsBlock[T](e: Exp[T]): Exp[T] = e match {
    case c: Const[T] => c
    case s: Sym[T] => reflectBlock(Block(s))
    case _ => e
  }
  
  def getIfThenElse[T:Manifest](pConds: List[Exp[Boolean]], cRes: Exp[T], elseExp: Exp[T]): Exp[T] = {
    val tCRes = transformExpAsBlock(cRes) // first transform the body
    val c = pConds.reduce(boolean_and(_, _))
    val ite = __ifThenElse(c, tCRes, elseExp)(mtype(tCRes.tp), mpos(tCRes.pos))              
    reflectBlock(Block(ite))
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

trait HorizontalFusionTransformer extends FusionTransformer { 
  val IR: Impl with ArrayLoopFusionExtractors
  import IR.{__newVar => _, _}
  import scala.collection.mutable.{HashMap, HashSet}

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
        case Some(old) => error("FusedSet already had a set for symbol " + otherSym + ": " + old + " = " 
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
        case Some(existingNew) => error("(HFT) Error: existing remap to " + existingNew + 
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

// =========================================
//               LoopFusionOpt
// =========================================

// TODO generally need enable/disable option?
/** Do the actual loop fusion by combining loops ... TODO
  */
trait LoopFusionTransformOpt extends internal.FatBlockTraversal with internal.FatScheduling
    with internal.CodeMotion with SimplifyTransform {
  val IR: LoopsFatExp with IfThenElseFatExp
  import IR._  

  var remainingFusedSyms: List[List[Sym[Any]]] = Nil
  def setFusedSyms(syms: List[List[Sym[Any]]]) = remainingFusedSyms = syms


  // Copy-Paste from LoopFusionOpt.scala
  // TODO what does this do? Do we need it? can we do everything in one pass or
  // do we really need the recursion?
  override def focusExactScopeFat[A](resultB: List[Block[Any]])(body: List[Stm] => A): A = {
    val result0 = resultB.map(getBlockResultFull) flatMap { case Combine(xs) => xs case x => List(x) }
    val (scope,result) = fuseTopLevelLoops(innerScope)(result0)
    innerScope = scope

    // we don't currently propagate a modified result to the parent

    // the caller of traverseBlock will quite likely call getBlockResult afterwards,
    // and if we change the result here, the caller will emit a reference to a sym
    // that doesn't exist (because it was replaced)

    if (result0 != result) {
      printlog("super.focusExactScopeFat with result changed from " + result0 + " to " + result)

      (result0 zip result) foreach {
        case (r0 @ Def(Reify(x, _, _)),Def(Reify(y, u, es))) => 
          if (!x.isInstanceOf[Sym[Any]])
            printlog("non-sym block result: " + x + " to " + y)
          else if (x != y)
            innerScope = innerScope :+ TP(x.asInstanceOf[Sym[Any]], Forward(y))
          innerScope = innerScope :+ TP(r0.asInstanceOf[Sym[Any]], Reify(x,u,es))
          // should rewire result so that x->y assignment is inserted
        case (r0,r) => 
          if (r0 != r) innerScope = innerScope :+ TP(r0.asInstanceOf[Sym[Any]], Forward(r))
      }
    }

    super.focusExactScopeFat(result0.map(Block(_)))(body)
  }
  // End Copy-Paste

  def lines(l: List[Any]) = l.mkString("\n", "\n", "\n")
  def lines[A,B](l: scala.collection.immutable.Map[A,B]) = l.mkString("\n", "\n", "\n")
  /*
    apply fusion to loops at the top level of scope 'currentScope', which has outputs 'result'.
    uses 'getExactScope' provided by CodeMotion to find top level statements.
    returns updated scope and results.
  */
  def fuseTopLevelLoops(currentScope0: List[Stm])(result: List[Exp[Any]]): (List[Stm], List[Exp[Any]]) = {
    var currentScope: List[Stm] = currentScope0

    if (!remainingFusedSyms.isEmpty) { // check there's still fusion needed

      // find loops at current top level
      val loops = getExactScope(currentScope)(result).collect { 
        case e @ TTP(List(sym), _, SimpleFatLoop(_,_,_)) => (sym, e)
      }
      val loopsMap = loops.toMap
      val loopSyms = loops.unzip._1

      // find fusion sets at current top level
      var fusedSyms = remainingFusedSyms.partition({ set => 
        set.exists { s: Sym[Any] => loopSyms.contains(s) }
      }) match { case (thisScope, remaining) => 
        remainingFusedSyms = remaining
        thisScope
      }

      if (!fusedSyms.isEmpty) { // check there's still fusion needed
        
        // fuse TTPs of each set into one fat TTP per set
        val fusedTTPs = fusedSyms.map({ set: List[Sym[Any]] =>
          
          // the TTPs corresponding to this set
          val TTPsToFuse = set.map(sym => loopsMap.get(sym) match {
            case Some(ttp) => ttp
            case None => error("(FTO) ERROR: Fusion failed, loop statement not found for " + sym)
          })
          printlog("(FTO) Fusing these loops into one fat TTP: " + lines(TTPsToFuse))
          fuseTTPs(TTPsToFuse)
        })

        // replace TTPs with their fused loop and reschedule to get correct order
        val fusedSymsFlat = fusedSyms.flatten
        currentScope = fusedTTPs ::: currentScope.filter({ 
          case t@TTP(List(sym), _, _) if fusedSymsFlat.contains(sym) => false
          case _ => true
        })
        currentScope = getSchedule(currentScope)(result)
      }
    }
    
    (currentScope, result)
  }

  def fuseTTPs(TTPsToFuse: List[Stm]): Stm = {
    val (shape, index) = TTPsToFuse(0) match {
      case TTP(_, _, SimpleFatLoop(shape,index,_)) => (shape, index)
    }
    
    // extract info to create fused TTP
    val (lmhs, rhs) = TTPsToFuse.map({ 
      case TTP(lhs, mhs, SimpleFatLoop(shape2,index2,rhs)) => 
        assert(shape == shape2, "(FTO) ERROR: trying to fuse loops of different shapes: " + shape + " != " + shape2)
        assert(index == index2, "(FTO) ERROR: trying to fuse loops of different indices: " + index + " != " + index2)
        ((lhs, mhs), rhs)
      case s => error("(FTO) ERROR: Fusion failed, unrecognized loop statement: " + s)
    }).unzip
    val (lhs, mhs) = lmhs.unzip
    TTP(lhs.flatten, mhs.flatten, SimpleFatLoop(shape, index, rhs.flatten))
  }

}

// =========================================
//                  Tests
// =========================================


class TestFusion3 extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-wip-"

  def testFusionTransform00 = withOutFileChecked(prefix+"fusion00") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // and range not dce'd, hor.
        val range = array(100) { i => i + 1 }
        val odds = array(range.length) { i => range.at(i) + 1 }
        print(range.at(0))
        print(odds.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform01 = withOutFileChecked(prefix+"fusion01") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // and range dce'd
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val odds = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          val y = !x
          (y, range.at(i) + 2) }
        print(odds.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform02 = withOutFileChecked(prefix+"fusion02") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // but range kept around, hor.
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val odds = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          val y = !x
          (y, range.at(i) + 2) }
        print(odds.length)
        print(range.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform03 = withOutFileChecked(prefix+"fusion03") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // not consumer, shape replaced so range is dce'd
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val more = arrayIf(range.length) { i => (i > 50, i > 60) }
        print(more.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform04 = withOutFileChecked(prefix+"fusion04") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // length moved out and replaced with constant, so is consumer, hor.
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val arr2 = array(range.length) { i => range.at(i) + range.length }
        print(arr2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform05 = withOutFileChecked(prefix+"fusion05") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // not consumer, constant replaced
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        val arr3 = array(range.length) { i => range.at(i + 1) }
        print(arr3.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform06 = withOutFileChecked(prefix+"fusion06") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, arr1 is consumer and arr2 is also consumer of range
        // multiple indep consumers fused, range dce'd, hor.
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          (x, range.at(i) * 2) }
        val arr2 = arrayIf(range.length) { i =>
          (range.at(i) < 20, range.at(i) * 3) }
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform07 = withOutFileChecked(prefix+"fusion07") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, arr1 is consumer and arr2 is also consumer of range
        // multiple indep consumers fused, all hor.
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          (x, range.at(i) * 2) }
        val arr2 = arrayIf(range.length) { i =>
          (range.at(i) < 20, range.at(i) * 3) }
        print(range.at(0))
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform08 = withOutFileChecked(prefix+"fusion08") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // successive consumers fused     
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = array(range.length) { i =>
          val x = range.at(i) * 4
          x * 2 }
        val arr2 = arrayIf(arr1.length) { i =>
          val x = arr1.at(i) > 20
          (x, arr1.at(i) * 3) }
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform09 = withOutFileChecked(prefix+"fusion09") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // SimpleCollect + MultiCollect: inner loop not fused
        val range = array(100) { i => i + 1 }
        val range4 = flatten(range.length) { i => array(range.length) { ii => ii + range.at(i) } }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform10 = withOutFileChecked(prefix+"fusion10") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {     
        // nested array arrI not fused with range
        // arrI & arrJ fused
        // arrI dce'd, no hor. because arr1 deps on range
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = array(100) { i =>
          val x = i * 4
          val arrI = array(range.length) { ii =>
            range.at(ii) * 5 + i
          }
          val arrJ = array(arrI.length) { i =>
            arrI.at(i) * 6
          }

          x * arrJ.at(0)
        }
        print(arr1.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform11 = withOutFileChecked(prefix+"fusion11") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // no horizontal fusion across levels
        val arr1 = array(90) { i =>
          val arrI = array(100) { ii =>
            ii + i
          }
          arrI.at(0)
        }
        val arr2 = array(100) { i =>
          i + 2
        }
        print(arr1.at(0))
        print(arr2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform12 = withOutFileChecked(prefix+"fusion12") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {    
        // horizontal fusion in nested loop (not vertical since not consumer)
        val arr1 = array(100) { i =>
          val arrI = array(80) { ii =>
            ii + i
          }
          val arrJ = array(80) { ii =>
            ii * i
          }
          i * arrI.at(0) * arrJ.at(0)
        }
        print(arr1.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform13 = withOutFileChecked(prefix+"fusion13") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // reconstructed consumers fused, no hor.
        // constant
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => range.at(i) + 2 }
        print(range2.at(0))
        // immutable sym
        val l = x + 5
        val range3 = array(l) { i => i + 3 }
        val range4 = array(l) { i => range3.at(i) + 4 }
        print(range4.at(0))
        // TODO test other cases (subst)?
      }
    }
    new Prog with Impl
  }

  def testFusionTransform14 = withOutFileChecked(prefix+"fusion14") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple producers (zip): currently fuse with real only
        // TODO
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = array(range2.length) { i => range.at(i) + range2.at(i) }
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform15 = withOutFileChecked(prefix+"fusion15") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // multiple producers (zip): currently fuse with first reconstr. only
        // TODO
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
        val range3 = array(100) { i => range.at(i) + range2.at(i) }
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform16 = withOutFileChecked(prefix+"fusion16") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // Test all branches of consumerGoesOverRangeOfProducer
        val range = array(x) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) } //  1
        print(range2.length)
        val k = x + 2
        val range3 = array(k) { i => range.at(i) }            // -2
        val range4 = array(k) { i => range3.at(i) }           // 4
        print(range4.length)
        
        val rb = array(100) { i => i + 1 }
        val rb2 = array(rb.length) { i => rb.at(i) }          //  1
        val rb3 = array(rb.length) { i => rb2.at(i) }         //  2
        val rb4 = array(90) { i => rb.at(i) }                 // -3
        val rb5 = array(rb.length) { i => rb4.at(i) }         // -1
        val rb6 = array(100) { i => rb.at(i) }                //  3
        print(rb3.at(1))
        print(rb5.at(1))
        print(rb6.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform17 = withOutFileChecked(prefix+"fusion17") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        val v = range.at(0)
        // not consumer because of v (depends on producer), no hor.
        val range2 = arrayIf(range.length) { i => (range.at(i) > 0, range.at(i) + v) }
        print(range2.at(0))
        // not consumer because of v (depends on producer), hor. with range2
        val range3 = arrayIf(range.length) { i => (range.at(i) + v > 0, i) }
        print(range3.at(0))
        // not consumer because range3 uses index, no hor.
        val range4 = array(range2.length) { i => range2.at(i) + i }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

// TODO fix effects

/*
  def testFusionTransform18 = withOutFileChecked(prefix+"fusion18") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {    
        // TODO effects    
        val range = array(x) { i => i + 1 }
        print(range.length)
        val range2 = array(range.length) { i => print(range.at(i)); 1 } 
        // no  vertical (or hor.) fusion because of ordering of effects
        print(range2.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform19 = withOutFileChecked(prefix+"fusion19") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(100) { i => i + 1 }
        print(range.at(0))
        val range2 = array(100) { i => print(i); 1 } 
        // no  hor fusion because of ordering of effects
        print(range2.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform20 = withOutFileChecked(prefix+"fusion20") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // TODO AAAAHHHH effects of loops aren't tracked properly! range and print(y) get swapped!      
        val y = x + 1
        val range = array(100) { i => print(i); i }
        print(y)
        print(range.at(0))
      }
    }
    new Prog with Impl
  }
*/

  def testFusionTransform21 = withOutFileChecked(prefix+"fusion21") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple consumers all fused, range dce'd, 3 remaining hor. fused
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => range.at(i) + 2 }
        val range3 = array(100) { i => range.at(i) + 3 }
        val range4 = array(range.length) { i => range.at(i) + 4 }
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform22 = withOutFileChecked(prefix+"fusion22") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // multiple consumers not fused:
        // range4 depends on range3, but range already fused with range3,
        // so range4 not fused with range
        // range2&3 cse'd, range4 deps on range, so no hor.
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => range.at(i) }  // fuse
        val range3 = array(100) { i => range2.at(i) } // fuse
        val range4 = array(100) { i => range.at(i) + range3.at(0) } // don't fuse

        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  // Test Vertical Fusion: loop type combinations

  def testFusionTransform23 = withOutFileChecked(prefix+"fusion23") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SimpleCollect + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = array(100) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range3 = arrayIf(range.length) { i => (i > 10, range.at(i) + 3) }
        val range4 = flatten(range.length) { i => array(10) { ii => ii + range.at(i) } }
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) }
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform24 = withOutFileChecked(prefix+"fusion24") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SimpleCollectIf + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = arrayIf(100) { i => (i > 10, i + 1) }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) }
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => ii + range.at(i) } }
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) }
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform25 = withOutFileChecked(prefix+"fusion25") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(SC) + Any fusion
        // Fuse all vertically with range
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) } // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) } // R
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform26 = withOutFileChecked(prefix+"fusion26") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // SCIf + SC with common subexpression between condition&body
        // range dce'd, no hor.
        val range = arrayIf(10) { ii => (1+ii > 5, 1+ii) }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform27 = withOutFileChecked(prefix+"fusion27") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(SCIf) + Any fusion
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => arrayIf(10) { ii => (i+ii > 5, i+ii) } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) } // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) } // R
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform28 = withOutFileChecked(prefix+"fusion28") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(MC) + Any fusion
        // range dce'd, fuse remaining hor. except for inner
        val range = flatten(30) { i => flatten(10) { ii => array(5) {iii => i+ii+iii} } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) } // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) } // R
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform29 = withOutFileChecked(prefix+"fusion29") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // MultiCollect(constant array SCIf) + Any fusion
        // range dce'd, hor. fuse MC-inner with other inner, hor. fuse outer
        val range = flatten(30) { i => arrayIf(10) { ii => (ii > 5, ii+1) } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range.length) { i => (range.at(i) > 20, range.at(i) + 3) } // SCIf
        val range4 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range5 = sum(range.length) { i => unit(2.0) + int_double_value(range.at(i)) } // R
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
        print(range5)
      }
    }
    new Prog with Impl
  }


  // Test Horizontal Fusion Prep Pass

  def testFusionTransform30 = withOutFileChecked(prefix+"fusion30") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // range survives DCE, so hor. fusion with odds
        // rangeH not fused with range since in conflict with odds
        val range = array(100) { i => i + 1 }
        print(range.at(0))
        val rangeH = array(100) { i => i + 2 }
        print(rangeH.at(0))
        val odds = arrayIf(range.length) { i => (range.at(i) > 50, rangeH.at(0) + 1) }
        print(odds.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform31 = withOutFileChecked(prefix+"fusion31") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        val v = range.at(0)
        // not consumer because of v (depends on producer), no hor.
        val range2 = arrayIf(range.length) { i => (range.at(i) > 0, range.at(i) + v) }
        print(range2.at(0))
        // not consumer because of v (depends on producer), hor. with range2
        // new in subst
        val range3 = arrayIf(range.length) { i => (range.at(i) + v > 0, i) }
        print(range3.at(0))
        // consumer of range3, hor. with range2&3
        // subst already has it
        val range4 = array(range3.length) { i => range3.at(i) + 2 }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform32 = withOutFileChecked(prefix+"fusion32") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // regression test for bug when pBody is index, 
        // onceSubst was eaten by index transformation. Fuse vert. & hor.
        // TODO are there other cases when onceSubst expression is used too early?
        val range = arrayIf(x) { i => (i + 10 > 0, i) }
        print(range.at(0))
        val range2 = array(range.length) { i => range.at(i) + 2 }
        print(range2.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform33 = withOutFileChecked(prefix+"fusion33") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {
        // Constant array, successive works, but would like to yield directly to output
        val range = flatten(30) { i => arrayIf(10) { ii => (ii > 5, ii+1) } } 
        val range2 = array(range.length) { i => range.at(i) + 2 } // SC
        val range3 = arrayIf(range2.length) { i => (range2.at(i) > 20, range2.at(i) + 3) } // SCIf

        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform34 = withOutFileChecked(prefix+"fusion34") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // fuse two MC and then successive SC consumer, vert. & hor.
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = flatten(range.length) { i => array(range.at(i)) { ii => range.at(i) + ii } } // MC
        val range3 = array(range2.length) { i => range2.at(i) + 4 }
        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform35 = withOutFileChecked(prefix+"fusion35") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // Fusion happens correctly inside of MC, even when loop is
        // both consumer and producer.
        // Successively fuse all three loops, with existing and generated inner ones.
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = flatten(range.length) { i => 
          val inner = array(range.at(i)) { ii => range.at(i) + ii } 
          array(inner.length) { ii => inner.at(ii) + 3 } 
        }
        val range3 = array(range2.length) { i => range2.at(i) + 4 }
        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform36 = withOutFileChecked(prefix+"fusion36") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // No vert., fuse outer horizontally and then fuse inner horizontally
        val range = flatten(30) { i => array(10) { ii => i+ii } } 
        val range2 = flatten(30) { i => array(10) { ii => i+ii+2 } } 
        val range3 = flatten(30) { i => array(10) { ii => i+ii+3 } } 

        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform37 = withOutFileChecked(prefix+"fusion37") {
    trait Prog extends MyFusionProg with Impl {
      def test(x: Rep[Int]) = {        
        // No vert., fuse outer horizontally and then fuse inner horizontally
        val range = array(30) { i => 
          val inner1 = array(10) { ii => i+ii }
          inner1.at(0)
        } 
        val range2 = array(30) { i => 
          val inner2 = array(10) { ii => i+ii+2 }
          inner2.at(0)
        }
        val range3 = array(30) { i => 
          val inner3 = array(10) { ii => i+ii+3 }
          inner3.at(0)
        } 

        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  // TODO Tiark's test for changing scope/multiple passes
  // Could something going away give us more fusion opportunities?


}

