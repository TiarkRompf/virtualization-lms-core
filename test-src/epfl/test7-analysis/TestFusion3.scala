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
  def unapplyMultiCollect(e: Def[Any]): Option[(Exp[Any], Option[Exp[Any]])] = None
  def unapplyReduce(e: Def[Any]): Option[(Exp[Any], Option[Exp[Any]], Option[Boolean])] = None

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
    def unapply(a: Def[Any]): Option[(Exp[Any], Option[Exp[Any]])] = unapplyMultiCollect(a)
  }

  // first exp is reduce block, second is neutral element, third is associativity
  object Reduce {
    def unapply(a: Def[Any]): Option[(Exp[Any], Option[Exp[Any]], Option[Boolean])] = unapplyReduce(a)
  }
}

trait ArrayLoopFusionExtractors extends ArrayLoopsExp with LoopFusionExtractors {

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

  override def unapplyMultiCollect(e: Def[Any]) = e match {
    case FlattenElem(Block(a)) => Some((a, None)) // need empty array
    case _ => super.unapplyMultiCollect(e)
  }

  override def unapplyReduce(e: Def[Any]) = e match {
    case ReduceElem(Block(a)) => Some((a, Some(unit(0)), Some(true)))
    // what about ReduceIfElem?
    case _ => super.unapplyReduce(e)
  }  

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleLoop(s, i, body) => 
      simpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], mirrorFatDef(body, f))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case ArrayElem(y) => ArrayElem(reifyEffects(f.reflectBlock(y)))
    case ReduceElem(y) => ReduceElem(reifyEffects(f.reflectBlock(y)))
    // TODO argh... why you no blocks?
    case ArrayIfElem(c,y) => ArrayIfElem(f.reflectBlock(Block(c)),reifyEffects(f.reflectBlock(y)))
    case FlattenElem(y) => FlattenElem(reifyEffects(f.reflectBlock(y)))
    case _ => super.mirrorFatDef(e,f)
  }).asInstanceOf[Def[A]]
}

trait MyFusionProg extends NumericOps with ArrayLoops with Print {
  def test(x: Rep[Int]): Rep[Unit]
}

trait Impl extends MyFusionProg with NumericOpsExp with ArrayLoopsFatExp with PrintExp
    with IfThenElseFatExp with OrderingOpsExp with BooleanOpsExp with ArrayLoopFusionExtractors { self =>
  override val verbosity = 2 // 1: only printlog, 2: also printdbg
  val runner = new Runner { val p: self.type = self }
  runner.run()
}

trait Codegen extends ScalaGenNumericOps with ScalaGenPrint with ScalaGenOrderingOps 
  with ScalaGenBooleanOps with ScalaGenArrayLoopsFat { val IR: Impl }

trait Runner /*extends internal.ScalaCompile*/ {
  val p: Impl
  def run() = {
    val x = p.fresh[Int]
    val y = p.reifyEffects(p.test(x))

    val codegen = new Codegen { val IR: p.type = p }

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
      println("\n-- after vertical transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(v)
      }

      println("\n-- horizontal transformation")
      val h = horTransf.transformBlock(v)
      println("\n-- after horizontal transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(h)
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

  // TODO: is this only for vertical?
  /** Get all statements that are necessarily in the loop with the
   *  given indexing symbol and result symbol. */
  def getLoopStms(indexSym: Sym[Int], resSym: Sym[Any]): List[Stm] = {
    val indexedStms = getDependentStuff(indexSym).flatMap(_.lhs)

    val start = List((resSym, findDefinition(resSym)))
    val loopStmSyms = GraphUtil.stronglyConnectedComponents(start, { symDef: Pair[Sym[Any],Option[Stm]] => 
      symDef match {
        case (_, Some(d)) => 
          val next = syms(d.rhs).intersect(indexedStms)
          next.map(s => (s, findDefinition(s)))
        case (_, None) => List()
      }}).flatten

    loopStmSyms.map(_._2.get)
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



trait VerticalFusionTransformer extends FusionTransformer /*with BaseLoopsTraversalFat*/ { 
  val IR: Impl with ArrayLoopFusionExtractors
  import IR.{__newVar => _, _}
  import scala.collection.mutable.{HashMap, HashSet}

  // List of the loops on the same level&scope
  val seenLoops = HashSet[Sym[Any]]()

  // Each scope has a fresh map, don't want to fuse loops from different levels or scopes
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    val save = seenLoops
    seenLoops = HashSet[Sym[Any]]()
    val res = super.reflectBlock(block)
    seenLoops = save
    res
  }

  // loops with fixed lengths (SimpleCollect with constant or immutable expression shape)
  val loopsToFixedLengths = new HashMap[Sym[Any], Exp[Int]]

  def recordFixedLengths(sym: Sym[Any], loop: AbstractLoop[_]) = {
    val shape = loop.size
    val replacement: Option[Exp[Int]] = loop match {
      case FixedDomain(Const(len)) => Some(Const(len))
      case FixedDomain(domainSym: Sym[Int]) => subst.get(domainSym) match {
        case Some(Const(len: Int))                       => Some(Const(len))
        case Some(sSym: Sym[Int]) if (!isWritableSym(sSym)) => Some(sSym)
        case _ if (!isWritableSym(domainSym))            => Some(domainSym)
        case _                                           => None
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

    private def getSet(i: Int): HashSet[Sym[Any]] = {
      fusedSymsSets(fusedSymsSets.length - 1 - i)
    }
    
    def record(pSym: Sym[Any], cSym: Sym[Any]) = {
      val index = fusedSyms.get(pSym) match {
        case Some(pIndex) => pIndex
        case None => 
          val s = fusedSymsSets.length
          fusedSymsSets = HashSet(pSym) :: fusedSymsSets
          fusedSyms.put(pSym, s)
          s
      }
      getSet(index).add(cSym)
      fusedSyms.put(cSym, index)
    }

    def getOldAndNew(pSym: Sym[Any]): Set[Sym[Any]] = {
      val fused = fusedSyms.get(pSym).map(getSet(_)).getOrElse(HashSet(pSym))
      fused.foreach({f => subst.get(f) match {
        case Some(s@Sym(_)) => fused.add(s)
        case _ =>
      }})
      fused.toSet
    }
  }


  override def transformStm(stm: Stm): Exp[Any] = { 
    // println("--transforming: " + stm)
    val transformedSym = stm match {
      case TP(_, SimpleIndex(pSym@Sym(_), cIndex@Sym(_))) => (subst.get(pSym) match {
          case Some(newPSym@Sym(_)) => simpleIndexReplacements.get((newPSym, cIndex))
          case None => None
        }).orElse(simpleIndexReplacements.get((pSym, cIndex))) 
          .getOrElse(super.transformStm(stm))

      case TP(sym, loop: AbstractLoop[_]) => // Check if we can do loop fusion
        val (prod, reconstrProds) = findProducers(sym, loop.size, loop.v, loop.body)
        
        def printProd(p: (Sym[Any], Sym[Any])): String = p._2 + (if (p._1 == p._2) "" else " (was " + p._1 + ")")

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
          .map({ case TP(s, l: AbstractLoop[_]) => 
            (s, l.size, l.v, l.body, sym, loop.size, loop.v, loop.body) 
          }).flatMap({
            case (pSym, pSize, pIndex, SimpleCollect(pRes), cSym, cSize, cIndex, cBody) =>
              subst += (cIndex -> pIndex)
              simpleIndexReplacements += ((pSym, cIndex) -> pRes)
              FusedSyms.record(pSym, cSym)
              printdbg("(VFT) SimpleCollect+Any fusion: remap index to " + pIndex + ", SimpleIndex to " + pRes + ".")
              None

            // TODO other types
            //case (pSym, pSize, pIndex, SimpleCollectIf(pRes, pConds), cSym, cSize, cIndex, SimpleCollect(cRes)) =>

            // FUSE YOU!

            // case (pSym, pSize, pIndex, pBody, cSym, cSize, cIndex, cBody) =>
            // case (pSym, pSize, pIndex, pBody, cSym, cSize, cIndex, cBody) =>
            case _ => 
              printdbg("not implemented yet")
              None

          })          

        // bookkeeping
        seenLoops += sym
        recordFixedLengths(sym, loop)

        resultLoop.getOrElse(super.transformStm(stm))

      case TP(lenSym, SimpleDomain(sym@Sym(_))) =>
        loopsToFixedLengths.getOrElse(sym, super.transformStm(stm))

      case _ =>
        super.transformStm(stm)
    }
    // println("--transformed " + stm + " to " + transformedSym)
    transformedSym
  }

  def findProducers[A](cSym: Sym[A], cShape: Exp[Int], cIndex: Sym[Int], cBody: Def[A]): (Option[(Sym[Any], Sym[Any])], List[(Sym[Any], Sym[Any])]) = {
    val cIndexStms = getDependentStuff(cIndex)
    
    val prodSyms = cIndexStms.collect({
      case TP(_, SimpleIndex(pSym@Sym(_), `cIndex`)) => pSym 
    }).filter({ pSym => 
      val sameScope = seenLoops.contains(pSym)
      if (!sameScope)
        printdbg("(VFT) " + cSym + " not fused with " + pSym + " because not in same level/scope.")
      sameScope
    }).map({ pSym => (pSym, subst.get(pSym) match {
        case Some(newPSym@Sym(_)) => newPSym
        case _ => pSym
      })
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
    val cBlocks = cBody match {
      case SimpleCollect(resSym@Sym(_)) => List(resSym)
      case SimpleCollectIf(resSym@Sym(_), conds: List[Sym[_]]) => resSym :: conds
      case MultiCollect(resSym@Sym(_), _) => List(resSym)
      case Reduce(resSym@Sym(_), _, _) => List(resSym)
      case _ => 
        return ("unknown consumer loop type: " + cBody, -1)
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

trait HorizontalFusionTransformer extends FusionTransformer /*with BaseLoopsTraversalFat*/ { 
  val IR: Impl with ArrayLoopFusionExtractors
  import IR.{__newVar => _, _}

  // TODO List per shape because of different deps
  var seenLoops = new scala.collection.mutable.HashMap[Exp[Any], (Sym[Any], Sym[Int], Option[List[Sym[Any]]])]

  // Each scope has a fresh map, don't want to fuse loops from different levels or scopes
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    val save = seenLoops
    seenLoops = new scala.collection.mutable.HashMap[Exp[Any], (Sym[Any], Sym[Int], Option[List[Sym[Any]]])]
    val res = super.reflectBlock(block)
    seenLoops = save
    res
  }

  // TODO check SimpleCollect
  override def transformStm(stm: Stm): Exp[Any] = {
    stm match {
      case TP(loopSym, SimpleLoop(shape, indexSym, body)) =>
        body match {
          case SimpleCollect(resSym:Sym[Any]) => getLoopStms(indexSym, resSym)
          case _ =>
        }


        seenLoops.get(shape) match {
          case Some((otherLoopSym, newIndex, d)) => 
            val deps = d.getOrElse({ 
              val ds = getFatDependentStuff(initialDefs)(List(otherLoopSym))
                  .collect({ case TP(sym, SimpleLoop(_, _, _)) => sym })
              seenLoops += (shape -> (otherLoopSym, newIndex, Some(ds)))
              ds
            })

            if (deps.contains(loopSym)) { 
              printlog("(HFT)  Loop " + loopSym + " not fused with " + otherLoopSym + " because it depends on it")
              super.transformStm(stm)
            } else {
              printlog("(HFT)  Loop " + loopSym + " fused with " + otherLoopSym + ", common index: " + newIndex)
              subst += (indexSym -> newIndex)
              super.transformStm(stm)
            }
          case None => 
            super.transformStm(stm) match {
              case newSym@Sym(_) => 
                findDefinition(newSym).get match {
                  case TP(newLoopSym, SimpleLoop(_, indexSym, _)) =>
                    seenLoops += (shape -> (newLoopSym, indexSym, None))
                    printlog("(HFT)  Recording new loop (prev. " + loopSym + "): " + (shape -> (newLoopSym, indexSym, None)))
                    newSym
                }
            }
        }
      case _ => super.transformStm(stm)
    }
    
  }
}

class TestFusion3 extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-wip-"

  def testFusionTransform00 = withOutFileChecked(prefix+"fusion00") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // and range not dce'd
        val range = array(100) { i => i + 1 }
        val odds = array(range.length) { i => range.at(i) + 1 }
        print(range.at(0))
        print(odds.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform01 = withOutFileChecked(prefix+"fusion01") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, odds is consumer, range fused into odds
        // but range kept around, TODO horizontal fusion
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // length moved out and replaced with constant, so is consumer
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // not consumer
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, arr1 is consumer and arr2 is also consumer of range
        // multiple indep consumers fused
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = arrayIf(range.length) { i =>
          val x = range.at(i) > 50
          (x, range.at(i) * 2) }
        val arr2 = arrayIf(range.length) { i =>
          val x = range.at(i) < 20
          (x, range.at(i) * 3) }
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform07 = withOutFileChecked(prefix+"fusion07") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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

  def testFusionTransform08 = withOutFileChecked(prefix+"fusion08") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // SimpleCollect + Any fusion
        val range = array(100) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) + 2 }
        val range3 = arrayIf(range.length) { i => (i > 10, range.at(i) + 3) }
        val range4 = flatten(range.length) { i => array(10) { ii => ii + range.at(i) } }
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform09 = withOutFileChecked(prefix+"fusion09") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {     
        // nested array arrI not fused with range
        // arrI & arrJ fused
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // reconstructed consumers fused
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        val v = range.at(0)
        // not consumer because of v (depends on producer)
        val range2 = arrayIf(range.length) { i => (range.at(i) > 0, range.at(i) + v) }
        print(range2.at(0))
        // not consumer because of v (depends on producer)
        val range3 = arrayIf(range.length) { i => (range.at(i) + v > 0, i) }
        print(range3.at(0))
        // not consumer because range3 uses index
        val range4 = array(range2.length) { i => range2.at(i) + i }
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }

// TODO fix effects

/*
  def testFusionTransform18 = withOutFileChecked(prefix+"fusion18") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {
        // multiple consumers all fused
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
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {
        // multiple consumers not fused:
        // range4 depends on range3, but range already fused with range3,
        // so range4 not fused with range
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
}

