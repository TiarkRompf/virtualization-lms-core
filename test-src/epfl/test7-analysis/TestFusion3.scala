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
    case SimpleLoop(s, i, ArrayElem(y)) => 
      simpleLoop(f(s), f(i).asInstanceOf[Sym[Int]], ArrayElem(reifyEffects(f.reflectBlock(y))))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait MyFusionProg extends NumericOps with ArrayLoops with Print {
  def test(x: Rep[Int]): Rep[Unit]
}

trait Impl extends MyFusionProg with NumericOpsExp with ArrayLoopsFatExp with PrintExp
    with IfThenElseFatExp with OrderingOpsExp with BooleanOpsExp with ArrayLoopFusionExtractors { self =>
  override val verbosity = 1 // 1: only printlog, 2: also printdbg
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


  // Including transitive dependencies and deps caused by horizontal
  // fusion requirement
  // val loopDependencies = new HashMap[Sym[Any], List[Sym[Any]]]()
  // val horizontalDependencies = new HashMap[Sym[Any], List[Sym[Any]]]()

  /*
    Any loop X has the following relationships:
    + not related with loop A
     -+ shape is same FixedDomain(A) -> horizontal possible
    + dependent
     -+ other than SimpleIndex(B, i) and/or shape is SimpleDomain(B) -> no fusion
     -+ only through SimpleIndex(C, i) -> maybe vertical
    + horizontally dependent set
    - 
  */

  // def computeDependencies(loop: Sym[Any]): List[Sym[Any]] = {
  //   val loopDepsList = loopDependencies.getOrElseUpdate(loop, {
  //     val start = List((loop, findDefinition(loop)))
  //     val deps = GraphUtil.stronglyConnectedComponents(start, { symDef: Pair[Sym[Any],Option[Stm]] => 
  //       symDef match {
  //         case (sym, _) if loopDependencies.contains(sym) => List() // don't look any further
  //         case (_, Some(d)) => 
  //           val next = syms(d.rhs)
  //           next.map(s => (s, findDefinition(s)))
  //         case (_, None) => Nil
  //       }})
  //     println("nonflat: " + deps)
  //     val flatDeps = deps.flatten.tail

  //     val loopDeps = new HashSet[Sym[Any]]()
  //     flatDeps.collect({ case (_, Some(TP(loopSym, _: AbstractLoop[_]))) => 
  //       loopDependencies.getOrElse(loopSym, Nil).foreach(loopDeps.add)
  //       loopDeps.add(loopSym)
  //     })
  //     loopDeps.toList
  //   })
  //   println("+++deps of " + loop + ": " + loopDepsList)
  //   loopDepsList
  // }

  // def recordHorizontalFusionRequirement(prodSym: Sym[Any], consSym: Sym[Any]) = {
  //   println("+++recording horizontal requirement: " + consSym + " -> " + prodSym)
  //   horizontalDependencies.put(consSym, prodSym :: horizontalDependencies.getOrElse(consSym, Nil))

  // }

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

// TODO: what if constant length optimization is done by DSL author?
// TODO: propagate constant lengths and replace arraylength statements

// A
// B consumes A -> fuse
// C consumes A -> ?

// A
// B consumes A -> fuse
// C consumes B -> fuse with?

// fusion of:
// unfused prod vs. fused prod - dep or indep
// regular consumer vs. deoptimized consumer - single or multiple

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
  // Map from fixed lengths to loops (TP(sym, SimpleLoop(...)))
  // TODO fixed length: also check not mutable: ! isWritableSym
  val fixedLengthsToLoops = new HashMap[Exp[Int], List[Stm]]
  val loopsTofixedLengths = new HashMap[Sym[Any], Exp[Int]]

  // Indexing statements replacement
  // (can't register before because of duplicate substitution error).
  val simpleIndexReplacements = new HashMap[Sym[Any], Exp[Any]]

  override def transformStm(stm: Stm): Exp[Any] = { 
    stm match {
      case TP(sym, SimpleIndex(_, _)) if (simpleIndexReplacements.contains(sym)) => simpleIndexReplacements(sym)

      // TODO other Collects?
      case TP(sym, loop@SimpleLoop(shape, indexSym, SimpleCollect(resSym@Sym(_)))) =>
        seenLoops += sym
        val loopStms = getLoopStms(indexSym, resSym)

        // Could fuse with each of these
        val loopIndexingArraySyms = loopStms.collect({
          case TP(_, SimpleIndex(arraySym@Sym(_), `indexSym`)) if ( // might be consumer of arraySym
              seenLoops.contains(arraySym) // else producer not on same level
              && consumerGoesOverRangeOfProducer(sym, shape, arraySym) > 0 // else not same range
              && indepExceptIndex(resSym, indexSym, arraySym) // else other parts of array used
          ) => (arraySym, consumerGoesOverRangeOfProducer(sym, shape, arraySym))
        }).sortWith((a, b) => a._2 < b._2)

          printlog("(VFT)  Loop " + sym + " is consumer of " + loopIndexingArraySyms.mkString("\n", "\n", "\n"))

          /* test */
          loopIndexingArraySyms.foreach(s => findDefinition(s._1).get match {
            case t@TP(prodSym, SimpleLoop(_, prodIndex, SimpleCollect(prodResSym))) => 
              recordHorizontalFusionRequirement(prodSym, sym)
              println("fuse with " + t)
              println("index: subst += (" + indexSym + " -> " + prodIndex + ") ")
              subst += (indexSym -> prodIndex)
              loopStms.collect { case TP(indexingSym, SimpleIndex(prodSym@Sym(_), `indexSym`)) => 
                println("prodRes: simpleIndexReplacements += (" + indexingSym + " -> " + prodResSym + ") ")
                simpleIndexReplacements += (indexingSym -> prodResSym) 
              }
          })
          /* /test */

          // TODO do fusion!

        

        // Record fixed lengths
        loop match {
          case FixedDomain(Const(len)) =>
            if (shape != Const(len))
              subst += (shape -> Const(len))
            loopsTofixedLengths += (sym -> Const(len))
          case FixedDomain(domainSym@Sym(_)) if subst.contains(domainSym) => println("B")
          case FixedDomain(domainSym@Sym(_)) => println("C")
          case _ => // TODO else?
        }
        subst.get(shape) match {
          case Some(Const(len: Int)) =>
            loopsTofixedLengths += (sym -> Const(len))
          case _ =>
        }

        super.transformStm(stm)

      case TP(lenSym, SimpleDomain(sym@Sym(_))) =>
        loopsTofixedLengths.getOrElse(sym, super.transformStm(stm))

      case _ =>
        super.transformStm(stm)
    }
  }

  /** Returns 1 if shape of consumer is prod.length,
   *  returns > 0  if same range,
   *  returns < 0 otherwise.
   */
  def consumerGoesOverRangeOfProducer(consSym: Sym[Any], shape: Exp[Int], prodSym: Sym[Any]): Int = {
    // range/shape can be: const or symbol
    // symbol can be array.len or expression with substitution to const recorded
    // producer can have fixed length recorded

    // TODO same symbol ok? (val l = x+4)

    val rangeSame = shape match {
      case shapeSym@Sym(_) => findDefinition(shapeSym) match {
        case Some(TP(_, SimpleDomain(`prodSym`))) => 1
        case _ => subst.get(shapeSym) match {
          case Some(Const(l: Int)) => loopsTofixedLengths.get(prodSym) match {
            case Some(Const(`l`)) => 2
            case _ => -1
          }
          case _ => -2
        }
      }
      case Const(l: Int) => loopsTofixedLengths.get(prodSym) match {
        case Some(Const(`l`)) => 3
        case _ => -3
      }
      case _ => -4
    }
    printdbg("consumerGoesOverRangeOfProducer(consSym: " + consSym +", shape: " + shape 
      + ", prodSym: " + prodSym + "): " + (rangeSame > 0) + " (" + rangeSame + ")")
    rangeSame
  }

  def indepExceptIndex(resSym: Sym[Any], indexSym: Sym[Any], arraySym: Sym[Any]): Boolean = {
    !GraphUtil.stronglyConnectedComponents(List(resSym), { sym: Sym[Any] => 
      findDefinition(sym) match {
        case Some(TP(_, SimpleIndex(`arraySym`, `indexSym`))) => List() // OK, don't add dep on arraySym
        case Some(d) => syms(d.rhs)
        case None => List()
      }
    }).flatten.contains(arraySym)
  } 

}
/*
      case TP(sym, SimpleLoop(shapeSym@Sym(_), indexSym, _)) => findDefinition(shapeSym) match {
        // TODO make resistant to constant replacement of length
        case Some(TP(`shapeSym`, SimpleDomain(arraySym@Sym(_)))) => findDefinition(arraySym) match {
          
          case Some(prod@TP(`arraySym`, SimpleLoop(prodShape, prodIndexSym, prodBody))) => prodBody match {
  
            case SimpleCollect(collected) => // TODO handle other types of collects
              
              val loopArrayStms = getDependentStuff(indexSym).filter(syms(_) contains arraySym)
              val consuming = loopArrayStms.collect { case stm @ TP(_, SimpleIndex(`arraySym`, `indexSym`)) => stm }
              assert(consuming.length < 2, "CSE should have eliminated duplicates")
              val notConsuming = loopArrayStms diff consuming
              
              if (loopArrayStms.isEmpty)
                printlog("(VFT)  no consumer: " + stm + "\n(VFT)  because body doesn't contain array.")
              else if (!notConsuming.isEmpty)
                printlog("(VFT)  no consumer: " + stm + "\n(VFT)  because of stms: " + notConsuming)
              else
                return fuseConsumerStm(stm, indexSym)
            case _ => 
          }
          case p => printlog("(VFT)  not fusing consumer: " + stm + "\n(VFT)  because producer is not SimpleCollect: " + p)
            printlog("(VFT)  not fusing consumer: " + stm + "\n(VFT)  because producer is not simpleLoop: " + p)
        }
        case _ => printlog("(VFT)  found loop, but it's not simpleDomain: " + stm)
      }
      case _ =>
    }
    val superTransformedSym = super.transformStm(stm)
    val superTransformedStm = superTransformedSym match {
      case newSym@Sym(_) => findDefinition(newSym).get
    }
    printdbg("(VFT)  ignored: " + stm)
    printdbg("(VFT)  super.transformed: " + superTransformedStm + "\n")
    superTransformedSym
  }

  def fuseConsumerStm(stm: Stm, oldConsIndexSym: Sym[Int]): Exp[Any] = {
    // First transform to remap to new symbols from previous transformations
    val newIndexSym = fresh[Int]
    subst += (oldConsIndexSym -> newIndexSym)

    val superTransformedSym = super.transformStm(stm)
    val superTransformedStm = superTransformedSym match {
      case newSym@Sym(_) => findDefinition(newSym).get
    }

    superTransformedStm match {
      case cons@TP(sym, SimpleLoop(shapeSym@Sym(_), indexSym, _)) => findDefinition(shapeSym) match {
        case None => error("FT unknown shapeSym: " + shapeSym)
        case Some(TP(`shapeSym`, SimpleDomain(arraySym@Sym(_)))) => findDefinition(arraySym) match {
          case None => error("FT unknown arraySym: " + arraySym)
          case Some(prod@TP(`arraySym`, SimpleLoop(prodShape, prodIndexSym, SimpleCollect(collected)))) =>
          
            printlog("\n(VFT)  starting loop fusion of producer:\n      " + prod)
            printlog("(VFT)  into consumer:\n      " + cons)
            printlog("(VFT)  original consumer:\n      " + stm)

            val consuming = getDependentStuff(oldConsIndexSym)
                .map(substOrTransform(_))
                .filter(syms(_) contains arraySym)
                .collect { case stm @ TP(_, SimpleIndex(`arraySym`, `indexSym`)) => stm }
            assert(consuming.length == 1, "CSE should have eliminated duplicates and loop should consume array")
            val consumingSyms = consuming(0).lhs
            
            val producerStms = getFatDependentStuff(initialDefs)(List(prodIndexSym)) // transitive closure

            // Duplicate statements in producer loop body, but remapping index to consumerIndex
            // They will therefore be scheduled into the fused loop
            val indexT = new SubstTransformer
            indexT.subst += (prodIndexSym -> indexSym)
            val reindexedProducerStms = producerStms map { mirrorAddGet(_, indexT) }
            printlog("(VFT)  original producerStms:\n" + producerStms.mkString("      ", "\n      ", "\n"))
            printlog("(VFT)  reindexed producerStms (prodIndex: " + prodIndexSym + " -> consumerIndex: " + indexSym + "):\n" + reindexedProducerStms.mkString("      ", "\n      ", "\n"))
            
            // Remap consuming statements to use the inlined result of the producer instead
            // of the SimpleIndex
            val collectSym = indexT.subst(collected)
            val collectT = new SubstTransformer
            collectT.subst ++= subst
            consumingSyms foreach { s => collectT.subst += (s -> collectSym) }
            val allConsuming = getFatDependentStuff(initialDefs)(consumingSyms)
            val fusedConsuming = allConsuming map { mirrorAddGet(_, collectT) }
            printlog("(VFT)  original consuming:\n" + allConsuming.mkString("      ", "\n      ", "\n"))
            printlog("(VFT)  fused consuming (SimpleIndex: " + consumingSyms + " -> collectSym: " + collectSym + "):\n" + fusedConsuming.mkString("      ", "\n      ", "\n"))

            collectT.subst += (shapeSym -> prodShape)
            val fusedLoop = mirrorAddGet(superTransformedStm, collectT)
            printlog("(VFT)  fusion successful! Fused consumer loop: " + fusedLoop)
            return fusedLoop match { case TP(sym, _) => sym }
        }
      }
    }
  }

  /** Mirrors the given statement with the given transformer, adds the new symbol
      to its substitution and returns the definition. */
  def mirrorAddGet(stm: Stm, transf: SubstTransformer): Stm = stm match {
    case TP(s, d) => mirror(d, transf)(mtype(s.tp), mpos(s.pos)) match {
      case newSym@Sym(_) => 
        transf.subst += (s -> newSym)
        findDefinition(newSym).get
    }
  }

  /** Check whether stm already has a transformation, else transforms it and
      records the substitution. */
  def substOrTransform(stm: Stm): Stm = stm match {
    case TP(sym, _) => subst.getOrElse(sym, super.transformStm(stm)) match {
      case newSym@Sym(_) =>
        subst += (sym -> newSym)
        findDefinition(newSym).get
    }
  }
}
*/



/*trait VerticalFusionTransformer extends ForwardTransformer /*with BaseLoopsTraversalFat*/ { 
  val IR: Impl with ArrayLoopFusionExtractors
  import IR.{__newVar => _, _}

// TODO: what if constant length optimization is done by DSL author?
// TODO: propagate constant lengths and replace arraylength statements

  // Want to record substitutions outside of scope because
  // we use producer statements later.
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    // withSubstScope { 
      traverseBlock(block)
      apply(getBlockResult(block))
    // }
  }

  override def transformStm(stm: Stm): Exp[Any] = { 
    stm match {
      case TP(sym, SimpleLoop(shapeSym@Sym(_), indexSym, _)) => findDefinition(shapeSym) match {
        // TODO make resistant to constant replacement of length
        case Some(TP(`shapeSym`, SimpleDomain(arraySym@Sym(_)))) => findDefinition(arraySym) match {
          
          case Some(prod@TP(`arraySym`, SimpleLoop(prodShape, prodIndexSym, prodBody))) => prodBody match {
  
            case SimpleCollect(collected) => // TODO handle other types of collects
              
              val loopArrayStms = getDependentStuff(indexSym).filter(syms(_) contains arraySym)
              val consuming = loopArrayStms.collect { case stm @ TP(_, SimpleIndex(`arraySym`, `indexSym`)) => stm }
              assert(consuming.length < 2, "CSE should have eliminated duplicates")
              val notConsuming = loopArrayStms diff consuming
              
              if (loopArrayStms.isEmpty)
                printlog("(VFT)  no consumer: " + stm + "\n(VFT)  because body doesn't contain array.")
              else if (!notConsuming.isEmpty)
                printlog("(VFT)  no consumer: " + stm + "\n(VFT)  because of stms: " + notConsuming)
              else
                return fuseConsumerStm(stm, indexSym)
            case _ => 
          }
          case p => printlog("(VFT)  not fusing consumer: " + stm + "\n(VFT)  because producer is not SimpleCollect: " + p)
            printlog("(VFT)  not fusing consumer: " + stm + "\n(VFT)  because producer is not simpleLoop: " + p)
        }
        case _ => printlog("(VFT)  found loop, but it's not simpleDomain: " + stm)
      }
      case _ =>
    }
    val superTransformedSym = super.transformStm(stm)
    val superTransformedStm = superTransformedSym match {
      case newSym@Sym(_) => findDefinition(newSym).get
    }
    printdbg("(VFT)  ignored: " + stm)
    printdbg("(VFT)  super.transformed: " + superTransformedStm + "\n")
    superTransformedSym
  }

  def fuseConsumerStm(stm: Stm, oldConsIndexSym: Sym[Int]): Exp[Any] = {
    // First transform to remap to new symbols from previous transformations
    val newIndexSym = fresh[Int]
    subst += (oldConsIndexSym -> newIndexSym)

    val superTransformedSym = super.transformStm(stm)
    val superTransformedStm = superTransformedSym match {
      case newSym@Sym(_) => findDefinition(newSym).get
    }

    superTransformedStm match {
      case cons@TP(sym, SimpleLoop(shapeSym@Sym(_), indexSym, _)) => findDefinition(shapeSym) match {
        case None => error("FT unknown shapeSym: " + shapeSym)
        case Some(TP(`shapeSym`, SimpleDomain(arraySym@Sym(_)))) => findDefinition(arraySym) match {
          case None => error("FT unknown arraySym: " + arraySym)
          case Some(prod@TP(`arraySym`, SimpleLoop(prodShape, prodIndexSym, SimpleCollect(collected)))) =>
          
            printlog("\n(VFT)  starting loop fusion of producer:\n      " + prod)
            printlog("(VFT)  into consumer:\n      " + cons)
            printlog("(VFT)  original consumer:\n      " + stm)

            val consuming = getDependentStuff(oldConsIndexSym)
                .map(substOrTransform(_))
                .filter(syms(_) contains arraySym)
                .collect { case stm @ TP(_, SimpleIndex(`arraySym`, `indexSym`)) => stm }
            assert(consuming.length == 1, "CSE should have eliminated duplicates and loop should consume array")
            val consumingSyms = consuming(0).lhs
            
            val producerStms = getFatDependentStuff(initialDefs)(List(prodIndexSym)) // transitive closure

            // Duplicate statements in producer loop body, but remapping index to consumerIndex
            // They will therefore be scheduled into the fused loop
            val indexT = new SubstTransformer
            indexT.subst += (prodIndexSym -> indexSym)
            val reindexedProducerStms = producerStms map { mirrorAddGet(_, indexT) }
            printlog("(VFT)  original producerStms:\n" + producerStms.mkString("      ", "\n      ", "\n"))
            printlog("(VFT)  reindexed producerStms (prodIndex: " + prodIndexSym + " -> consumerIndex: " + indexSym + "):\n" + reindexedProducerStms.mkString("      ", "\n      ", "\n"))
            
            // Remap consuming statements to use the inlined result of the producer instead
            // of the SimpleIndex
            val collectSym = indexT.subst(collected)
            val collectT = new SubstTransformer
            collectT.subst ++= subst
            consumingSyms foreach { s => collectT.subst += (s -> collectSym) }
            val allConsuming = getFatDependentStuff(initialDefs)(consumingSyms)
            val fusedConsuming = allConsuming map { mirrorAddGet(_, collectT) }
            printlog("(VFT)  original consuming:\n" + allConsuming.mkString("      ", "\n      ", "\n"))
            printlog("(VFT)  fused consuming (SimpleIndex: " + consumingSyms + " -> collectSym: " + collectSym + "):\n" + fusedConsuming.mkString("      ", "\n      ", "\n"))

            collectT.subst += (shapeSym -> prodShape)
            val fusedLoop = mirrorAddGet(superTransformedStm, collectT)
            printlog("(VFT)  fusion successful! Fused consumer loop: " + fusedLoop)
            return fusedLoop match { case TP(sym, _) => sym }
        }
      }
    }
  }

  /** Mirrors the given statement with the given transformer, adds the new symbol
      to its substitution and returns the definition. */
  def mirrorAddGet(stm: Stm, transf: SubstTransformer): Stm = stm match {
    case TP(s, d) => mirror(d, transf)(mtype(s.tp), mpos(s.pos)) match {
      case newSym@Sym(_) => 
        transf.subst += (s -> newSym)
        findDefinition(newSym).get
    }
  }

  /** Check whether stm already has a transformation, else transforms it and
      records the substitution. */
  def substOrTransform(stm: Stm): Stm = stm match {
    case TP(sym, _) => subst.getOrElse(sym, super.transformStm(stm)) match {
      case newSym@Sym(_) =>
        subst += (sym -> newSym)
        findDefinition(newSym).get
    }
  }
}
*/

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
        print(range.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform03 = withOutFileChecked(prefix+"fusion03") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // not consumer, TODO replace shape so range is dce'd
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
        // constant moved out, so is consumer
        val range = array(100) { i => 
          val x = i + 1
          val y = x * i
          i * y
        }
        // TODO could horizontally fuse once length constant replacement is done
        val arr2 = array(range.length) { i => range.at(i) + range.length }
        print(arr2.length)
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
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = array(range.length) { i =>
          val x = range.at(i) * 4
          x * 2 }
        val arr2 = array(100) { i => 
          i + range.length
        }
        print(range.length)
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform09 = withOutFileChecked(prefix+"fusion09") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {     
        // range, arrI & arrJ will be fused. TODO think about fusion in nested loops... what if
        // range was expensive to compute? @nofuse?
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
        print(arr1.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform10 = withOutFileChecked(prefix+"fusion10") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {     
        // TODO think more about resetting substitution in transformBlock, seems to work fine here
        val range = array(100) { i => 
          i + 1
        }
        val arr1 = array(90) { i =>
          val arrI = array(80) { ii =>
            ii + i
          }
          arrI.at(0)
        }
        val arr2 = array(100) { i =>
          i + 2
        }

        print(range.length)
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform11 = withOutFileChecked(prefix+"fusion11") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {     
        val arr1 = array(90) { i =>
          val arrI = array(100) { ii =>
            ii + i
          }
          arrI.at(0)
        }
        val arr2 = array(100) { i =>
          i + 2
        }
        print(arr1.length)
        print(arr2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform12 = withOutFileChecked(prefix+"fusion12") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {     
        val arr1 = array(100) { i =>
          val arrI = array(80) { ii =>
            ii + i
          }
          val arrJ = array(80) { ii =>
            ii * i
          }
          i * arrI.at(0) * arrJ.at(0)
        }
        print(arr1.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform13 = withOutFileChecked(prefix+"fusion13") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // not consumer
        val range = array(100) { i => i + 1 }
        val arr1 = array(100) { i => i + 2 }
        print(range.length)
        print(arr1.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform14 =  withOutFileChecked(prefix+"fusion14") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(100) { i => i + 1 }
        print(range.length)
        val range3 = array(range.length) { i => i + 1 }
        print(range3.length)
        val l = x + 4
        val range2 = array(l) { i => i + 1 }
        print(range2.length)
      }
    }
    new Prog with Impl
  }

  def testFusionTransform15 =  withOutFileChecked(prefix+"fusion15") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => i + 2 }
//        val range3 = array(100) { i => range.at(i) + range2.at(i) }
//        print(range3.length)
//        val range4 = array(range.length) { i => range.at(i) + range2.at(i) + range2.at(i+1) }
//        print(range4.length)
        // multiple producers:
        val range5 = array(range2.length) { i => range.at(i) + range2.at(i) }
        print(range5.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform16 =  withOutFileChecked(prefix+"fusion16") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // TODO how to set verbosity to 2 only for this test?
        // Test all branches of consumerGoesOverRangeOfProducer
        val range = array(x) { i => i + 1 }
        val range2 = array(range.length) { i => range.at(i) } //  1
        print(range2.length)
        val k = x + 2
        val range3 = array(k) { i => range.at(i) }            // -2
        print(range3.length)
        
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

  def testFusionTransform17 =  withOutFileChecked(prefix+"fusion17") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        val v = range.at(0)
        val range2 = array(range.length) { i => range.at(i) + v } // not consumer because of v
        print(range2.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform18 =  withOutFileChecked(prefix+"fusion18") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        val range = array(x) { i => i + 1 }
        print(range.length)
        val range2 = array(range.length) { i => print(range.at(i)); 1 } 
        // no  vertical (or hor.) fusion because of ordering of effects
        print(range2.at(1))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform19 =  withOutFileChecked(prefix+"fusion19") {
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

  def testFusionTransform20 =  withOutFileChecked(prefix+"fusion20") {
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

  def testFusionTransform21 =  withOutFileChecked(prefix+"fusion21") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => range.at(i) }
        val range3 = array(range.length) { i => range.at(i) }
        print(range2.at(0))
        print(range3.at(0))
      }
    }
    new Prog with Impl
  }

  def testFusionTransform22 =  withOutFileChecked(prefix+"fusion22") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {
        val range = array(100) { i => i + 1 }
        val range2 = array(100) { i => range.at(i) }
        val range3 = array(100) { i => range2.at(i) }
        val range4 = array(100) { i => range.at(i) }

        print(range.at(0))
        print(range2.at(0))
        print(range3.at(0))
        print(range4.at(0))
      }
    }
    new Prog with Impl
  }
}

