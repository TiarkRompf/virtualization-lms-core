package scala.virtualization.lms
package epfl
package test7

import common._
import test1._

import util.OverloadHack
import scala.reflect.SourceContext

import java.io.{PrintWriter,StringWriter,FileOutputStream}

trait LoopFusionExtractors extends internal.Expressions { // copied from LoopFusionOpt: LoopFusionCore trait

  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  def unapplySimpleDomain(e: Def[Int]): Option[Exp[Any]] = None
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
  def unapplySimpleCollectIf(e: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = None

  object SimpleIndex {
    def unapply(a: Def[Any]): Option[(Exp[Any], Exp[Int])] = unapplySimpleIndex(a)
  }

  object SimpleDomain {
    def unapply(a: Def[Int]): Option[Exp[Any]] = unapplySimpleDomain(a)
  }

  object SimpleCollect {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySimpleCollect(a)
  }

  object SimpleCollectIf {
    def unapply(a: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollectIf(a)
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

  override def unapplySimpleCollect(e: Def[Any]) = e match {
    case ArrayElem(Block(a)) => Some(a) //TODO: block??
    case _ => super.unapplySimpleCollect(e)
  }

  override def unapplySimpleCollectIf(e: Def[Any]) = e match {
    case ArrayIfElem(c,Block(a)) => Some((a,List(c))) //TODO: block?
    case _ => super.unapplySimpleCollectIf(e)
  }

  // override def applyAddCondition(e: Def[Any], c: List[Exp[Boolean]]) = e match { //TODO: should c be list or not?
  //   case ArrayElem(a) if c.length == 1 => ArrayIfElem(c(0),a)
  //   case ReduceElem(a) if c.length == 1 => ReduceIfElem(c(0),a)
  //   case _ => super.applyAddCondition(e,c)
  // }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case SimpleLoop(s,i, ArrayElem(y)) if f.hasContext => 
      array(f(s)) { j => f.asInstanceOf[internal.AbstractSubstTransformer{val IR:ArrayLoopFusionExtractors.this.type}].subst += (i -> j); f.reflectBlock(y) }
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

}

trait MyFusionProg extends NumericOps with ArrayLoops with Print {
  def test(x: Rep[Int]): Rep[Unit]
}

trait Impl extends MyFusionProg with NumericOpsExp with ArrayLoopsFatExp with PrintExp
    with IfThenElseFatExp with OrderingOpsExp with BooleanOpsExp with ArrayLoopFusionExtractors { self =>
  override val verbosity = 1 // print log statements
  val runner = new Runner { val p: self.type = self }
  runner.run()
}

trait Codegen extends ScalaGenNumericOps with ScalaGenPrint with ScalaGenOrderingOps 
  with ScalaGenBooleanOps with ScalaGenArrayLoopsFat { val IR: Impl }

trait Runner {
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
    } catch {
      case ex =>
      println("error: " + ex)
    }
  }
}

trait VerticalFusionTransformer extends ForwardTransformer /*with BaseLoopsTraversalFat*/ { 
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

trait HorizontalFusionTransformer extends ForwardTransformer /*with BaseLoopsTraversalFat*/ { 
  val IR: Impl with ArrayLoopFusionExtractors
  import IR.{__newVar => _, _}

  // Why can't I initialize the SubstTransformer here?
  var substTransformer: Option[SubstTransformer] = None
  // map from shape to (loop symbol, index symbol, dependent loop symbols)
  // Dependent loop symbols are only set when this loop is first retrieved, not
  // when storing each loop, because most probably won't be horizontally fused.
  // Dependent loops cannot be fused.
  val seenLoops = new scala.collection.mutable.HashMap[Exp[Any], (Sym[Any], Sym[Int], Option[List[Sym[Any]]])]

  override def transformBlock[A:Manifest](block: Block[A]): Block[A] = {
    substTransformer = Some(new SubstTransformer)
    printdbg("(HFT)  transformBlock resets substitution of HorizontalFusionTransformer")
    super.transformBlock(block)
  }

  override def transformStm(stm: Stm): Exp[Any] = stm match {
    case TP(loopSym, SimpleLoop(shape, indexSym, _)) =>
      seenLoops.get(shape) match {
        case Some((otherLoopSym, newIndex, d)) => 
          val deps = d.getOrElse({ 
            val ds = getFatDependentStuff(initialDefs)(List(otherLoopSym))
                .collect({ case TP(sym, SimpleLoop(_, _, _)) => sym })
            printlog("(HFT)  Updating loop: " + (shape -> (otherLoopSym, newIndex, Some(ds))))
            seenLoops += (shape -> (otherLoopSym, newIndex, Some(ds)))
            ds
          })

          if (deps.contains(loopSym)) { 
            printlog("(HFT)  Loop " + loopSym + " not fused with " + otherLoopSym + " because it depends on it")
            default(stm)
          } else {
            printlog("(HFT)  Loop " + loopSym + " fused with " + otherLoopSym + ", common index: " + newIndex)
            transformLoop(stm, indexSym, newIndex)          
          }
        case None => 
          seenLoops += (shape -> (loopSym, indexSym, None))
          printlog("(HFT)  Recording loop: " + (shape -> (loopSym, indexSym, None)))
          default(stm)
      }
    case _ => default(stm)
  }

  def transformLoop(stm: Stm, indexSym: Sym[Int], newIndex: Sym[Int]): Exp[Any] = {
    substTransformer.get.subst += (indexSym -> newIndex)
    val reindexedStms = getDependentStuff(indexSym) map(mirrorAddGet(_))
    printdbg("(HFT)  reindexed loop statements: " + reindexedStms)
    val mirr = mirrorAddGet(stm)
    printdbg("(HFT)  reindexed loop: " + mirr)
    mirr match { case TP(sym, _) => sym }
  }

  def default(stm: Stm, transf: SubstTransformer = substTransformer.get): Sym[Any] = {
      val mirr = mirrorAddGet(stm)
      printdbg("(HFT)  simply mirrored statement: " + mirr)
      mirr match { case TP(sym, _) => sym }
  }

  /** Mirrors the given statement with the given transformer, adds the new symbol
      to its substitution and returns the definition. */
  def mirrorAddGet(stm: Stm, transf: SubstTransformer = substTransformer.get): Stm = stm match {
    case TP(s, d) => mirror(d, transf)(mtype(s.tp), mpos(s.pos)) match {
      case newSym@Sym(_) => 
        transf.subst += (s -> newSym)
        findDefinition(newSym).get
    }
  }
}


class TestFusion3 extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-wip-"

  def testFusionTransform1 = withOutFileChecked(prefix+"fusion1") {
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

  def testFusionTransform2 = withOutFileChecked(prefix+"fusion2") {
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

  def testFusionTransform3 = withOutFileChecked(prefix+"fusion3") {
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

  def testFusionTransform4 = withOutFileChecked(prefix+"fusion4") {
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

  def testFusionTransform5 = withOutFileChecked(prefix+"fusion5") {
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

  def testFusionTransform6 = withOutFileChecked(prefix+"fusion6") {
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

  def testFusionTransform7 = withOutFileChecked(prefix+"fusion7") {
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

  def testFusionTransform8 = withOutFileChecked(prefix+"fusion8") {
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
}
