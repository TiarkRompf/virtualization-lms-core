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
  override val verbosity = 1
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
    
    println("\n-- transforming")

    val trans = new MyFusionTransformer {
      val IR: p.type = p
    }
    try {
      val z = trans.transformBlock(y)
      println("\n-- after transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(z)
      }
    } catch {
      case ex =>
      println("error: " + ex)
    }
    println("-- done")      
  }
}

trait MyFusionTransformer extends ForwardTransformer /*with BaseLoopsTraversalFat*/ { 
  val IR: Impl with ArrayLoopFusionExtractors
  import IR.{__newVar => _, _}

  // Don't want to record substitutions only inside of scope because
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
                println("(FT)  no consumer: " + stm + "\n(FT)  because body doesn't contain array.")
              else if (!notConsuming.isEmpty)
                println("(FT)  no consumer: " + stm + "\n(FT)  because of stms: " + notConsuming)
              else
                return fuseConsumerStm(stm, indexSym)
            case _ => 
          }
          case p => println("(FT)  not fusing consumer: " + stm + "\n(FT)  because producer is not SimpleCollect: " + p)
            println("(FT)  not fusing consumer: " + stm + "\n(FT)  because producer is not simpleLoop: " + p)
        }
        case _ => println("(FT)  found loop, but it's not simpleDomain: " + stm)
      }
      case _ =>
    }
    val superTransformedSym = super.transformStm(stm)
    val superTransformedStm = superTransformedSym match {
      case newSym@Sym(_) => findDefinition(newSym).get
    }
//    println("(FT)  ignored: " + stm)
//    println("(FT)  super.transformed: " + superTransformedStm + "\n")
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
        case None => error("FT unknown shapeSym")
        case Some(TP(`shapeSym`, SimpleDomain(arraySym@Sym(_)))) => findDefinition(arraySym) match {
          case None => error("FT unknown arraySym")
          case Some(prod@TP(`arraySym`, SimpleLoop(prodShape, prodIndexSym, SimpleCollect(collected)))) =>
          
            println("\n(FT)  starting loop fusion of producer:\n      " + prod)
            println("(FT)  into consumer:\n      " + cons)
            println("(FT)  original consumer:\n      " + stm)

           // println("\n---dep(old =" + oldConsIndexSym + "): " + getDependentStuff(oldConsIndexSym).mkString("\n      ", "\n      ", "\n"))
           // println("---mapped: " + getDependentStuff(oldConsIndexSym).map({ case stm @ TP(sym, _) => 
           //        subst.getOrElse(sym, super.transformStm(stm)) match {
           //          case newSym@Sym(_) => 
           //            findDefinition(newSym).get
           //        }}).mkString("\n      ", "\n      ", "\n"))

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
            println("(FT)  original producerStms:\n" + producerStms.mkString("      ", "\n      ", "\n"))
            println("(FT)  reindexed producerStms (prodIndex: " + prodIndexSym + " -> consumerIndex: " + indexSym + "):\n" + reindexedProducerStms.mkString("      ", "\n      ", "\n"))
            
            // Remap consuming statements to use the inlined result of the producer instead
            // of the SimpleIndex
            val collectSym = indexT.subst(collected)
            val collectT = new SubstTransformer
            collectT.subst ++= subst
            consumingSyms foreach { s => collectT.subst += (s -> collectSym) }
            val allConsuming = getFatDependentStuff(initialDefs)(consumingSyms)
            val fusedConsuming = allConsuming map { mirrorAddGet(_, collectT) }
            println("(FT)  original consuming:\n" + allConsuming.mkString("      ", "\n      ", "\n"))
            println("(FT)  fused consuming (SimpleIndex: " + consumingSyms + " -> collectSym: " + collectSym + "):\n" + fusedConsuming.mkString("      ", "\n      ", "\n"))

            collectT.subst += (shapeSym -> prodShape)
            val fusedLoop = mirrorAddGet(superTransformedStm, collectT)
            println("(FT)  fusion successful! Fused consumer loop: " + fusedLoop)
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
}

class TestFusion4 extends FileDiffSuite {

  val prefix = "test-out/epfl/test7-wip2-"

  def testFusionTransform1 = withOutFileChecked(prefix+"fusion1") {
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

  def testFusionTransform2 = withOutFileChecked(prefix+"fusion2") {
    trait Prog extends MyFusionProg with LiftNumeric with OrderingOps with BooleanOps with Impl {
      def test(x: Rep[Int]) = {        
        // range is producer, arr1 is consumer of range and arr2 is consumer of arr1
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

}




  // def testFusion1 = {
  //   withOutFile(prefix+"fusion1") {
  //     new MyFusionProg with ArithExp with MyLoopsExp with PrintExp { self =>
  //       val codegen = new ScalaGenMyLoops with ScalaGenArith with ScalaGenPrint { val IR: self.type = self }
  //       codegen.emitSource(test, "Test", new PrintWriter(System.out))

  //     }
  //   }

  //   println("hello world")
  //   new TestOutput().apply()
  //   println("goodbye world")



  //   assertFileEqualsCheck(prefix+"fusion1")
  // }


// trait MyLoops extends Loops with OverloadHack {
//   def loop(shape: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit]
// }

// trait MyLoopsExp extends LoopsExp {
//   case class LoopBody(y: Block[Unit]) extends Def[Unit]

//   def loop(shape: Rep[Int])(f: Rep[Int] => Rep[Unit]): Rep[Unit] = {
//     val x = fresh[Int]
//     val y = reifyEffects(f(x))
//     simpleLoop(shape, x, LoopBody(y))
//   }

//   // override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
//   //   case LoopBody(b) => e
//   //   case _ => super.mirrorDef(e,f)
//   // }

//   // override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
//   //   case Reflect(LoopBody(b), a, c) => e
//   //   case LoopBody(b) => e
//   //   case _ => super.mirror(e,f)
//   // }

//   override def mirrorFatDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
//    case LoopBody(b) => e
//    case _ => super.mirrorFatDef(e,f)
//   }

// }

// trait ScalaGenMyLoops extends ScalaGenLoops {
//   val IR: MyLoopsExp
//   import IR._

//   override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
//     case SimpleLoop(s,x,LoopBody(y)) =>  
//       stream.println("val " + quote(sym) + " = for ("+quote(x)+" <- 0 until " + quote(s) + ") {")
//       emitBlock(y)
//       stream.println(quote(getBlockResult(y)))
//       stream.println("}")
//     case _ => super.emitNode(sym, rhs)
//   }
// }