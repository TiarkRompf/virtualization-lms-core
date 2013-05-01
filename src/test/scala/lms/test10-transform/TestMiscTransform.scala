package scala.lms
package test10

import ops._
import internal._
import util.OverloadHack

import test1._
import test7.{Print,PrintExp,ScalaGenPrint}
import test7.{ArrayLoops,ArrayLoopsExp,ScalaGenArrayLoops}
import test8._

import java.io.{PrintWriter,StringWriter,FileOutputStream}
import scala.reflect.SourceContext


// test various transform routines -- these are not part of the core library
// and mostly serve to investigate alternative designs

// see TestTransform.scala and TestWorklistTransform2.scala for examples
// of the core library transformers

trait SimpleBlockTransformer extends internal.FatBlockTraversal {
  val IR: Expressions with Effects with FatExpressions with Transforming //with LoopsFatExp with IfThenElseFatExp
  import IR._

  def transformBlock[A](block: Block[A]): Block[A] = {
    focusBlock(block) {
      transformBlockFocused(block)
    }
  }

  def transformBlockFocused[A](block: Block[A]): Block[A] = {
    focusExactScope(block) { levelScope =>
      //val newScope = levelScope flatMap transformStm
      //Block(newScope) // block with exactly those stms. we don't currently support that.
      levelScope foreach { stm =>
        val stms2 = transformStm(stm);
        val stms3 = stms2 diff globalDefs // skip those already in the graph
        reflectSubGraph(stms3) // may fail if we're redefining a symbol that already exists
      }
      Block(block.res)
    }
  }

  def transformStm(stm: Stm): List[Stm] = stm match { // override this to implement custom traversal
    case TP(s,d) =>
      val trans = new AbstractTransformer {
        val IR: SimpleBlockTransformer.this.IR.type = SimpleBlockTransformer.this.IR
        def apply[A](x: Exp[A]) = x
        override def apply[A:TypeRep](x: Block[A]) = transformBlock(x)
      }
      List(TP(s, mirrorDef(d, trans)(mtype(s.tp),mpos(s.pos))))
    // blocks(d) map transformBlock
  }

  // problem: transform (s,d) to (s,d1) and (s,d2) in two different branches
}


trait NestedBlockTransformer extends internal.FatBlockTraversal {
  val IR: Expressions with Effects with FatExpressions with Transforming //with LoopsFatExp with IfThenElseFatExp
  import IR._

  var subst: scala.collection.immutable.Map[Sym[_], Exp[_]] = Map.empty

  def transformExp[A](e: Exp[A]): Exp[A] = e match {
    case s: Sym[A] =>
      val e2 = subst.getOrElse(s,e).asInstanceOf[Exp[A]]
      if (e2 == e) e2 else transformExp(e2)
    case _ => e
  }

  def transformBlock[A](block: Block[A]): Block[A] = {
    focusBlock(block) {
      transformBlockFocused(block)
    }
  }

  def transformBlockFocused[A](block: Block[A]): Block[A] = {
    focusExactScope(block) { levelScope =>
      //val newScope = levelScope flatMap transformStm
      //Block(newScope) // block with exactly those stms. we don't currently support that.
      val saveSubst = subst
      levelScope foreach { stm =>
        val stms2 = transformStm(stm);
        val stms3 = stms2 diff globalDefs // skip those already in the graph
        var changedSubst = false
        val stms4 = stms3 map { s =>
          if (changedSubst) println("*** potential problem: changed subst in the middle of iterating over transformed stms")
          val conflict = globalDefs exists (_.lhs exists (s.lhs contains _))
          if (conflict) {
            println("*** conflict " + s)
            s match {
              case TP(sym, rhs) =>
                val sym1 = fresh(mtype(sym.tp))
                assert(!subst.contains(sym))
                subst += (sym -> sym1) // TODO: if we are changing subst we may need to re-transform following elems in stm3!!
                changedSubst = true
                TP(sym1,rhs)
            }
          } else s
        }
        reflectSubGraph(stms4)
      }
      val res2 = transformExp(block.res)
      subst = saveSubst
      Block(res2)
    }
  }

  def transformStm(stm: Stm): List[Stm] = stm match { // override this to implement custom traversal
    case TP(s,d) =>
      val trans = new AbstractTransformer {
        val IR: NestedBlockTransformer.this.IR.type = NestedBlockTransformer.this.IR
        def apply[A](x: Exp[A]) = transformExp(x)
        override def apply[A:TypeRep](x: Block[A]) = transformBlock(x)
      }
      List(TP(s, mirrorDef(d, trans)(mtype(s.tp),mpos(s.pos))))
    // blocks(d) map transformBlock
  }

  // problem: transform (s,d) to (s,d1) and (s,d2) in two different branches
}


trait MirrorBlockTransformer extends internal.FatBlockTraversal {
  val IR: Expressions with Effects with FatExpressions with Transforming //with LoopsFatExp with IfThenElseFatExp
  import IR._

  var subst: scala.collection.immutable.Map[Sym[_], Exp[_]] = Map.empty

  def transformExp[A](e: Exp[A]): Exp[A] = e match {
    case s: Sym[A] =>
      val e2 = subst.getOrElse(s,e).asInstanceOf[Exp[A]]
      if (e2 == e) e2 else transformExp(e2)
    case _ => e
  }

  def transformBlock[A](block: Block[A]): Block[A] = {
    implicit val m = block.tp
    reifyEffects {
      mirrorBlock(block)
    }
  }

  def mirrorBlock[A](block: Block[A]): Exp[A] = {
    focusBlock(block) {
      focusExactScope(block) { levelScope =>
        val saveSubst = subst
        levelScope foreach traverseStm
        val res = getBlockResult(Block(transformExp(block.res))) // strip translated Reify...
        subst = saveSubst
        res
      }
    }
  }


  /*def traverseBlock[A](block: Block[A]): Unit = { // super
    focusBlock(block) {
      traverseBlockFocused(block)
    }
  }*/

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) =>
      val sym2 = transformExp(sym)
      assert(sym == sym2)
      if (sym2 == sym) {
        val replace = transformStm(stm)
        assert(!subst.contains(sym))
        subst = subst + (sym -> replace)
      }
  }

  def transformStm(stm: Stm): Exp[Any] = stm match { // override this to implement custom traversal
    case TP(s,d) =>
      val trans = new AbstractTransformer {
        val IR: MirrorBlockTransformer.this.IR.type = MirrorBlockTransformer.this.IR
        def apply[A](x: Exp[A]) = transformExp(x)
        override def apply[A:TypeRep](x: Block[A]) = transformBlock(x)
      }
      mirror(d,trans)(mtype(s.tp),mpos(s.pos))
  }

}


trait MirrorRetainBlockTransformer extends MirrorBlockTransformer {
  val IR: Expressions with Effects with FatExpressions with Transforming //with LoopsFatExp with IfThenElseFatExp
  import IR._

  override def traverseStm(stm: Stm): Unit = stm match {
    case TP(sym, rhs) =>
      val sym2 = transformExp(sym)
      assert(sym == sym2)
      if (sym2 == sym) {
        val replace = transformStm(stm)
        assert(!subst.contains(sym))
        if (sym != replace) { // record substitution only if result is different
          subst = subst + (sym -> replace)
        }
      }
  }

  override def transformStm(stm: Stm): Exp[Any] = stm match { // override this to implement custom traversal
    case TP(s,d) =>
      // we want to skip those statements that don't have symbols that need substitution
      // however we need to recurse into any blocks
      if (!syms(d).exists(subst contains _) && blocks(d).isEmpty) {
        if (!globalDefs.contains(stm)) reflectSubGraph(List(stm))
        return s
      }
      //println("need to replace " + stm + " / " + subst)
      val trans = new AbstractTransformer {
        val IR: MirrorRetainBlockTransformer.this.IR.type = MirrorRetainBlockTransformer.this.IR
        def apply[A](x: Exp[A]) = transformExp(x)
        override def apply[A:TypeRep](x: Block[A]) = transformBlock(x)
      }
      mirror(d,trans)(mtype(s.tp),mpos(s.pos))
  }

}



/*
trait FWXTransform extends BaseFatExp with EffectExp with IfThenElseFatExp with LoopsFatExp { self =>

  // we need to apply the current substitution to each Def we create:
  // Foo(x) atPhase(t) { bar(x) }   <--- x in bar(x)  will refer to a sym that may have been replaced itself

  var subst: Map[Sym[_], Exp[_]] = xform.subst

  override implicit def toAtom[A:TypeRep](d: Def[A]): Exp[A] = { // override createDefinition instead?
    val in = syms(d)
    val actual = in map (s => subst.getOrElse(s,s))

    if (in != actual) {
      println("toAtom transform "+d+" " + in + " -> " + actual)
      val t = new SubstTransformer
      t.subst ++= subst
      mirror(d,t)
    } else {
      super.toAtom(d)
    }
  }

}
*/




class TestMisc extends FileDiffSuite {

  val prefix = "test-out/epfl/test10-"

  trait DSL extends VectorOps with Arith with OrderingOps with BooleanOps with LiftVariables
    with IfThenElse with While with RangeOps with Print {
    def test(x: Rep[Int]): Rep[Unit]
  }

  trait Impl extends DSL with VectorExp with ArithExp with OrderingOpsExpOpt with BooleanOpsExp
    with EqualExpOpt with ArrayMutationExp with IfThenElseFatExp with LoopsFatExp with WhileExpOptSpeculative
    with RangeOpsExp with PrintExp with FatExpressions {
    override val verbosity = 1
  }

  trait Codegen extends ScalaGenVector with ScalaGenArrayMutation with ScalaGenArith with ScalaGenOrderingOps
    with ScalaGenVariables with ScalaGenEqual with ScalaGenIfThenElse with ScalaGenWhileOptSpeculative
    with ScalaGenRangeOps with ScalaGenPrint {
    val IR: Impl
  }


  // test simple block transform
  def testMisc1 = withOutFileChecked(prefix+"misc1") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val z = vzeros(100)
        val y = vzeros(100)
        val a = vplus(z,y)
        val b = vplus(z,a)
        print(b)
      }
    }
    val p = new Prog with Impl {
      val x = fresh[Int]
      val y = reifyEffects(test(x))
    }

    val codegen = new Codegen { val IR: p.type = p }

    val graph = p.globalDefs
    println("-- full graph")
    graph foreach println

    println("-- before transformation")
    codegen.withStream(new PrintWriter(System.out)) {
      codegen.emitBlock(p.y)
    }

    p.globalDefs = Nil // reset graph, transformer will build anew
    p.globalDefsCache = Map.empty
    val trans = new SimpleBlockTransformer { // a + b --> b + a
      val IR: p.type = p
      import IR._
      override def initialDefs = graph
      override def transformStm(stm: Stm): List[Stm] = stm match {
        case TP(s,VectorPlus(a,b)) =>
          println("replacing " + stm)
          List(TP(s, VectorPlus(b,a)))
        case _ => super.transformStm(stm)
      }
    }
    val z = trans.transformBlock(y)

    println("-- after transformation")
    codegen.withStream(new PrintWriter(System.out)) {
      codegen.emitBlock(z)
    }
    println("-- done")
  }

  // test simple block transform -- failure case when transforming
  // different occurences of same stm in different ways
  def testMisc2 = withOutFileChecked(prefix+"misc2") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val a = vzeros(100) // will be moved into branches
        val b = vzeros(50)
        val c = vplus(a,b)
        if (x == 0) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
      }
    }
    val p = new Prog with Impl
    val x = p.fresh[Int]
    val y = p.reifyEffects(p.test(x))

    val codegen = new Codegen { val IR: p.type = p }

    val graph = p.globalDefs
    println("-- full graph")
    graph foreach println

    println("-- before transformation")
    codegen.withStream(new PrintWriter(System.out)) {
      codegen.emitBlock(y)
    }

    p.globalDefs = Nil // reset graph, transformer will build anew
    p.globalDefsCache = Map.empty
    val trans = new SimpleBlockTransformer { // a + b --> b + a, but only in then-branches of an if-then-else
      val IR: p.type = p
      import IR.{__newVar => _, _}
      override def initialDefs = graph
      var isInThenBranch = false
      override def transformStm(stm: Stm): List[Stm] = stm match {
        case TP(s,VectorPlus(a,b)) if isInThenBranch =>
          println("replacing " + stm)
          List(TP(s, VectorPlus(b,a)))
        case TP(s,Reflect(IfThenElse(c,a,b), u, es)) =>
          println("encountering if then else " + stm)
          val saveFlag = isInThenBranch
          isInThenBranch = true
          val a2 = transformBlock(a)
          isInThenBranch = saveFlag
          val b2 = transformBlock(b)
          List(TP(s,Reflect(IfThenElse(c,a2,b2), u, es)))
        case _ => super.transformStm(stm)
      }
    }
    try {
      val z = trans.transformBlock(y)

      println("-- after transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(z)
      }
    } catch {
      case ex =>
      println("error: " + ex)
    }
    println("-- done")
  }

  // test better block transform -- fixing above case
  def testMisc3 = withOutFileChecked(prefix+"misc3") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val a = vzeros(100) // will be moved into branches
        val b = vzeros(50)
        val c = vplus(a,b)
        if (x == 0) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
        if (x == 1) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
      }
    }
    val p = new Prog with Impl
    val x = p.fresh[Int]
    val y = p.reifyEffects(p.test(x))

    val codegen = new Codegen { val IR: p.type = p }

    val graph = p.globalDefs
    println("-- full graph")
    graph foreach println

    println("-- before transformation")
    codegen.withStream(new PrintWriter(System.out)) {
      codegen.emitBlock(y)
    }

    p.globalDefs = Nil // reset graph, transformer will build anew
    p.globalDefsCache = Map.empty
    val trans = new NestedBlockTransformer { // a + b --> b + a, but only in then-branches of an if-then-else
      val IR: p.type = p
      import IR.{__newVar => _, _}
      override def initialDefs = graph
      var isInThenBranch = false
      override def transformStm(stm: Stm): List[Stm] = stm match {
        case TP(s,VectorPlus(a,b)) if isInThenBranch =>
          println("replacing " + stm)
          List(TP(s, VectorPlus(transformExp(b),transformExp(a))))
        case TP(s,Reflect(IfThenElse(c,a,b), u, es)) =>
          println("encountering if then else " + stm)
          val saveFlag = isInThenBranch
          isInThenBranch = true
          val c2 = transformExp(c)
          val a2 = transformBlock(a)
          isInThenBranch = saveFlag
          val b2 = transformBlock(b)
          List(TP(s,Reflect(IfThenElse(c2,a2,b2), u, es map transformExp))) // TODO: u
        case _ => super.transformStm(stm)
      }
    }
    try {
      val z = trans.transformBlock(y)

      println("-- after transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(z)
      }
      println("// note how the last else branch lost sharing of common subexpressions")
      println("// this is because NestedBlockTransformer does not go through findOrCreateDefinition")
    } catch {
      case ex =>
      println("error: " + ex)
    }
    println("-- done")
  }


  // test mirror block transform -- regain sharing info but mirroring all statements
  def testMisc4 = withOutFileChecked(prefix+"misc4") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val a = vzeros(100) // will be moved into branches
        val b = vzeros(50)
        val c = vplus(a,b)
        if (x == 0) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
        if (x == 1) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
      }
    }
    val p = new Prog with Impl
    val x = p.fresh[Int]
    val y = p.reifyEffects(p.test(x))

    val codegen = new Codegen { val IR: p.type = p }

    val graph = p.globalDefs
    println("-- full graph")
    graph foreach println

    println("-- before transformation")
    codegen.withStream(new PrintWriter(System.out)) {
      codegen.emitBlock(y)
    }

    p.globalDefs = Nil // reset graph, transformer will build anew
    p.globalDefsCache = Map.empty
    val trans = new MirrorBlockTransformer { // a + b --> b + a, but only in then-branches of an if-then-else
      val IR: p.type = p
      import IR.{__newVar => _, _}
      override def initialDefs = graph
      var isInThenBranch = false
      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s,VectorPlus(a,b)) if isInThenBranch =>
          println("replacing " + stm)
          vplus(transformExp(b),transformExp(a))
        case TP(s,Reflect(IfThenElse(c,a,b), u, es)) =>
          println("encountering if then else " + stm)
          __ifThenElse(transformExp(c), {
            val saveFlag = isInThenBranch
            isInThenBranch = true
            val r = mirrorBlock(a)
            isInThenBranch = saveFlag
            r
          }, {
            mirrorBlock(b)
          })(mtype(s.tp),mpos(s.pos))
        case _ => super.transformStm(stm)
      }
    }
    try {
      val z = trans.transformBlock(y)

      println("-- after transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(z)
      }
      println("// note how the else branches share symbols for expressions again (cf misc3)")
      println("// but we have created new identifiers for everything.")
      println("// we cannot detect convergence of transformation this way.")
    } catch {
      case ex =>
      println("error: " + ex)
    }
    println("-- done")
  }

  // test mirror block transform -- regain sharing info but mirroring all statements
  def testMisc5 = withOutFileChecked(prefix+"misc5") {
    trait Prog extends DSL with Impl {
      def test(x: Rep[Int]) = {
        val a = vzeros(100) // will be moved into branches
        val b = vzeros(50)
        val c = vplus(a,b)
        if (x == 0) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
        if (x == 1) { // dynamic condition
          print(vlength(c))
        } else {
          print(vlength(c))
        }
      }
    }
    val p = new Prog with Impl
    val x = p.fresh[Int]
    val y = p.reifyEffects(p.test(x))

    val codegen = new Codegen { val IR: p.type = p }

    val graph = p.globalDefs
    println("-- full graph")
    graph foreach println

    println("-- before transformation")
    codegen.withStream(new PrintWriter(System.out)) {
      codegen.emitBlock(y)
    }

    //p.globalDefs = Nil don't reset graph to get better sharing ... // reset graph, transformer will build anew
    val trans = new MirrorRetainBlockTransformer { // a + b --> b + a, but only in then-branches of an if-then-else
      val IR: p.type = p
      import IR.{__newVar => _, _}
      override def initialDefs = graph
      var isInThenBranch = false
      override def transformStm(stm: Stm): Exp[Any] = stm match {
        case TP(s,VectorPlus(a,b)) if isInThenBranch =>
          println("replacing " + stm)
          vplus(transformExp(b),transformExp(a))
        case TP(s,Reflect(IfThenElse(c,a,b), u, es)) =>
          println("encountering if then else " + stm)
          __ifThenElse(transformExp(c), {
            val saveFlag = isInThenBranch
            isInThenBranch = true
            val r = mirrorBlock(a)
            isInThenBranch = saveFlag
            r
          }, {
            mirrorBlock(b)
          })(mtype(s.tp),mpos(s.pos))
        case _ => super.transformStm(stm)
      }
    }
    try {
      val z = trans.transformBlock(y)

      println("-- after transformation")
      codegen.withStream(new PrintWriter(System.out)) {
        codegen.emitBlock(z)
      }
      println("// trying to retain more expressions from original program.")
      println("// not resetting graph inbetween runs and smarter pruning of statements based on their inputs.")
      println("// still lots of new symbols.")
    } catch {
      case ex =>
      println("error: " + ex)
    }
    println("-- done")
  }

}
