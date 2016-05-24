package scala.virtualization.lms
package internal

import util.GraphUtil
import java.io.{File, PrintWriter}


trait BlockTraversal extends GraphTraversal {
  val IR: Blocks
  import IR._

  //type Block[+T]

  def reifyBlock[T: Manifest](x: => Exp[T]): Block[T]

  def compactize(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = { throw new Exception("Method compactize should be overriden.") }

  def getFreeVarBlock(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = Nil

  def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = Nil // TODO: Nil or Exception??

  def traverseBlock[A](block: Block[A]): Unit
  def traverseStm(stm: Stm): Unit

  def reset: Unit = ()
}




trait NestedBlockTraversal extends BlockTraversal with NestedGraphTraversal {
  val IR: Effects
  import IR._

  // ----- block definition

  //type Block[+T] = IR.Block[T]

  /**
   * Reify a given code block into a block of IR nodes
   * Updates innerScope to reflect changes/additions while reifying block
   * This enables creation and traversal of a block in one traversal pass
   */
  def reifyBlock[T: Manifest](x: => Exp[T]): Block[T] = {
    val prevDefs = globalDefs
    val block = IR.reifyEffects(x)
    val newDefs = globalDefs filterNot ( prevDefs contains _ )
    if (innerScope ne null)
      innerScope ++= newDefs
    (block)
  }

  def focusBlock[A](result: Block[Any])(body: => A): A =
    focusFatBlock(List(result))(body)

  def focusFatBlock[A](result: List[Block[Any]])(body: => A): A =
    focusSubGraph[A](result.map(getBlockResultFull))(body)


  def focusExactScope[A](resultB: Block[Any])(body: Seq[Stm] => A): A =
    focusExactScopeFat(List(resultB))(body)

  def focusExactScopeFat[A](resultB: List[Block[Any]])(body: Seq[Stm] => A): A =
    focusExactScopeSubGraph[A](resultB.map(getBlockResultFull))(body)

  // ---- bound and free vars

  def boundInScope(x: List[Exp[Any]]): List[Sym[Any]] = {
    (x.flatMap(syms)++innerScope.flatMap(t => t.lhs:::boundSyms(t.rhs))).distinct
  }

  def usedInScope(y: List[Exp[Any]]): List[Sym[Any]] = {
    (y.flatMap(syms)++innerScope.flatMap(t => syms(t.rhs))).distinct
  }

  def readInScope(y: List[Exp[Any]]): List[Sym[Any]] = {
    (y.flatMap(syms)++innerScope.flatMap(t => readSyms(t.rhs))).distinct
  }

  // bound/used/free variables in current scope, with input vars x (bound!) and result y (used!)
  def boundAndUsedInScope(x: List[Exp[Any]], y: List[Exp[Any]]): (List[Sym[Any]], List[Sym[Any]]) = {
    (boundInScope(x), usedInScope(y))
  }

  def freeInScope(x: List[Exp[Any]], y: List[Exp[Any]]): List[Sym[Any]] = {
    val (bound, used) = boundAndUsedInScope(x,y)
    // aks: freeInScope used to collect effects that are not true input dependencies. TR, any better solution?
    // i would expect read to be a subset of used, but there are cases where read has symbols not in used (TODO: investigate)
    val read = readInScope(y)
    (used intersect read) diff bound
  }

  // TODO: remove
  override def getFreeVarBlock(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = {
    focusBlock(start) {
      freeInScope(local, List(getBlockResultFull(start)))
    }
  }

  override def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = Nil // FIXME: should have generic impl



  // ----- high level api

  def traverseBlock[A](block: Block[A]): Unit = {
    focusBlock(block) {
      traverseBlockFocused(block)
    }
  }

  def traverseBlockFocused[A](block: Block[A]): Unit = {
    focusExactScope(block) { levelScope =>
      traverseStmsInBlock(levelScope)
    }
  }

  // Bit of a hack here - use scheduling to return list of statements
  def getStmsInBlock[A](block: Block[A]): Seq[Stm] = {
    var stms: Seq[Stm] = Nil
    focusBlock(block) {
      focusExactScope(block){ levelScope =>
        stms = levelScope
      }
    }
    stms
  }

  def traverseStmsInBlock[A](stms: Seq[Stm]): Unit = {
    stms foreach traverseStm
  }

  def traverseStm(stm: Stm): Unit = { // override this to implement custom traversal
    blocks(stm.rhs) foreach traverseBlock
  }

  // ----- reset

  override def reset { // used anywhere?
    innerScope = null
    //shallow = false
    IR.reset
    super.reset
  }
}

