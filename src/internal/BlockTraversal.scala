package scala.lms
package internal

trait BlockTraversal extends GraphTraversal {
  val IR: BaseExp
  import IR._
   
  // --- High level API
  def traverseBlock[A](block: Block[A]): Unit = {
    focusBlock(block) { traverseBlockFocused(block) }
  }
  
  def traverseBlockFocused[A](block: Block[A]): Unit = {
    focusExactScope(block){ levelScope => 
      traverseStmsInBlock(levelScope)
    }
  }
  
  def traverseStmsInBlock[A](stms: List[Stm]): Unit = stms foreach traverseStm
  def traverseStm(stm: Stm): Unit = blocks(stm.rhs) foreach traverseBlock
  
  /**
   * Reify a given code block into a block of IR nodes
   * Updates innerScope to reflect changes/additions while reifying block
   * This enables creation and traversal of a block in one traversal pass
   */
  def reifyBlock[T:Typ](block: => Exp[T]): Block[T] = { 
    val prevDefs = globalDefs
    val tblock = IR.reifyEffects{ block }
    val newDefs = globalDefs filterNot { prevDefs contains _ }
    if (innerScope ne null)
      innerScope = innerScope ::: newDefs
    (tblock)
  }
  
  // --- Block definitions
  
  // TODO: What is this for?
  def compactize(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]]
	= throw new Exception("Method compactize should be overriden")
   
  def focusBlock[A](result: Block[Any])(body: => A): A = focusFatBlock(List(result))(body)
  
  def focusFatBlock[A](result: List[Block[Any]])(body: => A): A = {
    focusSubGraph[A](result.map(getBlockResultFull)){ 
      innerScope = fattenAll(innerScope)
      (body)
    }
  }
  
  def focusExactScope[A](result: Block[Any])(body: List[Stm] => A): A
    = focusExactScopeFat(List(result))(body)

  def focusExactScopeFat[A](result: List[Block[Any]])(body: List[Stm] => A): A
    = focusExactScopeSubGraph[A](result.map(getBlockResultFull))(body)
    
  // --- Bound and free symbols
  def boundInScope(x: List[Exp[Any]]): List[Sym[Any]]
    = (x.flatMap(syms) ::: innerScope.flatMap(t => t.lhs ::: boundSyms(t.rhs))).distinct
  
  def usedInScope(y: List[Exp[Any]]): List[Sym[Any]]
    = (y.flatMap(syms) ::: innerScope.flatMap(t => syms(t.rhs))).distinct
  
  def readInScope(y: List[Exp[Any]]): List[Sym[Any]]
    = (y.flatMap(syms) ::: innerScope.flatMap(t => readSyms(t.rhs))).distinct
    
  def boundAndUsedInScope(x: List[Exp[Any]], y: List[Exp[Any]]): (List[Sym[Any]], List[Sym[Any]])
    = (boundInScope(x), usedInScope(y))
  
  // TODO: Investigate - cases where read has symbols not in used
  def freeInScope(x: List[Exp[Any]], y: List[Exp[Any]]): List[Sym[Any]] = {
    val (bound, used) = boundAndUsedInScope(x, y)
    val read = readInScope(y)
    (used intersect read) diff bound
  }
  
  def getFreeVarBlock(start: Block[Any], local: List[Sym[Any]]): List[Sym[Any]] = focusBlock(start) {
    freeInScope(local, List(getBlockResultFull(start)))
  }
  
  // FIXME: Should have generic implementation
  def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any], Any)] = Nil
}
