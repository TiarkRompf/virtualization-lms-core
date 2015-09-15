package scala.virtualization.lms
package internal

/**
 * Single iteration traversal of the IR with pre- and post- processing
 */
trait Traversal extends FatBlockTraversal { self =>
  import IR._

  val name: String = self.getClass.getName.split('$').last

  def preprocess[A:Manifest](b: Block[A]): Block[A] = { b }
  def postprocess[A:Manifest](b: Block[A]): Block[A] = { b }

  def runOnce[A:Manifest](s: Block[A]): Block[A] = { traverseBlock(s); (s) }

  def run[A:Manifest](b: Block[A]): Block[A] = {
    val curBlock = preprocess(b)
    val resultBlock = runOnce(curBlock)
    val outputBlock = postprocess(resultBlock)
    (outputBlock)
  }

  override def traverseStm(stm: Stm): Unit = super.traverseStm(stm)
}

/**
 * Iterative traversal of the IR with some convergence condition
 */
trait IterativeTraversal extends Traversal {
  import IR._

  var MAX_ITERS: Int = 10   // maximum number of iterations to run
  var MAX_RETRIES: Int = 1  // maximum number of retries to allow
  protected var runs = 0    // Current analysis iteration
  protected var retries = 0 // Current retry

  var changed: Boolean = true    // Flag for if any unpropagated updates have been made to the IR
  def notifyChange() { changed = true }

  def hasConverged: Boolean = !changed
  def hasCompleted: Boolean = true

  def failedToConverge() { printwarn(s"$name did not converge within $MAX_ITERS iterations.") }
  def failedToComplete() { printwarn(s"$name reached convergence but did not report completion.") }

  private var _retry = false
  /**
   * Function to be called to try to recover when visitor converged but did not complete
   * In postprocess, modify state, then call resume() to resume looping. Resets run number.
   * Can also implement auto-increase of MAX_ITERS using resume() in postprocess
   */
  def resume() { _retry = true }

  /**
   * Run traversal/analysis on a given block until convergence or maximum # of iterations reached
   */
  override def run[A:Manifest](b: Block[A]): Block[A] = {
    var curBlock = preprocess(b)
    do {
      runs = 0
      _retry = false
      while (!hasConverged && runs < MAX_ITERS) { // convergence condition
        runs += 1
        changed = false
        curBlock = runOnce(curBlock)
      }
      curBlock = postprocess(curBlock)
      retries += 1
    } while (_retry && retries <= MAX_RETRIES)

    (curBlock)
  }

  override def traverseStm(stm: Stm): Unit = super.traverseStm(stm)
}