package scala.virtualization.lms
package internal

/**
 * Single traversal of the IR with pre- and post- processing
 */
trait Traversal extends FatBlockTraversal { self =>
  val IR: FatExpressions with Effects
  import IR._

  val name: String = self.getClass.getName.split('$').filterNot(_ forall Character.isDigit).mkString(".")
  val debugMode: Boolean = false

  def withDebugging[T](x: => T): T = {
    if (debugMode) {
      val oldVerbosity = IR.verbosity
      IR.verbosity = 2
      val out = x
      IR.verbosity = oldVerbosity
      out
    }
    else x
  }
  def debug(x: => Any) = withDebugging{ printdbg(x) }

  def preprocess[A:Manifest](b: Block[A]): Block[A] = { b }
  def postprocess[A:Manifest](b: Block[A]): Block[A] = { b }
  def processBlock[A:Manifest](b: Block[A]): Block[A] = { traverseBlock(b); (b) }

  def runOnce[A:Manifest](b: Block[A]): Block[A] = processBlock(b)

  def run[A:Manifest](b: Block[A]): Block[A] = withDebugging {
    val curBlock = preprocess(b)
    val resultBlock = runOnce(curBlock)
    postprocess(resultBlock)
  }
}

/**
 * Iterative traversal of the IR with some convergence condition
 */
trait IterativeTraversal extends Traversal { self =>
  import IR._

  protected val MAX_ITERS: Int = 10     // maximum number of iterations to run
  protected val MAX_RETRIES: Int = 1    // maximum number of retries to allow
  protected var runs = 0                // Current analysis iteration
  private var retries = 0               // Current retry
  private var retry = false

  def hasConverged: Boolean = runs > 0
  def hasCompleted: Boolean = true

  // TODO: Should these be exception throws instead?
  def failedToConverge() { printerr(s"$name did not converge within $MAX_ITERS iterations.") }
  def failedToComplete() { printerr(s"$name did not complete.") }

  /**
   * Function to be called to try to recover when visitor converged but did not complete
   * In postprocess, modify state, then call resume() to resume looping. Resets run number.
   * Can also implement auto-increase of MAX_ITERS using resume() in postprocess
   */
  def resume() { retry = true }

  /**
   * Run traversal/analysis on a given block until convergence or maximum # of iterations reached
   */
  override def run[A:Manifest](b: Block[A]): Block[A] = withDebugging {
    var curBlock = preprocess(b)
    do {
      runs = 0
      retry = false
      while (!hasConverged && runs < MAX_ITERS) { // convergence condition
        runs += 1
        curBlock = runOnce(curBlock)
      }
      curBlock = postprocess(curBlock)
      retries += 1
    } while (retry && retries <= MAX_RETRIES)

    if (!hasCompleted || !hasConverged) { IR.hadErrors = true }
    if (!hasCompleted) { failedToComplete() }
    else if (!hasConverged) { failedToConverge() }

    (curBlock)
  }
}
