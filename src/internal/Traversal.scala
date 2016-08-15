package scala.virtualization.lms
package internal

/**
 * Single traversal of the IR with pre- and post- processing
 */
trait Traversal extends FatBlockTraversal { self =>
  val IR: FatExpressions with Effects
  import IR._

  sealed abstract class RecurseCondition
  protected case object Always extends RecurseCondition     // Always recurse
  protected case object AsDefault extends RecurseCondition  // Only traverse as default
  protected case object Never extends RecurseCondition      // Never recurse

  val name: String = self.getClass.getName.split('$').filterNot(_ forall Character.isDigit).mkString(".")
  var debugMode: Boolean = false              // Traversal-specific debug enable
  var verboseMode: Boolean = false            // Traversal-specific verbosity
  var reportMode: Boolean = true
  val recurse: RecurseCondition = AsDefault   // Recursive traversal of IR hierarchy
  val eatReflect: Boolean = false             // Ignore reflect nodes when matching?

  def silence() {
    verboseMode = false
    debugMode = false
    reportMode = false
  }

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
  final def debug(x: => Any) = withDebugging{ printdbg(x) }
  final def msg(x: => Any) { if (verboseMode) System.out.println(x) }
  final def report(x: => Any) { if (reportMode) System.out.println(x) }

  def preprocess[A:Manifest](b: Block[A]): Block[A] = { b }
  def postprocess[A:Manifest](b: Block[A]): Block[A] = { b }
  def processBlock[A:Manifest](b: Block[A]): Block[A] = { traverseBlock(b); (b) }

  def runOnce[A:Manifest](b: Block[A]): Block[A] = processBlock(b)

  def run[A:Manifest](b: Block[A]): Block[A] = {
    debug("Starting traversal " + name)
    val curBlock = preprocess(b)
    val resultBlock = runOnce(curBlock)
    postprocess(resultBlock)
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(lhs, rhs) =>
      traverse(lhs, rhs)
      if (recurse == Always) blocks(rhs).foreach{blk => traverseBlock(blk)}

    case TTP(lhs, mhs, rhs) =>
      traverse(lhs, mhs, rhs)
      // TODO: Recursive traversal for TTP?
  }
  def traverse(lhs: Sym[Any], rhs: Def[Any]): Unit = rhs match {
    case Reflect(d, u, es) if eatReflect => traverse(lhs, d)
    case _ => if (recurse == AsDefault) blocks(rhs).foreach{blk => traverseBlock(blk)}
  }

  def traverse(lhs: List[Sym[Any]], mhs: List[Def[Any]], rhs: FatDef): Unit = {}
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
  private var _retry = false

  def hasConverged: Boolean = runs > 0
  def hasCompleted: Boolean = true

  // TODO: Should these be exception throws instead?
  def failedToConverge() { printerr(s"$name did not converge within $MAX_ITERS iterations.") }
  def failedToComplete() { printerr(s"$name did not complete.") }

  /**
   * Function to be called to try to recover when visitor converged but did not complete
   * In postprocess, modify state, then call retry() to resume looping. Resets run number.
   * Can also implement auto-increase of MAX_ITERS using retry() in postprocess
   */
  def retry() { _retry = true }

  /**
   * Run traversal/analysis on a given block until convergence or maximum # of iterations reached
   */
  override def run[A:Manifest](b: Block[A]): Block[A] = {
    msg("Starting traversal " + name)
    var curBlock = preprocess(b)
    do {
      runs = 0
      _retry = false
      while (!hasConverged && runs < MAX_ITERS) { // convergence condition
        runs += 1
        curBlock = runOnce(curBlock)
      }
      curBlock = postprocess(curBlock)
      retries += 1
    } while (_retry && retries <= MAX_RETRIES)

    if (!hasCompleted || !hasConverged) { IR.hadErrors = true }
    if (!hasCompleted) { failedToComplete() }
    else if (!hasConverged) { failedToConverge() }

    (curBlock)
  }
}
