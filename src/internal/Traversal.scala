package scala.virtualization.lms
package internal

/**
 * Single traversal of the IR with pre- and post- processing
 */
trait Traversal extends FatBlockTraversal { self =>
  val IR: FatExpressions with Effects with MetadataOps
  import IR._

  protected var datRequire: List[Datakey[_]] = Nil
  protected var datUpdate:  List[Datakey[_]] = Nil
  protected var datCreate:  List[Datakey[_]] = Nil
  protected var datInvalid: List[Datakey[_]] = Nil

  // Metadata invalidated by running this traversal
  protected def invalidates(x: Datakey[_]*) { datInvalid = datInvalid ++ x.toList }
  // Metadata required prior to running traversal
  protected def requires(x: Datakey[_]*) { datRequire = datRequire ++ x.toList }
  // Metadata updated (made valid) by running this traversal
  protected def updates(x: Datakey[_]*) { datCreate = datCreate ++ datCreate }
  // Metadata created (made valid) by running this traversal
  protected def creates(x: Datakey[_]*) { datUpdate = datUpdate ++ datUpdate }

  val name: String = self.getClass.getName.split('$').filterNot(_ forall Character.isDigit).mkString(".")

  def preprocess[A:Manifest](b: Block[A]): Block[A] = { b }
  def postprocess[A:Manifest](b: Block[A]): Block[A] = { b }
  def processBlock[A:Manifest](b: Block[A]): Block[A] = { traverseBlock(b); (b) }

  // TODO: Move metadata logic out of these methods to make overriding easier?
  def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    for (data <- datRequire) {
      if (!(validData contains data))
        analyzers(data).run(b)
    }

    val b2 = processBlock(b)

    // Invalidate all metadata which are subtypes of the list of this traversal's invalidation
    // Allows us to write, for example, invalidates (classOf[Metadata]) to invalidate all metadata
    validData = validData filterNot (dat => datInvalid.exists(isSubtype(dat, _)) )
    (b2)
  }

  def run[A:Manifest](b: Block[A]): Block[A] = {
    val curBlock = preprocess(b)
    val resultBlock = runOnce(curBlock)
    val outputBlock = postprocess(resultBlock)

    datUpdate foreach {dat => analyzers += (dat -> self.asInstanceOf[Analyzer]) }
    validData = (validData ++ datCreate ++ datUpdate).distinct
    (outputBlock)
  }

  datCreate foreach {dat => analyzers += (dat -> self.asInstanceOf[Analyzer]) }
}

/**
 * Iterative traversal of the IR with some convergence condition
 *
 * TODO: Expected behavior for iterative traversal to attempt to run an analyzer prior to every iteration?
 */
trait IterativeTraversal extends Traversal { self =>
  import IR._

  protected val MAX_ITERS: Int = 10     // maximum number of iterations to run
  protected val MAX_RETRIES: Int = 1    // maximum number of retries to allow
  protected var runs = 0                // Current analysis iteration
  private var retries = 0               // Current retry
  private var _retry = false

  def hasConverged: Boolean = runs > 0 && !getMetadataUpdateFlag()
  def hasCompleted: Boolean = true

  // TODO: Should these be exception throws instead?
  def failedToConverge() { printerr(s"$name did not converge within $MAX_ITERS iterations.") }
  def failedToComplete() { printerr(s"$name did not complete.") }

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

        //println("-----------------------------------")
        //println(s"Beginining run #$runs of $name")

        clearMetadataUpdateFlag()
        curBlock = runOnce(curBlock)
      }
      curBlock = postprocess(curBlock)
      retries += 1

      //if (_retry && retries <= MAX_RETRIES) println(s"Resetting and retrying $name")

    } while (_retry && retries <= MAX_RETRIES)

    if (!hasCompleted || !hasConverged) { IR.hadErrors = true }

    if (!hasCompleted) failedToComplete()
    else if (!hasConverged) failedToConverge()
    else {
      // Update metadata state
      datUpdate foreach {dat => analyzers += (dat -> self.asInstanceOf[Analyzer]) }
      validData = (validData ++ datCreate ++ datUpdate).distinct
    }

    (curBlock)
  }
}