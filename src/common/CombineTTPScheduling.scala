package scala.virtualization.lms
package common


/** This extension to the scheduler combines sets of TTPs whose
  * FatDefs extend trait CanBeFused and have the same fusedSetID.
  * The fusion into one TTP per set is controlled by overriding
  * FatScheduling.combineFat(List[TTP]).
  * This pass must be run if the LoopFusionTransformers are in
  * use to prevent duplication of code and computations.
  */
trait CombineTTPScheduling extends internal.FatBlockTraversal {
  val IR: LoopFusionCore
  import IR._  

  // Assuming that the first focusExactScopeFat call contains the full scope
  // of the entire program. Don't rescan inner scopes.
  var needsFusion = true

  override def focusExactScopeFat[A](resultB: List[Block[Any]])(body: List[Stm] => A): A = {
    if (needsFusion) {
      val fusedScope = fuseAllLoops(innerScope)
      needsFusion = false
      innerScope = getSchedule(fusedScope)(resultB) // reschedule to get correct order
      // println("-- full graph after fusion " + innerScope.mkString("\n", "\n", "\n"))
    }
    super.focusExactScopeFat(resultB)(body)
  }

  def fuseAllLoops(currentScope: List[Stm]): List[Stm] = {
    // build map to lookup loops&ifs to be fused
    val (toBeFused, restOfScope) = currentScope.partition { 
      case TTP(_, _, c: CanBeFused) if c.getFusedSetID.isDefined => true
      case _ => false
    }
    val fusedSetsMap = new scala.collection.mutable.HashMap[Int, List[TTP]]()
    toBeFused.foreach({ 
      case e @ TTP(_, _, c: CanBeFused) => 
        val id = c.getFusedSetID.get
        fusedSetsMap.update(id, e :: fusedSetsMap.getOrElse(id, Nil))
      case _ => sys.error("(FTO) impossible error")
    })

    // fuse TTPs of each set into one fat TTP per set
    val fusedTTPs = fusedSetsMap.toList.reverse.map({ t => t._2.reverse }).map({ TTPsToFuse =>
      TTPsToFuse(0) match {
        case TTP(_, _, l: AbstractFatLoop) => 
          printlog("(FTO) Fusing these loops into one fat TTP: " + TTPsToFuse.mkString("\n", "\n", "\n"))
        case TTP(_, _, i: AbstractFatIfThenElse) => 
          printlog("(FTO) Fusing these ifs into one fat TTP: " + TTPsToFuse.mkString("\n", "\n", "\n"))
      }
      combineFat(TTPsToFuse)
    })

    fusedTTPs ::: restOfScope
  }
}

/** This trait is mixed into all Defs&FatDefs that can be fused (new loop&if fusion,
  * SoA transformation, ...). Sets of TTPs with the same fusedSetID will be fused by
  * CombineTTPScheduling, using FatScheduling.combineFat(List[TTP]).
  */
trait CanBeFused {
  import CanBeFused._
  private var fusedSetID: Option[Int] = None
  private def updateID() = fusedSetID match {
    case Some(id) => fusedSetID = Some(CanBeFused.getUpdatedID(id))
    case None =>
  }
  def registerFusion(other: CanBeFused): Unit = {
    updateID()
    other.updateID()
    (fusedSetID, other.fusedSetID) match {
      case (None, None) => 
        fusedSetID = Some(getAndIncrementID)
        other.fusedSetID = fusedSetID
      case (None, id@Some(_)) => fusedSetID = id
      case (id@Some(_), None) => other.fusedSetID = id
      case (Some(id1), Some(id2)) => 
        // TODO warning msg instead of assert
        assert(id1 != id2, "ERROR: CanBeFused ids " + id1 + " and " + id2 + "are already fused")
        fusedSetID = Some(id2)
        registerMerge(id1, id2)
    }
  }
  def copyMirroredCanBeFused(old: Any): this.type = old match {
    case c: CanBeFused =>
      fusedSetID = c.fusedSetID
      updateID()
      this
    case _ => this
  }
  def isFusedWith(other: CanBeFused) = {
    updateID()
    other.updateID()
    (fusedSetID.isDefined && fusedSetID == other.fusedSetID)
  }
  def getFusedSetID = fusedSetID
}
object CanBeFused {
  private var nextId = 0
  private lazy val mergedSets = new scala.collection.mutable.HashMap[Int, Int]()
  def getAndIncrementID = { nextId += 1; nextId - 1 }
  def getUpdatedID(id: Int): Int = mergedSets.get(id).map({ secondID =>
    val thirdID = getUpdatedID(secondID)
    if (thirdID != secondID) 
      mergedSets.update(id, thirdID)
    thirdID
  }).getOrElse(id)
  def registerMerge(oldID: Int, newID: Int) = mergedSets.update(oldID, newID)
}
