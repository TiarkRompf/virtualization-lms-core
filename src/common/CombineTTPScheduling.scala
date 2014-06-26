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
  val IR: LoopsFatExp with IfThenElseFatExp
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
      case _ => sys.error("(CTS) impossible error")
    })
    // fuse TTPs of each set into one fat TTP per set
    val fusedTTPs = fusedSetsMap.toList.reverse.map({ t => t._2.reverse }).map({ TTPsToFuse =>
      TTPsToFuse match {
        case single :: Nil => single
        case TTP(_, _, fat) :: others => 
          val ttpType = fat match {
            case l: AbstractFatLoop => "loops"
            case i: AbstractFatIfThenElse => "ifs"
            case _ => sys.error("(CTS) unknown TTPs: " + TTPsToFuse)
          }
          printlog("(CTS) Fusing these " + ttpType + " into one fat TTP: " + TTPsToFuse.mkString("\n", "\n", "\n"))
          combineFat(TTPsToFuse)
        case _ => sys.error("(CTS) not TTPs: " + TTPsToFuse)
      }
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
  private def updateID(): Unit = { fusedSetID = fusedSetID.map({ id => CanBeFused.getUpdatedID(id) }) }
  def registerFusion(other: CanBeFused): Unit = {
    updateID()
    other.updateID()
    (fusedSetID, other.fusedSetID) match {
      case (None, None) => 
        fusedSetID = Some(newSetID(this, other))
        other.fusedSetID = fusedSetID
      case (None, id@Some(_)) => 
        fusedSetID = id
        addChanged(id, this)
      case (id@Some(_), None) => 
        other.fusedSetID = id
        addChanged(id, other)
      case (Some(id1), Some(id2)) => 
        other.fusedSetID = Some(id1)
        registerMerge(id1, id2)
    }
  }
  // This should be called during mirroring, so that the fusion information
  // survives. Usually the old node will be DCE'd, but if both nodes
  // survive this has the side effect that they get fused.
  def copyCanBeFused(old: Any): this.type = old match {
    case c: CanBeFused =>
      fusedSetID = c.fusedSetID
      updateID()
      addChanged(fusedSetID, this)
      this
    case _ => this
  }
  def isFusedWith(other: CanBeFused) = {
    updateID()
    other.updateID()
    (fusedSetID.isDefined && fusedSetID == other.fusedSetID)
  }
  def getFusedSetID = fusedSetID
  def getFusedSet: Option[List[CanBeFused]] = {
    updateID()
    getSet(fusedSetID)
  }
}
object CanBeFused {
  private var nextId = 0
  private lazy val mergedSets = new scala.collection.mutable.HashMap[Int, Int]()
  // Contains all CanBeFused elements of the set, old and new.
  private lazy val setMembers = new scala.collection.mutable.HashMap[Int, List[CanBeFused]]()
  def newSetID(first: CanBeFused, second: CanBeFused) = {
    setMembers.update(nextId, List(first, second))
    nextId += 1
    nextId - 1
  }
  def getUpdatedID(id: Int): Int = mergedSets.get(id).map({ secondID =>
    val thirdID = getUpdatedID(secondID)
    if (thirdID != secondID) 
      mergedSets.update(id, thirdID)
    thirdID
  }).getOrElse(id)
  def registerMerge(preservedID: Int, droppedID: Int) = {
    if (preservedID != droppedID) {
      mergedSets.update(droppedID, preservedID)
      setMembers.update(preservedID, setMembers(preservedID) ::: setMembers.remove(droppedID).get)
    }
  }
  def addChanged(id: Option[Int], newC: CanBeFused) = {
    id.map({ ID => setMembers.update(ID, newC :: setMembers.get(ID).get) })
  }
  def getSet(id: Option[Int]) = id.map(setMembers(_))
}
