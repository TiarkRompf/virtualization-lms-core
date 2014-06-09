package scala.virtualization.lms
package common

trait LoopFusionExtractors extends internal.Expressions with LoopsExp {

  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  def unapplySimpleDomain(e: Def[Any]): Option[Exp[Any]] = None
  def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = None

  def unapplyEmptyColl(a: Def[Any]): Boolean = false
  def unapplyEmptyCollNewEmpty[T:Manifest](a: (Def[Any], Exp[T], Option[Sym[Int]])): Option[Exp[T]] = None

  def unapplySingletonColl(a: Def[Any]): Option[Exp[Any]] = None
  def unapplyMultiCollect[T](a: Def[T]): Option[Exp[T]] = None

  // exp is valueFunc block of reduce and bodies of for and foreach
  // boolean is true if type is Unit (for and foreach), false otherwise (reduce usually)
  def unapplyForlike[T](e: Def[T]): Option[(Exp[T], Boolean)] = None

  def ignoreIndex(e: Def[Any], index: Sym[Int]): Boolean = false

}

/** This trait is mixed into all nodes that can be fused (loop fusion, if-fusion, SoA
  * transformation).
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

trait LoopFusionCore2 extends LoopFusionExtractors with BaseFatExp with LoopsFatExp with IfThenElseExp with BooleanOpsExp {
  object SimpleIndex {
    def unapply(a: Def[Any]): Option[(Exp[Any], Exp[Int])] = unapplySimpleIndex(a)
  }

  object SimpleDomain {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySimpleDomain(a)
  }

  object FixedDomain {
    def unapply(a: Def[Any]): Option[Exp[Int]] = unapplyFixedDomain(a)
  }

  object EmptyColl {
    def unapply(a: Def[Any]): Boolean = unapplyEmptyColl(a) 
  }
  object EmptyCollNewEmpty {
    def unapply[T:Manifest](a: (Def[Any], Exp[T], Option[Sym[Int]])): Option[Exp[T]] = unapplyEmptyCollNewEmpty(a) 
  }

  object SingletonColl {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySingletonColl(a) 
  }

  object MultiCollect {
    def unapply[T](a: Def[T]): Option[Exp[T]] = unapplyMultiCollect(a)
  }

  object ForLike {
    def unapply[T](a: Def[T]): Option[(Exp[T], Boolean)] = unapplyForlike(a)
  }

  object ProducerResultBlock {
    def unapply(a: Def[Any]): Option[Exp[Any]] = a match {
      case SingletonColl(res) => Some(res)
      case MultiCollect(res) => Some(res)
      case _ => None
    }
  }

  override def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = e match {
    case EmptyColl() => Some(Const(0))
    case SingletonColl(_) => Some(Const(1))
    case EatReflect(loop: AbstractLoop[_]) => loop.body match {
      case MultiCollect(Def(EatReflect(SingletonColl(_)))) => Some(loop.size)
      case MultiCollect(Def(Reify(Def(EatReflect(SingletonColl(_))),_,_))) => Some(loop.size)
      case _ => super.unapplyFixedDomain(e)
    }
    case _ => super.unapplyFixedDomain(e)
  }
}
