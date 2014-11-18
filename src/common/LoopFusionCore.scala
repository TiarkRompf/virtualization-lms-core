package scala.virtualization.lms
package common

/** Override these functions to get fusion for your loops. */
trait LoopFusionExtractors extends internal.Expressions with LoopsExp {

  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  def unapplySimpleDomain(e: Def[Any]): Option[Exp[Any]] = None
  def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = None

  def unapplyEmptyColl(a: Def[Any]): Boolean = false
  def unapplyEmptyCollNewEmpty[T:Manifest](a: (Def[Any], Exp[T], Option[Sym[Int]])): Option[Exp[T]] = None

  def unapplySingletonColl(a: Def[Any]): Option[Exp[Any]] = None

  // FlatMap loops, which encompass map and filter through the use
  // of Empty and Singleton collections and if-then-else nodes
  def unapplyMultiCollect[T](a: Def[T]): Option[Exp[T]] = None

  // For/foreach loops
  def unapplyFor(e: Def[Unit @unchecked]): Option[Exp[Unit]] = None
  // Reduce loop, extractor gives valueFunc block, not reduceFunc
  def unapplyReduce[T](e: Def[T]): Option[Exp[T]] = None

  def ignoreIndex(e: Def[Any], index: Sym[Int]): Boolean = false

  def shouldApplyFusion = true
}

trait LoopFusionCore extends LoopFusionExtractors with BaseFatExp with LoopsFatExp with IfThenElseFatExp with BooleanOpsExp {
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

  object For {
    def unapply(a: Def[Unit]): Option[Exp[Unit]] = unapplyFor(a)
  }

  object Reduce {
    def unapply[T](a: Def[T]): Option[Exp[T]] = unapplyReduce(a)
  }

  object ProducerResultBlock {
    def unapply(a: Def[Any]): Option[Exp[Any]] = a match {
      case SingletonColl(res) => Some(res)
      case MultiCollect(res) => Some(res)
      case _ => None
    }
  }

  override def unapplyFixedDomain(e: Def[Any @unchecked]): Option[Exp[Int]] = e match {
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
