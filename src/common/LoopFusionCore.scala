package scala.virtualization.lms
package common


trait LoopFusionCore2 extends LoopFusionExtractors with BaseFatExp with LoopsFatExp with IfThenElseExp with BooleanOpsExp

// TODO doc
trait LoopFusionExtractors extends internal.Expressions { // copied from LoopFusionOpt: LoopFusionCore trait

  def unapplySimpleIndex(e: Def[Any]): Option[(Exp[Any], Exp[Int])] = None
  // TODO should be unapplySimpleDomain(e: Def[Int]), but how to make unchecked warnings go away?
  // Def[Int @unchecked] isn't sufficient
  def unapplySimpleDomain(e: Def[Any]): Option[Exp[Any]] = None
  def unapplyFixedDomain(e: Def[Any]): Option[Exp[Int]] = None
  def unapplySimpleCollect(e: Def[Any]): Option[Exp[Any]] = None
  def unapplySimpleCollectIf(e: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = None
  def unapplyMultiCollect[T](a: Def[T]): Option[(Exp[T], Option[() => Exp[T]])] = None
  def unapplyReduce[T](e: Def[T]): Option[(Exp[T], Option[Exp[T]], Option[Boolean])] = None
  // def unapplyForeach(e: Def[Any]): Option[Exp[Unit]] = None

  object SimpleIndex {
    def unapply(a: Def[Any]): Option[(Exp[Any], Exp[Int])] = unapplySimpleIndex(a)
  }

  object SimpleDomain {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySimpleDomain(a)
  }

  object FixedDomain {
    def unapply(a: Def[Any]): Option[Exp[Int]] = unapplyFixedDomain(a)
  }

  object SimpleCollect {
    def unapply(a: Def[Any]): Option[Exp[Any]] = unapplySimpleCollect(a)
  }

  object SimpleCollectIf {
    def unapply(a: Def[Any]): Option[(Exp[Any],List[Exp[Boolean]])] = unapplySimpleCollectIf(a)
  }

  // first exp is array result, second exp is empty array expression
  object MultiCollect {
    def unapply[T](a: Def[T]): Option[(Exp[T], Option[() => Exp[T]])] = unapplyMultiCollect(a)
  }

  // first exp is reduce block, second is neutral element, third is associativity
  object Reduce {
    def unapply[T](a: Def[T]): Option[(Exp[T], Option[Exp[T]], Option[Boolean])] = unapplyReduce(a)
  }

  // object Foreach {
  //   def unapply(a: Def[Any]): Option[Exp[Unit]] = unapplyForeach(a)
  // }

  object ResultBlock {
    def unapply(a: Def[Any]): Option[Exp[Any]] = a match {
      case SimpleCollect(res) => Some(res)
      case SimpleCollectIf(res, _) => Some(res)
      case MultiCollect(res, _) => Some(res)
      case Reduce(res, _, _) => Some(res)
      case _ => None
    }
  }

  object AllBlocks {
    def unapply(a: Def[Any]): Option[List[Exp[Any]]] = a match {
      case SimpleCollect(res) => Some(List(res))
      case SimpleCollectIf(res, conds) => Some(res :: conds)
      case MultiCollect(res, _) => Some(List(res))
      case Reduce(res, _, _) => Some(List(res))
      case _ => None
    }
  }
}