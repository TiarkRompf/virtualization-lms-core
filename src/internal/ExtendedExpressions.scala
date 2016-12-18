package scala.lms
package internal

import scala.reflect.SourceContext
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ListBuffer
import java.lang.{StackTraceElement,Thread}


trait ExtendedExpressions extends Expressions with Blocks {
  val RefCountAttributeKey = "refCnt"
  val ParentBlockAttributeKey = "pBlk"

  def infix_refCount(s: Sym[Any]): Int = {
    s.attributes.get(RefCountAttributeKey).getOrElse(0).asInstanceOf[Int]
  }

  def infix_incRefCount(s: Sym[Any], inc: Int): Unit = {
    s.setRefCount(s.refCount + inc)
  }

  def infix_setRefCount(s: Sym[Any], refCnt: Int): Unit = {
    s.attributes.update(RefCountAttributeKey, refCnt)
  }

  def infix_parentBlock(s: Sym[Any]): Option[Block[Any]] = {
    s.attributes.get(ParentBlockAttributeKey).asInstanceOf[Option[Block[Any]]]
  }

  def infix_setParentBlock(s: Sym[Any], pBlk: Option[Block[Any]]): Unit = pBlk match {
    case Some(blk) => s.attributes.update(ParentBlockAttributeKey, blk)
    case None =>
  }

  def infix_inSameParentBlockAs(thiz: Sym[Any], other: Sym[Any]): Boolean = {
    val thizParent: Option[Block[Any]] = thiz.attributes.get(ParentBlockAttributeKey).asInstanceOf[Option[Block[Any]]]
    val otherParent: Option[Block[Any]] = other.attributes.get(ParentBlockAttributeKey).asInstanceOf[Option[Block[Any]]]
    thizParent match {
      case Some(thizP) => otherParent match {
        case Some(otherP) => thizP.res == otherP.res
        case None => false
      }
      case None => otherParent match {
        case Some(otherP) => false
        case None => true
      }
    }
  }

  def infix_possibleToInline(s: Sym[Any]): Boolean = s.refCount == 1

  def infix_noReference(s: Sym[Any]): Boolean = s.refCount == 0
}