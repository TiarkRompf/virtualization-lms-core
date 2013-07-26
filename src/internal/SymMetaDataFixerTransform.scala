package scala.virtualization.lms
package internal

import scala.virtualization.lms.util.ReflectionUtil

/**
 * There are some meta-data added to Sym using infix
 * operations in ExtendedExpressions.
 * This trait fixes this properties, e.g parentBlock
 * and refCount.
 *
 * This information are gathered by a single pass
 * traversal over Exp graph.
 */
trait SymMetaDataFixerTransform extends NestedBlockTraversal {
  val IR: ExtendedExpressions with Effects
  import IR._

  override def traverseBlockFocused[A](block: Block[A]): Unit = {
    focusExactScope(block) { levelScope =>
      levelScope foreach { stm => stm match {
          case TP(sym, rhs) => sym.setParentBlock(Some(block))
          case _ => 
        }
      }

      traverseStmsInBlock(levelScope)
    }
  }

  override def traverseStm(stm: Stm): Unit = { // override this to implement custom traversal
    stm match {
      case TP(sym, rhs) => {
        rhs match {
          case Reflect(s, u, effects) => {
            if(!mustOnlyRead(u) && !mustOnlyAlloc(u)) {
              sym.incRefCount(100)
            }
            increaseRefCountsOnRhs(sym, s)
          }
          case Reify(s, u, effects) => {
            sym.incRefCount(-1000) // just ignore -- effects are accounted for in emitBlock
            s match {
              case s@Sym(n) => s.incRefCount(1)
              case Const(x) =>
            }
          }
          case rhs => increaseRefCountsOnRhs(sym, rhs)
        }
      }
      case _ =>
    } 
    blocks(stm.rhs) foreach traverseBlock
  }

  private def increaseRefCountsOnRhs[A](s: Exp[Any], rhs: Def[A]): Unit = {
    val sym = s.asInstanceOf[Sym[Any]]
    ReflectionUtil.caseNameTypeValues(rhs) foreach {
      x => if(x._2 == classOf[Block[A]]) {
        //println("Blockckckckck {===")
        val blk: Block[A] = x._3.asInstanceOf[Block[A]]
        blk.res match {
          case s:Sym[_] => if(s.inSameParentBlockAs(sym)) { s.incRefCount(1) } else { s.incRefCount(10) }
          case _ =>
        }
        //transformBlock[Any](blk)
      } else if(x._2 == classOf[Exp[A]]) {
        x._3 match {
          case s:Sym[_] => if(s.inSameParentBlockAs(sym)) { s.incRefCount(1) } else { s.incRefCount(10) }
          case _ =>
        }
      } else if (x._2 == classOf[Variable[A]]) {
        x._3.asInstanceOf[Variable[A]].e match {
          case s:Sym[_] => if(s.inSameParentBlockAs(sym)) { s.incRefCount(1) } else { s.incRefCount(10) }
          case _ =>
        }
      }
    }
  }

}