package scala.virtualization.lms
package common

import scala.virtualization.lms.util._
import scala.virtualization.lms.internal._

trait HashCodeOps extends Base {
    def __hashCode(x: Rep[Any]): Rep[Int]
}

trait HashCodeOpsExp extends BaseExp with EffectExp {
    case class HashCode(x: Rep[Any]) extends Def[Int]
    def __hashCode(x: Rep[Any]) = reflectEffect(HashCode(x))
}

trait ScalaGenHashCodeOps extends ScalaGenBase {
    val IR: HashCodeOpsExp
	import IR._
 
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) =  { 
        rhs match {
            case HashCode(x) => emitValDef(sym, quote(x) + ".hashCode")
		    case _ => super.emitNode(sym, rhs)
        }
    }
}
