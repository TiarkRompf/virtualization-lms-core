package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.GenericNestedCodegen

trait StaticDataExp extends EffectExp {
  case class StaticData[T](x: T) extends Def[T]
  def staticData[T:Manifest](x: T): Exp[T] = StaticData(x)

  override def isWritableSym[A](w: Sym[A]): Boolean = findDefinition(w) match {
    case Some(TP(_, StaticData(_))) => true
    case _ => super.isWritableSym(w)
  }
}

trait BaseGenStaticData extends GenericNestedCodegen {
  val IR: StaticDataExp
  import IR._
  
  def getFreeDataExp[A](sym: Sym[A], rhs: Def[A]): List[(Sym[Any],Any)] = rhs match {
    case StaticData(x) => List((sym,x))
    case _ => Nil
  }
  
  override def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = {
    focusBlock(start) {
      focusExactScope(start) { levelScope =>
        levelScope flatMap { case TP(sym, rhs) =>
          getFreeDataExp(sym, rhs)
        }
      }
    }
  }
  
}

trait ScalaGenStaticData extends ScalaGenEffect with BaseGenStaticData {
  val IR: StaticDataExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any])(implicit stream: PrintWriter) = rhs match {
    case StaticData(x) => 
      emitValDef(sym, "p"+quote(sym) + " // static data: " + x)
    case _ => super.emitNode(sym, rhs)
  }
  
}

