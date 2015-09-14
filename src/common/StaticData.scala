package scala.lms
package common

import java.io.PrintWriter
import scala.lms.codegen.GenericCodegen
import scala.reflect.SourceContext
import scala.lms.internal._

trait StaticData extends Base {
  def staticData[T:Typ](x: T): Rep[T]
}

trait StaticDataExp extends BaseExp {
  case class StaticData[T](x: T) extends Def[T]
  def staticData[T:Typ](x: T)(implicit pos: SourceContext): Exp[T] = toAtom(StaticData(x))

  // StaticData doesn't play well with control dependencies.. looks like we somehow lose updates
  override implicit def toAtom[T:Typ](d: Def[T])(implicit pos: SourceContext) = d match {
    case StaticData(x) if addControlDeps =>
      val save = conditionalScope
      conditionalScope = false
      val z = super.toAtom(d)
      conditionalScope = save
      z
    case _ => super.toAtom(d)
  }

  override def isMutable[A](w: Sym[A]): Boolean = findStm(w) match {
    case Some(TP(_, StaticData(_))) => true
    case _ => super.isMutable(w)
  }

  override def mirror[A:Typ](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case StaticData(x) => staticData(x)(mtype(typ[A]),pos)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait BaseGenStaticData extends GenericCodegen {
  val IR: StaticDataExp
  import IR._

  def getFreeDataExp[A](sym: Sym[A], rhs: Def[A]): List[(Sym[Any],Any)] = rhs match {
    case StaticData(x) => List((sym,x))
    case _ => Nil
  }

  override def getFreeDataBlock[A](start: Block[A]): List[(Sym[Any],Any)] = {
    focusBlock(start) {
      innerScope flatMap {
        case TP(sym, rhs) =>
          getFreeDataExp(sym, rhs)
        case _ => Nil //static data is never fat
      }
      /*focusExactScope(start) { levelScope =>
        levelScope flatMap {
          case TP(sym, rhs) =>
            getFreeDataExp(sym, rhs)
          case _ => Nil //static data is never fat
        }
      }*/
    }
  }

}

trait ScalaGenStaticData extends ScalaGenBase with BaseGenStaticData {
  val IR: StaticDataExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StaticData(x) =>
      emitValDef(sym, "p"+quote(sym) + " // static data: " + (x match { case x: Array[_] => "Array("+x.mkString(",")+")" case _ => x }))
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenStaticData extends CGenBase with BaseGenStaticData {
  val IR: StaticDataExp
  import IR._

  // TODO: this does not quite do the right thing!
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case StaticData(x) =>
      emitValDef(sym, /*"p"+quote(sym)*/ (x match {
        case x: Array[a] =>
          val tp = sym.tp.typeArguments(0).asInstanceOf[Typ[a]]
          "("+remap(tp)+"[]){"+x.map(v=>quote(Const(v)(tp))).mkString(",")+"}"
        case _ => quote(Const(x)(sym.tp))
      }))
    case _ => super.emitNode(sym, rhs)
  }
}
