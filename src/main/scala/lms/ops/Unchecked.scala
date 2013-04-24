package scala.lms
package ops

import java.io.PrintWriter
import internal._
import scala.reflect.SourceContext

trait UncheckedOps extends Base {

  def unchecked[T:Manifest](s: Any*): Rep[T]
  def uncheckedPure[T:Manifest](s: Any*): Rep[T]

  implicit def richQuote(c: StringContext) = new {
    def raw(args: Thunk[Rep[Any]]*) = new {
      def as[T]: Rep[T] = {
        //reflect(c.s(args map (a => reify(a.eval())):_*))
        def merge(a: List[Any], b: List[Any]): List[Any] = a match {
          case Nil => Nil
          case x::xs => x::merge(b,a)
        }
        unchecked(merge(c.parts.toList, args.toList.map(_.eval())):_*)
      }
    }
  }

  // args: =>Code* is not allowed so we make thunks explicit
  case class Thunk[+A](eval: () => A)
  implicit def toThunk[A](x: =>A) = new Thunk(() => x)

}

trait UncheckedOpsExp extends EffectExp {

  // TODO: use reifyEffects

  case class Unchecked[T](s: List[Any]) extends Def[T]
  def unchecked[T:Manifest](s: Any*): Rep[T] = reflectEffect[T](Unchecked(s.toList))
  def uncheckedPure[T:Manifest](s: Any*): Rep[T] = toAtom[T](Unchecked(s.toList))

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    //case Reflect(ThrowException(s), u, es) => reflectMirrored(Reflect(ThrowException(f(s)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    // TODO mirror Unchecked and Reflect(Unchecked)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]
}

trait ScalaGenUncheckedOps extends ScalaGenBase {
  val IR: UncheckedOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Unchecked(xs) =>
      emitValDef(sym, xs map ((x:Any)=> x match { case x: Exp[_] => quote(x) case x => x.toString }) mkString "")
    case _ => super.emitNode(sym, rhs)
  }
}

trait CGenUncheckedOps extends CGenBase {
  val IR: UncheckedOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case Unchecked(xs) =>
      emitValDef(sym, xs map ((x:Any)=> x match { case x: Exp[_] => quote(x) case x => x.toString }) mkString "")
    case _ => super.emitNode(sym, rhs)
  }
}
