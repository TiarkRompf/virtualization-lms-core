package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.internal.{GenericNestedCodegen, ScalaGenEffect}

trait While extends Base {
  def __whileDo(cond: => Rep[Boolean], body: => Rep[Unit])
}


trait WhileExp extends While with FunctionsExp { 
  case class While(cond: () => Exp[Boolean], body: Exp[Unit]) extends Def[Unit]

  override def __whileDo(cond: => Exp[Boolean], body: => Rep[Unit]) {
    val a = reifyEffects(body)
    reflectEffect(While(() => cond, a))
  }
}


trait BaseGenWhile extends GenericNestedCodegen {
  val IR: WhileExp
  import IR._

  override def syms(e: Any): List[Sym[Any]] = e match {
    case While(c, b) if shallow => Nil
    case _ => super.syms(e)
  }
}
trait ScalaGenWhile extends ScalaGenEffect with BaseGenWhile {
  import IR._

  override def emitNode(sym: Sym[_], rhs: Def[_])(implicit stream: PrintWriter) = rhs match {
    case While(c,b) =>
      val c_blk = reifyEffects(c())
      stream.print("while ({")
      emitBlock(c_blk)
      stream.print(quote(getBlockResult(c_blk)))
      stream.println("}) {")
      emitBlock(b)
      stream.println(quote(getBlockResult(b)))
      stream.println("}")

    case _ => super.emitNode(sym, rhs)
  }
}