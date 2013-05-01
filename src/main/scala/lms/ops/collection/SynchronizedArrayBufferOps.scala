package scala.lms
package ops

import internal.GenericNestedCodegen

import java.io.PrintWriter
import scala.collection.mutable.ArrayBuffer
import scala.reflect.SourceContext

trait SynchronizedArrayBufferOps extends ArrayBufferOps {

/*
  object SynchronizedArrayBuffer {
    def apply[A:TypeRep](xs: Rep[A]*)(implicit pos: SourceContext) = arraybuffer_new(xs)
  }
*/

}

trait SynchronizedArrayBufferOpsExp extends SynchronizedArrayBufferOps with ArrayBufferOpsExp with LiftArrayBufferType {
  case class SyncArrayBufferNew[A:TypeRep](xs: Seq[Exp[A]]) extends Def[ArrayBuffer[A]]  {
    val mA = typeRep[A]
  }

  // all array buffers are synchronized (nackward compat). TODO: separate constructor

  override def arraybuffer_new[A:TypeRep](xs: Seq[Exp[A]])(implicit pos: SourceContext) = reflectMutable(SyncArrayBufferNew(xs))
}

trait BaseGenSynchronizedArrayBufferOps extends BaseGenArrayBufferOps {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._
}

trait ScalaGenSynchronizedArrayBufferOps extends BaseGenSynchronizedArrayBufferOps with ScalaGenArrayBufferOps {
  val IR: SynchronizedArrayBufferOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case a@SyncArrayBufferNew(xs) => emitValDef(sym, "(new scala.collection.mutable.ArrayBuffer[" + remap(a.mA) + "] with scala.collection.mutable.SynchronizedBuffer[" + remap(a.mA) + "]) ++= List(" + (xs map {quote}).mkString(",") + ")")
    case _ => super.emitNode(sym, rhs)
  }
}
