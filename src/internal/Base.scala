package scala.lms
package internal

trait LiftAll extends Base {
  protected implicit def __unit[T:Typ](x: T) = unit(x)
}

trait Base extends EmbeddedControls {
  type Rep[+T]
  type Typ[T]

  def typ[T:Typ]: Typ[T]
  implicit def unitTyp: Typ[Unit]
  implicit def nullTyp: Typ[Null]

  // Always lift Unit and Null (for now)
  protected def unit[T:Typ](x: T): Rep[T]
  implicit def unitToRepUnit(x: Unit) = unit(x)
  implicit def nullToRepNull(x: Null) = unit(x)
}

trait BaseExp extends Base with TransformingExp with AnalyzingExp {
  type Rep[+T] = Exp[T]
}

// --- Codegen
// The traits below provide an interface to codegen so that clients do not need to depend on internal._
trait ScalaGenBase extends ScalaCodegen
trait CLikeGenBase extends CLikeCodegen
trait CudaGenBase extends CudaCodegen
trait CGenBase extends CCodegen
trait OpenCLGenBase extends OpenCLCodegen
trait GPUGenBase extends GPUCodegen
