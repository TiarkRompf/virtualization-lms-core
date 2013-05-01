package scala.lms

import internal._
import scala.reflect.SourceContext

/**
 * This trait automatically lifts any concrete instance to a representation.
 */
trait LiftAll extends Base {
  protected implicit def __unit[T:TypeRep](x: T) = unit(x)
}

/**
 * Component for abstraction over the run-time representation of types. The `TypeRep` abstraction
 * can carry additional information with the run-time type (e.g. bit width for hardware representation).
 *
 * NOTE: Parametric types must be lifted explicitly since compiler does not generate
 * TypeRep[X[T]] if implict TypeRep[T] is in scope. For example, @see LiftArrayType.
 */
trait TypeRepBase {
  trait TypeRep[T] {
    def mf: Manifest[T]

    def typeArguments: List[Manifest[_]]
    def arrayManifest: Manifest[Array[T]]
    def runtimeClass: java.lang.Class[_]
    def erasure: java.lang.Class[_]
    def <:<(that: TypeRep[_]): Boolean
  }

  case class TypeExp[T](mf: Manifest[T]) extends TypeRep[T] {
    def typeArguments: List[Manifest[_]]   = mf.typeArguments
    def arrayManifest: Manifest[Array[T]] = mf.arrayManifest
    def runtimeClass: java.lang.Class[_] = mf.runtimeClass
    def <:<(that: TypeRep[_]): Boolean = mf.<:<(that.mf)
    def erasure: java.lang.Class[_] = mf.erasure
    override def canEqual(that: Any): Boolean = mf.canEqual(that)
    override def equals(that: Any): Boolean = mf.equals(that)
    override def hashCode = mf.hashCode
    override def toString = mf.toString
  }

  def typeRep[T](implicit tr: TypeRep[T]): TypeRep[T] = tr
  implicit def typeRepFromManifest[T](implicit mf: Manifest[T]): TypeRep[T] = TypeExp(mf)
  implicit def convertFromManifest[T](mf: Manifest[T]): TypeRep[T] = TypeExp(mf)
}

/**
 * The Base trait defines the type constructor Rep, which is the higher-kinded type that allows for other DSL types to be
 * polymorphically embedded.
 *
 * @since 0.1
 */
trait Base extends EmbeddedControls with TypeRepBase {
  type API <: Base

  type Rep[+T]

  protected def unit[T:TypeRep](x: T): Rep[T]

  // always lift Unit and Null (for now)
  implicit def unitToRepUnit(x: Unit) = unit(x)
  implicit def nullToRepNull(x: Null) = unit(x)
}

/**
 * This trait sets the representation to be based on AST Expression nodes.
 *
 * @since 0.1
 */
trait BaseExp extends Base with Expressions with Blocks with Transforming {
  type Rep[+T] = Exp[T]

  protected def unit[T:TypeRep](x: T) = Const(x)
}

trait BlockExp extends BaseExp

/*
trait BlockExp extends BaseExp with Blocks {

  implicit object CanTransformBlock extends CanTransform[Block] {
    def transform[A](x: Block[A], t: Transformer): Block[A] = Block(t(x.res))
  }

}
*/

trait EffectExp extends BaseExp with Effects {

  def mapOver(t: Transformer, u: Summary) = { // TODO: move to effects class?
    u.copy(mayRead = t.onlySyms(u.mayRead), mstRead = t.onlySyms(u.mstRead),
      mayWrite = t.onlySyms(u.mayWrite), mstWrite = t.onlySyms(u.mstWrite))
  }

  override def mirrorDef[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = e match {
    case Reflect(x, u, es) => Reflect(mirrorDef(x,f), mapOver(f,u), f(es))
    case Reify(x, u, es) => Reify(f(x), mapOver(f,u), f(es))
    case _ => super.mirrorDef(e,f)
  }

  override def mirror[A:TypeRep](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = e match {
/*
    case Reflect(x, u, es) =>
      reifyEffects {
        context = f(es)
        mirror(x)
      }

*/
//    case Reflect(Print(x), u, es) => Reflect(Print(f(x)), es map (e => f(e)))
    case Reflect(x, u, es) => reflectMirrored(mirrorDef(e,f).asInstanceOf[Reflect[A]])
    case Reify(x, u, es) => Reify(f(x), mapOver(f,u), f(es)) //TODO: u
    case _ => super.mirror(e,f)
  }

}

trait BaseFatExp extends BaseExp with FatExpressions with FatTransforming


// The traits below provide an interface to codegen so that client do
// not need to depend on internal._

trait ScalaGenBase extends ScalaCodegen
trait ScalaGenEffect extends ScalaNestedCodegen with ScalaGenBase
trait ScalaGenFat extends ScalaFatCodegen with ScalaGenBase
