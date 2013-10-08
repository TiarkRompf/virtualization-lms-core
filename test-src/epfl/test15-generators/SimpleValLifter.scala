package dbtoptimizer.lifters

import xml._
import scala.actors.Actor._
import scala.virtualization.lms.common._
import org.dbtoaster.dbtoasterlib.K3Collection._
import scala.reflect.SourceContext

/**
 * Lifter Classes for SimpleVal
 */
trait SimpleValOps extends Base with Variables {

  class SimpleValOpsCls[K: Manifest](x: Rep[SimpleVal[K]]) {
    def apply(name: Rep[String], defVal: Rep[K]): Rep[SimpleVal[K]] =  svApply[K](name, defVal);
    def get(): Rep[K] = svGet[K](x)
    def update(value: Rep[K]) = svUpdate[K](x, value)
    def mutable: Rep[SimpleVal[K]] = svMutable[K](x)
  }

  implicit def simpleVal2SimpleValOpsCls[K: Manifest](x: Rep[SimpleVal[K]]): SimpleValOpsCls[K] = new SimpleValOpsCls[K](x)
  def svApply[K: Manifest](name: Rep[String], defVal: Rep[K]): Rep[SimpleVal[K]]
  def svGet[K:Manifest](x: Rep[SimpleVal[K]]): Rep[K]
  def svUpdate[K](x: Rep[SimpleVal[K]], value: Rep[K]): Rep[Unit]
  def svMutable[K: Manifest](x: Rep[SimpleVal[K]]): Rep[SimpleVal[K]]
}

trait SimpleValExp extends SimpleValOps with BaseExp with EffectExp with VariablesExp {

  case class SvGet[K:Manifest](x: Exp[SimpleVal[K]]) extends Def[K] {
    val mK = manifest[K]
  }
  case class SvUpdate[K](x: Exp[SimpleVal[K]], value: Exp[K]) extends Def[Unit]
  case class NewSimpleVal[K](mK: Manifest[K], name: Exp[String], defVal: Exp[K]) extends Def[SimpleVal[K]]
  case class SvMutable[K](x: Exp[SimpleVal[K]]) extends Def[SimpleVal[K]]

  def newSimpleVal[K: Manifest](name: Exp[String], defVal: Exp[K]): Exp[SimpleVal[K]] = reflectMutable(NewSimpleVal(manifest[K], name, defVal));

  def svApply[K: Manifest](name: Exp[String], defVal: Exp[K]): Exp[SimpleVal[K]] = newSimpleVal[K](name, defVal);
  def svGet[K:Manifest](x: Exp[SimpleVal[K]]) = SvGet[K](x);
  def svUpdate[K](x: Exp[SimpleVal[K]], value: Exp[K]) = reflectWrite(x)(SvUpdate[K](x, value));
  def svMutable[K: Manifest](x: Exp[SimpleVal[K]]) = reflectMutable(SvMutable[K](x))
 
  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case NewSimpleVal(mk, name, defVal) => NewSimpleVal(mtype(mk), f(name), f(defVal))
    case e@SvGet(x) => svGet(f(x))(mtype(e.mK))
    case SvUpdate(x, value) => svUpdate(f(x), f(value))
    case SvMutable(x) => svMutable(f(x))
    case Reflect(NewSimpleVal(mk, name, defVal), u, es) => reflectMirrored(Reflect(NewSimpleVal(mtype(mk), f(name), f(defVal)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@SvGet(x), u, es) => reflectMirrored(Reflect(SvGet(f(x))(mtype(e.mK)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(SvUpdate(x, value), u, es) => reflectMirrored(Reflect(SvUpdate(f(x), f(value)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(SvMutable(x), u, es) => reflectMirrored(Reflect(SvMutable(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
}

trait ScalaGenSimpleVal extends ScalaGenBase {
  val IR: SimpleValExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewSimpleVal(mK, name, defVal) => emitValDef(sym, "SimpleVal[" + mK + "](" + quote(name) + "," + quote(defVal) + ")")
    case SvGet(x) => emitValDef(sym, "" + quote(x) + ".get()");
    case SvUpdate(x, value) => emitValDef(sym, "" + quote(x) + ".update(" + quote(value) + ")");
    case SvMutable(x) => emitValDef(sym, "" + quote(x) + " // mutable SimpleVal");
    case _ => super.emitNode(sym, rhs)
  }
}