/**
 *     _______  _______  ___   __      ____     Automatic
 *    / __/ _ \/  _/ _ \/ _ | / /     / __/     * Implementation
 *   _\ \/ ___// // , _/ __ |/ /__   _\ \       * Optimization
 *  /___/_/  /___/_/|_/_/ |_/____/  /___/       * Platform Adaptation
 *                                              of DSP Algorithms
 *  https://bitbucket.org/GeorgOfenbeck/spirals
 *  SpiralS 0.1 Prototype - ETH Zurich
 *  Copyright (C) 2013  Alen Stojanov  (astojanov@inf.ethz.ch)
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see http://www.gnu.org/licenses/.
 */

package ethz.test15

import scala.reflect.SourceContext
import scala.virtualization.lms.common._
import ethz.test15.instrinsics.ISAGen
import scala.Some

trait IntrinsicsBase extends DSLBaseTypes {

  class Packed[T:Manifest](r: Rep[Packed[T]], size: Int) extends Vector(r, size) {
    val m = manifest[T]
    def this(size: Int) = this(fresh[Packed[T]], size)
    def getIRep () = super.getRep.asInstanceOf[Rep[Packed[T]]]
  }

  def getIVector[T](rep: Rep[Packed[T]]): Packed[T] = {
    val vec = getDSLTypeInstances(rep.asInstanceOf[Rep[DSLType]])
    if (vec.isInstanceOf[Packed[T]]) {
      vec.asInstanceOf[Packed[T]]
    } else {
      throw new DSLTypeIsNotAVector(rep.toString)
    }
  }

}

trait IntrinsicsArray extends IntrinsicsBase with Variables {
  def infix_vload [T:Manifest](a: Rep[Array[T]], pos: Rep[Int], vsize: Int): Rep[Packed[T]]
  def infix_vstore[T:Manifest](a: Rep[Array[T]], pos: Rep[Int], v: Rep[Packed[T]]): Rep[Unit]
}

trait IntrinsicsNumerics extends IntrinsicsBase with Variables {
  def infix_vadd    [T:Manifest](a: Rep[Packed[T]], b: Rep[Packed[T]]): Rep[Packed[T]]
  def infix_vsub    [T:Manifest](a: Rep[Packed[T]], b: Rep[Packed[T]]): Rep[Packed[T]]
  def infix_vmul    [T:Manifest](a: Rep[Packed[T]], b: Rep[Packed[T]]): Rep[Packed[T]]
  def infix_vdiv    [T:Manifest](a: Rep[Packed[T]], b: Rep[Packed[T]]): Rep[Packed[T]]
  def infix_hadd    [T:Manifest](a: Rep[Packed[T]], b: Rep[Packed[T]]): Rep[Packed[T]]
  def infix_permute2[T:Manifest](a: Rep[Packed[T]], b: Rep[Packed[T]], mask: Int): Rep[Packed[T]]
}

trait IntrinsicsArrayExp extends IntrinsicsArray with EffectExp with VariablesExp {

  case class VLoad  [T:Manifest] (a: Exp[Array[T]], pos: Exp[Int], vsize: Int)        extends Def[Packed[T]]
  case class VStore [T:Manifest] (a: Exp[Array[T]], pos: Exp[Int], v: Exp[Packed[T]]) extends Def[Unit]

  def infix_vload[T:Manifest](a: Exp[Array[T]], pos: Exp[Int], vsize: Int): Exp[Packed[T]] = {
    new Packed[T](VLoad(a, pos, vsize), vsize).getIRep
  }
  def infix_vstore[T:Manifest](a: Exp[Array[T]], pos: Exp[Int], v: Exp[Packed[T]]): Exp[Unit] = {
    reflectWrite(a)(VStore(a, pos, v))
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case VLoad(a, pos, size) => infix_vload (f(a),f(pos), size)
    case Reflect(VLoad(a,pos,size), u, es) => reflectMirrored(Reflect(VLoad(f(a),f(pos),size), mapOver(f,u), f(es)))
    case Reflect(VStore(a,pos,v), u, es) => reflectMirrored(Reflect(VStore(f(a),f(pos),f(v)), mapOver(f,u), f(es)))
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]

  override def isPrimitiveType[T](m: Manifest[T]) = m.toString match {
    case x if x.contains("Packed") => true
    case _ => super.isPrimitiveType(m)
  }

}

trait IntrinsicsNumericExp extends IntrinsicsNumerics with VariablesExp with BaseFatExp {

  case class HAdd     [T:Manifest] (a: Exp[Packed[T]], b: Exp[Packed[T]]) extends Def[Packed[T]]
  case class Permute2 [T:Manifest] (a: Exp[Packed[T]], b: Exp[Packed[T]], mask: Int) extends Def[Packed[T]]
  case class VSet1    [T:Manifest] (a: Exp[T], vsize: Int) extends Def[Packed[T]]

  case class VAdd [T:Manifest] (a: Exp[Packed[T]], b: Exp[Packed[T]]) extends Def[Packed[T]]
  case class VSub [T:Manifest] (a: Exp[Packed[T]], b: Exp[Packed[T]]) extends Def[Packed[T]]
  case class VMul [T:Manifest] (a: Exp[Packed[T]], b: Exp[Packed[T]]) extends Def[Packed[T]]
  case class VDiv [T:Manifest] (a: Exp[Packed[T]], b: Exp[Packed[T]]) extends Def[Packed[T]]

  def infix_vset1[T:Manifest](v: Exp[T], vsize: Int): Exp[Packed[T]] = {
    new Packed[T](VSet1(v, vsize), vsize).getIRep
  }

  def infix_hadd[T:Manifest](a: Exp[Packed[T]], b: Exp[Packed[T]]): Exp[Packed[T]] = {
    val va = getIVector(a)
    val vb = getIVector(b)
    assert(va.size == vb.size, "VAdd: Vectors must have equal size")
    new Packed[T](HAdd(a, b), va.size).getIRep
  }

  def infix_permute2[T:Manifest](a: Exp[Packed[T]], b: Exp[Packed[T]], mask: Int): Exp[Packed[T]] = {
    val va = getIVector(a)
    val vb = getIVector(b)
    assert(va.size == vb.size, "Permute2: Vectors must have equal size")
    new Packed[T](Permute2(a, b, mask), va.size).getIRep
  }

  def infix_vadd[T:Manifest](a: Exp[Packed[T]], b: Exp[Packed[T]]): Exp[Packed[T]] = {
    val va = getIVector(a)
    val vb = getIVector(b)
    assert(va.size == vb.size, "VAdd: Vectors must have equal size")
    new Packed[T](VAdd(a, b), va.size).getIRep
  }

  def infix_vsub[T:Manifest](a: Exp[Packed[T]], b: Exp[Packed[T]]): Exp[Packed[T]] = {
    val va = getIVector(a)
    val vb = getIVector(b)
    assert(va.size == vb.size, "VSub: Vectors must have equal size")
    new Packed[T](VSub(a, b), va.size).getIRep
  }

  def infix_vmul[T:Manifest](a: Exp[Packed[T]], b: Exp[Packed[T]]): Exp[Packed[T]] = {
    val va = getIVector(a)
    val vb = getIVector(b)
    assert(va.size == vb.size, "VMul: Vectors must have equal size")
    new Packed[T](VMul(a, b), va.size).getIRep
  }

  def infix_vdiv[T:Manifest](a: Exp[Packed[T]], b: Exp[Packed[T]]): Exp[Packed[T]] = {
    val va = getIVector(a)
    val vb = getIVector(b)
    assert(va.size == vb.size, "VDiv: Vectors must have equal size")
    new Packed[T](VDiv(a, b), va.size).getIRep
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case VAdd (a, b) => infix_vadd(f(a), f(b))
    case VSub (a, b) => infix_vsub(f(a), f(b))
    case VMul (a, b) => infix_vmul(f(a), f(b))
    case VDiv (a, b) => infix_vdiv(f(a), f(b))
    case HAdd (a, b) => infix_hadd(f(a), f(b))
    case VSet1(a, s) => infix_vset1(f(a), s)
    case Permute2(a, b, mask) => infix_permute2(f(a), f(b), mask)
    case _ => super.mirror(e,f);
  }).asInstanceOf[Exp[A]] // why??
}

trait IntrinsicsArrayExpOpt extends IntrinsicsArrayExp {

  override def infix_vload[T:Manifest](x: Exp[Array[T]], n: Exp[Int], vsize: Int): Exp[Packed[T]] = {
    if (context ne null) {
      val vs = x.asInstanceOf[Sym[Array[T]]]
      val rhs = context.reverse.collectFirst {
        case Def(Reflect(VStore(`x`, `n`, rhs: Exp[_]), _, _)) => Some(rhs)
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None
      }
      rhs.flatten.getOrElse(super.infix_vload(x,n,vsize)).asInstanceOf[Exp[Packed[T]]]
    } else {
      super.infix_vload(x,n,vsize)
    }
  }

  override def infix_vstore[T:Manifest](x: Exp[Array[T]], n: Exp[Int], y: Exp[Packed[T]]) = {
    if (context ne null) {
      val vs = x.asInstanceOf[Sym[Array[T]]]
      val rhs = context.reverse.collectFirst {
        case Def(Reflect(VStore(`x`, `n`, `y`), _, _)) => Some(Const(()))
        case Def(Reflect(_, u, _)) if mayWrite(u, List(vs)) => None
      }
      rhs.flatten.getOrElse(super.infix_vstore(x,n,y))
    } else {
      super.infix_vstore(x,n,y)
    }
  }
}

trait Intrinsics_DSL extends IntrinsicsNumericExp with IntrinsicsArrayExpOpt { self =>
  val codegen : ISAGen {
    val IR: self.type
  }
}