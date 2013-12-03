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

package ethz.test15.instrinsics

import scala.virtualization.lms.internal.GenerationFailedException
import ethz.test15.CUnparser
import ethz.test15.Intrinsics_DSL
import java.io._

trait ISAGen extends CUnparser {

  val IR: Intrinsics_DSL
  import IR._

  class InvalidVectorType(msg: String) extends Exception (msg)

  val instructionSet = ISA.None

  def isSIMD () = instructionSet != ISA.None

  override def writeIncludeHeaders(out: PrintWriter) {
    super.writeIncludeHeaders(out)
    out.println(includeHeader())
  }

  def includeHeader () : String = ""

  def getInstructionSetVectorSize[T] (implicit m: Manifest[T]): Int = m.toString match {
    case _ => 1
  }

  def emit_vstore[T](s: Sym[_],    a: Rep[Array[T]], p: Rep[Int], v: Packed[T]) = exep("emit_vstore(%s, %s, %s, %s)".format(s, a, p, v))
  def emit_vload [T](s: Packed[_], a: Rep[Array[T]], p: Rep[Int], size: Int)    = exep("emit_vstore(%s, %s, %s, %s)".format(s, a, p, size))
  def emit_vadd  [T](s: Packed[_], a: Packed[T], b: Packed[T]) = exep("emit_vadd(%s, %s)".format(a, b))
  def emit_vsub  [T](s: Packed[_], a: Packed[T], b: Packed[T]) = exep("emit_vsub(%s, %s)".format(a, b))
  def emit_vdiv  [T](s: Packed[_], a: Packed[T], b: Packed[T]) = exep("emit_vmul(%s, %s)".format(a, b))
  def emit_vmul  [T](s: Packed[_], a: Packed[T], b: Packed[T]) = exep("emit_vdiv(%s, %s)".format(a, b))

  def emit_vset1   [T](s: Packed[_], a: Rep[T]) = exep("emit_vset1d(%s)".format(a))
  def emit_hadd    [T](s: Packed[_], a: Packed[T], b: Packed[T]) = exep("emit_hadd(%s, %s)".format(a, b))
  def emit_permute2[T](s: Packed[_], a: Packed[T], b: Packed[T], m: Int) = exep("emit_hadd(%s, %s)".format(a, b))

  def vremap[A](m: Manifest[A]) : String = {
    throw new GenerationFailedException("IntrinsicsGen: vremap(m) : Type %s cannot be remapped.".format(m.toString))
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case VLoad (a, pos, size) => emit_vload(getIVector(sym.asInstanceOf[Rep[Packed[Any]]]), a, pos, size)
    case VStore(a, pos, vec) => emit_vstore(sym, a, pos, getIVector(vec))
    case VAdd(a, b)  => emit_vadd(getIVector(sym.asInstanceOf[Rep[Packed[Any]]]), getIVector(a), getIVector(b))
    case VSub(a, b)  => emit_vsub(getIVector(sym.asInstanceOf[Rep[Packed[Any]]]), getIVector(a), getIVector(b))
    case VMul(a, b)  => emit_vmul(getIVector(sym.asInstanceOf[Rep[Packed[Any]]]), getIVector(a), getIVector(b))
    case VDiv(a, b)  => emit_vdiv(getIVector(sym.asInstanceOf[Rep[Packed[Any]]]), getIVector(a), getIVector(b))
    case HAdd(a, b)  => emit_hadd(getIVector(sym.asInstanceOf[Rep[Packed[Any]]]), getIVector(a), getIVector(b))
    case VSet1(a,_)  => emit_vset1(getIVector(sym.asInstanceOf[Rep[Packed[Any]]]), a)
    case Permute2(a, b, mask) => emit_permute2(getIVector(sym.asInstanceOf[Rep[Packed[Any]]]), getIVector(a), getIVector(b), mask)
    case _ => super.emitNode(sym, rhs)
  }

  def emitVectorDef(sym: Exp[Any], tpe: Manifest[_], rhs: String): Unit = {
    if(vremap(tpe) != "void") stream.println(vremap(tpe) + " " + quote(sym) + " = " + rhs + ";")
  }

  def exep(s: String): Unit = throw new GenerationFailedException("IntrinsicsGen: " + s + " can not be generated")
}
