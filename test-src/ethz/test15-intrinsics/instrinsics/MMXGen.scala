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

trait MMXGen extends ISAGen {

  // Short (16bit)
  // Vector size: 4
  //
  // load / store interchangeable types
  //
  // __m64 _mm_add_pi16 (__m64 a, __m64 b)
  // __m64 _mm_sub_pi16 (__m64 a, __m64 b)

  // Int (32bit)
  // Vector size: 2
  //
  // load / store interchangeable types
  //
  // __m64 _mm_add_pi32 (__m64 a, __m64 b)
  // __m64 _mm_sub_pi32 (__m64 a, __m64 b)

  import IR._

  override val instructionSet = ISA.MMX

  override def includeHeader () = "#include <mmintrin.h>"

  override def getInstructionSetVectorSize[T](implicit m: Manifest[T]): Int = m.toString () match {
    case "Int"  => 2
    case _ => super.getInstructionSetVectorSize(m)
  }

  override def vremap[A](m: Manifest[A]) : String = m.toString match {
    case "Int"  => "__m64"
    case _ => super.vremap(m)
  }

  override def emit_vstore [T] (sym: Sym[_], a: Rep[Array[T]], pos: Rep[Int], v: Packed[T]) : Unit = v.m.toString match {
    case "Int" => {
      stream.println(vremap(v.m) + "* " + quote(sym) + " = " + quote(a) + " + " + quote(pos) + ";")
      stream.println(quote(sym) + " = " + quote(v.getIRep()) + ";")
    }
    case _ => super.emit_vstore(sym, a, pos, v)
  }

  override def emit_vload [T] (v: Packed[_], a: Rep[Array[T]], pos: Rep[Int], size: Int) : Unit = v.m.toString match {
    case "Int" => stream.println(vremap(v.m) + "* " + quote(v.getIRep()) + " = " + quote(a) + " + " + quote(pos) + ";")
    case _ => super.emit_vload(v, a, pos, size)
  }

  override def emit_vadd [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Int" => emitVectorDef(v.getIRep(), a.m, "_mm_add_pi32(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vadd(v, a, b)
  }

  override def emit_vsub [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Int" => emitVectorDef(v.getIRep(), a.m, "_mm_sub_pi32(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vsub(v, a, b)
  }

}



