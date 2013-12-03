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

trait SSE2Gen extends SSEGen {

  // SSE2 replaces MMX

  // Short (16bit)
  // Vector size: 8
  //
  // __m128i _mm_loadu_si128 (__m128i const* mem_addr)
  // void _mm_store_si128 (__m128i* mem_addr, __m128i a)
  //
  // __m128i _mm_add_epi16 (__m128i a, __m128i b)
  // __m128i _mm_sub_epi16 (__m128i a, __m128i b)

  // Int (32bit)
  // Vector size: 4
  //
  // __m128i _mm_loadu_si128 (__m128i const* mem_addr)
  // void _mm_store_si128 (__m128i* mem_addr, __m128i a)
  //
  // __m128i _mm_add_epi32 (__m128i a, __m128i b)
  // __m128i _mm_sub_epi32 (__m128i a, __m128i b)

  // Long (64bit)
  // Vector size: 2
  //
  // __m128i _mm_loadu_si128 (__m128i const* mem_addr)
  // void _mm_store_si128 (__m128i* mem_addr, __m128i a)
  //
  // __m128i _mm_add_epi64 (__m128i a, __m128i b)
  // __m128i _mm_sub_epi64 (__m128i a, __m128i b)

  // Float (32bit)
  // Vector size: 4
  // Inherited from SSEGen

  // Double (64bit)
  // Vector size: 2
  //
  // __m128d _mm_loadu_pd (double const* mem_addr)
  // void _mm_storeu_pd (double* mem_addr, __m128d a)
  //
  // __m128d _mm_add_pd (__m128d a, __m128d b)
  // __m128d _mm_sub_pd (__m128d a, __m128d b)
  // __m128d _mm_mul_pd (__m128d a, __m128d b)
  // __m128d _mm_div_pd (__m128d a, __m128d b)

  import IR._

  override val instructionSet = ISA.SSE2

  override def includeHeader () = "#include <emmintrin.h>"

  override def getInstructionSetVectorSize[T](implicit m: Manifest[T]): Int = m.toString () match {
    case "Int"    => 4
    case "Long"   => 2
    case "Double" => 2
    case _ => super.getInstructionSetVectorSize(m)
  }

  override def vremap[A](m: Manifest[A]) : String = m.toString match {
    case "Int"    => "__m128i"
    case "Long"   => "__m128i"
    case "Double" => "__m128d"
    case _ => super.vremap(m)
  }

  override def emit_vstore [T] (sym: Sym[_], a: Rep[Array[T]], pos: Rep[Int], v: Packed[T]) : Unit = v.m.toString match {
    case "Int"    => stream.println("_mm_store_si128((__m128i*)(" + quote(a) + " + " + quote(pos) + "), " + quote(v.getIRep()) + ");")
    case "Long"   => stream.println("_mm_store_si128((__m128i*)(" + quote(a) + " + " + quote(pos) + "), " + quote(v.getIRep()) + ");")
    case "Double" => stream.println("_mm_storeu_pd("   + quote(a) + " + " + quote(pos) + ", " + quote(v.getIRep()) + ");")
    case _ => super.emit_vstore(sym, a, pos, v)
  }

  override def emit_vload [T] (v: Packed[_], a: Rep[Array[T]], pos: Rep[Int], size: Int) : Unit = v.m.toString match {
    case "Int"    => emitVectorDef(v.getIRep(), v.m, "_mm_loadu_si128((__m128i*)(" + quote(a) + " + " + quote(pos) + "))")
    case "Long"   => emitVectorDef(v.getIRep(), v.m, "_mm_loadu_si128((__m128i*)(" + quote(a) + " + " + quote(pos) + "))")
    case "Double" => emitVectorDef(v.getIRep(), v.m, "_mm_loadu_pd("    + quote(a) + " + " + quote(pos) + ")")
    case _ => super.emit_vload(v, a, pos, size)
  }

  override def emit_vadd [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Int"    => emitVectorDef(v.getIRep(), a.m, "_mm_add_epi32(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Long"   => emitVectorDef(v.getIRep(), a.m, "_mm_add_epi64(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm_add_pd("    + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vadd(v, a, b)
  }

  override def emit_vsub [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Int"    => emitVectorDef(v.getIRep(), a.m, "_mm_sub_epi32(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Long"   => emitVectorDef(v.getIRep(), a.m, "_mm_sub_epi64(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm_sub_pd("    + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vsub(v, a, b)
  }

  override def emit_vmul [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Int" => {
      val (x, y) = (quote(a.getIRep()), quote(b.getIRep()))
      // tmp1: mul 2,0, tmp2: mul 3,1
      val (tmp1, tmp2) = (s"_mm_mul_epu32($x, $y)", s"_mm_mul_epu32( _mm_srli_si128($x,4), _mm_srli_si128($y,4))")
      // shuffle results to [63..0] and pack
      val res = s"_mm_unpacklo_epi32(_mm_shuffle_epi32($tmp1, _MM_SHUFFLE (0,0,2,0)), _mm_shuffle_epi32($tmp2, _MM_SHUFFLE (0,0,2,0)));"
      emitVectorDef(v.getIRep(), a.m, res)
    }
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm_mul_pd(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vmul(v, a, b)
  }

  override def emit_vdiv [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm_div_pd(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vdiv(v, a, b)
  }

  override def emit_vset1[T](v: Packed[_], a: Rep[T]): Unit = a.tp.toString match {
    case "Int"    => emitVectorDef(v.getIRep(), a.tp, " _mm_set1_epi32(" + quote(a)+ ")")
    case "Long"   => emitVectorDef(v.getIRep(), a.tp, " _mm_set1_epi64(" + quote(a)+ ")")
    case "Double" => emitVectorDef(v.getIRep(), a.tp, " _mm_set1_pd(" + quote(a)+ ")")
    case _ => super.emit_vset1(v, a)
  }

}
