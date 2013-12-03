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

trait AVXGen extends SSE42Gen {

  // Float (32bit)
  // Vector size: 8
  //
  // __m256 _mm256_loadu_ps (float const * mem_addr)
  // void _mm256_storeu_ps (float * mem_addr, __m256 a)
  //
  // __m256 _mm256_add_ps (__m256 a, __m256 b)
  // __m256 _mm256_sub_ps (__m256 a, __m256 b)
  // __m256 _mm256_mul_ps (__m256 a, __m256 b)
  // __m256 _mm256_div_ps (__m256 a, __m256 b)
  // __m256 _mm256_addsub_ps (__m256 a, __m256 b)
  // __m256 _mm256_hadd_ps (__m256 a, __m256 b)

  // Double (64bit)
  // Vector size: 4
  //
  // __m256d _mm256_loadu_pd (double const * mem_addr)
  // void _mm256_storeu_pd (double * mem_addr, __m256d a)
  //
  // __m256d _mm256_add_pd (__m256d a, __m256d b)
  // __m256d _mm256_sub_pd (__m256d a, __m256d b)
  // __m256d _mm256_mul_pd (__m256d a, __m256d b)
  // __m256d _mm256_div_pd (__m256d a, __m256d b)
  // __m256d _mm256_addsub_pd (__m256d a, __m256d b)
  // __m256d _mm256_hadd_pd (__m256d a, __m256d b)

  import IR._

  override val instructionSet = ISA.AVX

  override def includeHeader () = "#include <immintrin.h>"

  override def getInstructionSetVectorSize[T](implicit m: Manifest[T]): Int = m.toString () match {
    case "Float"  => 8
    case "Double" => 4
    case _ => super.getInstructionSetVectorSize(m)
  }

  override def vremap[A](m: Manifest[A]) : String = m.toString match {
    case "Float"  => "__m256"
    case "Double" => "__m256d"
    case _ => super.vremap(m)
  }

  override def emit_vstore [T] (sym: Sym[_], a: Rep[Array[T]], pos: Rep[Int], v: Packed[T]) : Unit = v.m.toString match {
    case "Float"  => stream.println("_mm256_storeu_ps(" + quote(a) + " + " + quote(pos) + ", " + quote(v.getIRep()) + ");")
    case "Double" => stream.println("_mm256_storeu_pd(" + quote(a) + " + " + quote(pos) + ", " + quote(v.getIRep()) + ");")
    case _ => super.emit_vstore(sym, a, pos, v)
  }

  override def emit_vload [T] (v: Packed[_], a: Rep[Array[T]], pos: Rep[Int], size: Int) : Unit = v.m.toString match {
    case "Float"  => emitVectorDef(v.getIRep(), v.m, "_mm256_loadu_ps(" + quote(a) + " + " + quote(pos) + ")")
    case "Double" => emitVectorDef(v.getIRep(), v.m, "_mm256_loadu_pd(" + quote(a) + " + " + quote(pos) + ")")
    case _ => super.emit_vload(v, a, pos, size)
  }

  override def emit_vadd [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Float"  => emitVectorDef(v.getIRep(), a.m, "_mm256_add_ps(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm256_add_pd(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vadd(v, a, b)
  }

  override def emit_vsub [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Float"  => emitVectorDef(v.getIRep(), a.m, "_mm256_sub_ps(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm256_sub_pd(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vsub(v, a, b)
  }

  override def emit_vmul [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Float"  => emitVectorDef(v.getIRep(), a.m, "_mm256_mul_ps(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm256_mul_pd(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vmul(v, a, b)
  }

  override def emit_vdiv [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Float"  => emitVectorDef(v.getIRep(), a.m, "_mm256_div_ps(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm256_div_pd(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_vdiv(v, a, b)
  }

  override def emit_permute2[T](v: Packed[_], a: Packed[T], b: Packed[T], mask: Int): Unit = a.m.toString match {
    case "Float"  => emitVectorDef(v.getIRep(), a.m, "_mm256_permute2f128_ps(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + "," +  mask.toString + ")")
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm256_permute2f128_pd(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + "," +  mask.toString + ")")
    case _ => super.emit_vadd(v, a, b)
  }

  override def emit_vset1[T](v: Packed[_], a: Rep[T]): Unit = a.tp.toString match {
    case "Float"  => emitVectorDef(v.getIRep(), a.tp, " _mm256_set1_ps(" + quote(a)+ ")")
    case "Double" => emitVectorDef(v.getIRep(), a.tp, " _mm256_set1_pd(" + quote(a)+ ")")
    case _ => super.emit_vset1(v, a)
  }

  override def emit_hadd [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Float"  => emitVectorDef(v.getIRep(), a.m, "_mm256_hadd_ps(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case "Double" => emitVectorDef(v.getIRep(), a.m, "_mm256_hadd_pd(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_hadd(v, a, b)
  }

}
