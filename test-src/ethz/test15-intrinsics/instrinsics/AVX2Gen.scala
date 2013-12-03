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

trait AVX2Gen extends AVXGen {

  override val instructionSet =  ISA.AVX2

  // Short (16bit)
  // Vector size: 16
  // __m256i _mm256_add_epi16 (__m256i a, __m256i b)
  // __m256i _mm256_sub_epi16 (__m256i a, __m256i b)
  // __m256i _mm256_mullo_epi16 (__m256i a, __m256i b)
  // __m256i _mm256_hadd_epi16 (__m256i a, __m256i b)
  // __m256i _mm256_hsub_epi16 (__m256i a, __m256i b)

  // Integer (32bit)
  // Vector size: 8
  // __m256i _mm256_add_epi32 (__m256i a, __m256i b)
  // __m256i _mm256_sub_epi32 (__m256i a, __m256i b)
  // __m256i _mm256_mul_epi32 (__m256i a, __m256i b)
  // __m256i _mm256_hadd_epi32 (__m256i a, __m256i b)
  // __m256i _mm256_hsub_epi32 (__m256i a, __m256i b)

  // Long (64bit)
  // Vector size: 4
  // __m256i _mm256_add_epi64 (__m256i a, __m256i b)
  // __m256i _mm256_sub_epi64 (__m256i a, __m256i b)

}
