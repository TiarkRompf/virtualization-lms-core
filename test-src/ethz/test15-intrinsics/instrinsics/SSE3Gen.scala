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

trait SSE3Gen extends SSE2Gen {

  override val instructionSet = ISA.SSE3

  override def includeHeader () = "#include <pmmintrin.h>"

  // Float (32bit)
  // Vector size: 4
  // Inherits from SSE2Gen
  // __m128 _mm_addsub_ps(__m128 a, __m128 b)
  // __m128 _mm_hadd_ps  (__m128 a, __m128 b)
  // __m128 _mm_hsub_ps  (__m128 a, __m128 b)

  // Double (64bit)
  // Vector size: 2
  // Inherits from SSE2Gen
  // __m128d _mm_addsub_pd(__m128d a, __m128d b)
  // __m128d _mm_hadd_pd  (__m128d a, __m128d b)
  // __m128d _mm_hsub_pd  (__m128d a, __m128d b)

}
