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

trait SSSE3Gen extends SSE3Gen {

  override val instructionSet = ISA.SSSE3

  override def includeHeader () = "#include <tmmintrin.h>"

  import IR._

  override def emit_hadd [T] (v: Packed[_], a: Packed[T], b: Packed[T]) : Unit = a.m.toString match {
    case "Int"  => emitVectorDef(v.getIRep(), a.m, "_mm_hadd_epi32(" + quote(a.getIRep()) + ", " + quote(b.getIRep()) + ")")
    case _ => super.emit_hadd(v, a, b)
  }

}
