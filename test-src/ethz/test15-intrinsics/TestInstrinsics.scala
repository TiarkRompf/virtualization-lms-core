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

import org.scalatest.FunSpec
import ethz.test15.instrinsics._
import ethz.test15.instrinsics.ISA._
import java.io._

import scala.virtualization.lms.epfl.FileDiffSuite


class TestInstrinsics extends FunSpec with FileDiffSuite {

  /**
   * SimplestFIR assumes that k - the tap size (lenght of the filter) is the length of the ISA vector
   * and the input vector size is 256 * (tap size). It uses intrinsics to calculate the first n-k output
   * entries, and uses scalar code to calculate the rest k entries.
   *
   * @param isa The ISA for code generation
   * @tparam T Type of the FIR filter (Double, Float, Int)
   */
  
  def generateSimplestFIR[T:Manifest:Numeric](isa: InstructionSets) {

    val CIR = new CIR_DSL(isa)
    import CIR._

    val k = CIR.codegen.getInstructionSetVectorSize[T] // tap size
    val n = 256 * k // input size

    println("===============================================================")
    println("===  " + ISA2String(isa) + " (" + manifest[T].toString() + ")")
    println("===============================================================")

    def FIR(x: Rep[Array[T]], h: Rep[Array[T]], y: Rep[Array[T]]) {

      comment("Generating FIR filter of size: " + n + " with tap size: " + k)

      // vectorized computation
      for (i <- 0 genuntil ((n-k)/k)) {

        val xV = scala.Array.tabulate(k)(j => x.vload(i + j, k))
        val hV = scala.Array.tabulate(k)(j => h(j).vset1(k))
        val tV = (xV, hV).zipped map (_ vmul _)

        def sum(in: Array[Exp[Packed[T]]]): Exp[Packed[T]] = if (in.length == 1) in(0) else {
          val m = in.length / 2
          sum(in.slice(0,m)) vadd sum(in.slice(m,in.length))
        }

        y.vstore(i * k, sum(tV))
      }

      // remaining scalar computation
      for (i <- 0 genuntil k) {
        val offset = n - k + i
        y(offset) = implicitly[Numeric[T]].zero
        for (j <- 0 genuntil k-i) {
          val x_tmp = x(offset + j)
          val h_tmp = h(k - j - 1)
          y(offset) = y(offset) + (x_tmp * h_tmp)
        }
      }

    }

    val wrapped_f: (List[Exp[Array[T]]] => Exp[Unit]) = (in: List[Exp[Array[T]]]) => {
      FIR(in(0), in(1) , CIR.reflectMutableSym(in(2).asInstanceOf[Sym[Array[T]]]))
    };
    val mList = List(manifest[T], manifest[T], manifest[T]).asInstanceOf[List[Manifest[Any]]]
    CIR.emitSource[Unit](wrapped_f.asInstanceOf[List[CIR.Exp[Any]] => CIR.Exp[Unit]], "fir", new PrintWriter(System.out))(mList, manifest[Unit])

    println("===============================================================")
    println()
    println()
  }

  val prefix = "test-out/epfl/test15-"

  describe("Test"){
    withOutFileChecked(prefix+"intrinsics") {
      // One can generate all possibilities of types and given ISA
      generateSimplestFIR[Double](ISA.AVX)
      generateSimplestFIR[Float](ISA.AVX)
      generateSimplestFIR[Int](ISA.AVX)
      // ISA can also change
      generateSimplestFIR[Float](ISA.SSE42)
      generateSimplestFIR[Double](ISA.SSSE3)
      generateSimplestFIR[Int](ISA.SSE3)
    }
  }
}

