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
import org.bridj._

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

  def generateSimplestFIR[T:Manifest:Numeric](isa: InstructionSets, run: Boolean = false) {

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

        y.vstore(i, sum(tV))
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

    val xs = CIR.fresh(manifest[Array[T]])
    val hs = CIR.fresh(manifest[Array[T]])
    val ys = CIR.fresh(manifest[Array[T]])

    val args = List(xs, hs, ys)
    val block = CIR.reifyEffects[Unit](FIR(xs, hs, CIR.reflectMutableSym(ys)))

    CIR.emitBlock((args, block), new PrintWriter(System.out), "fir")

    println("===============================================================")
    println()
    println()

    def execute (xInput: Array[T], hInput: Array[T]) : Array[T] = {

      CIR.compiler = new GCC() // ICC ()
      CIR.compiler.debug = false // check file!
      CIR.debug = false

      timed("compile & run") {
        val (fir, firFileName) = CIR.compileBlock[Unit]((args, block))

        val xBridJ = Pointer.allocateArray[T](CIR.mapBridJType(manifest[T]), n)
        val hBridJ = Pointer.allocateArray[T](CIR.mapBridJType(manifest[T]), k)
        val yBridJ = Pointer.allocateArray[T](CIR.mapBridJType(manifest[T]), n)

        for (i <- 0 until n) xBridJ.set(i, xInput(i))
        for (i <- 0 until k) hBridJ.set(i, hInput(i))

        fir(xBridJ, hBridJ, yBridJ)

        val yOutput = new Array[T](n)
        for (i <- intWrapper(0) until n) {
          yOutput(i) = yBridJ.as(CIR.mapBridJType(manifest[T])).get(i)
        }

        Pointer.release(xBridJ)
        Pointer.release(hBridJ)
        Pointer.release(yBridJ)

        CIR.unloadProgram(firFileName)
        yOutput
      }
    }

    if (run) {
      //val x = scala.Array.tabulate[Float](n) { i => i % 10}
      val x = scala.Array.tabulate[Float](n) { i => if (i%10 == 8) 10 else 0 }
      val h = scala.Array[Float](0.4f,0.3f,0.2f,0.1f)
      //val h = scala.Array[Float](0.25f,0.25f,0.25f,0.25f)

      assert(k == h.length)

      def printGraph(xs: Array[Float]) = {
        println(xs.map(_=>"-").mkString)
        for (i <- 9 to 0 by -1) {
          def m(x: Float) = if (x > i) "#" else " "
          println(xs.map(m).mkString)
        }
        println(xs.map(_=>"-").mkString)
      }

      println("x: " + x.mkString(","))
      println("h: " + h.mkString(","))

      val y = execute(x.asInstanceOf[Array[T]], h.asInstanceOf[Array[T]])
      println("y: " + y.mkString(","))
      printGraph(x)
      println()
      printGraph(y.asInstanceOf[Array[Float]])
    }
  }

  val demo = false

  def timed[A](s:String)(x: => A) = {
    val t0 = System.currentTimeMillis
    try x finally if (demo) println(s+": "+(System.currentTimeMillis-t0)+"ms")
  }


  val prefix = "test-out/epfl/test15-"

  describe("Test1"){
    val withOut = if (demo) withOutFile _ else withOutFileChecked _
    withOut(prefix+"intrinsics-run") {
      generateSimplestFIR[Float](ISA.SSE3, true)
    }
  }

  describe("Test2"){
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

