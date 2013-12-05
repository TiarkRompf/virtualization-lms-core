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

import Utilities._ //import ch.ethz.spirals.util.Utilities._

class GCC extends Compiler {

  private val I386 = "i386"
  private val I486 = "i486"
  private val I586 = "i586"
  private val PENTIUM = "pentium"
  private val PENTIUM_MMX = "pentium_mmx"
  private val PENTIUMPRO = "pentiumpro"
  private val I686 = "i686"
  private val PENTIUM2 = "pentium2"
  private val PENTIUM3 = "pentium3"
  private val PENTIUM3M = "pentium3m"
  private val PENTIUM_M = "pentium_m"
  private val PENTIUM4 = "pentium4"
  private val PENTIUM4M = "pentium4m"
  private val PRESCOTT = "prescott"
  private val NOCONA = "nocona"
  private val CORE2 = "core2"
  private val COREI7 = "corei7"
  private val COREI7_AVX = "corei7_avx"
  private val CORE_AVX_I = "core_avx_i"
  private val CORE_AVX2 = "core_avx2"
  private val ATOM = "atom"
  private val SLM = "slm"
  private val K6 = "k6"
  private val K6_2 = "k6_2"
  private val K6_3 = "k6_3"
  private val ATHLON = "athlon"
  private val ATHLON_TBIRD = "athlon_tbird"
  private val ATHLON_4 = "athlon_4"
  private val ATHLON_XP = "athlon_xp"
  private val ATHLON_MP = "athlon_mp"
  private val K8 = "k8"
  private val OPTERON = "opteron"
  private val ATHLON64 = "athlon64"
  private val ATHLON_FX = "athlon_fx"
  private val K8_SSE3 = "k8_sse3"
  private val OPTERON_SSE3 = "opteron_sse3"
  private val ATHLON64_SSE3 = "athlon64_sse3"
  private val AMDFAM10 = "amdfam10"
  private val BARCELONA = "barcelona"
  private val BDVER1 = "bdver1"
  private val BDVER2 = "bdver2"
  private val BDVER3 = "bdver3"
  private val BTVER1 = "btver1"
  private val BTVER2 = "btver2"
  private val WINCHIP_C6 = "winchip_c6"
  private val WINCHIP2 = "winchip2"
  private val C3 = "c3"
  private val C3_2 = "c3_2"
  private val GEODE = "geode"

  val compilerName = "gcc"

  override val xHost = ""

  def getCompilerExec () = getOS () match {
    case "Windows" => findExec("gcc.exe")
    case _ => findExec("gcc")
  }
}
