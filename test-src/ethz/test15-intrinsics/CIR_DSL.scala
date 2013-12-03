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

import etgz.test15.cir.ForLoopFatExp
import ethz.test15.cir.{CIRCodegen, CommentExp}
import scala.virtualization.lms.common._
import ethz.test15.instrinsics._
import ethz.test15.instrinsics.ISA._
import java.io._

trait PrimitiveOpsExpOpt extends PrimitiveOpsExp
trait NumericOpsExpOpt extends NumericOpsExp

class CIR_DSL (isa: InstructionSets) extends Intrinsics_DSL with PrimitiveOpsExpOpt with NumericOpsExpOpt
  with LiftNumeric with ArrayOpsExp with ForLoopFatExp with CommentExp { self =>

  val codegen = isa match {
    case None   => new CIRCodegen               { val IR: self.type = self }
    case MMX    => new CIRCodegen with MMXGen   { val IR: self.type = self }
    case SSE    => new CIRCodegen with SSEGen   { val IR: self.type = self }
    case SSE2   => new CIRCodegen with SSE2Gen  { val IR: self.type = self }
    case SSE3   => new CIRCodegen with SSE3Gen  { val IR: self.type = self }
    case SSSE3  => new CIRCodegen with SSSE3Gen { val IR: self.type = self }
    case SSE41  => new CIRCodegen with SSE41Gen { val IR: self.type = self }
    case SSE42  => new CIRCodegen with SSE42Gen { val IR: self.type = self }
    case AVX    => new CIRCodegen with AVXGen   { val IR: self.type = self }
    case AVX2   => new CIRCodegen with AVX2Gen  { val IR: self.type = self }
  }

  def ISA2String(isa: InstructionSets): String = isa match {
    case None   => "None"
    case MMX    => "MMX"
    case SSE    => "SSE"
    case SSE2   => "SSE2"
    case SSE3   => "SSE3"
    case SSSE3  => "SSSE3"
    case SSE41  => "SSE41"
    case SSE42  => "SSE42"
    case AVX    => "AVX"
    case AVX2   => "AVX2"
  }

  def emitSource[B](f: List[Exp[Any]] => Exp[B], functionName: String, out: PrintWriter)
      (implicit mList: List[Manifest[Any]], mB:Manifest[B]): List[(Sym[Any], Any)] = {
    codegen.emitTransformedSource(f, functionName, out)
  }


}
