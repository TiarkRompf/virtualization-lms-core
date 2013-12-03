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

package ethz.test15.cir

import scala.virtualization.lms.common.{ArrayOpsExp, CLikeGenArrayOps, CGenBase}
import scala.virtualization.lms.internal.NestedBlockTraversal
import ethz.test15.CUnparser

trait CGenArrayOpsExt extends CGenBase with CLikeGenArrayOps with CUnparser { self =>

  val IR: ArrayOpsExp
  import IR._

  var mallocArraySyms = List.empty[Sym[Any]]

  // 'Annotate' the arrays that are supposed to be created by malloc
  // This is a quick hack to make sure that memory is not being leaked in the system
  // TODO: Alen Stojanov
  // Better way to implement this is to introduce ArrayNewMalloc in the CIR_DSL_Object
  def setMallocArrays(syms: List[Sym[Any]]) = {
    mallocArraySyms = syms
    includeHeaders = includeHeaders ::: List("stdlib.h")
  }

  def checkForReturnArray(block: Block[Any]) = {
    mallocArraySyms = List.empty[Sym[Any]]
    findDefinition(getBlockResultFull(block).asInstanceOf[Sym[Any]]) match {
      case Some(tp) => tp match {
        case TP(_, Reify(s, _, _)) if (s.isInstanceOf[Sym[_]]) => findDefinition(s.asInstanceOf[Sym[Any]]) match {
          case Some(stm) => stm match {
            case TP(sym, Reflect(ArrayNew(_), _, _)) => setMallocArrays(List(sym.asInstanceOf[Sym[Any]]))
            case TP(sym, ArrayNew(_)) => setMallocArrays(List(sym.asInstanceOf[Sym[Any]]))
            case _ =>
          }
          case None =>
        }
      }
      case None =>
    }
  }

  override def checkReturnValue(block: Block[Any]) = {
    super.checkReturnValue(block)
    //checkForReturnArray(block)
  }



  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = {
    rhs match {
      case ArrayLength(x) => emitValDef(sym, quote(x) + ".length")
      case ArrayApply(x,n) => emitValDef(sym, quote(x) + "[" + quote(n) + "]")
      case ArrayUpdate(x,n,y) => stream.println(quote(x) + "[" + quote(n) + "] = " + quote(y) + ";")
      case ArrayNew(n) => if ( mallocArraySyms.contains(sym) ) {
        stream.println(remap(sym.tp) + " " + quote(sym) + " = (" + remap(sym.tp) +") malloc (sizeof(" + getArrayInnerTypeManifest(sym.tp) + ") * " + quote(n) + ");")
      } else {
        stream.println(getArrayInnerTypeManifest(sym.tp) + " " + quote(sym) + "[" + quote(n) + "];")
      }
      case Reify(s, _, _) => if ( validReturns.contains(s) ) {
        stream.println("return " + quote(s) + ";")
      } else {
        // stream.println("//return " + quote(s) + ";")
      }
      case _ => super.emitNode(sym, rhs)
    }
  }
}