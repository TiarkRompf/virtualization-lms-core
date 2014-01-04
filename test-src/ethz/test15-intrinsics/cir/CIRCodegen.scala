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

import etgz.test15.cir.CGenForOps
import ethz.test15.{CIR_DSL, CUnparser}

import scala.virtualization.lms.common.{CLikeGenNumericOps, CGenVariables, CGenPrimitiveOps}
import scala.virtualization.lms.util.GraphUtil
import scala.collection.mutable

import java.io.PrintWriter
import ethz.test15.instrinsics.ISAGen

trait CIRCodegen extends CUnparser with CGenArrayOpsExt with CGenPrimitiveOps with CLikeGenNumericOps
  with GenComment with CGenForOps with CGenVariables with ISAGen  {

  val IR: CIR_DSL
  import IR._


  def getRank(s: Stm) = s match {
    case TP(_, VLoad(_,_,_)) => 120
    case TP(_, VSet1(_,_)) => 130
    case TP(_, Reflect(VStore(_,_,_),_,_)) => 150
    case _ => 100
  }

  override def getStronglySortedSchedule2(scope: List[Stm], level: List[Stm], result: Any): (List[Stm], List[Sym[Any]]) = {
    (level, Nil) // override -- we don't have recursive dependencies ...
  }


  override def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true): List[Stm] = {
    val scopeCache = new mutable.HashMap[Sym[Any],Stm]
    for (stm <- scope; s <- stm.lhs)
      scopeCache(s) = stm

    def deps(st: List[Sym[Any]]): List[Stm] = {//st flatMap (scopeCache.get(_).toList)
      // scope.filter(d => (st intersect d.lhs).nonEmpty)
      // scope.filter(d => containsAny(st, d.lhs))
      st flatMap (scopeCache.get(_).toList)
    }

    val xx = GraphUtil.stronglyConnectedComponents[Stm](deps(syms(result)), t => deps(syms(t.rhs)))

    if (sort) xx.foreach { x =>
      if (x.length > 1) {
        printerr("warning: recursive schedule for result " + result + ": " + x)
        (new Exception) printStackTrace
      }
    }

    var remstms = xx.flatten.reverse.sortBy(getRank)
    val remsyms = new mutable.HashSet[Sym[Any]]
    val temp = new mutable.ListBuffer[Stm]

    for (r <- remstms) remsyms ++= r.lhs

    while (remstms.nonEmpty) {
      val idx = remstms.indexWhere(st => !syms(st.rhs).exists(remsyms contains _))
      val (h,t) = remstms.splitAt(idx)
      temp += t.head
      remsyms --= syms(t.head.lhs)
      remstms = h ++ t.tail
    }

    val yy = temp.result
    yy
  }

}
