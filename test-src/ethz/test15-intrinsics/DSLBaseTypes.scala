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

import scala.virtualization.lms.common.BaseExp
import collection.immutable.HashMap

trait DSLBaseTypes extends BaseExp {

  class DSLTypeDoesNotExist(msg: String) extends Exception (msg)
  class DSLTypeIsNotAVector(msg: String) extends Exception (msg)

  var reverseDSLTypeInstancesMap = HashMap.empty[Rep[DSLType], DSLType]
  def getDSLTypeInstances (rep: Rep[DSLType]): DSLType = {
    if ( reverseDSLTypeInstancesMap.contains(rep) ) {
      reverseDSLTypeInstancesMap(rep)
    } else {
      throw new DSLTypeDoesNotExist(rep.toString)
    }
  }

  abstract class DSLType(rep: Rep[DSLType]) {
    if ( rep != null ) reverseDSLTypeInstancesMap += (rep -> this)
    def this() = this(fresh[DSLType])
    def getDSLRep[T <: DSLType]() : Rep[T] = rep.asInstanceOf[Rep[T]]
  }

  class Vector(r: Rep[Vector], val size: Int) extends DSLType(r) with Serializable {
    def this(size: Int) = this(fresh[Vector], size)
    def getRep = getDSLRep[Vector]
    override def toString(): String = {
      "Vector[" + size.toString + "]: " + getRep
    }
  }

  def getDSLVector(rep: Rep[DSLType]): Vector = {
    val vec = getDSLTypeInstances(rep)
    if (vec.isInstanceOf[Vector]) {
      vec.asInstanceOf[Vector]
    } else {
      throw new DSLTypeIsNotAVector(rep.toString)
    }
  }

  case class Matrix(r: Rep[Matrix], m: Int, n: Int) extends DSLType(r) with Serializable {
    def this(m: Int, n:Int) = this(fresh[Matrix], m, n)
    def getRep = getDSLRep[Matrix]
    override def toString(): String = {
      "Matrix[" + m.toString + ", " + n.toString + "]: " + getRep
    }
  }

  def getDSLMatrix(rep: Rep[DSLType]): Matrix = {
    val vec = getDSLTypeInstances(rep)
    if (vec.isInstanceOf[Matrix]) {
      vec.asInstanceOf[Matrix]
    } else {
      throw new DSLTypeDoesNotExist(rep.toString)
    }
  }

}