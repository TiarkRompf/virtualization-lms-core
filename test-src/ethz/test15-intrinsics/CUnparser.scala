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

import scala.virtualization.lms.internal.{Expressions, CCodegen}
import scala.virtualization.lms.common.EffectExp
import java.io._

trait CUnparser extends CCodegen  {

  val IR: Expressions with EffectExp
  import IR._

  var includeHeaders = List.empty[String]
  var validReturns = List.empty[Sym[Any]]

  def getArrayInnerTypeManifest[A](m: Manifest[A]) = {
    Manifest.classType(m.erasure.getComponentType)
  }

  override def remap[A](m: Manifest[A]) : String = {
    if (m.erasure == classOf[Variable[Any]] ) {
      remap(m.typeArguments.head)
    }
    else {
      if (m.erasure.isArray()) {
        val mp = getArrayInnerTypeManifest(m)
        remap(mp) + "*"
      } else {
        m.toString match {
          case "double" => "double "
          case "float" => "float "
          case "int" => "int "
          case _ => super.remap(m)
        }
      }
    }
  }

  def checkReturnValue(block: Block[Any]) = {
    validReturns = List.empty[Sym[Any]]
  }

  def writeIncludeHeaders (out: PrintWriter) = {
    for ( header <- includeHeaders ) {
      out.println("#include <" + header + ">")
    }
  }

  def emitGlobalNodes (block: Block[Any]) = {}


  def emitTransformedBlock[B](b: (List[Sym[Any]], Block[B]), fName: String, out: PrintWriter, beautify: Boolean = true): List[(Sym[Any], Any)] = {
    //  (implicit mList: List[Manifest[Any]], mB: Manifest[B]) = {
    val stringOutput = new StringWriter()
    val stringWriter = new PrintWriter(stringOutput)
    //includeHeaders = List.empty[String]
    includeHeaders = List("math.h")
    validReturns = List.empty[Sym[Any]]
    val (syms, block) = (b._1, b._2)
    val staticData = getFreeDataBlock(block)
    checkReturnValue(block)
    writeIncludeHeaders(stringWriter)
    withStream(stringWriter) {
      emitGlobalNodes (block)
      stream.println(remap(getBlockResult(block).tp) + " " + fName + "(" + syms.map(m => remap(m.tp) + " " + quote(m)).mkString(", ") + ") {")
      emitBlock(block)
      stream.println("}")
    }
    stringWriter.flush()
    out.write(stringOutput.toString);
    out.flush()
    staticData
  }

  def emitTransformedSource[B](f: List[Exp[Any]] => Exp[B], fName: String, out: PrintWriter, beautify: Boolean = true)
                              (implicit mList: List[Manifest[Any]], mB: Manifest[B]): List[(Sym[Any], Any)] = {
    var p = List.empty[Exp[Any]]
    mList.foreach( m => { p = p :+ fresh(m) } )
    var q = reifyBlock[B](f(p))
    val (x, y) = (p.asInstanceOf[List[IR.Sym[Any]]], q)
    emitTransformedBlock((x, y), fName, out, beautify)
  }

  override def quote(x: Exp[Any]) : String = x match {
    case s@Sym(n) if (s.tp.toString() == "Int") => "i"+n
    case _ => super.quote(x)
  }
}
