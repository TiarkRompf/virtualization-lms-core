/**
 *  SpiralS - ETH Zurich
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

//import ch.ethz.spirals.rewrites.CUnparser
import Utilities._ //import ch.ethz.spirals.util.Utilities._

import virtualization.lms.internal._
import collection.mutable.HashMap
import scala.Some

import org.bridj._
import java.io._
//import ch.ethz.spirals.util.compile.{MinGW, ICC, Compiler, GCC}

trait CCompile extends Blocks {

  val codegen: CUnparser { val IR: CCompile.this.type }
  var compiler: Compiler = null

  val debug = true //val debug = ch.ethz.spirals.conf.Config().spiralsconfig.debug
  var funcToLib = new HashMap[String, (String,NativeLibrary,DynamicFunction[Nothing])]()

  protected def printDebug(s: String) = if (debug) println(s)

  protected def compileTransformedFile[B](codeFile: File)(implicit mList: List[Manifest[Any]], mB: Manifest[B]) =
  {
    printDebug("BridJ compilation started ...");
    if ( compiler == null ) {
      compiler = new GCC ()
      if (!compiler.exist()) compiler = getOS () match {
        case "Windows" => new MinGW ()
        case _ => new GCC ()
      }
    }
    printDebug("Using compiler: " + compiler.compilerVersion)
  
    printDebug("Compiling the object file ...")
    val objFile = compiler.compileObjectFile(codeFile, List(compiler.O3, compiler.xHost, compiler.cstd, compiler.fPIC))

    printDebug("Creating the library file ...")
    val libFile = compiler.compileLibraryFile(objFile, List())
    val libFileName = libFile.getAbsolutePath

    printDebug("Link the library to bridJ ... " + libFileName );
    val library = BridJ.getNativeLibrary(libFileName)

    printDebug("Create dynamic function ...");
    val func = createBridJDynamicFunction[B](library, mB, mList)
    funcToLib.update(libFileName, (libFileName,library,func))
    (func,libFileName)
  }

  def createBridJDynamicFunction[B](libFile: NativeLibrary, tB: Manifest[B], L: List[Manifest[Any]]) = {
    val f = libFile.getSymbolPointer("staged")
    val mB = mapBridJType(tB)
    def mT(x: Manifest[Any]) = mapBridJType(x)
    L.size match {
      case 1 => f.asDynamicFunction(null, mB, mT(L(0)))
      case 2 => f.asDynamicFunction(null, mB, mT(L(0)), mT(L(1)))
      case 3 => f.asDynamicFunction(null, mB, mT(L(0)), mT(L(1)), mT(L(2)))
      case 4 => f.asDynamicFunction(null, mB, mT(L(0)), mT(L(1)), mT(L(2)), mT(L(3)))
      case 5 => f.asDynamicFunction(null, mB, mT(L(0)), mT(L(1)), mT(L(2)), mT(L(3)), mT(L(4)))
      case 6 => f.asDynamicFunction(null, mB, mT(L(0)), mT(L(1)), mT(L(2)), mT(L(3)), mT(L(4)), mT(L(5)))
      case _ => throw new IllegalArgumentException("BridJ can currently handle only functions with 6 arguments")
    }
  }


  def unloadProgram(programFileName: String) = funcToLib.get(programFileName) match {
    case Some((libFileName,libFile,_)) => funcToLib.remove(programFileName); BridJ.releaseLibrary(libFileName)
    case None => if (debug) println("Library could not be released: " + programFileName)
  }

  override def reset = {
    if ( debug ) println("Releasing all libraries attached by BridJ")
    funcToLib.foreach(m => BridJ.releaseLibrary(m._2._1))
    funcToLib = new HashMap()
    super.reset
  }

  def mapBridJType(mList: List[Manifest[Any]]) : java.lang.reflect.Type = if ( mList.size == 1 ) {
    mapBridJType(mList(0))
  } else {
    throw new IllegalArgumentException("BridJ can currently handle only one value")
  }

  def mapBridJType[A](mA: Manifest[A]) : java.lang.reflect.Type = if ( mA.erasure.isArray ) {
    val T = mapBridJType(Manifest.classType(mA.erasure.getComponentType))
    Pointer.pointerType(T)
  } else {
    mA.toString.toLowerCase match {
      case "int"    => Integer.TYPE
      case "float"  => java.lang.Float.TYPE
      case "double" => java.lang.Double.TYPE
      case "unit"   => java.lang.Void.TYPE
    }
  }
}
