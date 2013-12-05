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
//import org.apache.commons.io.FilenameUtils
import scala.sys.process.{ProcessLogger, Process}
import java.io._

object FilenameUtils {
  def removeExtension(s: String) = s //TODO
}

abstract class Compiler {

  val cstd = "std=c99"
  val O1 = "O1"
  val O2 = "O2"
  val O3 = "O3"
  val xHost = "xHost"
  val fPIC = "fPIC"
  val shared = "shared"

  class CCompilerException(msg: String) extends Exception("Compiler error: " + msg)

  val debug = true //ch.ethz.spirals.conf.Config().spiralsconfig.debug

  protected lazy val compiler: File = getCompilerExec ()
  lazy val compilerInfo    = if (compiler eq null) List.empty[String] else getCompilerInfo ()
  lazy val compilerTarget  = if (compiler eq null) "" else getCompilerTarget()
  lazy val compilerVersion = if (compiler eq null) "" else getCompilerVersion ()

  val compilerName: String
  protected def getCompilerExec (): File

  def compileObjectFile (source: File, flags: List[String]): File = {
    if ( !source.exists() ) {
      throw new CCompilerException("Source file does not exist")
    }
    val objectFilePath = FilenameUtils.removeExtension(source.getAbsolutePath) + ".o"
    val opts = toOpts(flags ::: List("c", "o"))
    if (compile(opts ::: List(objectFilePath, source.getAbsolutePath))) {
      new File(objectFilePath)
    } else {
      throw new CCompilerException("Compilation had errors")
    }
  }

  def compileLibraryFile (objFile: File, flags: List[String]): File = {
    if ( !objFile.exists() ) {
      throw new CCompilerException("Object file does not exist")
    }
    val ext = getOS () match {
      case "Windows" => "dll"
      case "Mac" => "dylib"
      case "Unix" => "so"
      case _ => throw new CCompilerException("Can not determine dynamic lib extension")
    }
    val libFileName = FilenameUtils.removeExtension(objFile.getAbsolutePath) + "." + ext
    val opts = toOpts(flags ::: List(shared, "o"))
    if (compile(opts ::: List(libFileName, objFile.getAbsolutePath))) {
      new File(libFileName)
    } else {
      throw new CCompilerException("Compilation had errors")
    }
  }

  protected def compile(params: List[String]): Boolean = {

    val c = compiler.getAbsolutePath
    execute((c :: params).toSeq) match {
      case (0, o, e) => if (debug) (o:::e) map println; true
      case (_, o, e) => if (debug) (o:::e) map println; false
    }
  }

  protected def execute(cmd: Seq[String]): (Int, List[String], List[String]) = {
    val command = cmd.map(x=>x.trim()).filter(x=>x!="")
    if (debug) println(command.mkString(" "))
    val qb = Process(command)
    var out = List[String]()
    var err = List[String]()
    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)
    (exit, out.reverse, err.reverse)
  }

  protected def execute (command: String): (Int, List[String], List[String]) = execute(command.split(" "))

  protected def toOpts(p: List[String]): List[String] = p.filter(x=>x!="").map(x => "-" + x)

  def getCompilerVersion (): String = compilerInfo.map (line => {
    val l = line.toLowerCase
    if (l.contains("version") && l.contains(compilerName.toLowerCase)) line else ""
  }).mkString("").trim()

  def getCompilerTarget (): String = compilerInfo.map (line => {
    val l = line.toLowerCase
    if (l.contains("target:")) line else ""
  }).mkString("").trim()


  def getCompilerInfo (): List[String] = execute(compiler.getAbsolutePath + " -v") match {
    case (0, o, e) => o ::: e
    case _ => List.empty[String]
  }

  def exist () : Boolean = !(compiler eq null ) && compilerVersion != ""
}
