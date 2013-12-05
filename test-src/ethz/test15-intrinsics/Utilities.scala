/**
 *  SpiralS - ETH Zurich
 *  Copyright (C) 2013  Alen Stojanov  (astojanov@inf.ethz.ch)
 *
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

import java.io._
import com.github.abrarsyed.jastyle.ASFormatter
import com.github.abrarsyed.jastyle.constants.SourceMode

object Utilities {

  def findExec(executableName: String): File = {
    val systemPath = System.getenv("PATH");
    val pathDirs = systemPath.split(File.pathSeparator).reverse
    var fullyQualifiedExecutable:File = null;
    pathDirs.foreach ( pathDir => {
      val file = new File(pathDir, executableName);
      if (file.isFile()) {
        fullyQualifiedExecutable = file;
      }
    })
    fullyQualifiedExecutable
  }

  def indent (code: String): String = {
    val in  = new StringReader(code)
    val out = new StringWriter()
    val formatter = new ASFormatter ()
    formatter.setSourceStyle(SourceMode.C)
    formatter.setPreprocessorIndent(true)
    formatter.format(in, out)
    out.flush()
    out.toString
  }

  def isWin  (): Boolean = System.getProperty("os.name").toLowerCase.contains("win")
  def isMac  (): Boolean = {
    val os = System.getProperty("os.name")
    os.toLowerCase.contains("mac") || os.toLowerCase.contains("darwin")
  }
  def isUnix (): Boolean = {
    val os = System.getProperty("os.name")
    os.toLowerCase.contains("nix") || os.toLowerCase.contains("nux") || os.toLowerCase.contains("aix")
  }

  def getOS (): String = {
    if (isWin())
      "Windows"
    else if (isMac())
      "Mac"
    else if (isUnix())
      "Unix"
    else
      "Unknown"
  }

}
