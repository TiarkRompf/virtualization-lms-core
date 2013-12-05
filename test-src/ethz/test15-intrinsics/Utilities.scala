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
    def indentExec = findExec("indent")
    if (indentExec != null) {
      var returnCode: String = ""
      val runtime = java.lang.Runtime.getRuntime()
      val indentProcess = runtime.exec(indentExec.getAbsolutePath + " -fc1 -i8 -st -br")
      val stdOut = new BufferedReader(new InputStreamReader (indentProcess.getInputStream()));
      val stdIn  = new BufferedWriter(new OutputStreamWriter(indentProcess.getOutputStream()));
      stdIn.write(code); stdIn.newLine();
      stdIn.flush(); stdIn.close();
      var line: String = null
      do {
        line = stdOut.readLine()
        if (line != null )
          returnCode += (line + System.getProperty("line.separator"))
      } while(line != null)
      val exitVal = indentProcess.waitFor();
      indentProcess.destroy()
      if (exitVal != 0) {
        code
      } else {
        returnCode
      }
    } else {
      code
    }
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
