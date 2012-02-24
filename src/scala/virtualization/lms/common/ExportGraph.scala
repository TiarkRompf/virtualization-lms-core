package scala.virtualization.lms
package common

import internal.GraphVizExport

trait ExportGraph extends GraphVizExport {
  import IR._
  
  def exportGraph(file: String, landscape: Boolean = false)(x: Exp[Any]) =
    emitDepGraph(x, "test2-fft2-dot", landscape)
  
}