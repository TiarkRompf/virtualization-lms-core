package scala.lms

import internal.GraphVizExport

trait ExportGraph extends GraphVizExport {
  import IR._

  def exportGraph(file: String, landscape: Boolean = false)(x: Exp[Any]) =
    emitDepGraph(x, file, landscape)

}