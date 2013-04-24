package scala.lms
package targets.clike.codegen

import internal.{FatExpressions,GenericNestedCodegen,GenericFatCodegen}
import ops.{StructExp, StructFatExpOptCommon, BaseGenFatStruct}

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait CudaGenStruct extends CudaGenBase {
  val IR: StructExp
  import IR._
}

trait OpenCLGenStruct extends OpenCLGenBase {
  val IR: StructExp
  import IR._
}


trait CudaGenFatStruct extends CudaGenStruct with BaseGenFatStruct
trait OpenCLGenFatStruct extends OpenCLGenStruct with BaseGenFatStruct

