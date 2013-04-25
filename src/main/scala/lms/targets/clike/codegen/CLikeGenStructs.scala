package scala.lms
package targets.clike.codegen

import internal.{FatExpressions,GenericNestedCodegen,GenericFatCodegen}
import ops.{StructExp, StructFatExpOptCommon, BaseGenStruct, BaseGenFatStruct}

import java.io.{PrintWriter,StringWriter,FileOutputStream}


trait CGenStruct extends CGenBase with BaseGenStruct
trait CudaGenStruct extends CudaGenBase with BaseGenStruct
trait OpenCLGenStruct extends OpenCLGenBase with BaseGenStruct

trait CudaGenFatStruct extends CudaGenStruct with BaseGenFatStruct
trait OpenCLGenFatStruct extends OpenCLGenStruct with BaseGenFatStruct
trait CGenFatStruct extends CGenStruct with BaseGenFatStruct
