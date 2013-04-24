package scala.lms
package targets.clike.codegen

import java.io.PrintWriter
import scala.lms.internal.{FatBlockTraversal,GenericNestedCodegen,GenericFatCodegen}
import ops.{BaseGenLoops, BaseGenLoopsFat}

trait CLikeGenLoops extends CLikeGenBase with BaseGenLoops
trait CLikeGenLoopsFat extends CLikeGenLoops with CLikeGenFat with BaseGenLoopsFat

trait GPUGenLoops extends GPUGenBase with CLikeGenLoops
trait GPUGenLoopsFat extends GPUGenLoops with GPUGenFat with CLikeGenLoopsFat 

trait CudaGenLoops extends CudaGenBase with GPUGenLoops
trait CudaGenLoopsFat extends CudaGenLoops with CudaGenFat with GPUGenLoopsFat

trait OpenCLGenLoops extends OpenCLGenBase with GPUGenLoops
trait OpenCLGenLoopsFat extends OpenCLGenLoops with OpenCLGenFat with GPUGenLoopsFat