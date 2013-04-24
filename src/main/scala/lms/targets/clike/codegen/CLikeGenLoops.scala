package scala.lms
package targets.clike.codegen

import internal.{FatBlockTraversal,GenericNestedCodegen,GenericFatCodegen}
import ops.{BaseGenLoops, BaseGenLoopsFat}

import java.io.PrintWriter

trait CLikeGenLoops extends CLikeGenBase with BaseGenLoops
trait CLikeGenLoopsFat extends CLikeGenLoops with CLikeGenFat with BaseGenLoopsFat

trait CGenLoops extends CGenBase with CLikeGenLoops
trait CGenLoopsFat extends CGenLoops with CGenFat with CLikeGenLoopsFat

trait GPUGenLoops extends GPUGenBase with CLikeGenLoops
trait GPUGenLoopsFat extends GPUGenLoops with GPUGenFat with CLikeGenLoopsFat

trait CudaGenLoops extends CudaGenBase with GPUGenLoops
trait CudaGenLoopsFat extends CudaGenLoops with CudaGenFat with GPUGenLoopsFat

trait OpenCLGenLoops extends OpenCLGenBase with GPUGenLoops
trait OpenCLGenLoopsFat extends OpenCLGenLoops with OpenCLGenFat with GPUGenLoopsFat
