package scala.lms

import internal._

// The traits in this file provide an interface to codegen so that
// client do not need to depend on internal._

trait CLikeGenBase extends CLikeCodegen
trait CLikeGenEffect extends CLikeNestedCodegen with CLikeGenBase
trait CLikeGenFat extends CLikeFatCodegen with CLikeGenBase

trait GPUGenBase extends GPUCodegen
trait GPUGenEffect extends GPUGenBase with CLikeNestedCodegen
trait GPUGenFat extends GPUGenBase with CLikeFatCodegen

trait CudaGenBase extends CudaCodegen
trait CudaGenEffect extends CudaNestedCodegen with CudaGenBase
trait CudaGenFat extends CudaFatCodegen with CudaGenBase

trait OpenCLGenBase extends OpenCLCodegen
trait OpenCLGenEffect extends OpenCLNestedCodegen with OpenCLGenBase
trait OpenCLGenFat extends OpenCLFatCodegen with OpenCLGenBase

trait CGenBase extends CCodegen
trait CGenEffect extends CNestedCodegen with CGenBase
trait CGenFat extends CFatCodegen with CGenBase
