package scala.lms
package targets.clike.codegen

import ops.TupleGenBase

trait CGenTupleOps extends CGenBase with TupleGenBase with CGenStruct
trait CudaGenTupleOps extends CudaGenBase with TupleGenBase with CudaGenStruct
trait OpenCLGenTupleOps extends OpenCLGenBase with TupleGenBase with OpenCLGenStruct
