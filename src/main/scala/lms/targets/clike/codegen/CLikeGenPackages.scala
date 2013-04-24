package scala.lms
package targets.clike.codegen

import scala.lms.common._
import ops.ScalaOpsPkgExp


/////
// C
trait CCodeGenPkg extends CGenDSLOps with CGenImplicitOps with CGenNumericOps with CGenFractionalOps with CGenOrderingOps
    with CGenStringOps with CGenRangeOps with CGenIOOps with CGenArrayOps with CGenBooleanOps
    with CGenPrimitiveOps with CGenMiscOps with CGenFunctions with CGenEqual with CGenIfThenElse
    with CGenVariables with CGenWhile
    with CGenMathOps with CGenCastingOps with CGenSetOps with CGenArrayBufferOps
    { val IR: ScalaOpsPkgExp  }

///////
// Cuda
// CudaGenDSLOps will be used after all the basic generators are passed
trait CudaCodeGenPkg extends CudaGenDSLOps with CudaGenImplicitOps with CudaGenNumericOps with CudaGenFractionalOps with CudaGenOrderingOps
    with CudaGenStringOps with CudaGenRangeOps with CudaGenIOOps with CudaGenArrayOps with CudaGenBooleanOps
    with CudaGenPrimitiveOps with CudaGenMiscOps with CudaGenFunctions with CudaGenEqual with CudaGenIfThenElse
    with CudaGenVariables with CudaGenWhile
    with CudaGenMathOps with CudaGenCastingOps with CudaGenSetOps with CudaGenArrayBufferOps
    { val IR: ScalaOpsPkgExp  }

//trait CudaCodeGenPkg extends CudaGenNumericOps with CudaGenRangeOps with CudaGenFractionalOps
//    with CudaGenMiscOps with CudaGenFunctions with CudaGenVariables with CudaGenDSLOps with CudaGenImplicitOps { val IR: ScalaOpsPkgExp  }

trait OpenCLCodeGenPkg extends OpenCLGenDSLOps with OpenCLGenImplicitOps with OpenCLGenNumericOps with OpenCLGenFractionalOps with OpenCLGenOrderingOps
    with OpenCLGenStringOps with OpenCLGenRangeOps with OpenCLGenIOOps with OpenCLGenArrayOps with OpenCLGenBooleanOps
    with OpenCLGenPrimitiveOps with OpenCLGenMiscOps with OpenCLGenFunctions with OpenCLGenEqual with OpenCLGenIfThenElse
    with OpenCLGenVariables with OpenCLGenWhile
    with OpenCLGenMathOps with OpenCLGenCastingOps with OpenCLGenSetOps with OpenCLGenArrayBufferOps
    { val IR: ScalaOpsPkgExp  }