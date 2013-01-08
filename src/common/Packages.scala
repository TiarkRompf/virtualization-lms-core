package scala.virtualization.lms
package common

import scala.virtualization.lms.common._

/**
 * IR: this is the front-end that defines what operations are available in user applications. Backends can share
 *     packages if they choose.
 */

trait LiftScala extends LiftAll with LiftVariables with LiftEquals {
  this: ScalaOpsPkg =>
}

trait ScalaOpsPkg extends Base
    with ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
    with RangeOps with IOOps with ArrayOps with BooleanOps with PrimitiveOps with MiscOps
    with Equal with IfThenElse with Variables with While with TupleOps with ListOps
    with SeqOps with MathOps with CastingOps with SetOps with ObjectOps with ArrayBufferOps

trait ScalaOpsPkgExp extends ScalaOpsPkg
    with ImplicitOpsExp with NumericOpsExp with FractionalOpsExp with OrderingOpsExp with StringOpsExp
    with RangeOpsExp with IOOpsExp with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp
    with FunctionsExp with EqualExp with IfThenElseExp with VariablesExp with WhileExp with TupleOpsExp with ListOpsExp
    with SeqOpsExp with DSLOpsExp with MathOpsExp with CastingOpsExp with SetOpsExp with ObjectOpsExp with ArrayBufferOpsExp


/**
 * Code gen: each target must define a code generator package.
 */


/////////
// Scala
trait ScalaCodeGenPkg extends ScalaGenImplicitOps with ScalaGenNumericOps with ScalaGenFractionalOps with ScalaGenOrderingOps
    with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps with ScalaGenArrayOps with ScalaGenBooleanOps
    with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenFunctions with ScalaGenEqual with ScalaGenIfThenElse
    with ScalaGenVariables with ScalaGenWhile with ScalaGenTupleOps with ScalaGenListOps
    with ScalaGenSeqOps with ScalaGenDSLOps with ScalaGenMathOps with ScalaGenCastingOps with ScalaGenSetOps
    with ScalaGenObjectOps with ScalaGenArrayBufferOps
  { val IR: ScalaOpsPkgExp  }


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