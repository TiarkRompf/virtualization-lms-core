package scala.virtualization.lms
package common.embedded.scala

import scala.virtualization.lms.common._

trait ScalaOpsPkg extends
    ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
    with RangeOps with IOOps with ArrayOps with BooleanOps with PrimitiveOps with MiscOps
    with Equal with IfThenElse with Variables with While

trait ScalaOpsPkgExp extends ScalaOpsPkg
    with ImplicitOpsExp with NumericOpsExp with FractionalOpsExp with OrderingOpsExp with StringOpsExp
    with RangeOpsExp with IOOpsExp with ArrayOpsExp with BooleanOpsExp with PrimitiveOpsExp with MiscOpsExp
    with FunctionsExp with EqualExp with IfThenElseExp with VariablesExp with WhileExp
    with DSLOpsExp


trait ScalaCodeGenPkg extends ScalaGenImplicitOps with ScalaGenNumericOps with ScalaGenFractionalOps with ScalaGenOrderingOps
    with ScalaGenStringOps with ScalaGenRangeOps with ScalaGenIOOps with ScalaGenArrayOps with ScalaGenBooleanOps
    with ScalaGenPrimitiveOps with ScalaGenMiscOps with ScalaGenFunctions with ScalaGenEqual with ScalaGenIfThenElse
    with ScalaGenVariables with ScalaGenWhile with ScalaGenDSLOps { val IR: ScalaOpsPkgExp  }