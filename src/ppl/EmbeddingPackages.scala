package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._

/**
 * Convenience traits that can be used to import embedded operations as a group.
 */

trait EmbeddingPkg extends Base with Equal with Functions with IfThenElse with Variables
trait EmbeddingPkgExp extends EmbeddingPkg with BaseExp with EqualExp with FunctionsExp with IfThenElseExp with VariablesExp
trait ScalaGenEmbeddingPkg extends EmbeddingPkgExp with ScalaGenEqual with ScalaGenFunctions with ScalaGenIfThenElse with ScalaGenVariables

trait ScalaOpsPkg extends Base
        with ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
        with RangeOps with IOOps with ArrayOps with BooleanOps with TupleOpsExp with ScalaOps

trait ScalaOpsPkgExp extends ScalaOpsPkg with BaseExp with FunctionsExp
        with ImplicitOpsExp with NumericOpsExp with FractionalOpsExp with OrderingOpsExp with StringOpsExp
        with RangeOpsExp with IOOpsExp with ArrayOpsExp with BooleanOpsExp with TupleOpsExp with ScalaOpsExp

trait ScalaGenScalaOpsPkg extends ScalaOpsPkgExp with ScalaGenBase with ScalaGenDSL
        with ScalaGenImplicit with ScalaGenNumeric with ScalaGenFractional with ScalaGenOrdering with ScalaGenString
        with ScalaGenRange with ScalaGenIO with ScalaGenArray with ScalaGenBoolean with ScalaGenTuple with ScalaGenScalaOps
