package scala.virtualization.lms
package ppl

import scala.virtualization.lms.common._

/**
 * Convenience traits that can be used to import embedded operations as a group.
 */

trait ScalaOpsPkg extends Base
        with ImplicitOps with NumericOps with FractionalOps with OrderingOps with StringOps
        with RangeOps with IOOps with ArrayOps with ScalaOps

trait ScalaOpsPkgExp extends ScalaOpsPkg with BaseExp with FunctionsExp
        with ImplicitOpsExp with NumericOpsExp with FractionalOpsExp with OrderingOpsExp with StringOpsExp
        with RangeOpsExp with IOOpsExp with ArrayOpsExp with ScalaOpsExp

trait ScalaGenScalaOpsPkg extends ScalaOpsPkgExp with ScalaGenBase
        with ScalaGenImplicit with ScalaGenNumeric with ScalaGenFractional with ScalaGenOrdering with ScalaGenString
        with ScalaGenRange with ScalaGenIO with ScalaGenArray with ScalaGenScalaOps
