package scala.virtualization.lms
package epfl
package test13

import common._

// Does nothing but checks the code compiles
trait TestNumeric {

  def typed[A](a: => A) {}

  trait Usage { this: Base with NumericOps =>

    val a = unit(1) + unit(1)
    typed[Rep[Int]](a)

    val b = unit(1.0) + unit(1.0)
    typed[Rep[Double]](b)
  }

  trait UsageWithLift { this: Base with NumericOps with LiftNumeric =>

    // val a = 1 + unit(1)
    val a = anyToNumericOps(1) + unit(1)
    typed[Rep[Int]](a)

    // val b = 1.0 + unit(1.0)
    val b = anyToNumericOps(1.0) + unit(1.0)
    typed[Rep[Double]](b)

    val c = unit(1) + 1
    typed[Rep[Int]](c)

    val d = unit(1.0) + 1.0
    typed[Rep[Double]](d)
  }

  trait UsageWithPromotions { this: Base with NumericOps with NumericPromotions =>

    val a = unit(1) + unit(1.0)
    typed[Rep[Double]](a)

    val b = unit(1.0) + unit(1)
    typed[Rep[Double]](b)
  }

  trait UsageWithPromotionsAndLift { this: Base with NumericOps with NumericPromotions with LiftNumeric =>

    val a = unit(1) + 1.0
    typed[Rep[Double]](a)

    // val b = 1 + unit(1.0)
    val b = anyToNumericOps(1) + unit(1.0)
    typed[Rep[Double]](b)

    val c = unit(1.0) + 1
    typed[Rep[Double]](c)
    // val d = 1.0 + unit(1)

    val d = anyToNumericOps(1.0) + unit(1)
    typed[Rep[Double]](d)
  }

}