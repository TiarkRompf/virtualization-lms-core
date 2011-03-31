package scala.virtualization.lms
package epfl
package test7

import original.MDArray

import common._
import test1.Arith

class ScopeTestStaged() { this: MDArrayBaseExp with IfThenElseExp =>

  def testStaged(a: Rep[MDArray[Int]], b: Rep[MDArray[Int]]): Rep[MDArray[Int]] = {

    // Without scopes, this should fail shape inference
    val constraints =
      if (sel(0::Nil, a) === 1) {
        val add: Rep[MDArray[Int]] = reshape(1::3::Nil, 1::2::3::Nil)
        sel(0::Nil, (b + add))
      } else {
        val add: Rep[MDArray[Int]] = reshape(1::1::Nil, 1::Nil)
        sel(0::Nil, (b + add))
      }

    // With reconciliation, this should be specialized
    sel(0::Nil, With(function = iv => 1).ModArray(b)) + constraints
  }
}
