package scala.virtualization.lms
package epfl
package test2

import internal._

trait DisableCSE extends Expressions {
  override def findDefinition[T](d: Def[T]) = None
}


trait DisableDCE extends Scheduling {
  import IR._
  override def buildScheduleForResult(start: Exp[Any]): List[TP[Any]] =
    globalDefs
}

