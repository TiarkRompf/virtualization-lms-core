package scala.virtualization.lms
package internal

import scala.reflect.SourceContext

trait MeetFunc

trait Meetable[T] {
  // Tests whether a and b are identical
  def _matches(a: T, b: T): Boolean
  // Output list of why a and b cannot meet (for error reporting)
  def _incompatibilities(a: T, b: T)(implicit t: MeetFunc): List[String]
  // Tests whether a and b can be met successfully (equiv. to _incompatibilities.isEmpty)
  def _canMeet(a: T, b: T)(implicit t: MeetFunc): Boolean
  // Meet a and b
  def _meet(a: T, b: T)(implicit t: MeetFunc): T
  // Test if a is completely filled in (known)
  def _isComplete(a: T): Boolean
  // Debugging / pretty printing
  def _makeString(a: T, prefix: String): String
  def _multiLine(a: T): Boolean
}

trait MeetableOps {
  class IllegalMeetException extends Exception("Attempted to meet incompatible metadata instances")

  // TODO: This might be going a bit overboard.. How to narrow these down?
  trait MetaAlias extends MeetFunc                // General aliasing of metadata
  case object ReduceAlias extends MetaAlias       // Aliasing due to reduction of elements
  case object BranchAlias extends MetaAlias       // Aliasing due to conditional branches
  case object UpdateAlias extends MetaAlias       // Data structure field/element updates
  case object UnionAlias extends MetaAlias        // Set Union
  case object IntersectAlias extends MetaAlias    // Set Intersection
  case object MetaTypeInit extends MeetFunc       // Metadata meeting with initial type metadata
  case object MetaOverwrite extends MeetFunc      // Metadata overwrite
  case object AddAlias extends MetaAlias          // VarPlusEquals
  case object SubAlias extends MetaAlias          // VarMinusEquals
  case object MulAlias extends MetaAlias          // VarTimesEquals
  case object DivAlias extends MetaAlias          // VarDivEquals

  // Defs use underscore prefix since some implementations require calling other forms of the
  // overloaded method, which is more annoying (need to directly call implicitly[Meetable[...]].func)
  // if both the type class definition and the templated function have the same name
  // This effectively does the same thing as using implicitly[...] but with less code
  // Internal API for metadata
  def matches[T: Meetable](a: T, b: T): Boolean = implicitly[Meetable[T]]._matches(a,b)
  def incompatibilities[T:Meetable](a: T, b: T)(implicit t: MeetFunc): List[String] = implicitly[Meetable[T]]._incompatibilities(a,b)(t)
  def canMeet[T: Meetable](a: T, b: T)(implicit t: MeetFunc): Boolean = { implicitly[Meetable[T]]._canMeet(a,b)(t) }
  def meet[T:Meetable](ts: T*)(implicit t: MeetFunc): T = ts.reduce{(a,b) => tryMeet(a,b) }
  def meet[T:Meetable](t: MeetFunc, ts: T*): T = { implicit val func = t; meet(ts:_*) }
  def isComplete[T: Meetable](a: T): Boolean = implicitly[Meetable[T]]._isComplete(a)
  def makeString[T: Meetable](a: T, prefix: String = "") = implicitly[Meetable[T]]._makeString(a,prefix)
  def multiLine[T: Meetable](a: T) = implicitly[Meetable[T]]._multiLine(a)

  // Meet with error reporting for incompatible metadata
  private def tryMeet[T: Meetable](a: T, b: T)(implicit func: MeetFunc, ctx: SourceContext): T = {
    if (canMeet(a,b)) { implicitly[Meetable[T]]._meet(a,b) }
    else {
      //val inc = incompatibilities(a,b,func)
      sys.error("Attempted to meet incompatible metadata for symbol used here:\n" +
                "\tLHS metadata: " + makeString(a) + "\n" +
                "\tRHS metadata: " + makeString(b) + "\n"
               )
    }
  }




}