package scala.virtualization.lms
package internal

import java.io.PrintWriter

trait CLikeCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._
/*
  //TODO: is sym of type Any or Variable[Any] ?
  def emitConstDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit
*/
  def emitVarDef(sym: Sym[Variable[Any]], rhs: String)(implicit stream: PrintWriter): Unit
  def emitValDef(sym: Sym[Any], rhs: String)(implicit stream: PrintWriter): Unit
  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit
}