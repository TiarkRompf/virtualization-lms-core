package scala.virtualization.lms
package internal

import java.io.PrintWriter

trait CLikeCodegen extends GenericCodegen {
  val IR: Expressions
  import IR._

  def emitConstDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit
  def emitVarDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit
  def emitValDef(sym: Sym[_], rhs: String)(implicit stream: PrintWriter): Unit
  def emitAssignment(lhs:String, rhs: String)(implicit stream: PrintWriter): Unit
}