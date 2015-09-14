package scala.lms
package internal

trait AbstractAnalyzer {
  val IR: AnalyzingExp
  import IR._
}

trait AnalyzingExp extends Expressions {
  // --- Shortcuts for properties
  // These shortcuts should only be used when child is guaranteed to be defined
  def child(p: SymbolProperties): SymbolProperties = getChild(p).get
  def child(p: SymbolProperties, index: String): SymbolProperties = getField(p, index).get
  def props(e: Exp[Any]): SymbolProperties = getProps(e).get
  def child(e: Exp[Any]): SymbolProperties = getChild(e).get
  def child(e: Exp[Any], index: String): SymbolProperties = getField(e, index).get

  def props(b: Block[Any]): SymbolProperties = getProps(b.res).get
  def child(b: Block[Any]): SymbolProperties = getChild(b.res).get
  def child(b: Block[Any], index: String): SymbolProperties = getField(b.res, index).get
}
