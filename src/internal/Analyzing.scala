package scala.virtualization.lms
package internal

import scala.collection.{immutable,mutable}
import scala.reflect.SourceContext


// Experimental version of traversal where traversal schedule can be determined dynamically by metadata dependencies
trait AbstractAnalyzer { self =>
  val IR: Analyzing
  import IR._

  def analyze(lhs: Exp[Any], rhs: Def[Any]): Unit
  def run[A:Manifest](b: Block[A]): Block[A]

  protected var datRequire: List[Datakey[_]] = Nil
  protected var datUpdate:  List[Datakey[_]] = Nil
  protected var datCreate:  List[Datakey[_]] = Nil
  protected var datInvalid: List[Datakey[_]] = Nil

  // Metadata invalidated by running this traversal
  protected def invalidates(x: Datakey[_]*) { datInvalid = datInvalid ++ x.toList }
  // Metadata required prior to running traversal
  protected def requires(x: Datakey[_]*) { datRequire = datRequire ++ x.toList }
  // Metadata updated (made valid) by running this traversal
  protected def updates(x: Datakey[_]*) { datCreate = datCreate ++ datCreate }
  // Metadata created (made valid) by running this traversal
  protected def creates(x: Datakey[_]*) { datUpdate = datUpdate ++ datUpdate }

  datCreate foreach {dat => analyzers += (dat -> self.asInstanceOf[Analyzer]) }
}


trait Analyzing extends Expressions with Blocks with SymbolMetadata { self =>

  type Analyzer = AbstractAnalyzer { val IR: self.type }

  // State for compiler traversal use
  var metadata: Map[Exp[Any], SymbolProperties] = Map.empty

  // -----
  // TODO: These are currently unused, but may be useful later?
  var analyzers: Map[Datakey[_], Analyzer] = Map.empty
  var validData: List[Datakey[_]] = Nil
  // -----

  // Metadata propagation rules
  def propagate(s: Exp[Any], d: Def[Any]): Unit = {  }
}
