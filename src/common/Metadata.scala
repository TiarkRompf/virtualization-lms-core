package scala.virtualization.lms
package common

import internal.{SymbolMetadata, Expressions, Blocks, Effects}
import internal.{Traversal, IterativeTraversal}

import scala.reflect._

trait MetadataOps extends Base with SymbolMetadata {

  // --- API
  // Directly add symbol property metadata mapping for symbol
  def setProps(e: Rep[Any], p: SymbolProperties)(implicit ctx: SourceContext): Unit
  def initRep(e: Rep[Any], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties

  def setMetadata(e: Rep[Any], m: Option[Metadata])(implicit ctx: SourceContext) { setProps(e, initRep(e, m)) }
  def setChild(e: Rep[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) { setProps(e, initRep(e, None, p)) }
  def setField(e: Rep[Any], p: Option[SymbolProperties], index: String)(implicit ctx: SourceContext) { setProps(e, initRep(e, None, p, Some(index))) }

  // Alternative versions
  def setProps(e: Rep[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) { if (p.isDefined) setProps(e, p.get) }
  def setMetadata(e: Rep[Any], m: Metadata)(implicit ctx: SourceContext) { setMetadata(e, Some(m)) }
  def setChild(e: Rep[Any], p: SymbolProperties)(implicit ctx: SourceContext) { setChild(e, Some(p)) }
  def setField(e: Rep[Any], p: SymbolProperties, index: String)(implicit ctx: SourceContext) { setField(e, Some(p), index) }

  // Get properties for given symbol
  def getProps(e: Rep[Any]): Option[SymbolProperties]

  // Get child metadata for given symbol properties
  def getChild(p: SymbolProperties): Option[SymbolProperties] = p match {
    case p: ArrayProperties => p.child
    case _ => None
  }
  def getField(p: SymbolProperties, index: String): Option[SymbolProperties] = p match {
    case p: StructProperties => p.child(index)
    case _ => None
  }

  def getMetadata[T<:Metadata](e: Rep[Any], k: Datakey[T]): Option[T] = getProps(e).flatMap{p => p(k)}
  def getChild(e: Rep[Any]): Option[SymbolProperties] = getProps(e).flatMap{p => getChild(p)}
  def getField(e: Rep[Any], index: String): Option[SymbolProperties] = getProps(e).flatMap{p => getField(p, index)}

  def meta[T<:Metadata](p: SymbolProperties)(implicit ct: ClassTag[T]): Option[T] = p[T]
  def meta[T<:Metadata](x: Rep[Any])(implicit ct: ClassTag[T]): Option[T] = getMetadata(x, ct.runtimeClass.asInstanceOf[Class[T]])

  // Shortcuts for properties
  // These shortcuts should only be used when child is guaranteed to be defined
  def child(p: SymbolProperties): SymbolProperties = getChild(p).get
  def child(p: SymbolProperties, index: String): SymbolProperties = getField(p, index).get

  def props(e: Rep[Any]): SymbolProperties = getProps(e).get
  def child(e: Rep[Any]): SymbolProperties = getChild(e).get
  def child(e: Rep[Any], index: String): SymbolProperties = getField(e, index).get
}

trait MetadataExp extends MetadataOps with Expressions with Blocks { self: BaseExp =>

  type Analyzer = AbstractAnalyzer { val IR: self.type }

  // State for compiler traversal use
  var metadata: Map[Exp[Any], SymbolProperties] = Map.empty

  // -----
  // TODO: These are currently unused, but may be useful later?
  var analyzers: Map[Datakey[_], Analyzer] = Map.empty
  var validData: List[Datakey[_]] = Nil
  // -----

  private var metadataUpdateFlag: Boolean = false
  private def setMetadataUpdateFlag() { metadataUpdateFlag = true }
  def clearMetadataUpdateFlag() { metadataUpdateFlag = false }
  def getMetadataUpdateFlag() = metadataUpdateFlag

  // Setters
  /**
   * Merge previous metadata for token and new data, notifying update if changes occurred
   * During merge, new metadata overrides pre-existing data when possible
   */
  def setProps(e: Rep[Any], p: SymbolProperties)(implicit ctx: SourceContext) {
    val prevProps = metadata.get(e)
    val newProps = meet(MetaOverwrite, prevProps, Some(p))
    metadata += e -> newProps.get
    if (!matches(prevProps, newProps)) setMetadataUpdateFlag()
  }

  // Getters
  def getProps(e: Rep[Any]): Option[SymbolProperties] = Some(metadata.getOrElse(e, initRep(e)(mpos(e.pos))))

  // TODO: Use getBlockResult instead?
  def getProps(b: Block[Any]): Option[SymbolProperties] = getProps(b.res)
  def getMetadata[T<:Metadata](b: Block[Any], k: Datakey[T]): Option[T] = getMetadata(b.res, k)
  def getChild(b: Block[Any]): Option[SymbolProperties] = getChild(b.res)
  def getField(b: Block[Any], index: String): Option[SymbolProperties] = getField(b.res, index)

  def defaultMetadata[T](tp: Manifest[T]): List[Metadata] = Nil

  // Symbol property initialization
  def initRep(e: Rep[Any], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties
    = initType(e.tp, data, child, index)

  def initType[A](tp: Manifest[A], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties = {
    val givenData = PropMap[Datakey[_],Metadata](data.map{m => m.key -> m}.toList)
    val typeData = PropMap[Datakey[_],Metadata](defaultMetadata(tp).map{m => m.key -> m})
    val symData = meet(MetaTypeInit, givenData, typeData)
    initProps(tp, symData, child, index)
  }

  def unapplyArrayLike[A](tp: Manifest[A]): Option[Manifest[_]] = None
  def unapplyStructLike[A](tp: Manifest[A]): Option[List[(String,Manifest[_])]] = None

  object ArrayLike {
    def unapply[A](tp: Manifest[A]): Option[Manifest[_]] = unapplyArrayLike(tp)
  }
  object StructLike {
    def unapply[A](tp: Manifest[A]): Option[List[(String,Manifest[_])]] = unapplyStructLike(tp)
  }

  // Should be overwritten for data structure types (e.g. structs, arrays)
  def initProps[A](tp: Manifest[A], symData: PropMap[Datakey[_],Metadata], child: Option[SymbolProperties], index: Option[String])(implicit ctx: SourceContext): SymbolProperties = tp match {
    case ArrayLike(childtp) =>
      val typeChild = initType(childtp)
      val symChild = meet(MetaTypeInit, child, Some(typeChild))
      ArrayProperties(symChild, symData)

    case StructLike(elems) =>
      val typeFields = PropMap(elems.map{case (index,tp) => index -> initType(tp) })
      val symFields = (index,child) match {
        case (Some(index),Some(child)) => meet(MetaTypeInit, PropMap(index, child), typeFields)
        case _ => typeFields
      }
      StructProperties(symFields, symData)

    case _ => ScalarProperties(symData)
  }
}

// Experimental version of traversal where traversal schedule can be determined dynamically by metadata dependencies
// TODO: Expected behavior for iterative traversal to attempt to run an analyzer prior to every iteration?
trait AbstractAnalyzer extends IterativeTraversal { self =>
  val IR: BaseFatExp with Effects
  import IR._

  override def hasConverged: Boolean = runs > 0 && !getMetadataUpdateFlag()

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

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    clearMetadataUpdateFlag()

    for (data <- datRequire) {
      if (!(validData contains data))
        analyzers(data).run(b)
    }
    val out = super.runOnce(b)

    // Invalidate all metadata which are subtypes of the list of this traversal's invalidation
    // Allows us to write, for example, invalidates (classOf[Metadata]) to invalidate all metadata
    validData = validData filterNot (dat => datInvalid.exists(isSubtype(dat, _)) )

    (out)
  }

  override def run[A:Manifest](b: Block[A]): Block[A] = withDebugging {
    val out = super.run(b)
    if (hasCompleted && hasConverged) {
      // Update metadata state
      datUpdate foreach {dat => analyzers += (dat -> self.asInstanceOf[Analyzer]) }
      validData = (validData ++ datCreate ++ datUpdate).distinct
    }
    (out)
  }

  datCreate foreach {dat => analyzers += (dat -> self.asInstanceOf[Analyzer]) }
}
