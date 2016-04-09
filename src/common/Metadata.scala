package scala.virtualization.lms
package common

import internal.{SymbolMetadata, Expressions, Blocks, Analyzing}

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

trait MetadataExp extends MetadataOps with Expressions with Blocks { this: BaseExp =>

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

  def getProps(b: Block[Any]): Option[SymbolProperties] = getProps(getBlockResult(b))
  def getMetadata[T<:Metadata](b: Block[Any], k: Datakey[T]): Option[T] = getMetadata(getBlockResult(b), k)
  def getChild(b: Block[Any]): Option[SymbolProperties] = getChild(getBlockResult(b))
  def getField(b: Block[Any], index: String): Option[SymbolProperties] = getField(getBlockResult(b), index)

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
