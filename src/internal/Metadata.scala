package scala.virtualization.lms
package internal

import scala.collection.immutable.HashMap
import scala.reflect.SourceContext

trait SymbolMetadata extends MeetableOps {
  // TODO: Better way to reference metadata?
  // Bonus points for being able to represent hierarchy (subclasses of Metadata)
  type Datakey = Class[_]

  implicit def OptionCanBeMeetable[T: Meetable]: Meetable[Option[T]] = new Meetable[Option[T]] {
    def _matches(a: Option[T], b: Option[T]) = (a,b) match {
      case (Some(a),Some(b)) => matches(a,b)
      case (Some(_), None) => false
      case (None, Some(_)) => false
      case (None,None) => true
    }
    def _incompatibilities(a: Option[T], b: Option[T], t: MeetFunc): List[String] = (a,b) match {
      case (Some(am), Some(bm)) => incompatibilities(am,bm,t)
      case _ => Nil
    }
    def _canMeet(a: Option[T], b: Option[T], t: MeetFunc): Boolean = (a,b) match {
      case (Some(am), Some(bm)) => canMeet(am,bm,t)
      case _ => true
    }
    def _meet(a: Option[T], b: Option[T], t: MeetFunc): Option[T] = (a,b) match {
      case (Some(am), Some(bm)) if canMeet(am,bm,t) => Some(meet(am,bm,t))
      case (Some(am), None) => Some(am)
      case (None, Some(bm)) => Some(bm)
      case (None, None) => None
      case _ => throw new IllegalMeetException
    }
    def _isComplete(a: Option[T]): Boolean = a match {
      case Some(am) => isComplete(am)
      case None => false
    }
    def _makeString(a: Option[T], prefix: String): String = a match {
      case Some(am) => makeString(am, prefix)
      case None => " is unknown!"
    }
    def _multiLine(a: Option[T]): Boolean = a match {
      case Some(am) => multiLine(am)
      case None => false
    }
  }

  /**
   * New Metadata types should extend this abstract class
   * and add cases to metadataMatches, canMeetMetadata, meetMetadata, metadataIsComplete
   */
  abstract class Metadata { self =>
    def key: Datakey = self.getClass
    //def name: String
    //Test if this metadata instance has been sufficiently filled in
    def isComplete: Boolean = true

    // Pretty printing metadata (mostly for debugging)
    def makeString(prefix: String): String = this.toString()
    def multiLine = false
  }

  /**
   * Tests if this and that are identical (have no differences)
   * returns true if they are identical, false otherwise
   */
  def metadataMatches(a: Metadata, b: Metadata): Boolean = false
  def metadataIncompatibilities(a: Metadata, b: Metadata, t: MeetFunc): List[String] = Nil

  /**
   * Test if this and that can be met to produce valid metadata
   * returns true if the two definitely can be met successfully
   */
  def canMeetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Boolean = (a,b,t) match {
    case _ => false
  }

  /**
   * Meet this and that. If canMeetMetadata returns false for this and that,
   * meet should throw an exception, since the meet is invalid
   */
  def meetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Metadata = (a,b,t) match {
    case _ => throw new IllegalMeetException
  }

  implicit object MetadataIsMeetable extends Meetable[Metadata] {
    def _matches(a: Metadata, b: Metadata) = metadataMatches(a,b)
    def _incompatibilities(a: Metadata, b: Metadata, t: MeetFunc) = metadataIncompatibilities(a,b,t)
    def _canMeet(a: Metadata, b: Metadata, t: MeetFunc) = canMeetMetadata(a,b,t)
    def _meet(a: Metadata, b: Metadata, t: MeetFunc) = meetMetadata(a,b,t)
    def _isComplete(a: Metadata) = a.isComplete
    def _makeString(a: Metadata, prefix: String) = " = " + a.makeString(prefix)
    def _multiLine(a: Metadata) = a.multiLine
  }

  // TODO: Can this just be changed to a HashMap?
  // TODO: Is the extra Option layer necessary here?
  case class PropertyMap[K,V:Meetable](__info: Iterable[(K, Option[V])]) {
    def this(info: PropertyMap[K,V]) { this(info.toList) }

    val data = new HashMap[K, Option[V]] ++ __info

    def size: Int = data.size
    def apply(x: K): Option[V] = data.getOrElse(x, None)
    def contains(x: K): Boolean = data.contains(x)
    def toList: List[(K, Option[V])] = data.toList
    def keys: Set[K] = data.keySet

    def map(f: (K, Option[V]) => (K, Option[V])): PropertyMap[K,V]
      = PropertyMap[K,V](data.map{e => f(e._1,e._2)}.toList)

    def zip(that: PropertyMap[K,V])(f: (Option[V], Option[V]) => Option[V]): PropertyMap[K,V] = {
      val allKeys = this.keys ++ that.keys
      PropertyMap[K,V](allKeys.map{k => k -> f(this(k), that(k))} )
    }

    def zipToList[R](that: PropertyMap[K,V])(f: (Option[V], Option[V]) => R): List[R] = {
      val allKeys = this.keys ++ that.keys
      allKeys.map{k => f(this(k), that(k))}.toList
    }

    /**
     * Reduce operation. Check that all properties in this map match the given condition
     * Trivially true if this contains no keys
     */
    def forall(f: Option[V] => Boolean): Boolean = {
      this.keys.isEmpty || this.keys.map{k => f(this(k))}.reduce{_&&_}
    }

    /**
     * Zip-Reduce operation. Get all keys from both maps, apply the zip function f which
     * produces a boolean for every pair. Then reduce the booleans using the AND operation
     * Trivially true if neither this nor that contains any keys
     */
    def zipForall(that: PropertyMap[K,V])(f: (Option[V], Option[V]) => Boolean): Boolean = {
      val allKeys = this.keys ++ that.keys
      allKeys.isEmpty || allKeys.map{k => f(this(k), that(k)) }.reduce{_&&_}
    }
  }
  object PropertyMap {
    // Have to be careful with this construction - could easily create a
    // PropertyMap[Option[Option[T]]] when PropertyMap[Option[T]] was desired
    def apply[K,V:Meetable](index: K, datum: Option[V]) = new PropertyMap(List(index -> datum))
    def apply[K,V:Meetable](index: K, datum: V) = new PropertyMap(List(index -> Some(datum)))
    def apply[K,V:Meetable]() = new PropertyMap[K,V](Nil)
  }

  implicit def PropertyMapIsMeetable[K,V:Meetable]: Meetable[PropertyMap[K,V]] = new Meetable[PropertyMap[K,V]] {
    def _matches(a: PropertyMap[K,V], b: PropertyMap[K,V]): Boolean = a.zipForall(b){(am,bm) => matches(am,bm)}
    def _incompatibilities(a: PropertyMap[K,V], b: PropertyMap[K,V], t: MeetFunc): List[String] = a.zipToList(b){(am,bm) => incompatibilities(am,bm,t)}.flatMap{i => i}
    def _canMeet(a: PropertyMap[K,V], b: PropertyMap[K,V], t: MeetFunc): Boolean = a.zipForall(b){(am,bm) => canMeet(am,bm,t)}
    def _isComplete(a: PropertyMap[K,V]): Boolean = a.forall{am => isComplete(am)}
    def _meet(a: PropertyMap[K,V], b: PropertyMap[K,V], t: MeetFunc): PropertyMap[K,V] = a.zip(b){(am,bm) => meet(am,bm,t)}

    def _makeString(a: PropertyMap[K,V], prefix: String): String = {
      if (a.data.isEmpty) ""
      else
        a.data.toList.sortBy{x => x._1.toString}.map{dat => prefix + "." + dat._1 + makeString(dat._2, prefix)}.mkString("\n")
    }
    def _multiLine(a: PropertyMap[K,V]): Boolean = a.size > 0
  }

  /**
   * Parent class for metadata containers. Holds a hash map of
   * string keys to single properties (Metadata instances)
   */
  sealed abstract class SymbolProperties (val data: PropertyMap[Datakey,Metadata]) {
    def apply(x: Datakey) = data(x)
  }

  // Metadata for scalar symbols (single element)
  case class ScalarProperties(override val data: PropertyMap[Datakey,Metadata]) extends SymbolProperties(data)

  // Metadata for DeliteStructs
  case class StructProperties(children: PropertyMap[String,SymbolProperties], override val data: PropertyMap[Datakey,Metadata])
    extends SymbolProperties(data)
  {
    def child(field: String) = children(field)
    def fields = children.keys
  }

  // Metadata for arrays
  case class ArrayProperties(child: Option[SymbolProperties], override val data: PropertyMap[Datakey,Metadata])
    extends SymbolProperties(data)

  implicit object SymbolPropertiesIsMeetable extends Meetable[SymbolProperties] {
    def _matches(a: SymbolProperties, b: SymbolProperties): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) =>
        matches(a.data, b.data)
      case (a: StructProperties, b: StructProperties) =>
        matches(a.data,b.data) && matches(a.children, b.children)
      case (a: ArrayProperties, b: ArrayProperties) =>
        matches(a.data, b.data) && matches(a.child, b.child)
      case _ => false
    }
    def _incompatibilities(a: SymbolProperties, b: SymbolProperties, t: MeetFunc): List[String] = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) =>
        incompatibilities(a.data, b.data, t)
      case (a: StructProperties, b: StructProperties) =>
        incompatibilities(a.data, b.data, t) ::: incompatibilities(a.children, b.children, t)
      case (a: ArrayProperties, b: ArrayProperties) =>
        incompatibilities(a.data, b.data, t) ::: incompatibilities(a.child, b.child, t)
      case _ =>
        List("different symbol property types")
    }
    def _canMeet(a: SymbolProperties, b: SymbolProperties, t: MeetFunc): Boolean = (a,b,t) match {
      case (a: ScalarProperties, b: ScalarProperties, _) =>
        canMeet(a.data, b.data, t)
      case (a: StructProperties, b: StructProperties, _) =>
        canMeet(a.data, b.data, t) && canMeet(a.children, b.children, t)
      case (a: ArrayProperties, b: ArrayProperties, _) =>
        canMeet(a.data, b.data, t) && canMeet(a.child, b.child, t)
      case _ => false
    }
    def _meet(a: SymbolProperties, b: SymbolProperties, t: MeetFunc): SymbolProperties = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) if _canMeet(a,b,t) =>
        ScalarProperties(meet(a.data, b.data, t))
      case (a: StructProperties, b: StructProperties) if _canMeet(a,b,t)=>
        StructProperties(meet(a.children, b.children, t), meet(a.data, b.data, t))
      case (a: ArrayProperties, b: ArrayProperties) if _canMeet(a,b,t) =>
        ArrayProperties(meet(a.child, b.child, t), meet(a.data, b.data, t))
      case _ => throw new IllegalMeetException
    }
    def _isComplete(a: SymbolProperties): Boolean = a match {
      case a: ScalarProperties => isComplete(a.data)
      case a: StructProperties => isComplete(a.data) && isComplete(a.children)
      case a: ArrayProperties => isComplete(a.data) && isComplete(a.child)
    }

    def _makeString(a: SymbolProperties, prefix: String): String = a match {
      case a: ScalarProperties =>
        ": Scalar {" + (if (multiLine(a.data)) "\n" else "") + makeString(a.data, prefix + "  ") + (if (multiLine(a.data)) "\n" + prefix else "") + "}"
      case a: StructProperties =>
        ": Struct {\n" + prefix + " Metadata:" + (if (multiLine(a.data)) "\n" + prefix else "")  + makeString(a.data, prefix + "  ") + "\n" +
                       prefix + " Fields:\n" + makeString(a.children, prefix + "  ") + "\n" + prefix + "}"
      case a: ArrayProperties =>
        ": Array {\n" + prefix + " Metadata:" + (if (multiLine(a.data)) "\n" + prefix else "") + makeString(a.data, prefix + "  ") + "\n" +
                      prefix + " Child" + makeString(a.child, prefix + "  ") + "\n" + prefix + "}"
    }
    def _multiLine(a: SymbolProperties): Boolean = a match {
      case a: ScalarProperties => multiLine(a.data)
      case _ => true
    }
  }

  object NoData extends PropertyMap[Datakey, Metadata](Nil)
  object NoChildren extends PropertyMap[String, SymbolProperties](Nil)
}


trait MetadataOps extends Expressions with Blocks with SymbolMetadata { self =>
  type Analyzer = Traversal { val IR: self.type }

  ///////////////////
  // Symbol Metadata

  // TODO: Is this the right spot for these?
  // Note that SourceContext is used all over the place with the intention for use in error messages - may not need to keep these around

  var metadata: Map[Exp[Any], SymbolProperties] = Map.empty
  var validData: List[Datakey] = Nil
  var analyzers: Map[Datakey, Analyzer] = Map.empty

  private var metadataUpdateFlag: Boolean = false
  private def setMetadataUpdateFlag() { metadataUpdateFlag = true }
  def clearMetadataUpdateFlag() { metadataUpdateFlag = false }
  def getMetadataUpdateFlag() = metadataUpdateFlag

  // Setters
  // Directly add symbol property metadata mapping for symbol
  def setProps(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) { updateProperties(e, p) }
  def setProps(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) { if (p.isDefined) setProps(e, p.get) }

  // Add metadata information for this symbol (possibly using meet)
  def setMetadata(e: Exp[Any], m: Option[Metadata])(implicit ctx: SourceContext) { updateProperties(e, initExp(e, m)) }
  def setMetadata(e: Exp[Any], m: Metadata)(implicit ctx: SourceContext) { setMetadata(e, Some(m)) }

  // Add child information for this symbol (possibly using meet)
  def setChild(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) { updateProperties(e, initExp(e, None, p)) }
  def setChild(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) { setChild(e, Some(p)) }

  def setField(e: Exp[Any], p: Option[SymbolProperties], index: String)(implicit ctx: SourceContext) { updateProperties(e, initExp(e, None, p, Some(index))) }
  def setField(e: Exp[Any], p: SymbolProperties, index: String)(implicit ctx: SourceContext) { setField(e, Some(p), index) }

  // Getters
  // Get child metadata for given symbol properties
  def getChild(p: SymbolProperties): Option[SymbolProperties] = p match {
    case p: ArrayProperties => p.child
    case _ => printwarn("Attempted to get child of symbol properties with no child!"); None
  }
  def getField(p: SymbolProperties, index: String): Option[SymbolProperties] = p match {
    case p: StructProperties => p.child(index)
    case _ => printwarn("Attempted to get field of symbol properties with no fields!"); None
  }

  // Get child metadata for given symbol
  def getProps(e: Exp[Any]): Option[SymbolProperties] = Some(metadata.getOrElse(e, initExp(e)(mpos(e.pos))))
  def getMetadata(e: Exp[Any], k: Datakey): Option[Metadata] = getProps(e).flatMap{p => p(k)}
  def getChild(e: Exp[Any]): Option[SymbolProperties] = getProps(e).flatMap{p => getChild(p)}
  def getField(struct: Exp[Any], index: String): Option[SymbolProperties] = getProps(struct).flatMap{p => getField(p, index)}

  // TODO: Use getBlockResult instead?
  def getProps(b: Block[Any]): Option[SymbolProperties] = getProps(b.res)
  def getMetadata(b: Block[Any], k: Datakey): Option[Metadata] = getMetadata(b.res, k)
  def getChild(b: Block[Any]): Option[SymbolProperties] = getChild(b.res)
  def getField(b: Block[Any], index: String): Option[SymbolProperties] = getField(b.res, index)

  /**
   * Merge previous metadata for token and new data, notifying update if changes occurred
   * During merge, new metadata overrides pre-existing data when possible
   */
  private def updateProperties(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) {
    val prevProps = metadata.get(e)
    val newProps = tryMeet(prevProps, Some(p), func = MetaOverwrite)
    metadata = metadata ++ List(e -> newProps.get)
    if (!matches(prevProps, newProps)) setMetadataUpdateFlag()
  }

  def defaultMetadata[T](tp: Manifest[T]): List[Metadata] = Nil

  // Symbol property initialization
  def initExp(e: Exp[Any], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties
    = initType(e.tp, data, child, index)

  def initType[A](tp: Manifest[A], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties = {
    val givenData = PropertyMap[Datakey,Metadata](data.map{m => m.key -> Some(m)}.toList)
    val typeData = PropertyMap[Datakey,Metadata](defaultMetadata(tp).map{m => m.key -> Some(m)})
    val symData = tryMeet(givenData, typeData, func = MetaTypeInit)
    initProps(tp, symData, child, index)
  }

  // Should be overwritten for data structure types (e.g. structs, arrays)
  def initProps[A](tp: Manifest[A], symData: PropertyMap[Datakey,Metadata], child: Option[SymbolProperties], index: Option[String])(implicit ctx: SourceContext): SymbolProperties = tp match {
    case _ => ScalarProperties(symData)
  }
}