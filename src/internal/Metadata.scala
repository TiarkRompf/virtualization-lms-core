package scala.virtualization.lms
package internal

import scala.collection.immutable.HashMap
import scala.reflect._
import scala.virtualization.lms.common.{Base,BaseExp}

// TODO: This should probably be moved to LMS common since we reference Base?

trait SymbolMetadata extends MeetableOps {
  // TODO: Better way to reference metadata?
  // Bonus points for being able to represent hierarchy (subclasses of Metadata)
  // TODO: Should T be required to be subclass of Metadata? This makes things somewhat annoying..
  type Datakey[T] = Class[T]

  def keyOf[T](implicit ct: ClassTag[T]): Datakey[T] = ct.runtimeClass.asInstanceOf[Class[T]]

  implicit def OptionCanBeMeetable[T:Meetable]: Meetable[Option[T]] = new Meetable[Option[T]] {
    def _matches(a: Option[T], b: Option[T]) = (a,b) match {
      case (Some(a),Some(b)) => matches(a,b)
      case (Some(_), None) => false
      case (None, Some(_)) => false
      case (None,None) => true
    }
    def _incompatibilities(a: Option[T], b: Option[T])(implicit t: MeetFunc): List[String] = (a,b) match {
      case (Some(am), Some(bm)) => incompatibilities(am,bm)
      case _ => Nil
    }
    def _canMeet(a: Option[T], b: Option[T])(implicit t: MeetFunc): Boolean = (a,b) match {
      case (Some(am), Some(bm)) => canMeet(am,bm)
      case _ => true
    }
    def _meet(a: Option[T], b: Option[T])(implicit t: MeetFunc): Option[T] = (a,b) match {
      case (Some(am), Some(bm)) if canMeet(am,bm) => Some(meet(am,bm))
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
   * and add cases to metadataMatches, canMeetMetadata, meetMetadata
   * TODO: The general implementations here are a bit sketchy...
   */
  abstract class Metadata { self =>
    def key = self.getClass
    //Test if this metadata instance has been sufficiently filled in
    def isComplete: Boolean = true

    // Tests if this and that are identical
    def _matches(that: self.type): Boolean
    def metaMatches(that: Metadata) = this.getClass == that.getClass && _matches(that.asInstanceOf[self.type])

    // Test if this and that can be met to produce valid metadata
    // TODO: Probably don't need both canMeet and incompatibilities, can just have latter
    def _canMeet(that: self.type)(implicit t: MeetFunc): Boolean = _incompatibilities(that).isEmpty
    def metaCanMeet(that: Metadata)(implicit t: MeetFunc) = this.getClass == that.getClass && _canMeet(that.asInstanceOf[self.type])

    def _incompatibilities(that: self.type)(implicit t: MeetFunc): List[String]
    def metaIncompatibilities(that: Metadata)(implicit t: MeetFunc) = if (this.getClass == that.getClass) _incompatibilities(that.asInstanceOf[self.type]) else List("Cannot meet metadata of different types")

    def _meet(that: self.type)(implicit t: MeetFunc): Metadata
    def metaMeet(that: Metadata)(implicit t: MeetFunc) = if (this.getClass == that.getClass) _meet(that.asInstanceOf[self.type])(t) else throw new IllegalMeetException

    // Pretty printing metadata (mostly for debugging)
    def makeString(prefix: String): String = this.toString()
    def multiLine = false
  }

  /**
   * Tests if this and that are identical (have no differences)
   * returns true if they are identical, false otherwise
   */
  //def metadataMatches(a: Metadata, b: Metadata): Boolean = (a == b)

  /**
   * Test if this and that can be met to produce valid metadata
   * returns true if the two definitely can be met successfully
   */
  /*def canMeetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Boolean = a.getClass == b.getClass

  def metadataIncompatibilities(a: Metadata, b: Metadata, t: MeetFunc): List[String] = {
    if (a.getClass != b.getClass)
      List("Cannot meet different metadata types!")
    else
      Nil
  }*/

  /**
   * Meet this and that. If canMeetMetadata returns false for this and that,
   * meet should throw an exception, since the meet is invalid
   */
  //def meetMetadata(a: Metadata, b: Metadata, t: MeetFunc): Metadata = (a,b,t) match {
  //  case _ => throw new IllegalMeetException
  //}

  //implicit object MetadataIsMeetable extends Meetable[Metadata] {

  // All concrete classes (T) that extend Metadata should be meetable
  // This has the disadvantage of not being a static object, but it means we can write things like
  // case class MyData(...) extends Metadata
  // val x = MyData(..); val y = MyData(...); meet(x,y)
  // without having to define implicit meet rules for each subclass explicitly
  implicit def MetadataIsMeetable[T<:Metadata]: Meetable[T] = new Meetable[T] {
    def _matches(a: T, b: T) = a.metaMatches(b)
    def _incompatibilities(a: T, b: T)(implicit t: MeetFunc) = a.metaIncompatibilities(b)(t)
    def _canMeet(a: T, b: T)(implicit t: MeetFunc) = a.metaCanMeet(b)(t)
    def _meet(a: T, b: T)(implicit t: MeetFunc) = a.metaMeet(b)(t).asInstanceOf[T]
    def _isComplete(a: T) = a.isComplete
    def _makeString(a: T, prefix: String) = " = " + a.makeString(prefix)
    def _multiLine(a: T) = a.multiLine
  }

  /**
   * Extension of HashMap for use with symbol metadata
   * TODO: A lot of this is simply a wrapper around HashMap - possible to just use HashMap instead?
   */
  case class PropMap[K,V](__info: Iterable[(K,V)]) {
    def this(info: PropMap[K,V]) { this(info.toList) }

    private val data = new HashMap[K,V] ++ __info

    def size: Int = data.size
    def apply(x: K): Option[V] = data.get(x)
    def contains(x: K): Boolean = data.contains(x)
    def toList: List[(K,V)] = data.toList
    def keys: Set[K] = data.keySet
    def values: List[V] = data.values.toList

    def map(func: V => V) = PropMap( data.toList.map(kv => (kv._1, func(kv._2))) )

    /**
     * Combine two property maps by key, merging value entries using function f
     */
    def zip[R](that: PropMap[K,V])(f: (Option[V],Option[V]) => Option[R]): PropMap[K,R] = {
      PropMap((this.keys ++ that.keys).flatMap{k => f(this(k), that(k)).map(k -> _) })
    }

    /**
     * Check that all properties in this map match the given condition
     * Trivially true if this contains no keys
     */
    def forall(f: V => Boolean): Boolean = data.forall{kv => f(kv._2)}

    /**
     * Fused zip and forall - combine two property maps, then check if function f is true for each
     * Trivially true if neither this nor that contains any keys
     */
    def zipForall(that: PropMap[K,V])(f: (Option[V], Option[V]) => Boolean): Boolean = {
      (this.keys ++ that.keys).forall{k => f(this(k), that(k)) }
    }
  }
  object PropMap {
    def apply[K,V](index: K, data: Option[V]) = new PropMap(data.map(index -> _))
    def apply[K,V](index: K, data: V) = new PropMap(List(index -> data))
    def apply[K,V]() = new PropMap[K,V](Nil)
  }

  implicit def PropMapCanBeMeetable[K,V:Meetable]: Meetable[PropMap[K,V]] = new Meetable[PropMap[K,V]] {
    def _matches(a: PropMap[K,V], b: PropMap[K,V]): Boolean = a.zipForall(b){(am,bm) => matches(am,bm)}
    def _incompatibilities(a: PropMap[K,V], b: PropMap[K,V])(implicit t: MeetFunc): List[String] = {
      a.zip(b){ (am,bm) =>
        val inc = incompatibilities(am,bm)
        if (inc.isEmpty) None else Some(inc.head)
      }.values
    }
    def _canMeet(a: PropMap[K,V], b: PropMap[K,V])(implicit t: MeetFunc): Boolean = a.zipForall(b){(am,bm) => canMeet(am,bm)}
    def _isComplete(a: PropMap[K,V]): Boolean = a.forall{am => isComplete(am)}
    def _meet(a: PropMap[K,V], b: PropMap[K,V])(implicit t: MeetFunc): PropMap[K,V] = a.zip(b){(am,bm) => meet(am,bm)}

    def _makeString(a: PropMap[K,V], prefix: String): String = {
      a.toList.sortBy{x => x._1.toString}.map{case (k,v) => prefix + "." + k + makeString(v, prefix)}.mkString("\n")
    }
    def _multiLine(a: PropMap[K,V]): Boolean = a.size > 0
  }

  /**
   * Parent class for metadata containers. Holds a hash map of
   * string keys to single properties (Metadata instances)
   */
  sealed abstract class SymbolProperties (val data: PropMap[Datakey[_],Metadata]) {
    def apply[T<:Metadata](x: Datakey[T]): Option[T] = data(x).map(_.asInstanceOf[T])
    def apply[T:ClassTag]: Option[T] = data(classTag[T].runtimeClass.asInstanceOf[Datakey[T]]).map(_.asInstanceOf[T])
  }

  // Metadata for scalar symbols (single element)
  case class ScalarProperties(override val data: PropMap[Datakey[_],Metadata]) extends SymbolProperties(data)

  // Metadata for DeliteStructs
  case class StructProperties(children: PropMap[String,SymbolProperties], override val data: PropMap[Datakey[_],Metadata])
    extends SymbolProperties(data)
  {
    def child(field: String) = children(field)
    def fields = children.keys
  }

  // Metadata for arrays
  case class ArrayProperties(child: Option[SymbolProperties], override val data: PropMap[Datakey[_],Metadata])
    extends SymbolProperties(data)

  implicit object SymbolPropertiesIsMeetable extends Meetable[SymbolProperties] {
    def _matches(a: SymbolProperties, b: SymbolProperties): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => matches(a.data, b.data)
      case (a: StructProperties, b: StructProperties) => matches(a.data,b.data) && matches(a.children, b.children)
      case (a: ArrayProperties, b: ArrayProperties) => matches(a.data, b.data) && matches(a.child, b.child)
      case _ => false
    }
    def _incompatibilities(a: SymbolProperties, b: SymbolProperties)(implicit t: MeetFunc): List[String] = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => incompatibilities(a.data, b.data)
      case (a: StructProperties, b: StructProperties) => incompatibilities(a.data, b.data) ::: incompatibilities(a.children, b.children)
      case (a: ArrayProperties, b: ArrayProperties) => incompatibilities(a.data, b.data) ::: incompatibilities(a.child, b.child)
      case _ => List("different symbol property types")
    }
    def _canMeet(a: SymbolProperties, b: SymbolProperties)(implicit t: MeetFunc): Boolean = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) => canMeet(a.data, b.data)
      case (a: StructProperties, b: StructProperties) => canMeet(a.data, b.data) && canMeet(a.children, b.children)
      case (a: ArrayProperties, b: ArrayProperties) => canMeet(a.data, b.data) && canMeet(a.child, b.child)
      case _ => false
    }
    def _meet(a: SymbolProperties, b: SymbolProperties)(implicit t: MeetFunc): SymbolProperties = (a,b) match {
      case (a: ScalarProperties, b: ScalarProperties) if _canMeet(a,b) => ScalarProperties(meet(a.data, b.data))
      case (a: StructProperties, b: StructProperties) if _canMeet(a,b)=> StructProperties(meet(a.children, b.children), meet(a.data, b.data))
      case (a: ArrayProperties, b: ArrayProperties) if _canMeet(a,b) => ArrayProperties(meet(a.child, b.child), meet(a.data, b.data))
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

  object NoData extends PropMap[Datakey[_], Metadata](Nil)
  object NoChildren extends PropMap[String, SymbolProperties](Nil)
}

trait MetadataOps extends SymbolMetadata { this: Base =>

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

  // State for compiler traversal use
  var metadata: Map[Exp[Any], SymbolProperties] = Map.empty

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

  // Should be overwritten for data structure types (e.g. structs, arrays)
  def initProps[A](tp: Manifest[A], symData: PropMap[Datakey[_],Metadata], child: Option[SymbolProperties], index: Option[String])(implicit ctx: SourceContext): SymbolProperties = tp match {
    case _ => ScalarProperties(symData)
  }
}
