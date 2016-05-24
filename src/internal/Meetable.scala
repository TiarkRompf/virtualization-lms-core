package scala.virtualization.lms
package internal

import scala.reflect.SourceContext
import scala.collection.immutable.HashMap
import scala.reflect._

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
  case object MetaAlias extends MeetFunc          // General aliasing of metadata
  case object MetaTypeInit extends MeetFunc       // Metadata meeting with initial type metadata
  case object MetaOverwrite extends MeetFunc      // Metadata overwrite

  // Defs use underscore prefix since some implementations require calling other forms of the
  // overloaded method, which is more annoying (need to directly call implicitly[Meetable[...]].func)
  // if both the type class definition and the templated function have the same name
  // This effectively does the same thing as using implicitly[...] but with less code
  // Internal API for metadata
  def matches[T: Meetable](a: T, b: T): Boolean = implicitly[Meetable[T]]._matches(a,b)
  def incompatibilities[T:Meetable](a: T, b: T)(implicit t: MeetFunc): List[String] = implicitly[Meetable[T]]._incompatibilities(a,b)(t)
  def canMeet[T: Meetable](a: T, b: T)(implicit t: MeetFunc): Boolean = { implicitly[Meetable[T]]._canMeet(a,b)(t) }
  def meet[T:Meetable](ts: T*)(implicit t: MeetFunc = MetaAlias): T = ts.reduce{(a,b) => tryMeet(a,b) }
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


// TODO: Factor out to separate file?
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
    def _matches(that: self.type): Boolean = {this == that}
    def metaMatches(that: Metadata) = this.getClass == that.getClass && _matches(that.asInstanceOf[self.type])

    // Test if this and that can be met to produce valid metadata
    // TODO: Probably don't need both canMeet and incompatibilities, can just have latter
    def _canMeet(that: self.type)(implicit t: MeetFunc): Boolean = _incompatibilities(that).isEmpty
    def metaCanMeet(that: Metadata)(implicit t: MeetFunc) = this.getClass == that.getClass && _canMeet(that.asInstanceOf[self.type])

    def _incompatibilities(that: self.type)(implicit t: MeetFunc): List[String] = Nil
    def metaIncompatibilities(that: Metadata)(implicit t: MeetFunc) = if (this.getClass == that.getClass) _incompatibilities(that.asInstanceOf[self.type]) else List("Cannot meet metadata of different types")

    // this: always preserve newest value
    // that: always preserve oldest value
    // or something else entirely, depends on metadata!
    def _meet(that: self.type)(implicit t: MeetFunc): Metadata = this
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
