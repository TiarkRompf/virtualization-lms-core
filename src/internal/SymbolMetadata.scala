package scala.virtualization.lms
package internal

import scala.collection.immutable.HashMap

import Meetable._

trait SymbolMetadata {
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
  abstract class Metadata { 
    def name: String
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

  case class PropertyMap[T:Meetable](__info: Iterable[(String, Option[T])]) {
    def this(info: PropertyMap[T]) { this(info.toList) }
    
    val data = new HashMap[String, Option[T]] ++ __info

    def size: Int = data.size
    def apply(x: String): Option[T] = data.getOrElse(x, None)
    def contains(x: String): Boolean = data.contains(x)
    def toList: List[(String, Option[T])] = data.toList
    def keys: Set[String] = data.keySet

    def map(f: (String, Option[T]) => (String, Option[T])): PropertyMap[T]
      = PropertyMap(data.map{e => f(e._1,e._2)}.toList)

    def zip(that: PropertyMap[T])(f: (Option[T], Option[T]) => Option[T]): PropertyMap[T] = {
      val allKeys = this.keys ++ that.keys
      PropertyMap[T](allKeys.map{k => k -> f(this(k), that(k))} )
    }

    def zipToList[R](that: PropertyMap[T])(f: (Option[T], Option[T]) => R): List[R] = {
      val allKeys = this.keys ++ that.keys
      allKeys.map{k => f(this(k), that(k))}.toList
    }

    /**
     * Reduce operation. Check that all properties in this map match the given condition
     * Trivially true if this contains no keys
     */
    def requireAll(f: Option[T] => Boolean): Boolean = {
      this.keys.isEmpty || this.keys.map{k => f(this(k))}.reduce{_&&_}
    }

    /**
     * Zip-Reduce operation. Get all keys from both maps, apply the zip function f which 
     * produces a boolean for every pair. Then reduce the booleans using the AND operation
     * Trivially true if neither this nor that contains any keys
     */
    def zipRequireAll(that: PropertyMap[T])(f: (Option[T], Option[T]) => Boolean): Boolean = {
      val allKeys = this.keys ++ that.keys
      allKeys.isEmpty || allKeys.map{k => f(this(k), that(k)) }.reduce{_&&_}
    }
  }
  object PropertyMap {
    // Have to be careful with this construction - could easily create a 
    // PropertyMap[Option[Option[T]]] when PropertyMap[Option[T]] was desired
    def apply[T:Meetable](index: String, datum: Option[T]) = new PropertyMap(List(index -> datum))
    def apply[T:Meetable](index: String, datum: T) = new PropertyMap(List(index -> Some(datum)))
    def apply[T:Meetable]() = new PropertyMap[T](Nil)
  }

  implicit def PropertyMapIsMeetable[T:Meetable]: Meetable[PropertyMap[T]] = new Meetable[PropertyMap[T]] {
    def _matches(a: PropertyMap[T], b: PropertyMap[T]): Boolean = a.zipRequireAll(b){(am,bm) => matches(am,bm)}
    def _incompatibilities(a: PropertyMap[T], b: PropertyMap[T], t: MeetFunc): List[String] = a.zipToList(b){(am,bm) => incompatibilities(am,bm,t)}.flatMap{i => i}
    def _canMeet(a: PropertyMap[T], b: PropertyMap[T], t: MeetFunc): Boolean = a.zipRequireAll(b){(am,bm) => canMeet(am,bm,t)}
    def _isComplete(a: PropertyMap[T]): Boolean = a.requireAll{am => isComplete(am)}
    def _meet(a: PropertyMap[T], b: PropertyMap[T], t: MeetFunc): PropertyMap[T] = a.zip(b){(am,bm) => meet(am,bm,t)}
   
    def _makeString(a: PropertyMap[T], prefix: String): String = {
      if (a.data.isEmpty) ""
      else 
        a.data.toList.sortBy{x => x._1}.map{dat => prefix + "." + dat._1 + makeString(dat._2, prefix)}.mkString("\n")
    }
    def _multiLine(a: PropertyMap[T]): Boolean = a.size > 0
  }

  /**
   * Parent class for metadata containers. Holds a hash map of 
   * string keys to single properties (Metadata instances)
   */ 
  sealed abstract class SymbolProperties (val data: PropertyMap[Metadata]) {
    def apply(x: String) = data(x)
  }

  // Metadata for scalar symbols (single element)
  case class ScalarProperties(override val data: PropertyMap[Metadata]) extends SymbolProperties(data)

  // Metadata for DeliteStructs 
  case class StructProperties(children: PropertyMap[SymbolProperties], override val data: PropertyMap[Metadata]) 
    extends SymbolProperties(data) 
  {
    def child(field: String) = children(field)
    def fields = children.keys
  }

  // Metadata for arrays
  case class ArrayProperties(child: Option[SymbolProperties], override val data: PropertyMap[Metadata])
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
  
  object NoData extends PropertyMap[Metadata](Nil)
  object NoChildren extends PropertyMap[SymbolProperties](Nil)
}