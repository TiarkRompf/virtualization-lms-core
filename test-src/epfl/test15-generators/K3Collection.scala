import xml._
import java.util.Date
import java.text.SimpleDateFormat

package org.dbtoaster.dbtoasterlib {

  /**
   * This object contains the implementation of the persistent and intermediat
   * collections used by DBToaster
   */
  object K3Collection {
    // mutable maps are used for better performance
    import scala.collection.mutable.Map

    /**
     * The trait that defines the operations that can be done on a persistent
     * collection and contains some common methods.
     *
     * @param <K> Key type (a tuple in most cases)
     * @param <V> Value type
     */
    trait K3Collection[K, V] {

      /**
       * Checks whether an element with a given key exists in the collection
       *
       * @param key The key that should be checked
       * @return True if the element is in the collection, false otherwise
       */
      def contains(key: K): Boolean

      /**
       * Looks up an element in the collection given a key
       *
       * Note: If there is no element with the given key in the
       * collection, an error is thrown
       *
       * @param key The key of that should be looked up
       * @return The value of the element
       */
      def lookup(key: K): V

	  /**
       * Looks up an element in the collection given a key
       *
       * @param key The key of that should be looked up
	   * @param defVal The default value to be used if the key is not found
       * @return The value of the element
       */
      def lookup(key: K, defVal: V): V
	  
      /**
       * Turns the collection into a list of key-value tuples
       *
       * @return The list of elements
       */
      def toIterable(): Iterable[Tuple2[K, V]]

      /**
       * Calls the function f on every element of the collection
       * and stores the result in a new intermediate collection
       *
       * @param f The function that is used for the mapping
       * @return A collection containing the mapped elements
       */
      def map[K2, V2](f: Tuple2[K, V] => Tuple2[K2, V2]):
                                             K3IntermediateCollection[K2, V2]

      /**
       * Calls a function f for every element of the collection
       *
       * @param f The function
       */
      def foreach(f: Tuple2[K, V] => Unit): Unit

      /**
       * Gets all the elements with a certain partial key.
       * E.g. returns all the elements with the key <1,*,3>
       * where * stands for an arbitrary value
       *
       * @param keyPart The partial key
       * @param idx The indices of the elements that can be found in the
       * partial key
       * @return The matching elements
       */
      def slice[KP](keyPart: KP, idx: List[Int]): 
            K3IntermediateCollection[K, V]

      /**
       * Filters a collections by a function f
       *
       * @param f The filter function
       * @return A collection with the elements for which the function f
       * returned true
       */
      def filter(f: Tuple2[K, V] => Boolean): K3IntermediateCollection[K, V]

      def groupByAggregate[K2, V2](init: V2, group: Tuple2[K, V] => K2, 
            fn: Tuple2[K, V] => V2 => V2): K3IntermediateCollection[K2, V2]

      /**
       * Folds all the elements of a collection into one value
       *
       * @param init The initial value
       * @param fn A function taking the current value and an element and
       * returns the new value
       * @return The value after calling the function for each element
       */
      def fold[Y](init: Y, fn: Tuple2[K, V] => Y => Y): Y
      def foldLong(init: Long, fn: Tuple2[K, V] => Long => Long): Long

      /**
       * Flattens a nested collection
       *
       * @return The flattened collection
       */
      def flatten[K2, V2](): K3IntermediateCollection[K2, V2]

      /**
       * Turns a collection into a persistent collection
       *
       * @return The persistent collection
       */
      def toPersistentCollection(): K3PersistentCollection[K, V]

      val ft = new SimpleDateFormat("yyyyMMdd")

      /**
       * Turns a value into a string
       *
       * @param v The value to be turned into a string
       * @return A string representing the value
       */
      def valToStr(v: Any): String = {
        v match {
          case d: Date => ft.format(d)
          case x => x.toString
        }
      }

      /**
       * Generates an XML representation of the collection (similar to
       * the C++ backend) which is mostly used for debugging purposes
       *
       * @return The XML representing the collection
       */
      def toXML(): List[Elem] = {
        toIterable().foldLeft(List[Elem]()) {
          case (l, (k, v)) =>
            (try {
              val keyXML: List[xml.Elem] = (
                k.asInstanceOf[Product].productIterator.foldLeft(
                        (0, List[xml.Elem]())) { 
                    case ((i, l), k) => 
                        (i + 1, <xml>{ valToStr(k) }</xml>.copy(
                            label = ("__a" + i)) :: l) 
                    })._2
              <item> { keyXML.reverse } <__av>{ valToStr(v) }</__av></item>
            } catch {
              case e: java.lang.ClassCastException => 
                <item><__a0>{ valToStr(k) }</__a0>
                <__av>{ valToStr(v) }</__av></item>
            }) :: l
        }
      }
    }

    /**
     * Trait for an index on a collection
     *
     * @param <K> Key type of the collection
     * @param <V> Value type of the collection
     */
    trait Index[K, V] {
      /**
       * Updates the index
       * @param keyVal The element to be updated
       */
      def update(keyVal: Tuple2[K, V]): Unit
      /**
       * Removes an element from the index
       *
       * @param key The element to be removed
       */
      def remove(key: K): Unit
      /**
       * Returns all the elements that match a
       * certain partial key
       *
       * @param keyPart The partial key
       * @return The matching elements
       */
      def slice[PK](keyPart: PK): Map[K, V]

      /**
       * Removes all bindings from the map.
       */
      def clear(): Unit
    }

    /**
     * A persistent value
     *
     * @param <T> The type of the value
     */
    case class SimpleVal[T](name: String, defval: T) {
      var v: T = defval

      /**
       * Returns the current value
       *
       * @return The value
       */
      def get(): T = v

      /**
       * Updates the value
       *
       * @param nv The new value
       */
      def update(nv: T): Unit = {
        v = nv
      }

      override def toString(): String = v.toString

      def printSize() = println(name + ": 1")
    }

    /**
     * Implements a secondary index on a collection. The function project
     * defines how a key is mapped to a partial key.
     *
     * @param <PK> Partial key type
     * @param <K> Key type
     * @param <V> Value type
     */
    case class SecondaryIndex[PK, K, V](project: K => PK) extends Index[K, V] {
      val index = Map[PK, Map[K, V]]()

      def update(keyVal: Tuple2[K, V]): Unit = {
        val keyPart = project(keyVal._1)

        index.get(keyPart) match {
          case Some(m) => m += keyVal
          case None => index += ((keyPart, Map[K, V](keyVal)))
        }
      }

      def remove(key: K): Unit = {
        val keyPart = project(key)

        index.get(keyPart) match {
          case Some(m) => {
            m -= key
            if(m.isEmpty)
              index -= keyPart
          }
          case None => ()
        }
      }

      def slice[PK2](keyPart: PK2): Map[K, V] = {
        index.get(keyPart.asInstanceOf[PK]) match {
          case Some(x) => x
          case None => Map[K, V]()
        }
      }

      /**
       * Removes all bindings from the map.
       */
      def clear(): Unit = {
        index.clear
      }
    }

    /**
     * Implements a persistent collection with secondary indices sndIdx
     *
     * @param <K> The key type
     * @param <V> The value type
     */
    class K3PersistentCollection[K, V](name: String, elems: Map[K, V], 
        sndIdx: Option[Map[String, Index[K, V]]]) extends K3Collection[K, V] {
      var lastSize = 0

      def fold[Y](init: Y, fn: (K,V) => Y => Y): Y = {
        fold(init, fn.tupled)
      }
      
      def foldLong(init: Long, fn: (K,V) => Long => Long): Long = {
        foldLong(init, fn.tupled)
      }
      
      def map[K2, V2](f: (K,V) => (K2, V2)): K3IntermediateCollection[K2, V2] = {
        map(f.tupled)
      }

      def foreach(f: (K,V) => Unit): Unit = foreach(f.tupled)

      def map[K2, V2](f: Tuple2[K, V] => 
                         Tuple2[K2, V2]): K3IntermediateCollection[K2, V2] = {
		// IMPORTANT: we need the toList here because the map function can
		//            produce the same key multiple times.
		//            Removing it will result in hours of debugging once
		//            you encounter a query that relies on the property.
        new K3IntermediateCollection[K2, V2](elems.toList.map(f))
      }

      def contains(key: K): Boolean =
        elems.contains(key)

      def lookup(key: K): V = elems.get(key) match {
        case None => throw new RuntimeException("lookup of a non-existant key")
        case Some(v) => v
      }
	  
      def lookup(key: K, defVal: V): V = elems.get(key) match {
        case None => defVal
        case Some(v) => v
      }

      /**
       * Updates an element in the collection
       *
       * @param key Key of the element
       * @param value Value of the element
       */
      def updateValue(key: K, value: V): Unit = {
        val keyVal = (key, value)
        elems += keyVal
        sndIdx match {
          case Some(x) => x foreach { case (k, v) => v.update(keyVal) }
          case None => ()
        }
      }

      /**
       * Removes an element from the collection
       *
       * @param key Key of the element to be removed
       */
      def remove(key: K): Unit = {
        elems -= key
        sndIdx match {
          case Some(x) => x foreach { case (k, v) => v.remove(key) }
          case None => ()
        }
      }

      def foreach(f: Tuple2[K, V] => Unit): Unit = elems.foreach(f)

      def slice[KP](keyPart: KP, idx: List[Int]): 
            K3IntermediateCollection[K, V] = {
        val strIdx = idx.foldLeft("")(
            { case (agg, nb) => agg + (if (agg != "") "_" else "") + nb })
        sndIdx match {
          case Some(x) => new K3IntermediateCollection(x.get(strIdx) match {
            case Some(y) => y.slice(keyPart)
            case None => (List[(K,V)]()).toIterable
          })
          case None => throw new IllegalArgumentException
        }
      }

      def filter(f: Tuple2[K, V] => Boolean): 
            K3IntermediateCollection[K, V] = {
        new K3IntermediateCollection(elems.filter(f))
      }

      def groupByAggregate[K2, V2](init: V2, group: Tuple2[K, V] => K2, 
                                   fn: Tuple2[K, V] => V2 => V2):
                                  K3IntermediateCollection[K2, V2] = {
        val groupedCollection = elems.foldLeft(Map[K2, V2]()) {
          case (grps, keyval) =>
            val key = group(keyval)
            val value = grps.get(key) match {
              case Some(v) => fn(keyval)(v)
              case None => fn(keyval)(init)
            }
            grps += ((key, value))
          case _ => throw new RuntimeException("Group By Aggregate failed")
        }
        new K3IntermediateCollection(groupedCollection)
      }

      def fold[Y](init: Y, fn: Tuple2[K, V] => Y => Y): Y = {
        elems.foldLeft(init) { case (y, kv) => fn(kv)(y) }
      }

      def foldLong(init: Long, fn: Tuple2[K, V] => Long => Long): Long = {
        var result = init
        elems.foreach(kv => { result = fn(kv)(result) })
        result
      }

      def flatten[K2, V2](): K3IntermediateCollection[K2, V2] =
        throw new RuntimeException("flatten of non-nested collection")

      def toIterable(): Iterable[Tuple2[K, V]] = elems

      override def toString = {
        elems.foldLeft("") {
          case (str, (k, v)) =>
            str + k + " -> " + v + sys.props("line.separator")
        }
      }

      def toPersistentCollection(): K3PersistentCollection[K, V] = this

      def printSize() = {
        val currSize = elems.size
        println(name + ": " + currSize + "(" + (currSize - lastSize) + ")")
        lastSize = currSize
      }

      /**
       * Removes all bindings from the map.
       */
      def clear(): Unit = {
        elems.clear
        sndIdx match {
          case Some(idx) => idx foreach { case (k, v) => v.clear }
          case None =>
        }
      }
    }



    /**
     * Implementation of an intermediate collection
     *
     * Note: Intermediate collections can have multiple values for the same key
     *
     * @param <K> The key type
     * @param <V> The value type
     */
    class K3IntermediateCollection[K, V](elems: Iterable[Tuple2[K, V]]) 
        extends K3Collection[K, V] {
      def fold[Y](init: Y, fn: (K,V) => Y => Y): Y = {
        fold(init, fn.tupled)
      }
      
      def foldLong(init: Long, fn: (K,V) => Long => Long): Long = {
        foldLong(init, fn.tupled)
      }
      
      def map[K2, V2](f: (K,V) => (K2, V2)): K3IntermediateCollection[K2, V2] = {
        map(f.tupled)
      }

      def map[K2, V2](f: Tuple2[K, V] => Tuple2[K2, V2]):
                     K3IntermediateCollection[K2, V2] =
        new K3IntermediateCollection(elems.map(f))

      def foreach(f: (K,V) => Unit): Unit = foreach(f.tupled)

      def contains(key: K): Boolean = {
        (elems.find { case (k, v) => k == key }) != None
      }

      def lookup(key: K): V = {
        (elems.find { case (k, v) => k == key }) match {
          case None => throw new RuntimeException("lookup of a non-existant key")
          case Some((k, v)) => v
        }
      }
    
    def lookup(key: K, defVal: V): V = {
    (elems.find { case (k, v) => k == key }) match {
      case None => defVal
      case Some((k, v)) => v
    }
    }

      def foreach(f: Tuple2[K, V] => Unit): Unit =
        elems.foreach(f)

      def slice[K2](keyPart: K2, idx: List[Int]): 
                   K3IntermediateCollection[K, V] = {
        val kp = keyPart.asInstanceOf[Product].productIterator.toList
        new K3IntermediateCollection(elems.filter 
            { case (k, v) => 
                (kp zip idx).forall { 
                    case (kp, i) => 
                      kp == k.asInstanceOf[Product].productElement(i) } })
      }

      def filter(f: Tuple2[K, V] => Boolean): 
            K3IntermediateCollection[K, V] = {
        new K3IntermediateCollection(elems.filter(f))
      }

      def groupByAggregate[K2, V2](init: V2, group: Tuple2[K, V] => K2, 
                                   fn: Tuple2[K, V] => V2 => V2):
                                  K3IntermediateCollection[K2, V2] = {
    /*val result = Map[K2, V2]()
        elems.foreach(keyval => {
            val key = group(keyval)
            val value = result.get(key) match {
              case Some(v) => fn(keyval)(v)
              case None => fn(keyval)(init)
            }
            result += ((key, value))
      }
        )
        new K3IntermediateCollection(result)*/        val groupedCollection = elems.foldLeft(Map[K2, V2]()) {
          case (grps, keyval) =>
            val key = group(keyval)
            val value = grps.get(key) match {
              case Some(v) => fn(keyval)(v)
              case None => fn(keyval)(init)
            }
            grps += ((key, value))
          case _ => throw new RuntimeException("Group By Aggregate failed")
        }
        new K3IntermediateCollection(groupedCollection)
      }

      def fold[Y](init: Y, fn: Tuple2[K, V] => Y => Y): Y = {
        elems.foldLeft(init) { case (y, kv) => fn(kv)(y) }
      }

      def foldLong(init: Long, fn: Tuple2[K, V] => Long => Long): Long = {
        var result = init
        elems.foreach(kv => { result = fn(kv)(result) })
        result
      }


      def flatten[K2, V2](): K3IntermediateCollection[K2, V2] = {
        new K3IntermediateCollection(elems.foldLeft(List[Tuple2[K2, V2]]()) {
          (agg, elem) =>
            (agg, elem) match {
              case (agg, ((), v)) => 
                agg ++ v.asInstanceOf[K3Collection[K2, V2]].toIterable
              case _ => throw new IllegalArgumentException(elem.toString)
            }
        })
      }

      def toIterable(): Iterable[Tuple2[K, V]] = elems

      def toPersistentCollection(): K3PersistentCollection[K, V] =
        new K3PersistentCollection[K, V]("from_intermediate", Map() ++ elems, None)
    }
  }
}
