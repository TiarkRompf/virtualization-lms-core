package scala.virtualization.lms
package common

import java.io.PrintWriter
import scala.virtualization.lms.common._
import scala.collection.mutable
import scala.collection.mutable.Set
import scala.reflect.SourceContext
import scala.collection.mutable.Map

trait MapOps extends Base {
  type MapType[K,V]

  def newMapType[K: Manifest, V: Manifest](): Rep[MapType[K, V]]
  def lookupOrDefault[K, V: Manifest](x: Rep[MapType[K, V]], key: Rep[K], defaultVal: Rep[V]): Rep[V]
  def updateValue[K, V](x: Rep[MapType[K, V]], key: Rep[K], value: Rep[V]): Rep[Unit]
}

trait GeneratorOps extends Base with Variables with LiftVariables with IfThenElse with Equal with TupleOps with ListOps with MapOps with ObjectOps with StringOps with HashMapOps with ListBuffer with HashMultiMapOps with SetOps with LiftNumeric with NumericOps with ArrayOps {
  def materializeGenerator[T:Manifest,U:Manifest](gen: Generator[U]): Rep[T]
  def dematerializeGenerator[T:Manifest,U:Manifest](genCon: Rep[T]): Generator[U]

  def materializeTupleGenerator[T:Manifest,U:Manifest,V:Manifest](gen: TupleGenerator[U,V]): Rep[T]
  def dematerializeTupleGenerator[T:Manifest,U:Manifest,V:Manifest](genCon: Rep[T]): TupleGenerator[U,V]

  implicit def generatorToRep[T:Manifest](gen: Generator[T]): Rep[T] = materializeGenerator[T,T](gen)
  //TODO - This implicite should be defined for every collection type
  //implicit def repToGenerator[T:Manifest,U:Manifest](genCon: Rep[T]): Generator[U] = dematerializeGenerator[T,U](genCon)

  implicit def tupledGeneratorToRep[K:Manifest, V:Manifest](gen: TupleGenerator[K,V]): Rep[(K,V)] = materializeTupleGenerator[(K,V),K,V](gen)
  //TODO - This implicite should be defined for every collection type
  //implicit def repToGenerator[T:Manifest,U:Manifest](genCon: Rep[T]): Generator[U] = dematerializeGenerator[T,U](genCon)
  
  abstract class TupleGenerator[K:Manifest, V:Manifest] extends ((Rep[(K,V)] => Rep[Unit]) => Rep[Unit]) with Serializable /*extends Generator[(K,V)]*/ { self =>

    /*override*/ def map[K2:Manifest, V2:Manifest](g: Rep[(K,V)] => Rep[(K2,V2)]) = new TupleGenerator[K2,V2] {
      def apply(f: Rep[(K2,V2)] => Rep[Unit]) = self.apply {
        x:Rep[(K,V)] => f(g(x))
      }
    }

    /*override*/ def filter(p: Rep[(K,V)] => Rep[Boolean]) = new TupleGenerator[K,V] {
      def apply(f: Rep[(K,V)] => Rep[Unit]) = self.apply {
        x:Rep[(K,V)] => if(p(x)) f(x)
      }
    }

    /*override*/ def ++(that: TupleGenerator[K,V]) = new TupleGenerator[K,V] {
      def apply(f: Rep[(K,V)] => Rep[Unit]) = {
        self.apply(f)
        that.apply(f)
      }
    }

    /*override*/ def flatMap[K2:Manifest, V2:Manifest](g: Rep[(K,V)] => TupleGenerator[K2,V2]) = new TupleGenerator[K2,V2] {
      def apply(f: Rep[(K2,V2)] => Rep[Unit]) = self.apply { x:Rep[(K,V)] =>
        val tmp : TupleGenerator[K2,V2] = g(x)
        tmp(f)
      }
    }

    /*override*/ def reduce(h:(Rep[(K,V)],Rep[(K,V)])=>Rep[(K,V)], z:Rep[(K,V)]) = new TupleGenerator[K,V] {
      def apply(f: Rep[(K,V)] => Rep[Unit]) = {
        var best = z;
        self.apply { x:Rep[(K,V)] => if (best==z) best=x; else best=h(best,x) }
        if (best!=z) f(best)
      }
    }

    /*override*/ def flatten[K2:Manifest, V2:Manifest] = flatMap[K2,V2] {
      x:Rep[(K,V)] => dematerializeTupleGenerator[(K,V),K2,V2](x)
    }

    /*override*/ def fold[Y:Manifest](init: Rep[Y], g: Rep[(K,V)] => (Rep[Y] => Rep[Y])): Rep[Y] = {
      var res = init
      self.apply {
        x:Rep[(K,V)] => res = g(x)(res)
      }
      res
    }

    /*override*/ def foldLong(init: Rep[Long], g: Rep[(K,V)] => (Rep[Long] => Rep[Long])): Rep[Long] = {
      var res = init
      self.apply {
        x:Rep[(K,V)] => res = g(x)(res)
      }
      res
    }

    /*override*/ def foreach(g: Rep[(K,V)] => Rep[Unit]) = self.apply {
      x:Rep[(K,V)] => g(x)
    }

    /*override*/ def toList: Rep[List[(K,V)]] = {
      var resList = List[(K,V)]()
      self.apply {
        x:Rep[(K,V)] => resList = x :: resList
      }
      resList
    }

    def groupByAggregate[K2:Manifest, V2:Manifest](init: Rep[V2], group: Rep[(K, V)] => Rep[K2], 
            fn: Rep[(K, V)] => (Rep[V2] => Rep[V2])): Rep[MapType[K2, V2]] = {
      val grps = newMapType[K2,V2]()
      self.apply {
        x:Rep[(K,V)] => {
          val key: Rep[K2] = group(x)
          val value = fn(x)(lookupOrDefault(grps,key,init))
          updateValue(grps,key,value)
        }
      }
      grps
    }

	def groupByMultipleAggregates[K2:Manifest, V2:Manifest](newMapFun: () => Rep[scala.collection.mutable.HashMap[K2, Array[V2]]], numAggs: Rep[Long], group: Rep[(K, V)] => Rep[K2], fn: (Rep[V], Rep[V2]) => Rep[V2]*): Rep[scala.collection.mutable.HashMap[K2, Array[V2]]] = {
      val grps = newMapFun()//HashMap[K2,Array[V2]]()
      self.apply {
        x:Rep[(K,V)] => {
        	val key: Rep[K2] = group(x)
			val aggs = grps.getOrElseUpdate(key, NewArray[V2](numAggs))//fn.length))// lookupOrDefault[K2,Array[V2]](grps,key,init))
			fn.foldLeft(0L) { (cnt,aggfn) => {
	        	val value = aggfn(x._2,aggs(cnt))
				aggs(cnt) = value
				cnt+1
			} }
			unit()
        }
      }
      grps
    }

    def mkString(delimiter: Rep[String] = unit("")): Rep[String] = {
	  var res = string_new(unit(""))
	  self.apply {
		x:Rep[(K,V)] => res = res + infix_ToString(x)//.ToString
		if (delimiter != unit("")) res = res + delimiter
	  }
	  res
	}


    /*def slice[K2: Manifest](kp: Rep[K2], idx: Rep[List[Int]]): TupleGenerator[K, V] = {
      self.filter{
        kv:Rep[(K,V)] => {
          val k = kv._1.asInstanceOf[Rep[(K2,_)]]
          kp == tuple2_get1(k)
        }
      }
    }*/
  }

  // Generator[T] === (T => Unit) => Unit
  abstract class Generator[T:Manifest] extends ((Rep[T] => Rep[Unit]) => Rep[Unit]) with Serializable { self =>

    //Rep[T => U] != Rep[T] => Rep[U]
    def map[U:Manifest](g: Rep[T] => Rep[U]) = new Generator[U] {
      def apply(f: Rep[U] => Rep[Unit]) = self.apply {
        x:Rep[T] => f(g(x))
      }
    }

    def filter(p: Rep[T] => Rep[Boolean]) = new Generator[T] {
      def apply(f: Rep[T] => Rep[Unit]) = self.apply {
        x:Rep[T] => if(p(x)) f(x)
      }
    }

    def ++(that: Generator[T]) = new Generator[T] {
      def apply(f: Rep[T] => Rep[Unit]) = {
        self.apply(f)
        that.apply(f)
      }
    }

    def flatMap[U:Manifest](g: Rep[T] => Generator[U]) = new Generator[U]{
      def apply(f: Rep[U] => Rep[Unit]) = self.apply{ x:Rep[T] =>
        val tmp : Generator[U] = g(x)
        tmp(f)
      }
    }

    def reduce(h:(Rep[T],Rep[T])=>Rep[T], z:Rep[T]) = new Generator[T] {
      def apply(f: Rep[T] => Rep[Unit]) = {
        var best = z;
        self.apply { x:Rep[T] => if (best==z) best=x; else best=h(best,x) }
        if (best!=z) f(best)
      }
    }

    def flatten[U:Manifest] = flatMap[U] {
      x:Rep[T] => dematerializeGenerator[T,U](x)
    }

    def fold[Y:Manifest](init: Rep[Y], g: Rep[T] => (Rep[Y] => Rep[Y])): Rep[Y] = {
      var res = init
      self.apply {
        x:Rep[T] => res = g(x)(res)
      }
      res
    }

	def sum(implicit num: Numeric[T]): Rep[T] = {
      var res = unit(num.zero)
      self.apply {
        x:Rep[T] => res = numeric_plus(x, readVar(res))
      }
      readVar(res)
    }


    def foldLong(init: Rep[Long], g: Rep[T] => (Rep[Long] => Rep[Long])): Rep[Long] = {
      var res = init
      self.apply {
        x:Rep[T] => res = g(x)(res)
      }
      res
    }

    def foreach(g: Rep[T] => Rep[Unit]) = self.apply {
      x:Rep[T] => g(x)
    }

    def toList: Rep[List[T]] = {
      var resList = List[T]()
      self.apply {
        x:Rep[T] => resList = x :: resList
      }
      resList
    }

	  }

  case class EmptyGen[T:Manifest]() extends Generator[T]{
    def apply(f: Rep[T] => Rep[Unit]) = {}
  }

  def emptyGen[A:Manifest](): Generator[A] = EmptyGen[A]

  def elGen[A:Manifest](a: Rep[A]): Generator[A] = new Generator[A]{
    def apply(f: Rep[A] => Rep[Unit]) = {
      f(a)
    }
  }

  def cond[A:Manifest](cond: Rep[Boolean], a: Generator[A], b: Generator[A]) = new Generator[A]{
    def apply(f: Rep[A] => Rep[Unit]) = {
      if(cond) a(f) else b(f)
    }
  }
}

trait GeneratorOpsExp extends GeneratorOps with EffectExp with VariablesExp with IfThenElseExp with EqualExp with TupleOpsExp with ListOpsExp with ObjectOpsExp with StringOpsExp with HashMapOpsExp with ListBufferExp with HashMultiMapOpsExp with SetOpsExp with ArrayOpsExp {

  case class GeneratorContainer[T: Manifest,U:Manifest](gen: Generator[U]) extends Def[T]
  case class TupleGeneratorContainer[T: Manifest,U:Manifest,V:Manifest](gen: TupleGenerator[U,V]) extends Def[T]

  def materializeGenerator[T:Manifest,U:Manifest](gen: Generator[U]): Rep[T] = GeneratorContainer[T,U](gen)
  def dematerializeGenerator[T:Manifest,U:Manifest](genCon: Rep[T]): Generator[U] = {
    findDefinition(genCon.asInstanceOf[Sym[T]]).get.rhs match {
      case Reflect(ReadVar(x), _, _) => x.asInstanceOf[GeneratorContainer[T,U]].gen
      case x => x.asInstanceOf[GeneratorContainer[T,U]].gen
    }
  }
  def materializeTupleGenerator[T:Manifest,U:Manifest,V:Manifest](gen: TupleGenerator[U,V]): Rep[T] = TupleGeneratorContainer[T,U,V](gen)
  def dematerializeTupleGenerator[T:Manifest,U:Manifest,V:Manifest](genCon: Rep[T]): TupleGenerator[U,V] = {
    findDefinition(genCon.asInstanceOf[Sym[T]]).get.rhs match {
      case Reflect(ReadVar(x), _, _) => x.asInstanceOf[TupleGeneratorContainer[T,U,V]].gen
      case x => x.asInstanceOf[TupleGeneratorContainer[T,U,V]].gen
    }
  }
}

trait ScalaGenGeneratorOps extends ScalaGenVariables
  with ScalaGenIfThenElse with ScalaGenEqual with ScalaGenListOps with ScalaGenTupleOps {
  val IR: GeneratorOpsExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    // currently, we shoud explicitly call toList method on a generator, in order to convert it again to list

    case TupleGeneratorContainer(gen) => val genList = gen.toList; emitNode(sym, Def.unapply(genList).get)
    case GeneratorContainer(gen) => 
    case _ => super.emitNode(sym, rhs)
  }

}

trait CGenGeneratorOps extends CGenVariables
  with CGenIfThenElse with CLikeGenEqual with CLikeGenListOps {
  val IR: GeneratorOpsExp
  import IR._

  /*override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }*/

}
