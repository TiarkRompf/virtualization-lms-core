package dbtoptimizer.lifters

import org.dbtoaster.dbtoasterlib.K3Collection._
import scala.virtualization.lms.common._

import scala.reflect.SourceContext

trait ListContainerOps extends K3PersistentCollectionOps{
  object ListContainer {
    def apply[K:Manifest,V:Manifest](vals: Rep[List[(K,V)]])(implicit pos: SourceContext) = lclToGenerator[K,V](vals)
  }

  def lclToGenerator[K:Manifest,V:Manifest](x: Rep[List[(K, V)]]) : TupleGenerator[K,V] = new TupleGenerator[K,V]{
    def apply(f: Rep[(K,V)] => Rep[Unit]) = {
      list_foreach(x, (y:Rep[(K,V)]) => f(y))
    }
  }
  def lck3ToGenerator[K:Manifest,V:Manifest](x: Rep[K3PersistentCollection[K, V]]) : TupleGenerator[K,V] = new TupleGenerator[K,V]{
    def apply(f: Rep[(K,V)] => Rep[Unit]) = {
      k3Foreach(x, (y:Rep[(K,V)]) => f(y))
    }
  }
  def sngToGenerator[K:Manifest,V:Manifest](x: Rep[(K, V)]) : TupleGenerator[K,V] = new TupleGenerator[K,V]{
    def apply(f: Rep[(K,V)] => Rep[Unit]) = {
      f(x)
    }
  }
  def newK3IntermediateCollection[K: Manifest, V: Manifest](elems: Rep[List[Tuple2[K,V]]]) = lclToGenerator(elems)
  def newSingletonK3IntermediateCollection[K: Manifest, V: Manifest](elem: Rep[Tuple2[K,V]]) = sngToGenerator(elem)
  def newK3PCK3IntermediateCollection[K: Manifest, V: Manifest](elems: Rep[K3PersistentCollection[K,V]]) = lck3ToGenerator(elems)
}

trait ListContainerExp extends K3PersistentCollectionExp

trait ScalaGenListContainer extends  ScalaGenK3PersistentCollection{
  val IR: ListContainerExp
  import IR._

  /*override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case _ => super.emitNode(sym, rhs)
  }*/
}