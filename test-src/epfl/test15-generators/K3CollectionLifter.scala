package dbtoptimizer.lifters

import org.dbtoaster.dbtoasterlib.K3Collection._
import scala.collection.mutable.Map
import xml._
import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.{GenericNestedCodegen, NestedGraphTraversal}
import scala.reflect.SourceContext

/**
 * Creating a separate trait for Generator related operations has two benefits:
 * 1- Separation of concerns
 * 2- Giving lower priority to implicit function for converting a collection
 *    to a generator, making it possible to have specific implementation for some
 *    methods in generator and lifting them if it is needed. slice method is a
 *    good example for this required behaviour.
 */
trait K3PersistentCollectionGenerator extends GeneratorOps {
  type MapType[K,V] = K3PersistentCollection[K,V]

  implicit def k3PersistentCollection2Generator[K:Manifest,V:Manifest](x: Rep[K3PersistentCollection[K, V]]): TupleGenerator[K,V] = k3ToGenerator(x)

  def newK3PersistentCollection[K: Manifest, V: Manifest](name: Rep[String], elems: Rep[Map[K, V]], sndIdx: Rep[Option[Map[String, Index[K, V]]]]): Rep[K3PersistentCollection[K, V]]
  def k3Lookup[K, V: Manifest](x: Rep[K3PersistentCollection[K, V]], key: Rep[K]): Rep[V]
  def k3LookupOrDefault[K, V: Manifest](x: Rep[MapType[K, V]], key: Rep[K], defaultVal: Rep[V]): Rep[V]
  def k3UpdateValue[K, V](x: Rep[MapType[K, V]], key: Rep[K], value: Rep[V]): Rep[Unit]
  def k3Foreach[K: Manifest, V: Manifest](x: Rep[K3PersistentCollection[K, V]], fn: => Rep[Tuple2[K, V]] => Rep[Unit]): Rep[Unit]
  def k3ToGenerator[K:Manifest,V:Manifest](x: Rep[K3PersistentCollection[K, V]]) : TupleGenerator[K,V] = new TupleGenerator[K,V]{
    def apply(f: Rep[(K,V)] => Rep[Unit]) = {
      k3Foreach(x, (y:Rep[(K,V)]) => f(y))
    }
  }
}

/**
 * Lifter Classes for K3PersistentCollection
 */
trait K3PersistentCollectionOps extends Variables with K3PersistentCollectionGenerator{

  class K3PersistentCollectionOpsCls[K: Manifest, V: Manifest](x: Rep[K3PersistentCollection[K, V]]) {
    def contains(k: Rep[K]) = k3Contains[K, V](x, k)
    def lookup(k: Rep[K]) = k3Lookup[K, V](x, k)
    def lookupOrDefault(k: Rep[K], defaultVal: Rep[V]) = k3LookupOrDefault[K, V](x, k, defaultVal)
    def updateValue(k: Rep[K], v: Rep[V]) = k3UpdateValue[K, V](x, k, v)
    def remove(k: Rep[K]) = k3Remove[K, V](x, k)
    def slice[K2:Manifest](kp: Rep[K2], idx: Rep[List[Int]]) = k3Slice[K, V, K2](x, kp, idx)
    def clear = k3Clear[K,V](x)
    def mutable: Rep[K3PersistentCollection[K, V]] = k3Mutable[K,V](x)
    //def foreach(fn: Rep[Tuple2[K, V]] => Rep[Unit]): Rep[Unit] = k3Foreach[K, V](x, fn)
  }

  implicit def k3PersistentCollection2K3PersistentCollectionOpsCls[K: Manifest, V: Manifest](x: Rep[K3PersistentCollection[K, V]]): K3PersistentCollectionOpsCls[K, V] = new K3PersistentCollectionOpsCls[K, V](x)

  def k3Contains[K, V](x: Rep[K3PersistentCollection[K, V]], key: Rep[K]): Rep[Boolean]
  def k3Remove[K, V](x: Rep[K3PersistentCollection[K, V]], key: Rep[K]): Rep[Unit]
  def k3Slice[K:Manifest,V:Manifest,K2:Manifest](x: Rep[K3PersistentCollection[K, V]], kp: Rep[K2], idx: Rep[List[Int]]): Rep[K3PersistentCollection[K, V]]
  def k3Clear[K, V](x: Rep[K3PersistentCollection[K, V]]): Rep[Unit]
  def k3Mutable[K: Manifest, V: Manifest](x: Rep[K3PersistentCollection[K, V]]): Rep[K3PersistentCollection[K, V]]

}

trait K3PersistentCollectionExp extends K3PersistentCollectionOps with GeneratorOpsExp with BaseExp with EffectExp with VariablesExp {

  case class NewK3PersistentCollection[K, V](mK: Manifest[K], mV: Manifest[V], name: Exp[String], elems: Exp[Map[K, V]],
    sndIdx: Exp[Option[Map[String, Index[K, V]]]]) extends Def[K3PersistentCollection[K, V]]
  case class K3Contains[K, V](x: Exp[K3PersistentCollection[K, V]], key: Exp[K]) extends Def[Boolean]
  case class K3Lookup[K, V:Manifest](x: Exp[K3PersistentCollection[K, V]], key: Exp[K]) extends Def[V] {
    val mV = manifest[V]
  }
  case class K3LookupOrDefault[K, V:Manifest](x: Exp[K3PersistentCollection[K, V]], key: Exp[K], defaultVal: Exp[V]) extends Def[V] {
    val mV = manifest[V]
  }
  case class K3UpdateValue[K, V](x: Exp[K3PersistentCollection[K, V]], key: Exp[K], value: Exp[V]) extends Def[Unit]
  case class K3Remove[K, V](x: Exp[K3PersistentCollection[K, V]], key: Exp[K]) extends Def[Unit]
  case class K3Slice[K:Manifest,V:Manifest,K2:Manifest](x: Exp[K3PersistentCollection[K, V]], kp: Exp[K2], idx: Exp[List[Int]]) extends Def[K3PersistentCollection[K, V]] {
    val mK = manifest[K]
    val mV = manifest[V]
    val mK2 = manifest[K2]
  }
  case class K3Clear[K, V](x: Exp[K3PersistentCollection[K, V]]) extends Def[Unit]
  case class K3Mutable[K, V](x: Exp[K3PersistentCollection[K, V]]) extends Def[K3PersistentCollection[K, V]]
  case class K3Foreach[K, V](x: Exp[K3PersistentCollection[K, V]], v: Sym[(K,V)], block: Block[Unit]) extends Def[Unit]

  def newK3PersistentCollection[K: Manifest, V: Manifest](name: Exp[String], elems: Exp[Map[K, V]],
    sndIdx: Exp[Option[Map[String, Index[K, V]]]]): Exp[K3PersistentCollection[K, V]] = reflectMutable(NewK3PersistentCollection(manifest[K], manifest[V], name, elems, sndIdx));

  def k3Contains[K, V](x: Exp[K3PersistentCollection[K, V]], key: Exp[K]) = K3Contains(x, key)
  def k3Lookup[K, V: Manifest](x: Exp[K3PersistentCollection[K, V]], key: Exp[K]) = K3Lookup(x, key)
  def k3LookupOrDefault[K, V: Manifest](x: Exp[K3PersistentCollection[K, V]], key: Exp[K], defaultVal: Exp[V]) = K3LookupOrDefault(x, key, defaultVal)
  def k3UpdateValue[K, V](x: Exp[K3PersistentCollection[K, V]], key: Exp[K], value: Exp[V]) = reflectWrite(x)(K3UpdateValue(x, key, value));
  
  def newMapType[K: Manifest, V: Manifest]() = newK3PersistentCollection[K,V](unit("NEW_K3_MAP"), unit(Map()), unit(None))
  def lookupOrDefault[K, V: Manifest](x: Exp[K3PersistentCollection[K, V]], key: Exp[K], defaultVal: Exp[V]) = k3LookupOrDefault(x, key, defaultVal)
  def updateValue[K, V](x: Exp[K3PersistentCollection[K, V]], key: Exp[K], value: Exp[V]) = k3UpdateValue(x, key, value)

  def k3Remove[K, V](x: Exp[K3PersistentCollection[K, V]], key: Exp[K]) = reflectWrite(x)(K3Remove(x, key))
  def k3Slice[K:Manifest,V:Manifest,K2:Manifest](x: Exp[K3PersistentCollection[K, V]], kp: Exp[K2], idx: Exp[List[Int]]) = K3Slice[K,V,K2](x,kp,idx)
  def k3Mutable[K: Manifest, V: Manifest](x: Exp[K3PersistentCollection[K, V]]) = reflectMutable(K3Mutable[K,V](x))
  def k3Clear[K, V](x: Exp[K3PersistentCollection[K, V]]): Exp[Unit] = reflectWrite(x)(K3Clear(x))
  def k3Foreach[K: Manifest, V: Manifest](x: Exp[K3PersistentCollection[K, V]], fn: => Exp[Tuple2[K, V]] => Exp[Unit])  = {
    val v = fresh[(K,V)]
    val b = reifyEffects(fn(v))
    reflectEffect(K3Foreach[K,V](x, v, b), summarizeEffects(b).star)
  }
  
  //////////////
  // mirroring
  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = (e match {
    case e@NewK3PersistentCollection(mK, mV, name, elems, sndIdx) => newK3PersistentCollection(f(name), f(elems), f(sndIdx))(e.mK, e.mV)
    case K3Contains(x, key) => k3Contains(f(x), f(key))
    case e@K3Lookup(x, key) => k3Lookup(f(x), f(key))(mtype(e.mV))
    case e@K3LookupOrDefault(x, key, defaultVal) => k3LookupOrDefault(f(x).asInstanceOf[Exp[K3PersistentCollection[Any, A]]], f(key), f(defaultVal))(mtype(e.mV)) //why cast is needed?
    case K3UpdateValue(x, key, value) => k3UpdateValue(f(x), f(key), f(value))
    case K3Remove(x, key) => k3Remove(f(x), f(key))
    case K3Clear(x) => k3Clear(f(x))
    case K3Mutable(x) => k3Mutable(f(x))
    case e@K3Slice(x, kp, idx) => k3Slice(f(x), f(kp), f(idx))(mtype(e.mK),mtype(e.mV),mtype(e.mK2)) 
    case K3Foreach(a, x, body) => K3Foreach(f(a),x,f(body))
    case Reflect(e@NewK3PersistentCollection(mK, mV, name, elems, sndIdx), u, es) => reflectMirrored(Reflect(NewK3PersistentCollection(mtype(mK), mtype(mV), f(name), f(elems), f(sndIdx)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(K3Contains(x, key), u, es) => reflectMirrored(Reflect(K3Contains(f(x), f(key)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@K3Lookup(x, key), u, es) => reflectMirrored(Reflect(K3Lookup(f(x), f(key))(mtype(e.mV)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(e@K3LookupOrDefault(x, key, defaultVal), u, es) => reflectMirrored(Reflect(K3LookupOrDefault(f(x).asInstanceOf[Exp[K3PersistentCollection[Any, A]]], f(key), f(defaultVal))(mtype(e.mV)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(K3UpdateValue(x, key, value), u, es) => reflectMirrored(Reflect(K3UpdateValue(f(x), f(key), f(value)), mapOver(f,u), f(es)))(mtype(manifest[A]))
    case Reflect(K3Remove(x, key), u, es) => reflectMirrored(Reflect(K3Remove(f(x), f(key)), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(K3Clear(x), u, es) => reflectMirrored(Reflect(K3Clear(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(K3Mutable(x), u, es) => reflectMirrored(Reflect(K3Mutable(f(x)), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(e@K3Slice(x, kp, idx), u, es) => reflectMirrored(Reflect(K3Slice(f(x), f(kp), f(idx))(mtype(e.mK),mtype(e.mV),mtype(e.mK2)), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case Reflect(K3Foreach(a, x, body), u, es) => reflectMirrored(Reflect(K3Foreach(f(a), x, f(body)), mapOver(f,u), f(es)))(mtype(manifest[A]))  
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]] // why??
  
  override def syms(e: Any): List[Sym[Any]] = e match {
    case K3Foreach(a, x, body) => syms(a):::syms(body)
    case _ => super.syms(e)
  }

  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case K3Foreach(a, x, body) => x :: effectSyms(body)
    case _ => super.boundSyms(e)
  }

  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case K3Foreach(a, x, body) => freqNormal(a):::freqHot(body)
    case _ => super.symsFreq(e)
  }
}

trait ScalaGenK3PersistentCollection extends ScalaGenBase with GenericNestedCodegen {
  val IR: K3PersistentCollectionExp
  import IR._

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case NewK3PersistentCollection(mK, mV, name, elems, sndIdx) => emitValDef(sym, "new K3PersistentCollection[" + remap(mK) + "," + remap(mV) + "](" + quote(name) + ",new HashMap[" + remap(mK) + "," + remap(mV) + "]," + quote(sndIdx) + ")")
    case K3Contains(x, key) => emitValDef(sym, "" + quote(x) + ".contains(" + quote(key) + ")");
    case K3Lookup(x, key) => emitValDef(sym, "" + quote(x) + ".lookup(" + quote(key) + ",0)");
    case K3LookupOrDefault(x, key, defaultVal) => emitValDef(sym, "" + quote(x) + ".lookup(" + quote(key) + "," + quote(defaultVal) + ")");
    case K3UpdateValue(x, key, value) => emitValDef(sym, "" + quote(x) + ".updateValue(" + quote(key) + "," + quote(value) + ")");
    case K3Remove(x, key) => emitValDef(sym, "" + quote(x) + ".remove(" + quote(key) + ")");
    case K3Clear(x) => emitValDef(sym, "" + quote(x) + ".clear");
    case K3Slice(x, kp, idx) => emitValDef(sym, "" + quote(x) + ".slice(" + quote(kp) + "," + quote(idx) + ")");
    case K3Mutable(x) => emitValDef(sym, "" + quote(x) + " // mutable K3PersistentCollection");
    case K3Foreach(a,x,block) => emitValDef(sym, quote(a) + ".foreach{")    
      stream.println(quote(x) + " => ")
      emitBlock(block)
      stream.println("}")
    case _ => super.emitNode(sym, rhs)
  }
}

trait K3PersistentCollectionExpOpt extends BaseExp with NestedGraphTraversal with K3PersistentCollectionExp with IfThenElseExp with TupleOpsExp { self =>
  val IR: self.type = self
  import IR._

  override def mirrorDef[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Def[A] = (e match {
    case e@NewK3PersistentCollection(mK, mV, name, elems, sndIdx) => newK3PersistentCollection(f(name), f(elems), f(sndIdx))(e.mK, e.mV)
    case K3Contains(x, key) => K3Contains(f(x), f(key))
    case e@K3Lookup(x, key) => K3Lookup(f(x), f(key))(mtype(e.mV))
    case e@K3LookupOrDefault(x, key, defaultVal) => K3LookupOrDefault(f(x).asInstanceOf[Exp[K3PersistentCollection[Any, A]]], f(key), f(defaultVal))(mtype(e.mV)) //why cast is needed?
    case K3UpdateValue(x, key, value) => K3UpdateValue(f(x), f(key), f(value))
    case K3Remove(x, key) => K3Remove(f(x), f(key))
    case K3Clear(x) => K3Clear(f(x))
    case K3Mutable(x) => K3Mutable(f(x))
    case e@K3Slice(x, kp, idx) => K3Slice(f(x), f(kp), f(idx))(mtype(e.mK),mtype(e.mV),mtype(e.mK2)) 
    case K3Foreach(a, x, body) => K3Foreach(f(a),x,f(body))
    case _ => super.mirrorDef(e,f)
  }).asInstanceOf[Def[A]]

  override def __ifThenElse[T:Manifest](cond: Exp[Boolean], thenp: => Exp[T], elsep: => Exp[T])(implicit pos: SourceContext) = {
    val thenpBlk = reifyEffectsHere(thenp)
    val elsepBlk = reifyEffectsHere(elsep)

    def defaultResGen = ifThenElse(cond,thenpBlk,elsepBlk)
    val thenpStmts = getStatementsInBlock(thenpBlk)
    val numThenpStmts = thenpStmts.size
    val numElsepStmts = numberOfStatementsInBlock(elsepBlk)
    if(numElsepStmts == 1) {
      (numThenpStmts == 2) match {
        case true => cond match {
          case condSym:Sym[_] => Def.unapply(condSym) match {
            case Some(condDef: Def[Any]) => condDef match {
              case Reflect(K3Contains(x, key), _ , _ ) => {
                val xAsSym = x.asInstanceOf[IR.Sym[_]]
                thenpStmts.head match {
                  case IR.TP(`xAsSym`, IR.Reflect(IR.K3Mutable(_), _, _)) => thenpStmts.tail.head match {
                    case IR.TP(_, IR.Reflect(IR.K3Lookup(`x`,`key`), u,effects)) => {
                      val optRes: Exp[T] = /*Reflect(*/K3LookupOrDefault(x, key, elsepBlk.res).asInstanceOf[Def[T]]/*,u, effects)*/
                      optRes
                    }
                    case other => defaultResGen
                  }
                  case other => defaultResGen
                }
              }
              case other => defaultResGen
            }
            case other => defaultResGen
          }
          case other => defaultResGen
        }
        case false => (numThenpStmts == 3) match {
          case true => cond match {
            case condSym:Sym[_] => Def.unapply(condSym) match {
              case Some(condDef: Def[Any]) => condDef match {
                case Reflect(K3Contains(x, key), _ , _ ) => {
                  val xAsSym = x.asInstanceOf[IR.Sym[_]]
                  val keyAsSym = key.asInstanceOf[IR.Sym[_]]
                  thenpStmts.head match {
                    case IR.TP(`xAsSym`, IR.Reflect(IR.K3Mutable(_), _, _)) => thenpStmts.tail.head match {
                      case IR.TP(`keyAsSym`, _) => thenpStmts.tail.tail.head match {
                        case IR.TP(_, IR.Reflect(IR.K3Lookup(`x`,`key`), u,effects)) => {
                          val optRes: Exp[T] = /*Reflect(*/K3LookupOrDefault(x, key, elsepBlk.res).asInstanceOf[Def[T]]/*,u, effects)*/
                          optRes
                        }
                        case other => defaultResGen
                      }
                      case other => defaultResGen
                    }
                    case other => defaultResGen
                  }
                }
                case other => defaultResGen
              }
              case other => defaultResGen
            }
            case other => defaultResGen
          }
          case false => (numThenpStmts > 3) match {
            case true => {
              val thenpStmtsReverse = thenpStmts.reverse
              cond match {
                case condSym:Sym[_] => Def.unapply(condSym) match {
                  case Some(condDef: Def[Any]) => condDef match {
                    case Reflect(K3Contains(x, key), _ , _ ) => {
                      val xAsSym = x.asInstanceOf[IR.Sym[_]]
                      thenpStmtsReverse.head match {
                        case IR.TP(_, IR.Reflect(IR.K3Lookup(`x`,`key`), u,effects)) => {
                          val optRes: Exp[T] = /*Reflect(*/K3LookupOrDefault(x, key, elsepBlk.res).asInstanceOf[Def[T]]/*,u, effects)*/
                          optRes
                        }
                        case other => defaultResGen
                      }
                    }
                    case other => defaultResGen
                  }
                  case other => defaultResGen
                }
                case other => defaultResGen
              }
            }
            case false => defaultResGen
          }
        }
      }
    } else {
      defaultResGen
    }
  }

  override def k3Foreach[K: Manifest, V: Manifest](x: Exp[K3PersistentCollection[K, V]], fn: => Exp[Tuple2[K, V]] => Exp[Unit]) = {
    val v = fresh[(K,V)]
    val blk = reifyEffects(fn(v))
    
    def defaultResGen = reflectEffect(K3Foreach[K,V](x, v, blk), summarizeEffects(blk).star)

    val stmtsInBlk = getStatementsInBlock(blk)
    if(stmtsInBlk.size == 3) {
      val xAsSym = x.asInstanceOf[IR.Sym[_]]
      stmtsInBlk.head match {
        case TP(`xAsSym`, IR.Reflect(IR.K3Mutable(_), _, _)) => stmtsInBlk.tail.head match {
          case TP(collKey,Tuple2Access1(`v`)) => stmtsInBlk.tail.tail.head match {
            case TP(_,Reflect(K3Remove(`x`,`collKey`), _, _)) => {
              val optRes: Exp[Unit] = k3Clear(x)
              optRes
            }
            case other => defaultResGen
          }
          case other => defaultResGen
        }
        case other => defaultResGen
      }
    } else if(stmtsInBlk.size == 6) {
      val xAsSym = x.asInstanceOf[IR.Sym[_]]
      stmtsInBlk.head match {
        case TP(`xAsSym`, IR.Reflect(IR.K3Mutable(_), _, _)) => stmtsInBlk.tail.head match {
          case TP(collKey,Tuple2Access1(`v`)) => stmtsInBlk.tail.tail.head match {
            case TP(tup1,Tuple2Access1(`collKey`)) => stmtsInBlk.tail.tail.tail.head match {
              case TP(tup2,Tuple2Access2(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.head match {
                case TP(keyTup,ETuple2(`tup1`,`tup2`)) => stmtsInBlk.tail.tail.tail.tail.tail.head match {
                  case TP(_,Reflect(K3Remove(`x`,`keyTup`), _, _)) => {
                    val optRes: Exp[Unit] = k3Clear(x)
                    optRes
                  }
                  case other => defaultResGen
                }
                case other => defaultResGen
              }
              case other => defaultResGen
            }
            case other => defaultResGen
          }
          case other => defaultResGen
        }
        case other => defaultResGen
      }
    } else if(stmtsInBlk.size == 7) {
      val xAsSym = x.asInstanceOf[IR.Sym[_]]
      stmtsInBlk.head match {
        case TP(`xAsSym`, IR.Reflect(IR.K3Mutable(_), _, _)) => stmtsInBlk.tail.head match {
          case TP(collKey,Tuple2Access1(`v`)) => stmtsInBlk.tail.tail.head match {
            case TP(tup1,Tuple3Access1(`collKey`)) => stmtsInBlk.tail.tail.tail.head match {
              case TP(tup2,Tuple3Access2(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.head match {
                case TP(tup3,Tuple3Access3(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.head match {
                  case TP(keyTup,ETuple3(`tup1`,`tup2`,`tup3`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.head match {
                    case TP(_,Reflect(K3Remove(`x`,`keyTup`), _, _)) => {
                      val optRes: Exp[Unit] = k3Clear(x)
                      optRes
                    }
                    case other => defaultResGen
                  }
                  case other => defaultResGen
                }
                case other => defaultResGen
              }
              case other => defaultResGen
            }
            case other => defaultResGen
          }
          case other => defaultResGen
        }
        case other => defaultResGen
      }
    } else if(stmtsInBlk.size == 8) {
      val xAsSym = x.asInstanceOf[IR.Sym[_]]
      stmtsInBlk.head match {
        case TP(`xAsSym`, IR.Reflect(IR.K3Mutable(_), _, _)) => stmtsInBlk.tail.head match {
          case TP(collKey,Tuple2Access1(`v`)) => stmtsInBlk.tail.tail.head match {
            case TP(tup1,Tuple4Access1(`collKey`)) => stmtsInBlk.tail.tail.tail.head match {
              case TP(tup2,Tuple4Access2(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.head match {
                case TP(tup3,Tuple4Access3(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.head match {
                  case TP(tup4,Tuple4Access4(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.head match {
                    case TP(keyTup,ETuple4(`tup1`,`tup2`,`tup3`,`tup4`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.tail.head match {
                      case TP(_,Reflect(K3Remove(`x`,`keyTup`), _, _)) => {
                        val optRes: Exp[Unit] = k3Clear(x)
                        optRes
                      }
                      case other => defaultResGen
                    }
                    case other => defaultResGen
                  }
                  case other => defaultResGen
                }
                case other => defaultResGen
              }
              case other => defaultResGen
            }
            case other => defaultResGen
          }
          case other => defaultResGen
        }
        case other => defaultResGen
      }
    } else if(stmtsInBlk.size == 9) {
      val xAsSym = x.asInstanceOf[IR.Sym[_]]
      stmtsInBlk.head match {
        case TP(`xAsSym`, IR.Reflect(IR.K3Mutable(_), _, _)) => stmtsInBlk.tail.head match {
          case TP(collKey,Tuple2Access1(`v`)) => stmtsInBlk.tail.tail.head match {
            case TP(tup1,Tuple5Access1(`collKey`)) => stmtsInBlk.tail.tail.tail.head match {
              case TP(tup2,Tuple5Access2(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.head match {
                case TP(tup3,Tuple5Access3(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.head match {
                  case TP(tup4,Tuple5Access4(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.head match {
                    case TP(tup5,Tuple5Access5(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.tail.head match {
                      case TP(keyTup,ETuple5(`tup1`,`tup2`,`tup3`,`tup4`,`tup5`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.tail.tail.head match {
                        case TP(_,Reflect(K3Remove(`x`,`keyTup`), _, _)) => {
                          val optRes: Exp[Unit] = k3Clear(x)
                          optRes
                        }
                        case other => defaultResGen
                      }
                      case other => defaultResGen
                    }
                    case other => defaultResGen
                  }
                  case other => defaultResGen
                }
                case other => defaultResGen
              }
              case other => defaultResGen
            }
            case other => defaultResGen
          }
          case other => defaultResGen
        }
        case other => defaultResGen
      }
    } else if(stmtsInBlk.size == 10) {
      val xAsSym = x.asInstanceOf[IR.Sym[_]]
      stmtsInBlk.head match {
        case TP(`xAsSym`, IR.Reflect(IR.K3Mutable(_), _, _)) => stmtsInBlk.tail.head match {
          case TP(collKey,Tuple2Access1(`v`)) => stmtsInBlk.tail.tail.head match {
            case TP(tup1,Tuple6Access1(`collKey`)) => stmtsInBlk.tail.tail.tail.head match {
              case TP(tup2,Tuple6Access2(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.head match {
                case TP(tup3,Tuple6Access3(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.head match {
                  case TP(tup4,Tuple6Access4(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.head match {
                    case TP(tup5,Tuple6Access5(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.tail.head match {
                      case TP(tup6,Tuple6Access6(`collKey`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.tail.tail.head match {
                        case TP(keyTup,ETuple6(`tup1`,`tup2`,`tup3`,`tup4`,`tup5`,`tup6`)) => stmtsInBlk.tail.tail.tail.tail.tail.tail.tail.tail.tail.head match {
                          case TP(_,Reflect(K3Remove(`x`,`keyTup`), _, _)) => {
                            val optRes: Exp[Unit] = k3Clear(x)
                            optRes
                          }
                          case other => defaultResGen
                        }
                        case other => defaultResGen
                      }
                      case other => defaultResGen
                    }
                    case other => defaultResGen
                  }
                  case other => defaultResGen
                }
                case other => defaultResGen
              }
              case other => defaultResGen
            }
            case other => defaultResGen
          }
          case other => defaultResGen
        }
        case other => defaultResGen
      }
    } else {
      defaultResGen
    }
  }

  def numberOfStatementsInBlock(b: Block[Any]): Int = getStatementsInBlock(b) match {
    case Nil => 1
    case l => l.size
  }

  def getStatementsInBlock(b: Block[Any]): List[IR.Stm] = b.res match {
    case s@Sym(n) => buildScheduleForResult(b.res, false).filter{ x => x match {
        case IR.TP(sym,IR.Reify(_,_,_)) => false
        case _ => true
      }
    }
    case _ => Nil
  }
}
