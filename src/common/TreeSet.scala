package scala.virtualization.lms.common

import scala.collection.mutable
import scala.virtualization.lms.internal._

trait TreeSet extends Base with Variables {

	class TreeSetOps[M: Manifest](x: Rep[mutable.TreeSet[M]]) {
		def +=(y: Rep[M]) = treeSetAdd[M](x,y)
        def -=(y: Rep[M]) = treeSetRemove[M](x,y)
		def head() = treeSetHead[M](x)
	    def size() = treeSetSize[M](x)
	}
	implicit def TreeSet2TreeSetOps[M: Manifest](x: Rep[mutable.TreeSet[M]]) = new TreeSetOps(x)
	implicit def varTreeSet2TreeSetOps[M: Manifest](x: Var[mutable.TreeSet[M]]) = new TreeSetOps(readVar(x))

	def newTreeSet[M: Manifest](x: (Rep[M],Rep[M])=>Rep[Int], sType: String = ""): Rep[mutable.TreeSet[M]]
	def treeSetAdd[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]): Rep[Unit]
	def treeSetRemove[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]): Rep[Unit]
	def treeSetHead[M: Manifest](x: Rep[mutable.TreeSet[M]]): Rep[M]
	def treeSetSize[M: Manifest](x: Rep[mutable.TreeSet[M]]): Rep[Int]
}

trait TreeSetExp extends TreeSet with BaseExp with Effects {
	case class NewTreeSet[M: Manifest](x: Block[Int], sType: String = "", o1: Sym[M], o2: Sym[M]) extends Def[mutable.TreeSet[M]] {
        val m = manifest[M]
    }
	case class TreeSetAdd[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]) extends Def[Unit]
	case class TreeSetRemove[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]) extends Def[Unit]
	case class TreeSetHead[M: Manifest](x: Rep[mutable.TreeSet[M]]) extends Def[M]
	case class TreeSetSize[M: Manifest](x: Rep[mutable.TreeSet[M]]) extends Def[Int]

	def newTreeSet[M: Manifest](x: (Exp[M],Exp[M])=>Exp[Int], sType: String = "") = {
        val o1 = fresh[M]
        val o2 = fresh[M]
        val f = reifyEffects(x(o1,o2))
        reflectEffect(NewTreeSet[M](f, sType, o1, o2))
    }
	def treeSetAdd[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]): Rep[Unit] = reflectEffect(TreeSetAdd[M](x,s))
	def treeSetRemove[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]): Rep[Unit] = reflectEffect(TreeSetRemove[M](x,s))
	def treeSetHead[M: Manifest](x: Rep[mutable.TreeSet[M]]): Rep[M] = reflectEffect(TreeSetHead[M](x))
	def treeSetSize[M: Manifest](x: Rep[mutable.TreeSet[M]]) = reflectEffect(TreeSetSize[M](x))

    override def boundSyms(e: Any): List[Sym[Any]] = e match {
        case NewTreeSet(x,sType,o1,o2) => effectSyms(x) ::: effectSyms(o1) ::: effectSyms(o2)
        case _ => super.boundSyms(e)
    }
}

trait ScalaGenTreeSet extends ScalaGenBase with GenericNestedCodegen {
	val IR: TreeSetExp
	import IR._
  
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case t@NewTreeSet(x,sType,o1,o2) => {
            val treeType = if (sType != "") sType else remap(t.m)
			emitValDef(sym, "new scala.collection.mutable.TreeSet[" + treeType + "]()(")
            stream.println("new Ordering[" + treeType + "] {")
            stream.println(" def compare(" + quote(o1) + ": " + treeType + ", " + quote(o2) +": " + treeType + ") = { ")
            emitBlock(x)
            stream.println(quote(getBlockResult(x)))
            stream.println("}})")
		}
		case TreeSetAdd(x,s) => emitValDef(sym, quote(x) + "+=" + quote(s))
		case TreeSetRemove(x,s) => emitValDef(sym, quote(x) + "-=" + quote(s))
		case TreeSetHead(x) => emitValDef(sym, quote(x) + ".head")
		case TreeSetSize(x) => emitValDef(sym, quote(x) + ".size")
		case _ => super.emitNode(sym, rhs)
  	}
}
