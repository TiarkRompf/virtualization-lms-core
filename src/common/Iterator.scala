package scala.virtualization.lms.common

import scala.virtualization.lms.internal._

trait Iterator extends Base with Variables {
	class IteratorOps[A: Manifest](x: Rep[scala.Iterator[A]]) {
		def hasNext() = iteratorHasNext(x) 
		def next() = iteratorNext(x)
        def isEmpty() = iteratorIsEmpty(x)
		def size() = iteratorSize(x) // Usefull for debugging but consumes all elements
        def head() = iteratorHead(x)
	}

	implicit def Iter2IteratorOps[A: Manifest](x: Rep[scala.Iterator[A]]) = new IteratorOps[A](x)
	implicit def varIterToIteratorOps[A: Manifest](x: Var[scala.Iterator[A]]) = new IteratorOps[A](readVar(x))
	implicit def BufIter2IteratorOps[A: Manifest](x: Rep[scala.BufferedIterator[A]]) = new IteratorOps[A](x)
	implicit def varBufIterToIteratorOps[A: Manifest](x: Var[scala.BufferedIterator[A]]) = new IteratorOps[A](readVar(x))

	def iteratorHasNext[A: Manifest](x: Rep[scala.Iterator[A]]): Rep[Boolean]
	def iteratorNext[A: Manifest](x: Rep[scala.Iterator[A]]): Rep[A]
    def iteratorIsEmpty[A: Manifest](x: Rep[scala.Iterator[A]]): Rep[Boolean]
	def iteratorSize[A: Manifest](x: Rep[scala.Iterator[A]]): Rep[Int]
    def iteratorHead[A: Manifest](x: Rep[scala.Iterator[A]]): Rep[A]
}

trait IteratorExp extends Iterator with BaseExp with Effects /*with VariablesExp*/ {
	case class IteratorHasNext[A: Manifest](x: Rep[scala.Iterator[A]]) extends Def[Boolean]
	case class IteratorNext[A: Manifest](x: Rep[scala.Iterator[A]]) extends Def[A]
    case class IteratorIsEmpty[A: Manifest](x: Rep[scala.Iterator[A]]) extends Def[Boolean]
	case class IteratorSize[A: Manifest](x: Rep[scala.Iterator[A]]) extends Def[Int]
    case class IteratorHead[A: Manifest](x: Rep[scala.Iterator[A]]) extends Def[A]

	def iteratorHasNext[A: Manifest](x: Rep[scala.Iterator[A]]) = reflectEffect(IteratorHasNext[A](x))
	def iteratorNext[A: Manifest](x: Rep[scala.Iterator[A]]) = reflectEffect(IteratorNext[A](x)) 
    def iteratorIsEmpty[A: Manifest](x: Rep[scala.Iterator[A]]) = reflectEffect(IteratorIsEmpty[A](x))
	def iteratorSize[A: Manifest](x: Rep[scala.Iterator[A]]) = reflectEffect(IteratorSize[A](x))
    def iteratorHead[A: Manifest](x: Rep[scala.Iterator[A]]) = reflectEffect(IteratorHead[A](x))
}

trait ScalaGenIterator extends ScalaGenBase {
	val IR: IteratorExp
	import IR._
 
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case IteratorHasNext(x) => 
			emitValDef(sym, quote(x) + ".hasNext")
		case IteratorNext(x) =>
			emitValDef(sym, quote(x) + ".next")
        case IteratorIsEmpty(x) =>
            emitValDef(sym, quote(x) + ".isEmpty")
		case IteratorSize(x) =>
			emitValDef(sym, quote(x) + ".length")
        // Note that this will throw an error at runtime if x is Iterator instead of BufferedIterator
        case IteratorHead(x) =>
            emitValDef(sym, quote(x) + ".head")
		case _ => super.emitNode(sym, rhs)
	}
}
