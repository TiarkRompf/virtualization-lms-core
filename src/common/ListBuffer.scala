package scala.virtualization.lms.common

import scala.collection.mutable
import scala.virtualization.lms.internal._

trait ListBuffer extends Base with Expressions with Variables {
	class ListBufferOps[A:Manifest](x: Rep[mutable.ListBuffer[A]]) {
		def +=(v: Rep[Any]) = listBufferAdd(x, v)
		def remove(v: Rep[Int]) = listBufferRemove(x, v)
        def size() = listBufferSize(x)
        def head() = listBufferHead(x)
        def mkString(v: Rep[String]) = listBufferMkString(x,v)
        def foreach[B: Manifest](f: Rep[A] => Rep[B]) = listBufferforeach[A,B](x, f)
	}

	implicit def listBuffer2listBufferOps[A:Manifest](x: Rep[mutable.ListBuffer[A]]) = new ListBufferOps(x)
	implicit def varlistBuffer2listBufferOps[A:Manifest](x: Var[mutable.ListBuffer[A]]) = new ListBufferOps(readVar(x))

	def newListBuffer[A:Manifest](x: Rep[String]): Rep[mutable.ListBuffer[A]]
	def listBufferAdd[A:Manifest](x: Rep[mutable.ListBuffer[A]], v: Rep[Any]): Rep[Unit]
	def listBufferRemove[A:Manifest](x: Rep[mutable.ListBuffer[A]], v: Rep[Int]): Rep[A]
	def listBufferSize[A:Manifest](x: Rep[mutable.ListBuffer[A]]): Rep[Int]
	def listBufferHead[A:Manifest](x: Rep[mutable.ListBuffer[A]]): Rep[Any]
    def listBufferMkString[A:Manifest](x: Rep[mutable.ListBuffer[A]], y: Rep[String]): Rep[String]
    def listBufferforeach[A:Manifest, B:Manifest](x: Rep[mutable.ListBuffer[A]], f: Rep[A] => Rep[B]): Rep[Unit]
}

trait ListBufferExp extends ListBuffer with BaseExp with EffectExp with Effects {
	case class ListBufferAdd[A:Manifest](x: Rep[mutable.ListBuffer[A]], v: Rep[Any]) extends Def[Unit]
	case class ListBufferRemove[A:Manifest](x: Rep[mutable.ListBuffer[A]], v: Rep[Int]) extends Def[A]
	case class ListBufferSize[A:Manifest](x: Rep[mutable.ListBuffer[A]]) extends Def[Int]
	case class ListBufferHead[A:Manifest](x: Rep[mutable.ListBuffer[A]]) extends Def[Any]
	case class NewListBuffer[A:Manifest](x: Rep[String]) extends Def[mutable.ListBuffer[A]]
    case class ListmkString[A:Manifest](x: Rep[mutable.ListBuffer[A]], y: Rep[String]) extends Def[String]
    case class ListBufferForeach[A:Manifest, B:Manifest](l: Exp[mutable.ListBuffer[A]], x: Sym[A], block: Block[B]) extends Def[Unit]

	def newListBuffer[A:Manifest](x: Rep[String]) = reflectEffect(NewListBuffer[A](x))
	def listBufferAdd[A:Manifest](x: Rep[mutable.ListBuffer[A]], v: Rep[Any]): Rep[Unit] = reflectEffect(ListBufferAdd(x,v))
	def listBufferRemove[A:Manifest](x: Rep[mutable.ListBuffer[A]], v: Rep[Int]): Rep[A] = reflectEffect(ListBufferRemove(x,v))
	def listBufferSize[A:Manifest](x: Rep[mutable.ListBuffer[A]]) = reflectEffect(ListBufferSize(x))
	def listBufferHead[A:Manifest](x: Rep[mutable.ListBuffer[A]]) = reflectEffect(ListBufferHead(x))
    def listBufferMkString[A:Manifest](x: Rep[mutable.ListBuffer[A]], y: Rep[String]) = ListmkString(x,y)
    def listBufferforeach[A:Manifest, B:Manifest](x: Rep[mutable.ListBuffer[A]], f: Rep[A] => Rep[B]) = {
        val a = fresh[A]
        val b = reifyEffects(f(a))
        reflectEffect(ListBufferForeach(x, a, b), summarizeEffects(b).star)
    }

    override def syms(e: Any): List[Sym[Any]] = e match {
        case ListBufferForeach(a, x, body) => syms(a):::syms(body)
        case _ => super.syms(e)
    }

    override def boundSyms(e: Any): List[Sym[Any]] = e match {
        case ListBufferForeach(a, x, body) => x :: effectSyms(body)
        case _ => super.boundSyms(e)
    }

    override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
        case ListBufferForeach(a, x, body) => freqNormal(a):::freqHot(body)
        case _ => super.symsFreq(e)
    }
}

trait ScalaGenListBuffer extends ScalaGenBase with ScalaNestedCodegen {
	val IR: ListBufferExp
	import IR._
  
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case NewListBuffer(x) =>
			emitValDef(sym, "new scala.collection.mutable.ListBuffer[" + quote(x).replaceAll("\"","") + "]")
		case ListBufferAdd(x,v) => emitValDef(sym, quote(x) + " += " + quote(v))
		case ListBufferRemove(x,v) => emitValDef(sym, quote(x) + ".remove(" + quote(v) + ")")
		case ListBufferSize(x) => emitValDef(sym, quote(x) + ".size")
		case ListBufferHead(x) => emitValDef(sym, quote(x) + ".head")
        case ListmkString(x,v) => emitValDef(sym, quote(x) + ".mkString(" + quote(v) + ")")
        case ListBufferForeach(l,x,blk) => {
            stream.println("val " + quote(sym) + " = " + quote(l) + ".foreach { "+ quote(x) + " => ")
            emitBlock(blk)
            stream.println(quote(getBlockResult(blk)))
            stream.println("}")
        }
		case _ => super.emitNode(sym, rhs)
  	}
}
