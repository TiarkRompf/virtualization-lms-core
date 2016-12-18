package scala.virtualization.lms.common

import scala.collection.mutable
import scala.reflect.SourceContext
import scala.virtualization.lms.internal._

trait TreeSet extends Base with Variables with Pointer {

  	object TreeSet {
    	def apply[M:Manifest](f: (Rep[M],Rep[M]) => Rep[Int])(implicit pos: SourceContext) = 
			newTreeSet[M](f)
	}

	class TreeSetOps[M: Manifest](x: Rep[mutable.TreeSet[M]]) {
		def insert(y: Rep[M]) = treeSetAdd[M](x,y)
        def -=(y: Rep[M]) = treeSetRemove[M](x,y)
		def head() = treeSetHead[M](x)
	    def size() = treeSetSize[M](x)
	}
	implicit def TreeSet2TreeSetOps[M: Manifest](x: Rep[mutable.TreeSet[M]]) = new TreeSetOps(x)
	implicit def varTreeSet2TreeSetOps[M: Manifest](x: Var[mutable.TreeSet[M]]) = new TreeSetOps(readVar(x))

	def newTreeSet[M: Manifest](x: (Rep[M],Rep[M])=>Rep[Int]): Rep[mutable.TreeSet[M]]
	def treeSetAdd[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]): Rep[Unit]
	def treeSetRemove[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]): Rep[Unit]
	def treeSetHead[M: Manifest](x: Rep[mutable.TreeSet[M]]): Rep[M]
	def treeSetSize[M: Manifest](x: Rep[mutable.TreeSet[M]]): Rep[Int]
}

trait TreeSetExp extends TreeSet with BaseExp with Effects with FunctionsExp with PointerExp {
	case class NewTreeSet[M: Manifest](x: Block[Int], o1: Sym[M], o2: Sym[M]) extends Def[mutable.TreeSet[M]] { val m = manifest[M] }
	case class TreeSetAdd[M:Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]) extends Def[Unit] { val m = manifest[M] }
	case class TreeSetRemove[M:Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]) extends Def[Unit] { val m = manifest[M] } 
	case class TreeSetHead[M:Manifest](x: Rep[mutable.TreeSet[M]]) extends Def[M] { val m = manifest[M] }
	case class TreeSetSize[M:Manifest](x: Rep[mutable.TreeSet[M]]) extends Def[Int] { val m = manifest[M] }

	def newTreeSet[M: Manifest](x: (Exp[M],Exp[M])=>Exp[Int]) = {
		val m = manifest[M]
        val o1 = fresh[M]
        val o2 = fresh[M]
        val f = reifyEffects(x(o1,o2))
        reflectEffect(NewTreeSet[M](f, o1, o2), summarizeEffects(f).star)
    }
	def treeSetAdd[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]): Rep[Unit] = reflectEffect(TreeSetAdd[M](x,s))
	def treeSetRemove[M: Manifest](x: Rep[mutable.TreeSet[M]], s: Rep[M]): Rep[Unit] = reflectEffect(TreeSetRemove[M](x,s))
	def treeSetHead[M: Manifest](x: Rep[mutable.TreeSet[M]]): Rep[M] = reflectEffect(TreeSetHead[M](x))
	def treeSetSize[M: Manifest](x: Rep[mutable.TreeSet[M]]) = reflectEffect(TreeSetSize[M](x))

    override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit pos: SourceContext): Exp[A] = {
        (e match {
            case e@NewTreeSet(func,o1,o2) => reflectEffect(NewTreeSet(f(func),o1,o2)(e.m))
            case Reflect(e@TreeSetSize(t), u, es) => reflectMirrored(Reflect(TreeSetSize(f(t))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))   
            case Reflect(e@TreeSetAdd(t,el), u, es) => reflectMirrored(Reflect(TreeSetAdd(f(t),f(el))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))   
            case Reflect(e@TreeSetHead(t), u, es) => reflectMirrored(Reflect(TreeSetHead(f(t))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))   
            case Reflect(e@TreeSetRemove(t,el), u, es) => reflectMirrored(Reflect(TreeSetRemove(f(t),f(el))(e.m), mapOver(f,u), f(es)))(mtype(manifest[A]))   
            case _ => super.mirror(e,f)
        }).asInstanceOf[Exp[A]] // why??
    }

    override def boundSyms(e: Any): List[Sym[Any]] = e match {
        case NewTreeSet(x,o1,o2) => o1::o2::effectSyms(x)
        case _ => super.boundSyms(e)
    }
}

trait ScalaGenTreeSet extends ScalaGenBase with GenericNestedCodegen {
	val IR: TreeSetExp
	import IR._
  
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case t@NewTreeSet(f,o1,o2) => {
            val treeType = remap(t.m)//.replace("Array[","ArrayOf").replace("]","")
			emitValDef(sym, "new scala.collection.mutable.TreeSet[" + treeType + "]()(")
            stream.println("new Ordering[" + treeType + "] {")
            stream.println(" def compare(" + quote(o1) + ": " + treeType + ", " + quote(o2) +": " + treeType + ") = { ")
            emitBlock(f)
            stream.println(quote(getBlockResult(f)))
            stream.println("}})")
		}
		case TreeSetAdd(x,s) => emitValDef(sym, quote(x) + "+=" + quote(s))
		case TreeSetRemove(x,s) => emitValDef(sym, quote(x) + "-=" + quote(s))
		case TreeSetHead(x) => emitValDef(sym, quote(x) + ".head")
		case TreeSetSize(x) => emitValDef(sym, quote(x) + ".size")
		case _ => super.emitNode(sym, rhs)
  	}
}

trait CGenTreeSet extends CGenBase with GenericNestedCodegen with CGenPointer {
	val IR: TreeSetExp
	import IR._
  
    override def remap[A](m: Manifest[A]) = m match {
	    case s if m <:< manifest[mutable.TreeSet[Any]] => "GTree*"
        case _ => super.remap(m)
    }
    
    override def lowerNode[T:Manifest](sym: Sym[T], rhs: Def[T]) = rhs match {
		case NewTreeSet(f,s1,s2) =>
			LIRTraversal(f)
			sym.atPhase(LIRLowering) {
				reflectEffect(NewTreeSet(LIRLowering(f),s1,s2)).asInstanceOf[Exp[T]]
			}
		case _ => super.lowerNode(sym,rhs)
	}
  
	override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
		case t@NewTreeSet(f,s1,s2) => 
		    val ucf = uninlinedFunc2(s2,s1,f)
            emitValDef(sym, "g_tree_new((GCompareFunc)" + quote(ucf) + ");")
		case TreeSetAdd(x,s) => 
			emitValDef(sym, "g_tree_insert(" + quote(x) + "," + quote(s) + "," + quote(s) + ");")
		case TreeSetSize(x) => emitValDef(sym, "g_tree_nnodes(" + quote(x) + ")")
		case h@TreeSetHead(x) => {
            def func[A,B](s1: Exp[A], s2: Exp[A], s3: Exp[B]): Exp[Int] = {
                pointer_assign(s3,s2)
                unit(0)
            }
            val ptr = getPointerManifest(h.m)
            val ucf = uninlinedFunc3(func)(h.m,h.m,ptr.asInstanceOf[Manifest[Any]],manifest[Int])
            emitValDef(sym, "NULL")
            stream.println("g_tree_foreach(" + quote(x) + ", (GTraverseFunc)" + quote(ucf) + ", &" + quote(sym) + ");")
		}
		case TreeSetRemove(x,s) => stream.println("g_tree_remove(" + quote(x) + "," + quote(s) + ");")
		case _ => super.emitNode(sym, rhs)
  	}
}
