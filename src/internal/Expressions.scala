package scala.lms
package internal

import scala.reflect.SourceContext
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.{ListBuffer,ArrayBuffer}
import java.lang.{StackTraceElement,Thread}


/**
 * The Expressions trait houses common AST nodes. It also manages a list of encountered Definitions which
 * allows for common sub-expression elimination (CSE).  
 * 
 * @since 0.1
 */
trait Expressions extends Utils {

  abstract class Exp[+T:Manifest] { // constants/symbols (atomic)
    var tp: Manifest[T @uncheckedVariance] = manifest[T] //invariant position! but hey...
    def pos: List[SourceContext] = Nil
    var emitted = false;
  }

  case class Const[+T:Manifest](x: T) extends Exp[T] {
    /**
    * equals implementation in Const can not simply rely on default
    * implementation for a case class, because we should check the 
    * type of Const for equality test.
    * Otherwise, we might end-up generating code with wrong typing,
    * specially upon CSE.
    *
    * For example, have a look at test1-arith/TestConstCSE:
    * 
    * trait Prog extends ScalaOpsPkg {
    *   def test1(test_param: Rep[Boolean], acc: Rep[Long]): Rep[Long] = {
    *     val dblVal = if(test_param) unit(1.0) else unit(0.0)
    *     val lngVal = if(test_param) unit(1L) else unit(0L)
    *     auxMethod(acc + lngVal, dblVal)
    *   }
    *
    *   def auxMethod(val1: Rep[Long], val2: Rep[Double]): Rep[Long] = {
    *     val1 + unit(133L) + rep_asinstanceof[Double, Long](val2,manifest[Double],manifest[Long])
    *   }
    * }
    *
    * That would generate a code containing a compile error:
    * 
    *       class test1 extends ((Boolean, Long)=>(Long)) {
    *         def apply(x0:Boolean, x1:Long): Long = {
    *           val x2 = if (x0) {
    *             1.0
    *           } else {
    *             0.0
    *           }
    *           val x3 = x1 + x2
    *           val x4 = x3 + 133L
    *           val x5 = x2.asInstanceOf[Long]
    *           val x6 = x4 + x5
    *           x6
    *         }
    *       }
    *
    *       <stdin>:15: error: type mismatch;
    *        found   : Double
    *        required: Long
    *       x6
    *       ^
    *       one error found
    *       compilation: had errors
    *
    * But, by introducing this new implementation for equals, the
    * correct code will be generated:
    *
    *       class test1 extends ((Boolean, Long)=>(Long)) {
    *         def apply(x0:Boolean, x1:Long): Long = {
    *           val x3 = if (x0) {
    *             1L
    *           } else {
    *             0L
    *           }
    *           val x4 = x1 + x3
    *           val x5 = x4 + 133L
    *           val x2 = if (x0) {
    *             1.0
    *           } else {
    *             0.0
    *           }
    *           val x6 = x2.asInstanceOf[Long]
    *           val x7 = x5 + x6
    *           x7
    *         }
    *       }
    *
    *       compilation: ok
    */
    override def equals(that: Any) = that match {
      case c@Const(y) => if(y == x) {
        val thisTp = tp
        //val thatTp = c.tp
        if (Const.isNumeric[T](thisTp) /*&& isNumeric(thatTp)*/)
          thisTp == c.tp //thatTp
        else
          true
      } else false
      case _ => false 
    }
  }

  object Const {
    val doubleManifest: Manifest[Double] = manifest[Double]
    val floatManifest: Manifest[Float] = manifest[Float]
    val longManifest: Manifest[Long] = manifest[Long]
    val intManifest: Manifest[Int] = manifest[Int]
    val shortManifest: Manifest[Short] = manifest[Short]
    val byteManifest: Manifest[Byte] = manifest[Byte]

    def isNumeric[T:Manifest](m: Manifest[T]) = m == doubleManifest ||
                                                m == floatManifest ||
                                                m == longManifest ||
                                                m == intManifest ||
                                                m == shortManifest ||
                                                m == byteManifest
  }

  case class Sym[+T:Manifest](val id: Int) extends Exp[T] {
    val attributes: scala.collection.mutable.Map[Any,Any] = scala.collection.mutable.ListMap.empty
    var sourceInfo = Thread.currentThread.getStackTrace // will go away
    var sourceContexts: List[SourceContext] = Nil
    override def pos = sourceContexts
    def withPos(pos: List[SourceContext]) = { sourceContexts :::= pos; this }
  }

  case class Variable[+T](val e: Exp[Variable[T]]) {
  } // TODO: decide whether it should stay here ... FIXME: should be invariant

  var nVars = 0
  def fresh[T:Manifest]: Sym[T] = Sym[T] { 
    nVars += 1;  
    //if (nVars%1000 == 0) println("nVars="+nVars);  
    nVars - 1 
  }
  def fresh[T:Manifest](id: Int): Sym[T] = Sym[T] { id } 

  def fresh[T:Manifest](pos: List[SourceContext]): Sym[T] = fresh[T].withPos(pos)

  def quotePos(e: Exp[Any]): String = e.pos match {
    case Nil => "<unknown>"
    case cs => 
      def all(cs: SourceContext): List[SourceContext] = cs.parent match {
        case None => List(cs)
        case Some(p) => cs::all(p)
      }
    cs.map(c => all(c).reverse.map(c => c.fileName.split("/").last + ":" + c.line).mkString("//")).mkString(";")
  }

/*
  def fresh[T:Manifest] = {
    val (name, id, nameId) = nextName("x")
    val sym = Sym[T](id)
    sym.name = name
    sym.nameId = nameId
    sym
  }

  def fresh[T:Manifest](d: Def[T], ctx: Option[SourceContext]) = {
    def enclosingNamedContext(sc: SourceContext): Option[SourceContext] = sc.bindings match {
      case (null, _) :: _ =>
        if (!sc.parent.isEmpty) enclosingNamedContext(sc.parent.get)
        else None
      case (name, line) :: _ =>
        Some(sc)
    }

    // create base name from source context
    val (basename, line, srcCtx) = if (!ctx.isEmpty) {
      enclosingNamedContext(ctx.get) match {
        case None =>
          // no enclosing context has variable assignment
          var outermost = ctx.get
          while (!outermost.parent.isEmpty) {
            outermost = outermost.parent.get
          }
          ("x", 0, Some(outermost))
        case Some(sc) => sc.bindings match {
          case (n, l) :: _ =>
            (n, l, Some(sc))
        }
      }
    } else ("x", 0, None)
    val (name, id, nameId) = nextName(basename)
    val sym = Sym[T](id)
    sym.name = name
    sym.nameId = nameId
    sym.sourceContext = srcCtx
    sym
  }
*/

  abstract class Def[+T] { // operations (composite)
    override final lazy val hashCode = scala.runtime.ScalaRunTime._hashCode(this.asInstanceOf[Product])
  }

  abstract class Stm // statement (links syms and definitions)
  
  def infix_lhs(stm: Stm): List[Sym[Any]] = stm match {
    case TP(sym, rhs) => sym::Nil
  }
  
  def infix_rhs(stm: Stm): Any = stm match { // clients use syms(e.rhs), boundSyms(e.rhs) etc.
    case TP(sym, rhs) => rhs
  }

  def infix_defines[A](stm: Stm, sym: Sym[A]): Option[Def[A]] = stm match {
    case TP(`sym`, rhs: Def[A]) => Some(rhs)
    case _ => None
  }

  def infix_defines[A](stm: Stm, rhs: Def[A]): Option[Sym[A]] = stm match {
    case TP(sym: Sym[A], `rhs`) => Some(sym)
    case _ => None
  }
  
  case class TP[+T](sym: Sym[T], rhs: Def[T]) extends Stm

  // graph construction state
  
  val globalDefs: ListBuffer[Stm] = new ListBuffer()
  //var globalDefs: List[Stm] = Nil
  //var localDefs: List[Stm] = Nil
  var globalDefsCache: Map[Sym[Any],Stm] = Map.empty

  def reifySubGraph[T](b: =>T): (T, List[Stm]) = ??? /*{
    val saveLocal = localDefs
    val saveGlobal = globalDefs
    val saveGlobalCache = globalDefsCache
    localDefs = Nil
    val r = b
    val defs = localDefs
    localDefs = saveLocal
    globalDefs = saveGlobal
    globalDefsCache = saveGlobalCache
    (r, defs)
  }*/

  def reflectSubGraph(ds: List[Stm]): Unit = ??? /*{
    val lhs = ds.flatMap(_.lhs)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    val existing = lhs flatMap (globalDefsCache get _)//globalDefs filter (_.lhs exists (lhs contains _))
    assert(existing.isEmpty, "already defined: " + existing + " for " + ds)
    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
    for (stm <- ds; s <- stm.lhs) {      
      globalDefsCache += (s->stm)
    }
  }*/

  def findDefinition[T](s: Sym[T]): Option[Stm] =
    globalDefsCache.get(s)
    //globalDefs.find(x => x.defines(s).nonEmpty)

  def findDefinition[T](d: Def[T]): Option[Stm] =
    ??? //globalDefs.find(x => x.defines(d).nonEmpty)

  def findOrCreateDefinition[T:Manifest](d: Def[T], pos: List[SourceContext]): Stm =
    //findDefinition[T](d) map { x => x.defines(d).foreach(_.withPos(pos)); x } getOrElse {
      createDefinition(fresh[T](pos), d)
    //}

  def findOrCreateDefinitionExp[T:Manifest](d: Def[T], pos: List[SourceContext]): Exp[T] =
    findOrCreateDefinition(d, pos).defines(d).get

  def createDefinition[T](s: Sym[T], d: Def[T]): Stm = {
    val f = TP(s, d)
    //reflectSubGraph(List(f))
    globalDefs += f
    globalDefsCache += (s->f)
    f
  }
  

  protected implicit def toAtom[T:Manifest](d: Def[T])(implicit pos: SourceContext): Exp[T] = {
    findOrCreateDefinitionExp(d, List(pos)) // TBD: return Const(()) if type is Unit??
  }

  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match { // really need to test for sym?
      case s @ Sym(_) =>
        findDefinition(s).flatMap(_.defines(s))
      case _ =>
        None
    }
  }


  // dependencies

  // regular data (and effect) dependencies
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product => 
      //return p.productIterator.toList.flatMap(syms(_))
      /* performance hotspot */
      val iter = p.productIterator
      val out = new ListBuffer[Sym[Any]]
      while (iter.hasNext) {
        val e = iter.next()
        out ++= syms(e)
      }
      out.result
    case _ => Nil
  }

  // symbols which are bound in a definition
  def boundSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(boundSyms(_))
    case p: Product => p.productIterator.toList.flatMap(boundSyms(_))
    case _ => Nil
  }

  // symbols which are bound in a definition, but also defined elsewhere
  def tunnelSyms(e: Any): List[Sym[Any]] = e match {
    case ss: Iterable[Any] => ss.toList.flatMap(tunnelSyms(_))
    case p: Product => p.productIterator.toList.flatMap(tunnelSyms(_))
    case _ => Nil
  }

  // symbols of effectful components of a definition
  def effectSyms(x: Any): List[Sym[Any]] = x match {
    case ss: Iterable[Any] => ss.toList.flatMap(effectSyms(_))
    case p: Product => p.productIterator.toList.flatMap(effectSyms(_))
    case _ => Nil
  }

  // soft dependencies: they are not required but if they occur, 
  // they must be scheduled before
  def softSyms(e: Any): List[Sym[Any]] = e match {
    // empty by default
    //case s: Sym[Any] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(softSyms(_))
    case p: Product => p.productIterator.toList.flatMap(softSyms(_))
    case _ => Nil
  }


  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[Any] => f(s)
    case ss: Iterable[Any] => ss.toList.flatMap(f)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }

  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case ss: Iterable[Any] => ss.toList.flatMap(symsFreq(_))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    //case _ => rsyms(e)(symsFreq)
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))



/*
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[Any] => List((s,1.0))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    case _ => Nil
  }
*/

/*
  def symsShare(e: Any): List[(Sym[Any], Int)] = {
    case s: Sym[Any] => List(s)
    case p: Product => p.productIterator.toList.flatMap(symsShare(_))
    case _ => Nil
  }
*/



  // bookkeeping

  def reset { // used by delite?
    nVars = 0
    globalDefs.clear()
    //localDefs = Nil
    globalDefsCache = Map.empty
  }

}
