package scala.lms
package internal

import scala.reflect.SourceContext
import scala.collection._

trait AbstractTransformer { self => 
  val IR: BaseExp with TransformingExp
  import IR._
  
  protected def t = self.asInstanceOf[Transformer]  // TODO: Probably want to refactor this
  
  val hasContext = false
  
  def reflectBlock[A](xs: Block[A]): Exp[A] = sys.error("reflectBlock requires transformer context")
  def apply[A](x: Exp[A]): Exp[A]
  def apply[A](xs: Block[A]): Block[A]
  def apply[A](xs: List[Exp[A]]): List[Exp[A]] = xs map (e => apply(e))
  def apply[A](xs: Seq[Exp[A]]): Seq[Exp[A]] = xs map (e => apply(e))
  
  // On functions
  def apply[X,A](f: X=>Exp[A]): X=>Exp[A] = (z:X) => apply(f(z))
  def apply[X,Y,A](f: (X,Y)=>Exp[A]): (X,Y)=>Exp[A] = (z1:X,z2:Y) => apply(f(z1,z2))
  def apply[X,Y,Z,A](f: (X,Y,Z)=>Exp[A]): (X,Y,Z)=>Exp[A] = (z1:X,z2:Y,z3:Z) => apply(f(z1,z2,z3))
  def apply[W,X,Y,Z,A](f: (W,X,Y,Z)=>Exp[A]): (W,X,Y,Z)=>Exp[A] = (z1:W,z2:X,z3:Y,z4:Z) => apply(f(z1,z2,z3,z4))
  def apply[V,W,X,Y,Z,A](f: (V,W,X,Y,Z)=>Exp[A]): (V,W,X,Y,Z)=>Exp[A] = (z1:V,z2:W,z3:X,z4:Y,z5:Z) => apply(f(z1,z2,z3,z4,z5))
  
  def onlySyms(xs: List[Sym[_]]): List[Sym[_]] = xs map {apply(_)} collect {case e: Sym[_] => e }
}

trait SubstitutionTransformer extends AbstractTransformer with Traversal { self =>
  val IR: BaseExp with TransformingExp
  import IR._
  // --- State
  var subst = immutable.Map.empty[Exp[Any], Exp[Any]]
  protected var blockSubst = immutable.Map.empty[Block[Any], Block[Any]]
  protected var copyingBlock = false
  
  // --- API
  def withSubstScope[A](extend: (Exp[Any],Exp[Any])*)(block: => A): A = 
    withSubstScope {
      subst ++= extend
      block
    }

  def withSubstScope[A](block: => A): A = {
    val save = subst
    val r = block
    subst = save
    r
  }
  
  def apply[A](xs: Block[A])(implicit ctx: SourceContext): Block[A] = blockSubst.get(xs) match {
    case Some(ys) if !copyingBlock => ys.asInstanceOf[Block[A]]
    case None => transformBlock(xs)
  }
  
  def apply[A](x: Exp[A]): Exp[A] = subst.get(x) match { 
    case Some(y) => y.asInstanceOf[Exp[A]]
    case None => x
  }
  
  // --- Transforming
  def transformBlock[A](block: Block[A])(implicit ctx: SourceContext): Block[A] = {
    implicit val tp = getBlockResult(block).tp
    val block2 = reifyBlock{ reflectBlock(block) }
    if (!copyingBlock) blockSubst += block -> block2
    (block2)
  }
  
  def copyBlock[A](block: Block[A])(implicit ctx: SourceContext): Block[A] = {
    implicit val tp = getBlockResult(block).tp
    val oldCopy = copyingBlock
    copyingBlock = true
    val block2 = reifyBlock{ reflectBlock(block) }
    copyingBlock = oldCopy
    (block2)
  }
  
  override def reflectBlock[A](block: Block[A]): Exp[A] = {
    withSubstScope {
      traverseBlock(block)
      apply(getBlockResult(block))
    }
  }
  
  protected def addSubst(sym: Exp[Any], replace: Exp[Any], op: Op) {
    assert(!subst.contains(sym) || subst(sym) == replace)
    if (sym != replace) subst += (sym -> replace)
  }
  
  // TODO: Add recursive check back?
  override def traverseStm(stm: Stm): Unit = {
    val syms = stm.lhs 
    if ( syms.forall(sym => apply(sym) == sym) ) {
      val syms2 = transformSyms(syms, stm.rhs)
      syms.zip(syms2).foreach{ case(sym,replace) => addSubst(sym,replace,stm.rhs) }
    }
    else {
      cwarn("Already have substitution for symbols: ")
      syms.foreach{sym => if (apply(sym) != sym) cwarn("  " + strDef(sym) ) } 
    }
  }
  
  def transformSyms(syms: List[Sym[Any]], rhs: Op): List[Exp[Any]] 
    = mirrorOp(syms, rhs, self.asInstanceOf[Transformer])
}

trait IterativeTransformer extends SubstitutionTransformer with IterativeTraversal { self =>
  val IR: BaseExp
  import IR._
  // substitutions which should carry over to the next iteration
  var nextSubst = immutable.Map.empty[Exp[Any], Exp[Any]]

  override def hasConverged = runs > 0 && nextSubst.isEmpty
  
  def register[A](x: Exp[A])(y: Exp[A]): Unit = {
    if (nextSubst.contains(x))
      cdbg("Discarding, already have a replacement for " + x)
    else {
      cdbg("Register replacement for " + x)
      nextSubst += (x -> y)
      subst += (x -> y)
    }
  }

  override def runOnce[A:Typ](b: Block[A]): Block[A] = {
    subst = subst ++ nextSubst
    nextSubst = Map.empty
    transformBlock(b)(mpos(b))
  }

  // Create Some replacement for given definition node if required, None if not
  def transformSym[A](s: Sym[A], d: Def[A])(implicit ctx: SourceContext): Option[Exp[Any]]
  // Transfer metadata from original symbol to transformed symbol (can also be done during transformation)
  def transferMetadata(sub: Exp[Any], orig: Exp[Any], d: Def[Any])(implicit ctx: SourceContext): Unit
}

trait TransformingExp extends Effects { self =>
  
  type Transformer = AbstractTransformer { val IR : self.type }
  type SubstTransformer = SubstitutionTransformer { val IR : self.type }
  
  // TODO: Fix the syntax for this method
  def copyMetadata(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) {
    p.data.keys foreach {k => setMetadata(e, p(k))}
  }
  
  final def mapOver(t: Transformer, u: Summary) = {
    u.copy(mayRead = t.onlySyms(u.mayRead), mstRead = t.onlySyms(u.mstRead),
      mayWrite = t.onlySyms(u.mayWrite), mstWrite = t.onlySyms(u.mstWrite))
  }
  
  final def mirrorOp(syms: List[Sym[Any]], rhs: Op, f: Transformer): List[Exp[Any]] = rhs match {
    case d: Def[_] => List( mirror(d, f)(mtype(syms.head.tp), mpos(syms.head.pos)) )
    case fd: FatDef => mirror(fd, f)(mpos(syms.head.pos))
  }
  
  // TODO: Unused?
  def mirrorDef[A:Typ](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Def[A] = e match {
    case Reflect(x, u, es) => Reflect(mirrorDef(x,f), mapOver(f,u), f(es))
    case Reify(x, u, es) => Reify(f(x), mapOver(f,u), f(es))
    case _ => cfatal(s"Don't know how to mirror $e") 
  }
  // TODO: Unused?
  def mirrorFatDef(e: FatDef, f: Transformer)(implicit ctx: SourceContext): FatDef = sys.error(s"Don't know how to mirror $e")
  
  def mirror[A:Typ](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = e match {
    case Reflect(x, u, es) => mirror(x, f) match {
      case s@Def(Reify(_, _, _)) => s
      case s@Def(Reflect(d2, u2, es2)) => 
        val out = reflectMirrored(Reflect(d2, mapOver(f,u) andAlso u2, (f(es) ++ es2).distinct))(mtype(typ[A]), ctx)
        if (out != s) scrubSym(s.asInstanceOf[Sym[Any]])
        (out)
      case s@Def(d2) => 
        val out = reflectMirrored(Reflect(d2, mapOver(f,u), f(es)))(mtype(typ[A]), ctx)    
        if (out != e) scrubSym(e.asInstanceOf[Sym[Any]])
        (out)
      case s: Const[_] => s
      case s if isBound(s) => s
      case _ => cfatal(s"Don't know how to mirror $e")
    }
    case Reify(x, u, es) => toAtom( Reify(f(x), mapOver(f,u), f(es)) )
    case _ => cfatal(s"Don't know how to mirror $e")
  }
  def mirror(e: FatDef, f: Transformer)(implicit ctx: SourceContext): List[Exp[Any]] = cfatal(s"Don't know how to mirror $e")
}