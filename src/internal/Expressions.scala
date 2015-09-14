package scala.lms
package internal

import scala.reflect.SourceContext
import scala.annotation.unchecked.uncheckedVariance
import scala.collection.mutable.ListBuffer

trait Expressions extends Typs with SymbolMetadata {
  
  // --- Symbols
  abstract class Exp[+T:Typ] {
    val tp: Typ[T @ uncheckedVariance] = typ[T]
    var pos: List[SourceContext] = Nil
    def withPos(ctx: List[SourceContext]) = { pos :::= ctx; this }
   
    // Infix shortcuts for returning a symbol with metadata added 
    private implicit def ctx = mpos(pos)
    def withProps(props: Any) = props match { case EatSome(p: SymbolProperties) => setProps(this, p); this }
    def withData(data: Any) = data match { case EatSome(m: Metadata) => setMetadata(this, m); this }
    def withChild(data: Any) = data match { case EatSome(p: SymbolProperties) => setChild(this, p); this }
    def withField(data: Any, index: String) = data match { case EatSome(p: SymbolProperties) => setField(this, p, index); this }
  }
    
  case class Sym[+T:Typ](id: Int) extends Exp[T]
  case class Const[+T:Typ](x: T) extends Exp[T]
  case class Variable[+T](val e: Exp[Variable[T]]) // FIXME: should be invariant

  // --- Operations (composite)
  abstract class Op
  abstract class Def[+R] extends Op
  abstract class FatDef extends Op {
    def tps: List[Typ[_]]
    def mhs: List[Def[Any]]
  }
  
  abstract class Def1[R:Typ] extends Def[R] { val mR = typ[R] }
  abstract class Def2[A:Typ,R:Typ] extends Def1[R] { val mA = typ[A] }
  abstract class Def3[A:Typ,B:Typ,R:Typ] extends Def2[A,R] { val mB = typ[B] }
  abstract class NumericDef1[A:Typ:Numeric] extends Def1[A] { val nR = implicitly[Numeric[A]] }
  
  // statement (links symbols and definitions)
  sealed abstract class Stm {
    def lhs: List[Sym[Any]]
    def rhs: Op
    def defines[A](l: Sym[A]): Option[Def[A]]
    def defines(r: Op): List[Sym[Any]]
  }
  case class TP[+T](left: Sym[T], right: Def[T]) extends Stm {
    override def lhs = List(left)
    override def rhs = right
    override def defines[A](l: Sym[A]) = if (left == l) Some(right.asInstanceOf[Def[A]]) else None
    override def defines(r: Op) = if (right == r) List(left) else Nil 
  }
  case class TTP(left: List[Sym[Any]], middle: List[Def[Any]], right: FatDef) extends Stm {
    override def lhs = left
    def mhs = middle
    override def rhs = right
    override def defines[A](l: Sym[A]) = lhs.indexOf(l) match { case idx if idx >= 0 => Some(mhs(idx).asInstanceOf[Def[A]]); case _ => None }
    override def defines(r: Op) = if (right == r) left else Nil
  }

  // --- Blocks
  case class Block[+T](res: Exp[T]) { def tp: Typ[T @uncheckedVariance] = res.tp }
  
  def getBlockResult[A](b: Block[A]): Exp[A] 
  def getBlockResultFull[A](b: Block[A]): Exp[A] = b.res
  
  ////////////////
  // --- Metadata
  var metadata: Map[Exp[Any], SymbolProperties] = Map.empty
  private var metadataUpdateFlag: Boolean = false
  private def setMetadataUpdateFlag() { metadataUpdateFlag = true }
  def clearMetadataUpdateFlag() { metadataUpdateFlag = false }
  def getMetadataUpdateFlag() = metadataUpdateFlag
  
  // Setters
  // Directly add symbol property metadata mapping for symbol 
  def setProps(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) { updateProperties(e, p) }
  def setProps(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) { if (p.isDefined) setProps(e, p.get) }

  // Add metadata information for this symbol (possibly using meet)
  def setMetadata(e: Exp[Any], m: Option[Metadata])(implicit ctx: SourceContext) { updateProperties(e, initExp(e, m)) }
  def setMetadata(e: Exp[Any], m: Metadata)(implicit ctx: SourceContext) { setMetadata(e, Some(m)) }

  // Add child information for this symbol (possibly using meet)
  def setChild(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) { updateProperties(e, initExp(e, None, p)) }
  def setChild(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) { setChild(e, Some(p)) }
  
  def setField(e: Exp[Any], p: Option[SymbolProperties], index: String)(implicit ctx: SourceContext) { updateProperties(e, initExp(e, None, p, Some(index))) }
  def setField(e: Exp[Any], p: SymbolProperties, index: String)(implicit ctx: SourceContext) { setField(e, Some(p), index) }

  // Getters
  // Get child metadata for given symbol properties
  def getChild(p: SymbolProperties): Option[SymbolProperties] = p match { 
    case p: ArrayProperties => p.child 
    case _ => cwarn("Attempted to get child of symbol properties with no child!"); None
  }
  def getField(p: SymbolProperties, index: String): Option[SymbolProperties] = p match { 
    case p: StructProperties => p.child(index) 
    case _ => cwarn("Attempted to get field of symbol properties with no fields!"); None
  }

  // Get child metadata for given symbol
  def getProps(e: Exp[Any]): Option[SymbolProperties] = Some(metadata.getOrElse(e, initExp(e)(mpos(e.pos))))
  def getMetadata(e: Exp[Any], k: String): Option[Metadata] = getProps(e).flatMap{p => p(k)}
  def getChild(e: Exp[Any]): Option[SymbolProperties] = getProps(e).flatMap{p => getChild(p)}
  def getField(struct: Exp[Any], index: String): Option[SymbolProperties] = getProps(struct).flatMap{p => getField(p, index)}

  def getProps(b: Block[Any]): Option[SymbolProperties] = getProps(b.res)
  def getMetadata(b: Block[Any], k: String): Option[Metadata] = getMetadata(b.res, k)
  def getChild(b: Block[Any]): Option[SymbolProperties] = getChild(b.res)
  def getField(b: Block[Any], index: String): Option[SymbolProperties] = getField(b.res, index)
  
  /**
   * Merge previous metadata for token and new data, notifying update if changes occurred
   * During merge, new metadata overrides pre-existing data when possible
   */ 
  private def updateProperties(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) {
    val prevProps = metadata.get(e)
    val newProps = tryMeet(prevProps, Some(p), func = MetaOverwrite)
    metadata = metadata ++ List(e -> newProps.get)
    if (!matches(prevProps, newProps)) setMetadataUpdateFlag()
  }
  
  def defaultMetadata[T](tp: Typ[T]): List[Metadata] = Nil

  // Symbol property initialization
  def initExp(e: Exp[Any], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties
    = initTpe(e.tp, data, child, index)

  def initTpe[A](tp: Typ[A], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties = {
    val givenData = PropertyMap(data.map{m => m.name -> Some(m)}.toList)
    val typeData = PropertyMap(defaultMetadata(tp).map{m => m.name -> Some(m)}) 
    val symData = tryMeet(givenData, typeData, func = MetaTypeInit)
    initProps(tp, symData, child, index)
  }

  // Should be overwritten for data structure types (e.g. structs, arrays)
  def initProps[A](tp: Typ[A], symData: PropertyMap[Metadata], child: Option[SymbolProperties], index: Option[String])(implicit ctx: SourceContext): SymbolProperties = tp match {
    case _ => ScalarProperties(symData)
  }
  
  ////////////////////////////////
  // --- Graph construction state
  var globalDefs: List[Stm] = Nil
  var localDefs: List[Stm] = Nil
  var globalDefsCache: Map[Sym[Any],Stm] = Map.empty
  
  def reifySubGraph[T](b: =>T): (T, List[Stm]) = {
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
  }

  def reflectSubGraph(ds: List[Stm]): Unit = {
    val lhs = ds.flatMap(_.lhs)
    assert(lhs.length == lhs.distinct.length, "multiple defs: " + ds)
    // equivalent to: globalDefs filter (_.lhs exists (lhs contains _))
    val existing = lhs flatMap (globalDefsCache get _)
    assert(existing.isEmpty, s"already defined: $existing for $ds")
    localDefs = localDefs ::: ds
    globalDefs = globalDefs ::: ds
    for (stm <- ds; s <- stm.lhs) {      
      globalDefsCache += (s -> stm)
    }
  }

  var nSyms = 0
  def bound[T:Typ]: Sym[T] = Sym[T] { nSyms += 1; nSyms - 1 }
  def fresh[T:Typ](implicit ctx: SourceContext): Sym[T] = Sym[T] { nSyms += 1;  nSyms -1 }
  
  def findStm[T](s: Sym[T]): Option[Stm] = globalDefsCache.get(s)
  def findStm(d: Op): Option[Stm] = globalDefs.find(x => x.defines(d).nonEmpty)

  def createStm(s: List[Sym[Any]], d: Op): Stm = {
    val f = d match {
      case d: Def[_] => TP(s.head, d)
      case d: FatDef => TTP(s, d.mhs, d)
    }
    reflectSubGraph(List(f))
    f
  }
  
  // Thin Defs
  def findOrCreateDefinition[T:Typ](d: Def[T])(implicit ctx: SourceContext): Stm 
    = findStm(d) map {x => x.defines(d).foreach(_.withPos(List(ctx))); x } getOrElse { createStm(List(fresh[T]), d) }
  def findOrCreateDefinitionExp[T:Typ](d: Def[T])(implicit ctx: SourceContext): Exp[T] 
    = findOrCreateDefinition(d).defines(d).head.asInstanceOf[Exp[T]]
  
  // Fat Defs
  def findOrCreateFatDefinition(d: FatDef)(implicit ctx: SourceContext): Stm 
    = findStm(d) map {x => x.defines(d).foreach(_.withPos(List(ctx))); x } getOrElse { createStm(d.tps.map(tp => fresh(mtype(tp),ctx)), d) }
  def findOrCreateFatDefinitionExp(d: FatDef)(implicit ctx: SourceContext): List[Exp[Any]] 
    = findOrCreateFatDefinition(d).defines(d)
  
  protected def toAtom[T:Typ](d: Def[T])(implicit ctx: SourceContext): Exp[T] = findOrCreateDefinitionExp(d)
  protected def toAtom(d: FatDef)(implicit ctx: SourceContext): List[Exp[Any]] = findOrCreateFatDefinitionExp(d)
 
  // TODO: Unused. Needed?
  object Op { 
    def unapply[T](e: Exp[T]): Option[Op] = e match {
      case s: Sym[_] => findStm(s).flatMap(_.defines(s))
      case _ => None
    }
  }
  
  // TODO: Should this return mhs def of TTPs?
  object Def {
    def unapply[T](e: Exp[T]): Option[Def[T]] = e match {
      case s: Sym[_] => findStm(s) match {
        case Some(TP(_,d)) => Some(d.asInstanceOf[Def[T]])
        case _ => None
      }
      case _ => None
    }
  }

  object FatDef {
    def unapply[T](e: Exp[T]): Option[(FatDef, Int)] = e match {
      case s: Sym[_] => findStm(s) match {
        case Some(TTP(syms,mhs,fd)) => Some(fd, syms.indexOf(s))
        case _ => None
      }
      case _ => None
    }
  }
  
  // TODO: Remove these - used by SimplifyTransform
  case class Combine(a: List[Exp[Any]]) extends Exp[Any]()(ManifestTyp(manifest[Any]))
  case class Forward[A](x: Exp[A]) extends Def[A]
  
  
  // HACK: got list but need to pass single source context
  def mpos(s: List[SourceContext]): SourceContext = if (s.nonEmpty) s.head else SourceContext.empty
  def mpos(e: Exp[Any]): SourceContext = mpos(e.pos)
  def mpos(b: Block[Any]): SourceContext = mpos(getBlockResult(b).pos)
  
  // Filter out symbols with primitive types (as defined in Typ.scala)
  def noPrim(sm: List[Sym[Any]]): List[Sym[Any]] = sm.filterNot(s=>isPrimitiveType(s.tp))
}
