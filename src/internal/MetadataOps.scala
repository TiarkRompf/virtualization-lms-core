package scala.virtualization.lms
package internal

import scala.reflect.SourceContext
import scala.collection.mutable.HashMap
import scala.virtualization.lms.common._

import Meetable._

trait MetadataOps extends Expressions with Blocks with SymbolMetadata {

  val symbolData = new HashMap[Exp[Any], SymbolProperties]

  // TODO: Move this elsewhere?
  def isDataStructureTpe[T](tp: Manifest[T]): Boolean = tp match {
    case _ => false
  }
  def defaultTypeMetadata[A](tp: Manifest[A]): List[Metadata] = Nil

  private var metadataUpdateFlag = false
  private def setMetadataUpdateFlag() { metadataUpdateFlag = true }
  def clearMetadataUpdateFlag() { metadataUpdateFlag = false }
  def getMetadataUpdateFlag() = metadataUpdateFlag

  // Meet with error reporting for incompatible metadata
  def attemptMeet[T: Meetable](a: T, b: T, func: MeetFunc)(implicit ctx: SourceContext): T = {
    if (canMeet(a,b,func)) { meet(a,b,func) }
    else {
      val inc = incompatibilities(a,b,func)
      if (!inc.isEmpty) {
        fatalerr(quotePos(ctx) + ": " + inc.head + "\n\t" + quoteCode(ctx).map{"\n\t" + _}.getOrElse("") + 
                  "LHS metadata: " + makeString(a) + "\n" +
                  "RHS metadata: " + makeString(b) + "\n")
      }
      else {
        fatalerr(quotePos(ctx) + ": Attempted to meet incompatible metadata for symbol used here:\n" + 
                  "LHS metadata: " + makeString(a) + "\n" +
                  "RHS metadata: " + makeString(b) + "\n" + 
                  quoteCode(ctx).map{"\t" + _}.getOrElse("")
                )
      }
      (a) // unreachable
    }
  }

  ///////////////////////////////
  // Metadata & Property Setters

  // TODO: This doesn't really belong here...
  object EatSome {
    def unapply(x: Any): Option[Any] = x match {
      case Some(y) => Some(y)
      case None => None
      case _ => Some(x)
    }
  }

  // Directly add symbol property metadata mapping for symbol 
  def setProps(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) { updateProperties(e, p) }
  def setProps(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) { if (p.isDefined) setProps(e, p.get) }

  // Add metadata information for this symbol (possibly using meet)
  def setMetadata(e: Exp[Any], m: Option[Metadata])(implicit ctx: SourceContext) { updateProperties(e, initExp(e, m)) }
  def setMetadata(e: Exp[Any], m: Metadata)(implicit ctx: SourceContext) { setMetadata(e, Some(m)) }

  def copyMetadata(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) {
    p.data.keys foreach {k => setMetadata(e, p(k))}
  }

  // Add child information for this symbol (possibly using meet)
  def setChild(e: Exp[Any], p: Option[SymbolProperties])(implicit ctx: SourceContext) { updateProperties(e, initExp(e, None, p)) }
  def setChild(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) { setChild(e, Some(p)) }
  
  def setField(e: Exp[Any], p: Option[SymbolProperties], index: String)(implicit ctx: SourceContext) { updateProperties(e, initExp(e, None, p, Some(index))) }
  def setField(e: Exp[Any], p: SymbolProperties, index: String)(implicit ctx: SourceContext) { setField(e, Some(p), index) }

  // Infix shortcuts for returning a symbol with metadata added 
  def infix_withProps[A:Manifest](exp: Exp[A], props: Any): Exp[A] = {
    props match {
      case EatSome(p: SymbolProperties) => setProps(exp, p)
      case _ => // Ignore
    }
    (exp)
  }

  def infix_withData[A:Manifest](exp: Exp[A], data: Any): Exp[A] = {
    data match {
      case EatSome(m: Metadata) => setMetadata(exp, m)
      case _ => // Ignore
    }
    (exp)
  }

  def infix_withChild[A:Manifest](exp: Exp[A], data: Any): Exp[A] = {
    data match {
      case EatSome(p: SymbolProperties) => setChild(exp, p)
      case _ => // Ignore
    }
    (exp)
  }
  def infix_withField[A:Manifest](exp: Exp[A], data: Any, index: String): Exp[A] = {
    data match {
      case EatSome(p: SymbolProperties) => setField(exp, p, index)
      case _ => // Ignore
    }
    (exp)
  }

  ///////////////////////////////
  // Metadata & Property Getters

  // Get child metadata for given symbol properties
  def getChild(p: SymbolProperties): Option[SymbolProperties] = p match {
    case p: ArrayProperties => p.child
    case _ =>
      cwarn("This is likely a compiler bug! No match when attempting to get child of metadata: \n" + makeString(p))
      None
  }
  def getField(p: SymbolProperties, index: String): Option[SymbolProperties] = p match {
    case p: StructProperties => p.child(index)
    case _ => 
      cwarn("This is likely a compiler bug! Attempted to get field " + index + " of metadata: \n" + makeString(p))
      None
  }

  // Get child metadata for given symbol
  def getProps(e: Exp[Any]): Option[SymbolProperties] = Some(symbolData.getOrElse(e, initExp(e)(mpos(e.pos))))
  def getMetadata(e: Exp[Any], k: String): Option[Metadata] = getProps(e).flatMap{p => p(k)}
  def getChild(e: Exp[Any]): Option[SymbolProperties] = getProps(e).flatMap{p => getChild(p)}
  def getField(struct: Exp[Any], index: String): Option[SymbolProperties] = getProps(struct).flatMap{p => getField(p, index)}

  def getProps(b: Block[Any]): Option[SymbolProperties] = getProps(b.res)
  def getMetadata(b: Block[Any], k: String): Option[Metadata] = getMetadata(b.res, k)
  def getChild(b: Block[Any]): Option[SymbolProperties] = getChild(b.res)
  def getField(b: Block[Any], index: String): Option[SymbolProperties] = getField(b.res, index)

  // --- Shortcuts for properties, manifests
  // These shortcuts should only be used when child is guaranteed to be defined
  def child(p: SymbolProperties): SymbolProperties = getChild(p).get
  def child(p: SymbolProperties, index: String): SymbolProperties = getField(p, index).get
  def props(e: Exp[Any]): SymbolProperties = getProps(e).get
  def child(e: Exp[Any]): SymbolProperties = getChild(e).get
  def child(e: Exp[Any], index: String): SymbolProperties = getField(e, index).get

  def props(b: Block[Any]): SymbolProperties = getProps(b.res).get
  def child(b: Block[Any]): SymbolProperties = getChild(b.res).get
  def child(b: Block[Any], index: String): SymbolProperties = getField(b.res, index).get

  /**
   * Merge previous metadata for token and new data, notifying update if changes occurred
   * During merge, new metadata overrides pre-existing data when possible
   */ 
  private def updateProperties(e: Exp[Any], p: SymbolProperties)(implicit ctx: SourceContext) {
    if (!symbolData.contains(e)) {
      symbolData(e) = p
      setMetadataUpdateFlag()
    }
    else {
      val prevProps = symbolData(e)
      val newProps = attemptMeet(prevProps, p, func = MetaOverwrite)
      symbolData(e) = newProps
      if (!matches(prevProps, newProps)) setMetadataUpdateFlag()
    }
  }

  //////////////////////////////////
  // Symbol property initialization

  def initExp(e: Exp[Any], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties
    = initTpe(e.tp, data, child, index)

  def initTpe[A](tp: Manifest[A], data: Option[Metadata] = None, child: Option[SymbolProperties] = None, index: Option[String] = None)(implicit ctx: SourceContext): SymbolProperties = {
    val givenData = PropertyMap(data.map{m => m.name -> Some(m)}.toList)
    val typeData = PropertyMap(defaultTypeMetadata(tp).map{m => m.name -> Some(m)}) 
    val symData = attemptMeet(givenData, typeData, func = MetaTypeInit)
    initProps(tp, symData, child, index)
  }

  // Should be overwritten by primitive types (e.g. structs, arrays)
  def initProps[A](tp: Manifest[A], symData: PropertyMap[Metadata], child: Option[SymbolProperties], index: Option[String])(implicit ctx: SourceContext): SymbolProperties = tp match {
    case _ => ScalarProperties(symData)
  }
}
