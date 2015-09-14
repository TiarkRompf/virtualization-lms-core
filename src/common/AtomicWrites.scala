package scala.virtualization.lms
package common

import scala.reflect.SourceContext
import internal.{GenerationFailedException, GenericCodegen}

/**
 * Purpose of these ops is to allow redirection of general nested mutations
 * This way, DSL authors should be able to use mutable data structures normally without
 * creating mutable aliases in the effects system. Secondary goal is to limit the number
 * of codegen-able IR nodes as much as possible using the
 *
 * (More generalized version of Kevin's nested mutations work in DeliteArray)
 *
 * Note that these accesses aren't 'atomic' in the concurrency sense, just in the
 * code generation sense
 */
// NOTE: Doesn't need to be in internal, but EffectExp is the first time where
// transforming and effects are mixed
trait AtomicWrites extends EffectExp {
  //////////////
  // "Tracers"

  /**
   * Nested Mutation tracer
   * Used to track a single field/apply lookup in a nested update
   * Instances should include a reference to the symbol being accessed
   * and any information normally included in the extract method for that symbol
   */
  trait AtomicTracer extends Product

  // Similar to FieldApply
  case class StructTracer(field: String) extends AtomicTracer
  // Needed for DeliteCodeGenRestage?
  case object VarTracer extends AtomicTracer
  // Similar to DeliteArrayApply
  case class ArrayTracer(i: Exp[Int]) extends AtomicTracer

  // Tracer mirroring
  def mirrorTrace(t: AtomicTracer, f: Transformer)(implicit pos: SourceContext): AtomicTracer = t match {
    case ArrayTracer(i) => ArrayTracer(f(i))
    case _ => t
  }

  // Filled in by Structs.scala, Variables.scala, and DeliteArray.scala
  // (and DeliteMultiArray.scala)
  def recurseLookup[T:Manifest](sym: Exp[Any], trace: List[AtomicTracer]): (Exp[Any],List[AtomicTracer]) = {
    (sym,trace)
  }

  /////////////////
  // Atomic Writes

  trait AtomicWrite[A] extends Def[Unit] {
    var isNested: Boolean = false
    def asNested: AtomicWrite[A] = {isNested = true; this}
    /*
     * Symbols which should always be externally visible, even with nesting
     * e.g.:
     * DeliteArrayUpdate(array, i, x) => need to see dependencies (array, i, x)
     * NestedAtomicWrite(struct, field, DeliteArrayUpdate(array, i, x)) => need (struct, i, x)
     * TODO: Better name for this?
     */
    def externalFields: List[Any]
    val mA: Manifest[A]
  }

  /**
   * Abstract atomic write node.
   */
  abstract class AtomicWriteDef[A:Manifest] extends AtomicWrite[A] {
    val mA = manifest[A]
  }

  // mirroring
  def mirrorNestedAtomic[A:Manifest](d: AtomicWrite[A], f: Transformer)(implicit ctx: SourceContext): AtomicWrite[A] = d match {
    case _ => sys.error("No mirror atomic rule found for " + d)
  }

  // Version of reflectEffect used for Atomic Writes
  def reflectAtomicWrite[A:Manifest](sym: Exp[Any])(d: AtomicWrite[A])(implicit ctx: SourceContext): Exp[Unit] = {
    val (outerSym, trace) = recurseLookup(sym, Nil)
    val outerDef = if (trace.isEmpty) { d } else { NestedAtomicWrite[A](outerSym, trace, d.asNested) }
    reflectWrite(outerSym)(outerDef)
  }

  ////////////////////
  // Nested Mutations

  /**
   * Nested Mutation IR node
   * Includes a list of tracers giving the series of extractions done to get to the
   * structure being updated. The first element in the tracer list should contain the
   * outer data structure being written to
   */
  case class NestedAtomicWrite[A:Manifest](sym: Exp[Any], trace: List[AtomicTracer], d: AtomicWrite[A])(implicit ctx: SourceContext) extends Def[Unit] {
    private lazy val deps = trace.flatMap{t => t.productIterator.toList} ::: List(sym, d)
    override def productIterator = deps.iterator
    override def productElement(n: Int) = deps(n)
    override def productArity = deps.length
    val mA = manifest[A]
  }

  override def mirror[A:Manifest](e: Def[A], f: Transformer)(implicit ctx: SourceContext): Exp[A] = (e match {
    case op: AtomicWrite[_] if op.isNested => sys.error("Shouldn't be mirroring a nested write!")
    case Reflect(NestedAtomicWrite(s,t,d), u, es) =>
      reflectMirrored(Reflect(NestedAtomicWrite(f(s),t.map{r => mirrorTrace(r,f)}, mirrorNestedAtomic(d,f).asNested)(mtype(manifest[A]),ctx), mapOver(f,u), f(es)))(mtype(manifest[Unit]), ctx)
    case _ => super.mirror(e,f)
  }).asInstanceOf[Exp[A]]


  // These are A LOT like overriding the product iterator for case classes, but
  // we only want to use the "overriden" version in dependencies/effects checking
  override def blocks(e: Any): List[Block[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(blocks(_))
    case _ => super.blocks(e)
  }

  // dependencies (Expressions)
  override def syms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(syms(_))
    case _ => super.syms(e)
  }
  override def boundSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(boundSyms(_))
    case _ => super.boundSyms(e)
  }
  override def tunnelSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(tunnelSyms(_))
    case _ => super.tunnelSyms(e)
  }
  override def effectSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(effectSyms(_))
    case _ => super.effectSyms(e)
  }
  override def softSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(softSyms(_))
    case _ => super.softSyms(e)
  }
  override def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(rsyms(_)(f))
    case _ => super.rsyms(e)(f)
  }
  override def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(symsFreq(_))
    case _ => super.symsFreq(e)
  }

  // effects
  override def readSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(readSyms(_))
    case _ => super.readSyms(e)
  }
  override def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(aliasSyms(_))
    case _ => super.aliasSyms(e)
  }
  override def containSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(containSyms(_))
    case _ => super.containSyms(e)
  }
  override def extractSyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(extractSyms(_))
    case _ => super.extractSyms(e)
  }
  override def copySyms(e: Any): List[Sym[Any]] = e match {
    case op: AtomicWrite[_] if op.isNested => op.externalFields.flatMap(copySyms(_))
    case _ => super.copySyms(e)
  }

}

// Codegen for AtomicWrite operations
// Not target specific! All other codegens for AtomicWrites should extend this
trait BaseGenAtomicOps extends GenericCodegen {
  val IR: AtomicWrites
  import IR._

  def quote(t: AtomicTracer): String = t match {
    case VarTracer => ""   // no sym to compare to deliteInputs here.. should be ok though
    case StructTracer(f) => "." + f                    // [struct].field
    case ArrayTracer(i) => "(" + quote(i) + ".toInt)"  // [array](i.toInt)
    case _ => sys.error("No codegen rule defined for atomic trace " + t)
  }
  def quote(trace: List[AtomicTracer]): String = trace.map{t => quote(t)}.mkString("")

  /**
   * Emit rules for nested AtomicWrite nodes
   * Must be filled in by codegen-able atomic write nodes!
   * @param sym   - output symbol for the result of this write operation (Unit)
   * @param d     - the atomic write IR node
   * @param trace - optional string representing codegenned write target
   * (if trace is None, codegen should use d's write target instead)
   */
  def emitAtomicWrite(sym: Sym[Any], d: AtomicWrite[_], trace: Option[String]): Unit = d match {
    // e.g.:
    // case DeliteArrayUpdate(a,i,x) =>
    //  emitValDef(sym, trace.getOrElse(quote(a)) + "(" + quote(i) + ".toInt) = " + quote(x))
    case _ => throw new GenerationFailedException("Don't know how to generate code for atomic write: " + d)
  }

  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case op: AtomicWrite[_] =>
      emitAtomicWrite(sym, op, None)
    case op@NestedAtomicWrite(s, trace, d) =>
      emitAtomicWrite(sym, d, Some(quote(s) + quote(trace)))
    case _ => super.emitNode(sym, rhs)
  }
}

// TODO: Do these need to mix in another target-specific trait?
trait ScalaGenAtomicOps extends ScalaGenBase with BaseGenAtomicOps

trait CudaGenAtomicOps extends CudaGenBase with BaseGenAtomicOps {
  import IR._

  override def quote(t: AtomicTracer): String = t match {
    case StructTracer(f) => "." + f
    case ArrayTracer(i) => ".apply(" + quote(i) + ")"
    case _ => super.quote(t)
  }

  // TODO: Are these exceptions inherent to cuda generation? Ask HJ later
  override def quote(trace: List[AtomicTracer]): String = {
    if (trace.count{case VarTracer => true case _ => false} > 0)
      throw new GenerationFailedException("CudaCodegen: Does not support arbitrarily nested updates with vars.\n")

    if (trace.count{_.isInstanceOf[ArrayTracer]} > 0)
      throw new GenerationFailedException("CudaCodegen: Does not support arbitrarily nested updates with arrays.\n")

    super.quote(trace)
  }
}

trait OpenCLGenAtomicOps extends OpenCLGenBase with BaseGenAtomicOps {
  import IR._

  override def quote(trace: List[AtomicTracer]): String = {
    throw new GenerationFailedException("OpenCL: Does not currently support nested updates.\n")

    super.quote(trace)  // unreachable
  }
}

trait CGenAtomicOps extends CGenBase with BaseGenAtomicOps {
  import IR._

  override def quote(t: AtomicTracer): String = t match {
    case StructTracer(f) => "->" + f
    case ArrayTracer(i) => "->apply(" + quote(i) + ")"
    case _ => super.quote(t)
  }
}
