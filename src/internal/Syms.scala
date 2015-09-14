package scala.lms
package internal

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.reflect.SourceContext

trait Syms { this: Effects =>

  def blocks(e: Any): List[Block[Any]] = e match {
    case b: Block[_] => List(b)
    case p: Product => p.productIterator.toList.flatMap(blocks(_))
    case _ => Nil
  }

  // regular data (and effect) dependencies
  def syms(e: Any): List[Sym[Any]] = e match {
    case s: Summary => Nil // don't count effect summaries as dependencies!

    // enable DCE of reflect nodes if they are only control dependencies
    case Reflect(x,u,es) if addControlDeps => syms(x) ::: nonControlSyms(es, syms)
    case Reify(x,u,es) if addControlDeps => syms(x) ::: nonControlSyms(es, syms)

    case s: Sym[_] => List(s)
    case ss: Iterable[Any] => ss.toList.flatMap(syms(_))
    // All case classes extend Product!
    case p: Product =>
      // performance hot spot: this is the same as: p.productIterator.toList.flatMap(syms(_))
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
    case Def(Reify(y, u, es)) => es.asInstanceOf[List[Sym[Any]]]
    case ss: Iterable[Any] => ss.toList.flatMap(effectSyms(_))
    case p: Product => p.productIterator.toList.flatMap(effectSyms(_))
    case _ => Nil
  }

  // soft dependencies: they are not required but if they occur,
  // they must be scheduled before
  def softSyms(e: Any): List[Sym[Any]] = e match {
    // empty by default
    case ss: Iterable[Any] => ss.toList.flatMap(softSyms(_))
    case p: Product => p.productIterator.toList.flatMap(softSyms(_))
    case _ => Nil
  }

  // generic symbol traversal: f is expected to call rsyms again
  def rsyms[T](e: Any)(f: Any=>List[T]): List[T] = e match {
    case s: Sym[_] => f(s)
    case s: Summary => Nil // don't count effect summaries as dependencies!
    case ss: Iterable[Any] => ss.toList.flatMap(f)
    case p: Product => p.productIterator.toList.flatMap(f)
    case _ => Nil
  }

  // frequency information for dependencies: used/computed
  // often (hot) or not often (cold). used to drive code motion.
  def symsFreq(e: Any): List[(Sym[Any], Double)] = e match {
    case s: Sym[_] => List((s,1.0))
    case s: Summary => Nil // don't count effect summaries as dependencies!

    // enable DCE of reflect nodes if they are only control dependencies
    case Reflect(x,u,es) if addControlDeps => symsFreq(x) ::: nonControlSyms(es, symsFreq)
    case Reify(x,u,es) if addControlDeps => symsFreq(x) ::: nonControlSyms(es, symsFreq)

    case ss: Iterable[Any] => ss.toList.flatMap(symsFreq(_))
    case p: Product => p.productIterator.toList.flatMap(symsFreq(_))
    case _ => Nil
  }

  def freqNormal(e: Any) = symsFreq(e)
  def freqHot(e: Any) = symsFreq(e).map(p=>(p._1,p._2*1000.0))
  def freqCold(e: Any) = symsFreq(e).map(p=>(p._1,p._2*0.5))

  def readSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => readSyms(x) // ignore effect dependencies (they are not read!)
    case Reify(x, u, es) =>
      // in general: the result of a block is not read but passed through.
      // FIXME this piece of logic is not clear. is it a special case for unit??
      // it looks like this was introduced to prevent the Reify to be reflected
      // if x is a mutable object defined within the block.
      // TODO: the globalMutableSyms part was added later (June 2012) -- make sure it does the right thing
      if ((es contains x) || (globalMutableSyms contains x)) Nil
      else readSyms(x)
    case s: Sym[_] => List(s)
    case p: Product => p.productIterator.toList.flatMap(readSyms(_))
    case _ => Nil
  }

  /*
    decisions to be made:
    1) does alias imply read? or are they separate?
    2) use a data structure to track transitive aliasing or recompute always?
  */


  /*
  the methods below define the sharing relation between the
  result of an operation and its arguments.

  how do i use them? what do i need to return?

  assume an operation foo:

  y = Foo(x)

  x should be returned in the following cases:

  x in aliasSyms(y)      if y = x      // if then else
  x in containSyms(y)    if *y = x     // array update
  x in extractSyms(y)    if y = *x     // array apply
  x in copySyms(y)       if *y = *x    // array clone

  y = x is to be understood as "y may be equal to x"
  *y = x as "dereferencing y (at some index) may return x"
  etc.

  */
  def aliasSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => aliasSyms(x)
    case Reify(x, u, es) => syms(x)
    case s: Sym[_] => List(s)
    case p: Product => p.productIterator.toList.flatMap(aliasSyms(_))
    case _ => Nil
  }

  def containSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => containSyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[_] => Nil
    case p: Product => p.productIterator.toList.flatMap(containSyms(_))
    case _ => Nil
  }

  def extractSyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => extractSyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[_] => Nil
    case p: Product => p.productIterator.toList.flatMap(extractSyms(_))
    case _ => Nil
  }

  def copySyms(e: Any): List[Sym[Any]] = e match {
    case Reflect(x, u, es) => copySyms(x)
    case Reify(x, u, es) => Nil
    case s: Sym[_] => Nil
    case p: Product => p.productIterator.toList.flatMap(copySyms(_))
    case _ => Nil
  }

  // --- Miscellaneous
  def strDef(e: Exp[Any]): String = e match {
    case Const(z) => z.toString
    case Def(d) => e.toString + " = " + d.toString
    case FatDef(d) => e.toString + " = " + d.toString
    case e: Exp[_] => "(bound " + e.toString + ")"
  }

  def quotePos(e: Exp[Any]): String = e.pos match {
    case Nil => "<unknown>"
    case cs =>
      cs.map(c => all(c).reverse.map(c => c.fileName.split("/").last +
                  ":" + c.line).mkString("//")).mkString(";")
  }
}