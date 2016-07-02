package scala.virtualization.lms
package common

import internal.{SymbolMetadata, Effects}
import internal.{IterativeTraversal, AbstractAnalyzer}

import scala.reflect._


trait IterativeAnalyzer extends IterativeTraversal with AbstractAnalyzer { self =>
  val IR: BaseFatExp with AtomicWrites with StructExp
  import IR._

  // --- Settings
  val autopropagate: Boolean = true

  override def hasConverged: Boolean = runs > 0 && !getMetadataUpdateFlag()

  /**
   * Main functions for analysis.
   * By default called after metadata propagation has been completed
   */
  def analyze(lhs: Exp[Any], rhs: Def[Any]): Unit = rhs match {
    case Reflect(d, u, es) if eatReflect => analyze(lhs, d)
    case _ => if (recurse == AsDefault) blocks(rhs).foreach{blk => traverseBlock(blk)}
  }
  def analyze(lhs: List[Exp[Any]], mhs: List[Def[Any]], rhs: FatDef): Unit = {}

  def tracerToProperties(t: AtomicTracer, child: Option[SymbolProperties]): Option[SymbolProperties] = t match {
    case StructTracer(index) => Some(StructProperties(PropMap(index,child), NoData))
    case ArrayTracer(_) => Some(ArrayProperties(child, NoData))
  }

  def getAtomicWriteRHS(d: AtomicWrite[Any])(implicit ctx: SourceContext): Option[SymbolProperties] = d match {
    case FieldUpdate(_,_,x) => getProps(x)
    case _ =>
      printwarn(s"No RHS rule extraction rule given for atomic write op $d")
      None
  }


  /**
   * Propagate metadata based on the -Syms function family (aliasSyms containSyms, etc.)
   **/
  def propagateViaSyms[A](lhs: Exp[A], rhs: Def[_])(implicit ctx: SourceContext): Unit = rhs match {
    case Reflect(d, u, es) =>
      /**
       * Aliasing via updates - merge metadata on writes with child of data structure
       *
       * Given a Def which updates exactly one collection with exactly one value, meet the metadata
       * of the child of the collection and the value
       *
       **/
      // Assumption - exactly one written value and one mutated data structure
      if (containSyms(d).length > 1 || u.mstWrite.length > 1)
        debug(s"$rhs \n   RHS values: ${containSyms(d).mkString(",")}, LHS values: ${u.mstWrite.mkString(",")}")
      else if (u.mstWrite.length == 1 && containSyms(d).isEmpty)
        debug(s"$rhs \n   RHS values: ${containSyms(d).mkString(",")}, LHS values: ${u.mstWrite.mkString(",")}")
      else if (containSyms(d).length == 1 && u.mstWrite.length == 1) {
        val updateChild = meet(containSyms(d).map(getProps):_*)
        val coll = u.mstWrite.head

        d match {
          case NestedAtomicWrite(_,trace,aw) =>
            var newProps: Option[SymbolProperties] = getAtomicWriteRHS(aw)
            for (t <- trace.reverse) { newProps = tracerToProperties(t, newProps) }

            val updatedProps = meet(newProps, getProps(coll))
            setProps(coll, updatedProps)

          case _ => coll.tp match {
            case StructLike(_) => // Assume this is FieldUpdate
            case ArrayLike(_) => setChild(coll, meet(updateChild, getChild(coll)))
            case _ => debug(s"$rhs \n   Appears to be writing to a scalar?")
          }
        }
      }

      propagateViaSyms(lhs, d)

    case _ =>
      /**
       * Aliasing via applies - set metadata of extracted value to metadata of child
       **/
      if (extractSyms(rhs).length > 1)
        debug(s"$rhs \n Extract syms: ${extractSyms(rhs)}")
      else if (extractSyms(rhs).length == 1) {
        val coll = extractSyms(rhs).head

        coll.tp match {
          case StructLike(_) => // Assume this is FieldApply
          case ArrayLike(_) => setProps(lhs, getChild(coll))
          case _ => debug(s"$rhs\n   Appears to be applying on a scalar?")
        }
      }

      /**
       * Aliasing via copying - merge properties of rhs with that of lhs
       **/
      if (copySyms(rhs).length > 1)
        debug(s"$rhs \n Copies multiple things: ${copySyms(rhs)}")
      else if (copySyms(rhs).length == 1) {
        val coll = copySyms(rhs).head
        setProps(lhs, getProps(coll))
      }

      /**
       * Other aliasing - branches, etc.
       *
       * NOTE: This will not work! The default implementation of aliasSyms right now is that
       * ALL inputs to a node alias with the output!
       **/
      //val aliasProps = meet( aliasSyms(rhs).map(getProps):_* )
      //setProps(lhs, meet(aliasProps, getProps()))
  }


  override final def traverse(lhs: Sym[Any], rhs: Def[Any]) = {
    if (autopropagate) {
      propagateViaSyms(lhs, rhs)
      propagate(lhs, rhs)
    }
    analyze(lhs, rhs)
  }

  override final def traverse(lhs: List[Sym[Any]], mhs: List[Def[Any]], rhs: FatDef) = {
    // TODO: Autopropagation for TTP?
    analyze(lhs, mhs, rhs)
  }

  override def runOnce[A:Manifest](b: Block[A]): Block[A] = {
    clearMetadataUpdateFlag()

    for (data <- datRequire) {
      if (!(validData contains data))
        analyzers(data).run(b)
    }
    val out = super.runOnce(b)

    // Invalidate all metadata which are subtypes of the list of this traversal's invalidation
    // Allows us to write, for example, invalidates (classOf[Metadata]) to invalidate all metadata
    validData = validData filterNot (dat => datInvalid.exists(isSubtype(dat, _)) )

    (out)
  }

  override def run[A:Manifest](b: Block[A]): Block[A] = withDebugging {
    val out = super.run(b)
    if (hasCompleted && hasConverged) {
      // Update metadata state
      datUpdate foreach {dat => analyzers += (dat -> self.asInstanceOf[Analyzer]) }
      validData = (validData ++ datCreate ++ datUpdate).distinct
    }
    (out)
  }

}
