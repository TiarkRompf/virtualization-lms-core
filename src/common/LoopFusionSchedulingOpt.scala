package scala.virtualization.lms
package common


// TODO generally need enable/disable option?
// TODO stuff in FatScheduling and LoopFusionOpt.scala should go away

/** Do the actual loop fusion by combining loops into one fat TTP-statement
  * per fusion set. This does not change any symbols or introduce any new nodes,
  * it only combines existing nodes into fat nodes.
  */
trait LoopFusionSchedulingOpt extends internal.FatBlockTraversal {
  val IR: LoopFusionCore2
  import IR._  

  var fusedSyms: List[List[Sym[Any]]] = Nil
  def setFusedSyms(syms: List[List[Sym[Any]]]) = fusedSyms = syms

  override def focusExactScopeFat[A](resultB: List[Block[Any]])(body: List[Stm] => A): A = {
    if (!fusedSyms.isEmpty) {
      val fusedScope = fuseAllLoops(innerScope)
      innerScope = getSchedule(fusedScope)(resultB)
      // println("=== (FTO) after fusion, rescheduled")
      // innerScope foreach println
    }
    super.focusExactScopeFat(resultB)(body)
  }

  def lines(l: List[Any]) = l.mkString("\n", "\n", "\n")
  def lines[A,B](l: scala.collection.immutable.Map[A,B]) = l.mkString("\n", "\n", "\n")

  def fuseAllLoops(currentScope: List[Stm]): List[Stm] = {
    
    // build map to lookup loops to be fused
    val fusedSymsFlat = fusedSyms.flatten
    val (loops, restOfScope) = currentScope.partition { 
      case e @ TTP(List(sym), _, SimpleFatLoop(_,_,_)) if (fusedSymsFlat.contains(sym)) => true
      case _ => false
    }
    val loopsMap = loops.map({ 
      case e @ TTP(List(sym), _, _) => (sym, e)
      case _ => sys.error("(FTO) impossible error")
    }).toMap

    // fuse TTPs of each set into one fat TTP per set
    val fusedTTPs = fusedSyms.map({ set: List[Sym[Any]] =>
      val TTPsToFuse = set.map(sym => loopsMap.get(sym) match {
        case Some(ttp) => ttp
        case _ => sys.error("(FTO) ERROR: Fusion failed, loop statement not found for " + sym)
      })
      printlog("(FTO) Fusing these loops into one fat TTP: " + lines(TTPsToFuse))
      fuseTTPs(TTPsToFuse)
    })

    // replace TTP sets with their fused loop
    fusedSyms = Nil
    fusedTTPs ::: restOfScope
  }

  def fuseTTPs(TTPsToFuse: List[Stm]): Stm = {
    val (shape, index) = TTPsToFuse(0) match {
      case TTP(_, _, SimpleFatLoop(shape,index,_)) => (shape, index)
    }
    
    // extract info to create fused TTP
    val (lmhs, rhs) = TTPsToFuse.map({ 
      case TTP(lhs, mhs, SimpleFatLoop(shape2,index2,rhs)) => 
        assert(shape == shape2, "(FTO) ERROR: trying to fuse loops " + lhs + " of different shapes: " + shape + " != " + shape2)
        assert(index == index2, "(FTO) ERROR: trying to fuse loops " + lhs + "of different indices: " + index + " != " + index2)
        ((lhs, mhs), rhs)
      case s => sys.error("(FTO) ERROR: Fusion failed, unrecognized loop statement: " + s)
    }).unzip
    val (lhs, mhs) = lmhs.unzip
    // The mhs lists the original statements including their effects
    TTP(lhs.flatten, mhs.flatten, SimpleFatLoop(shape, index, rhs.flatten))
  }

}
