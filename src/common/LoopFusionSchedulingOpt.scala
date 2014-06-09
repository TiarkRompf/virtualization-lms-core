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

  // Assuming that the first focusExactScopeFat call contains the full scope
  // of the entire program. Want to avoid rescanning inner scopes.
  var needsFusion = true
  override def focusExactScopeFat[A](resultB: List[Block[Any]])(body: List[Stm] => A): A = {
    if (needsFusion) {
      val fusedScope = fuseAllLoops(innerScope)
      innerScope = getSchedule(fusedScope)(resultB)
    }
    super.focusExactScopeFat(resultB)(body)
  }

  def lines(l: List[Any]) = l.mkString("\n", "\n", "\n")
  def lines[A,B](l: scala.collection.immutable.Map[A,B]) = l.mkString("\n", "\n", "\n")

  def fuseAllLoops(currentScope: List[Stm]): List[Stm] = {
    
    // build map to lookup loops to be fused
    val (loops, restOfScope) = currentScope.partition { 
      case TTP(_, _, c: CanBeFused) if c.getFusedSetID.isDefined => true
      case _ => false
    }
    val fusedSetsMap = new scala.collection.mutable.HashMap[Int, List[TTP]]()
    loops.foreach({ 
      case e @ TTP(_, _, c: CanBeFused) => 
        val id = c.getFusedSetID.get
        fusedSetsMap.update(id, e :: fusedSetsMap.getOrElse(id, Nil))
      case _ => sys.error("(FTO) impossible error")
    })

    // fuse TTPs of each set into one fat TTP per set
    val fusedTTPs = fusedSetsMap.toList.reverse.map({ case (_, ttpsToFuse) =>
      val reverseTTPs = ttpsToFuse.reverse
      printlog("(FTO) Fusing these loops into one fat TTP: " + lines(reverseTTPs))
      fuseTTPs(reverseTTPs)
    })

    // replace TTP sets with their fused loop
    needsFusion = false
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
        assert(index == index2, "(FTO) ERROR: trying to fuse loops " + lhs + " of different indices: " + index + " != " + index2)
        ((lhs, mhs), rhs)
      case s => sys.error("(FTO) ERROR: Fusion failed, unrecognized loop statement: " + s)
    }).unzip
    val (lhs, mhs) = lmhs.unzip
    // The mhs lists the original statements including their effects
    TTP(lhs.flatten, mhs.flatten, SimpleFatLoop(shape, index, rhs.flatten))
  }

}
