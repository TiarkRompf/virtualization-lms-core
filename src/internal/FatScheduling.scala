package scala.virtualization.lms
package internal

import util.GraphUtil
import scala.collection.mutable.HashMap

trait FatScheduling extends Scheduling {
  val IR: FatExpressions
  import IR._  
  
  import util.GraphUtil

  def fatten(e: Stm): Stm = e

  def fattenAll(e: List[Stm]): List[Stm] = e.map(fatten)
    
}