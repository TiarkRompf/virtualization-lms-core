package scala.virtualization.lms
package util

import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.mutable.Stack
import scala.collection.mutable.Buffer
import scala.collection.mutable.ArrayBuffer


object GraphUtil {
  
  class Ref[T](init: T) {
    var value: T = init
  }
  
  /* test cases

     stronglyConnectedComponents[String](List("A"), { case "A" => List("B") case "B" => List("C") case "C" => List("A","D") case "D" => Nil})
     List(List(A, B, C), List(D))

     stronglyConnectedComponents[String](List("A","B","C"), { case "A" => List("B") case "B" => List("C") case "C" => List("A","D") case "D" => Nil})

  
   
  */

  /** 
      Returns the strongly connected components
      of the graph rooted at the first argument,
      whose edges are given by the function argument.

      The scc are returned in topological order.
      Tarjan's algorithm (linear).
   */
  def stronglyConnectedComponents[T](start: List[T], succ: T=>List[T]): List[List[T]] = {

    val id: Ref[Int] = new Ref(0)
    val stack: Stack[T] = new Stack()
    val mark: Map[T,Int] = new HashMap()

    val res: Buffer[Buffer[T]] = new ArrayBuffer()
    for (node <- start)
      visit(node,succ,id,stack,mark,res)

    // TODO: get rid of reverse

    (for (scc <- res) yield scc.toList.reverse).toList.reverse
  }

  def visit[T](node: T, succ: T=>List[T], id: Ref[Int], stack: Stack[T], 
  		      mark: Map[T,Int], res: Buffer[Buffer[T]]): Int = {

    mark.getOrElse(node, {

      id.value = id.value + 1

      mark.put(node, id.value)
      stack.push(node)
//    println("push " + node)

      var min: Int = id.value
      for (child <- succ(node)) {
	      val m = visit(child, succ, id, stack, mark, res)

	      if (m < min) 
          min = m
      }

      if (min == mark(node)) {

	      val scc: Buffer[T] = new ArrayBuffer()
	      var loop: Boolean = true
	      do {
  	      val element = stack.pop()
//        println("appending " + element)
  	      scc.append(element)
  	      mark.put(element, Integer.MAX_VALUE)
  	      loop = element != node
	      } while (loop)
	      res.append(scc)
      }
      min
      
    })
  }
  
  
}
