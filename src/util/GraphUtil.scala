package scala.virtualization.lms
package util

import java.util.{ArrayDeque, HashMap}


object GraphUtil {

  class Ref[T](init: T) {
    var value: T = init
  }

  /**
      Returns the least common ancestor of two nodes in some directed, acyclic graph

   */
  def leastCommonAncestor[T](x: T, y: T, parent: T => Option[T]): Option[T] = {
    var pathX: List[Option[T]] = List(Some(x))
    var pathY: List[Option[T]] = List(Some(y))

    var curX = Some(x)
    while (curX.isDefined) { curX = parent(curX); pathX ::= curX }

    var curY = Some(y)
    while (curY.isDefined) { curY = parent(curY); pathY ::= curY }

    // Choose last node where paths are the same
    pathX.zip(pathY).filter{case (x,y) => x == y}.last
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
    val stack = new ArrayDeque[T]
    val mark = new HashMap[T,Int]

    val res = new Ref[List[List[T]]](Nil)
    for (node <- start)
      visit(node,succ,id,stack,mark,res)

    res.value
  }

  def visit[T](node: T, succ: T=>List[T], id: Ref[Int], stack: ArrayDeque[T],
            mark: HashMap[T,Int], res: Ref[List[List[T]]]): Int = {


    if (mark.containsKey(node))
      mark.get(node)
    else {
      id.value = id.value + 1

      mark.put(node, id.value)
      stack.addFirst(node)
//    println("push " + node)

      var min: Int = id.value
      for (child <- succ(node)) {
        val m = visit(child, succ, id, stack, mark, res)

        if (m < min)
          min = m
      }

      if (min == mark.get(node)) {
        var scc: List[T] = Nil
        var loop: Boolean = true
        do {
          val element = stack.removeFirst()
//        println("appending " + element)
          scc ::= element
          mark.put(element, Integer.MAX_VALUE)
          loop = element != node
        } while (loop)
        res.value ::= scc
      }
      min
    }
  }

}
