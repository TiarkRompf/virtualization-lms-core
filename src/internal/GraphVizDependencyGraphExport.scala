package scala.virtualization.lms
package internal

import java.io.{File, FileWriter, PrintWriter}
import scala.virtualization.lms.util.ReflectionUtil
import scala.reflect.SourceContext

/**
 * This code-generator, actually generates DependencyGraph
 * between IR nodes graph in GraphViz format
 * It works similar to other codeg-generators like
 * ScalaCodegen or etc.
 */
trait GraphVizDependencyGraphExport extends GenericCodegen with NestedBlockTraversal { self =>
  val IR: ExtendedExpressions with Effects
  import IR._

  val GraphNodeKindInput = "input"
  val GraphNodeKindOutput = "output"

  var levelCounter = 0;

  /**
   * Produces IR graph node representation string, given:
   *  - a: node's symbol
   *  - kind: meta-data for the shape of node
   *    (can be GraphNodeKindInput, GraphNodeKindOutput
   *    , or any name for special nodes)
   *  - rhs: definition of this node (for finding dependencies)
   */
  def getGraphNodeString[A](a: Sym[_], kind: String, rhs: Def[A]): String = { 
    var atp = remap(a.tp).toString;

    val extra = kind match {
      case GraphNodeKindInput => ""
      case _ => "shape=record"
    }

    var indentation = indentString(levelCounter)
    var output = {
      indentation + "\"" + quote(a, true) + "\" [ " + extra +
        " label = <" + getNodeLabel(a) + ":" +
        atp + " = " + kind + ">];"
    }
    indentation = indentString(levelCounter+1)
    if(rhs != null) {
      ReflectionUtil.caseNameTypeValues(rhs) foreach {
        x => if(x._2 == classOf[Block[A]]) {
          val blk: Block[A] = x._3.asInstanceOf[Block[A]]
          focusExactScope(blk) { levelScope =>
            val clusterName: String = findSymString(blk.res, levelScope)
            val clusterStartingElem: String = findSymString(Const(0), levelScope)
            if(clusterName != "") {
              output += "\n"+indentation+"\"" + quote(a,true) +
                        "\" -> \"" + clusterStartingElem + 
                        "\" [lhead=cluster_"+clusterName+" color=gray];"
            }
          }
          emitBlock(blk)
        } else if(x._2 == classOf[Exp[A]]) {
          x._3 match {
            case s:Sym[_] => {
              output += "\n"+indentation+"\"" + quote(s,true) +
                        "\" -> \"" + quote(a,true) + "\";"
            }
            case _ =>
          }
        } else if (x._2 == classOf[Variable[A]]) {
          x._3.asInstanceOf[Variable[A]].e match {
            case s:Sym[_] => {
              output += "\n"+indentation+"\"" + quote(s,true) +
                        "\" -> \"" + quote(a,true) + "\";"
            }
            case _ =>
          }
        }
      }
    }

    output
  }

  override def emitSource[A : Manifest](args: List[Sym[_]], body: Block[A], className: String, out: PrintWriter, serializable: Boolean = false) = {

    val sA = remap(manifest[A])

    // TODO - reflect this static data with some (if any exists) representation in the graph
    val staticData = getFreeDataBlock(body)

    withStream(out) {
      levelCounter+=2
      val indentation = indentString(levelCounter-1)
      val indentationPlus1 = indentString(levelCounter)

      stream.println(indentation + "subgraph cluster" + className + " {")
      
      emitFileHeader()

      var transformedBody = performTransformations(body)

      stream.println(args.map( a => getGraphNodeString(a, GraphNodeKindInput, null)).mkString("\n"))
      emitBlock(transformedBody)
      val res = quote(getBlockResult(transformedBody))
      if(res != "()" && res.isInstanceOf[Sym[_]]) {
        stream.print("\n" + getGraphNodeString(res.asInstanceOf[Sym[_]], GraphNodeKindOutput, null))
      }

      stream.println("\n"+indentationPlus1+"label = \"" + className + "\";")
      stream.println(indentationPlus1+"node [style=filled];")
      stream.println(indentationPlus1+"color=blue;")
      stream.println(indentation+"}")
      levelCounter-=2
    }
    
    staticData
  }

  override def traverseBlockFocused[A](block: Block[A]): Unit = {
    focusExactScope(block) { levelScope =>
      val clusterName: String = findSymString(block.res, levelScope)

      val indentation = indentString(levelCounter)
      val indentationPlus1 = indentString(levelCounter+1)

      if(clusterName != "") {
        stream.println(indentation+"subgraph cluster_" + clusterName + " {")
      }
      traverseStmsInBlock(levelScope)
      if(clusterName != "") {
        stream.println("\n"+indentationPlus1+"label = \"" + clusterName + "\";")
        stream.println(indentationPlus1+"node [style=filled];")
        stream.println(indentationPlus1+"color=red;")
        stream.println(indentation+"}")
      }
    }
  }

  override def traverseStmsInBlock[A](stms: List[Stm]): Unit = {
    levelCounter+=1
    stms foreach traverseStm
    levelCounter-=1
  }

  override def traverseStm(stm: Stm) = stm match {
    case TP(sym, rhs) => rhs match {
      case Reflect(s, u, effects) => stream.println(getGraphNodeString(sym, s+"", s))
      case Reify(s, u, effects) => // just ignore -- effects are accounted for in emitBlock
      case _ => stream.println(getGraphNodeString(sym, rhs+"", rhs))
    }
    case _ => throw new GenerationFailedException("don't know how to generate code for statement: " + stm)
  }

  /**
   * This method produces the symbol name for a block, given:
   *  - an Exp for block result
   *  - list of statements in block
   * returns:
   *  - in the case that block result is a symbol => symbol name
   *  - in the case that block result is a constant =>
   *    first symbol name in the statements list
   */
  def findSymString(e: Exp[_], stms: List[Stm]): String = {
    def findFirstSymString: String = {
      stms match {
        case head :: tail => findSymString(head.asInstanceOf[TP[_]].sym, tail)
        case Nil => ""
      }
    }

    e match {
      case Const(x) => findFirstSymString
      case s@Sym(n) => quote(s, true)
    }
  }

  /**
   * It is possible to print more meta-data in the node-label
   * by overriding this method
   */
  def getNodeLabel(s: Sym[_]): String = quote(s, true);

  /**
   * utility method for generating proper indentation prefix
   * for given level.
   */
  private def indentString(level: Int): String = {
    def indentString(level: Int, acc: String): String = {
      if(level <= 0) acc
      else indentString(level - 1 , acc + "    ")
    }
    indentString(level, "")
  }

  // emitValDef is not used in this code generator
  def emitValDef(sym: Sym[Any], rhs: String): Unit = {}

  override def performTransformations[A:Manifest](body: Block[A]): Block[A] = {
    val transformedBody = super.performTransformations[A](body)
    val fixer = new SymMetaDataFixerTransform{ val IR: self.IR.type = self.IR }
    fixer.traverseBlock(transformedBody.asInstanceOf[fixer.Block[A]])
    transformedBody
  }

  override def remap(s: String): String = {
    val rs = super.remap(s)
    val lastDot = rs.lastIndexOf('.')
    val len = rs.length
    if(lastDot > 0 && lastDot+1 < len) {
      rs.substring(lastDot+1, len)
    } else {
      rs
    }
  }
}
