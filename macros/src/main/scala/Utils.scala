package org.scala_lang.virtualized

import scala.reflect.macros.blackbox.Context
import language.experimental.macros
import scala.util.matching.Regex

trait MacroModule {
  type Ctx <: Context
  val c: Ctx
}

trait DataDefs extends MacroModule {
  import c.universe._
  case class DSLFeature(tpe: Option[Type], name: String, targs: List[Tree], args: List[List[Type]])
}

/**
 * Common utilities for the Yin-Yang project.
 */
trait TransformationUtils extends MacroModule {
  import c.universe._
  import internal.decorators._

  /* These two should be unified */
  def method(recOpt: Option[Tree], methName: String, args: List[List[Tree]], targs: List[Tree] = Nil): Tree = {
    val calleeName = TermName(methName)
    val callee = recOpt match {
      case Some(rec) => Select(rec, calleeName)
      case None      => Ident(calleeName)
    }
    val calleeAndTargs: Tree = typeApply(targs)(callee)
    args.foldLeft(calleeAndTargs) { Apply(_, _) }
  }

  private[virtualized] def symbolId(symbol: Symbol): Int =
    symbol.asInstanceOf[scala.reflect.internal.Symbols#Symbol].id

  private[virtualized] def symbolId(tree: Tree): Int = symbolId(tree.symbol)

  def typeApply(targs: List[Tree])(select: Tree) = if (targs.nonEmpty)
    TypeApply(select, targs)
  else
    select

  def makeConstructor(classname: String, arguments: List[Tree]): Tree =
    Apply(Select(newClass(classname), termNames.CONSTRUCTOR), arguments)

  def newClass(classname: String) =
    New(Ident(TypeName(classname)))

  def copy(orig: Tree)(nev: Tree): Tree = {
    nev.setSymbol(orig.symbol)
    nev.setPos(orig.pos)
    nev
  }

  def log(s: => String, level: Int = 0) = if (debugLevel > level) println(s)

  def debugLevel: Int

  /*
   * Utility methods for logging.
   */
  def className: String = ???
  lazy val typeRegex = new Regex("(" + className.replace("$", "\\$") + """\.this\.)(\w*)""")
  lazy val typetagRegex = new Regex("""(scala\.reflect\.runtime\.[a-zA-Z`]*\.universe\.typeTag\[)(\w*)\]""")
  def code(tree: Tree, shortenDSLNames: Boolean = true): String = {
    var short = showCode(tree)
    if (shortenDSLNames) {
      typeRegex findAllIn short foreach { m =>
        val typeRegex(start, typ) = m
        short = short.replace(start + typ, typ.toUpperCase())
      }
      typetagRegex findAllIn short foreach { m =>
        val typetagRegex(start, typ) = m
        short = short.replace(start + typ + "]", "TYPETAG[" + typ.toUpperCase() + "]")
      }
    }
    short
  }
}
