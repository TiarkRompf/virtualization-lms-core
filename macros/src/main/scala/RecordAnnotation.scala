package org.scala_lang.virtualized

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context //FIXME: should be whitebox
import scala.annotation.StaticAnnotation

/**
  * These helper annotations should provide help to generate the type and RefinedManifest of a record when being used
  *
  * INPUT: when given:
  *
  * @mRecord
  * case class Region(key: Int, name: String, comment: String)
  *
  * OUTPUT: it should generate:
  *
  * type Region = Record {
  *   val key: Int
  *   val name: String
  *   val comment: String
  * }
  *
  * def Region(key: Rep[Int], name: Rep[String], comment: Rep[String]): Rep[Region] = Record (
  *   key = key,
  *   name = name,
  *   comment = comment
  * )
  */

/** Annotation class for @mRecord macro annotation. */
final class mRecord extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro virtualizeRecord.impl
}

/** Companion object implementing @record macro annotation. */
object virtualizeRecord {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    if (annottees.length > 1) c.error(c.enclosingPosition, "Only case classes can be transformed using the @record annotation")

    val tree = annottees.head.tree
    tree match {
      // look for case classes
      case cd@ClassDef(mods, className: TypeName, tparams, impl@Template(parents, selfType, bodyList)) if mods.hasFlag(Flag.CASE) =>
        val (fields, methods) = bodyList.partition { case _:ValDef => true case _ => false }
        if (fields.isEmpty)
          c.abort(c.enclosingPosition, "case classes need at least one field in order to be transformed into records")
        if (methods.size > 1)
          c.abort(c.enclosingPosition, "case classes with a body (e.g. methods) cannot be transformed into records")
        if (tparams.size > 0)
          c.abort(c.enclosingPosition, "case classes with type parameters cannot be transformed into records")

        assert(methods.head match { case _: DefDef => true }) // the constructor

        val fieldList = fields map {
          case ValDef(mods, termName, typeIdent, rhs) if mods.hasFlag(Flag.MUTABLE) =>
            //q"var $termName: $typeIdent"
            c.abort(c.enclosingPosition, "virtualization of variable fields is currently unsupported")
          case ValDef(mods, termName, typeIdent, rhs) =>
            q"val $termName: $typeIdent"
        }
        val atype = q"type $className = Record { ..$fieldList }"

        val args = fields.map{
          case ValDef(_, termName, typeIdent, rhs) =>
            ValDef(Modifiers(Flag.PARAM), termName, AppliedTypeTree(Ident(TypeName("Rep")), List(typeIdent)), rhs)
        }
        val body = fields.map{
          case ValDef(_, termName, typeIdent, rhs) =>
            Assign(Ident(termName), Ident(termName))
        }

        val mdef = q"def ${className.toTermName}(..$args):Rep[$className] = Record ( ..$body )"

        val cc = q"$atype ; $mdef"

        // c.warning(tree.pos, showCode(cc))
        // c.warning(tree.pos, showRaw(cc))
        c.Expr(cc)
      case _ =>
        c.error(c.enclosingPosition, "Only case classes can be transformed using the @record annotation")
        annottees.head
    }
  }
}
