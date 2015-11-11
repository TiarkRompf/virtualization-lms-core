package scala.virtualization.lms.common

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context //FIXME: should be whitebox
import scala.annotation.StaticAnnotation

/**
  * These helper annotations should provide help to generate the type and RefinedManifest of a record when being used
  *
  * INPUT: when given:
  *
  * @record
  * case class Region(key: Int, name: String, comment: String)
  *
  * OUTPUT: it should generate:
  *
  * type Region = Record {
  *   val r_regionkey: Int
  *   val r_name: String
  *   val r_comment: String
  * }
  *
  * def Region(key: Rep[Int], name: Rep[String], comment: Rep[String]): Rep[Region] = Record (
  *   r_regionkey = key,
  *   r_name = name,
  *   r_comment = comment
  * )
  */

/** Annotation class for @record macro annotation. */
final class mRecord extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro mRecord.impl
}

/** Companion object implementing @record macro annotation. */
object mRecord {

  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val list = annottees.map(_.tree).toList
    //assert(list.size == 1) //we can only use @record on a single case class
    val tree = list.head
    tree match {
      case ClassDef(mods, className: TypeName, tparams, impl@Template(parents, selfType, bodyList))
        if mods.hasFlag(Flag.CASE) =>
        val fields = bodyList.take(bodyList.size - 1)
        assert(fields.forall { case _: ValDef => true }) //all except the last parameter should be field definitions
        assert(bodyList.last match { case _: DefDef => true }) //the constructor
        c.warning(tree.pos, "NOW IN RECORD TRANSFORM!")

        //just a dummy set of variable declarations so we can test our implementation without actually calling the macro
//        val dummyVars = fields.map{
//          case ValDef(_, termName, typeIdent, rhs) =>
//            val tn = TermName("r_"+termName)
//            ValDef(Modifiers(Flag.MUTABLE | Flag.DEFAULTINIT), tn, typeIdent, EmptyTree) //var c_r:C = _
//        }

        val fieldList = fields.map{
          case ValDef(_, termName, typeIdent, rhs) =>
            val tn = TermName("r_"+termName)
            q"val $tn: Rep[$typeIdent]"
        }
        val atype = q"type $className = Record { ..$fieldList }"

        val args = fields.map{
          case ValDef(_, termName, typeIdent, rhs) =>
            ValDef(Modifiers(Flag.PARAM), termName, AppliedTypeTree(Ident(TypeName("Rep")), List(typeIdent)), rhs)
        }
        val body = fields.map{
          case ValDef(_, termName, typeIdent, rhs) =>
            val tn = TermName("r_"+termName)
            Assign(Ident(tn), Ident(termName))
        }

        val mdef = q"def ${className.toTermName}(..$args):Rep[$className] = Record ( ..$body )"

        val objectName = TermName("O_"+className)

        //val importt = //FIXME

        val cc = q"object $objectName { $atype ; $mdef }"

        //        def ${className.toTermName}(
        //        ${
        //            fields.map {
        //              case ValDef(_, termName, typeIdent, rhs) => "" + termName + ": Rep[" + typeIdent + "]"
        //            }.mkString(", ")
        //          }
        //        )"""
        //        =
        //        Record (
        //        ${
        //            fields.map {
        //              case ValDef(_, termName, typeIdent, rhs) => "r_" + termName + " = " + termName
        //            }.mkString(", ")
        //          }
        //        )
        //            """
        //        ClassDef(
        //        modifiers,
        //        className,
        //        tparams,
        //        impl @ Template(
        //          parents,
        //          selfType,
        //          body @ List(
        //            ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName("i"), Ident(TypeName("Int")), EmptyTree),
        //            ValDef(Modifiers(CASEACCESSOR | PARAMACCESSOR), TermName("s"), Ident(TypeName("String")), EmptyTree),
        //            constructor = DefDef(
        //              Modifiers(),
        //              termNames.CONSTRUCTOR,
        //              List(),
        //              List(
        //                List(
        //                  ValDef(Modifiers(PARAM | PARAMACCESSOR),
        //                  TermName("i"),
        //                  Ident(TypeName("Int")),
        //                  EmptyTree),
        //                  ValDef(Modifiers(PARAM | PARAMACCESSOR), TermName("s"), Ident(TypeName("String")), EmptyTree)
        //                )
        //              ),
        //              TypeTree(),
        //              Block(List(pendingSuperCall), Literal(Constant(())))
        //            )
        //          )
        //        )
        //      )
        c.warning(tree.pos, showCode(cc))
        c.warning(tree.pos, showRaw(cc))
        println(showCode(cc))
        println(showRaw(cc))
        //c.Expr(Block(expandees, Literal(Constant(()))))
        c.Expr(cc)

      // sstucki: It seems necessary to keep the MUTABLE flag in the
      // new ValDef set, otherwise it becomes tricky to
      // "un-virtualize" a variable definition, if necessary
      // (e.g. if the DSL does not handle variable definitions in a
      // special way).
      case _ =>
        c.warning(tree.pos, "Only case classes can be transformed using the @record annotation!\r\n CODE: "+showCode(tree)+"\r\n RAW: "+showRaw(tree))
        annottees.head //FIXME
    }
  }
}