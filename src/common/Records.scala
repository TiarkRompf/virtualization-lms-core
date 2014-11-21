package scala.virtualization.lms
package common

import scala.language.experimental.macros
import scala.language.dynamics

import scala.reflect.macros.whitebox.Context

import org.scala_lang.virtualized.SourceContext
import org.scala_lang.virtualized.Struct
import org.scala_lang.virtualized.RefinedManifest

import util.OverloadHack

class RecordMacros(val c: Context) {
    import c.universe._
    def apply_impl[Rep: c.WeakTypeTag](method: c.Expr[String])(v: c.Expr[(String, Any)]*): c.Expr[Any] = {
      method.tree match {
        case Literal(Constant(str: String)) if str == "apply" =>
          recordApply(c.weakTypeTag[Rep].tpe)(v)
        case Literal(Constant(str: String)) =>
          val targetName = c.prefix.actualType.typeSymbol.fullName
          c.abort(NoPosition,
            s"value $str is not a member of $targetName")
        case _ =>
          val methodName = c.macroApplication.symbol.name
          c.abort(NoPosition,
            s"You may not invoke Rec.$methodName with a non-literal method name.")
      }
    }

    /**
     * Macro that implements [[Record.applyDynamicNamed]].
     */
    def recordApply(tp: Type)(v: Seq[c.Expr[(String, Any)]]): c.Expr[Any] = {
      val constantLiteralsMsg =
        "Records can only be constructed with constant keys (string literals)."
      val noEmptyStrMsg =
        "Records may not have a field with an empty name"

      object Tuple2 {
        def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
          case q"($a, $b)" => Some((a, b))
          case q"scala.this.Tuple2.apply[..${ _ }]($a, $b)" => Some((a, b))
          case _ => None
        }
      }
      val tuples = v.map(_.tree).map {
        case Tuple2(Literal(Constant(s: String)), v) =>
          if (s == "") c.abort(NoPosition, noEmptyStrMsg)
          else (s, v)
        case Tuple2(_, _) =>
          c.abort(NoPosition, constantLiteralsMsg)
        case x =>
          c.abort(NoPosition, "Records can only be constructed with named parameters on apply (a = b).")
      }

      val schema = tuples.map {
        case (s, v) =>
          val widened = v.tpe.widen
          val tpe = if (widened.typeSymbol == tp.typeSymbol) widened.dealias match {
            case TypeRef(_, _, arg :: Nil) => arg
          }
          else widened
          (s, tpe)
      }

      checkDuplicate(schema)
      val vals = schema.map {
        case (f, t) =>
          q"val ${TermName(f)}: $t"
      }

      val tpTree = tq"Record { ..$vals }"
      c.Expr(q"""
        __$$newRecord[$tpTree](..${tuples.map(x => q"(${x._1}, ${x._2})")})(
            ${refinedManifest(schema)}.asInstanceOf[Manifest[$tpTree]])
      """)
    }

    def materializeImpl[A: c.WeakTypeTag, B: c.WeakTypeTag]: Tree = {
      import compat._

      val srcMembers = c.weakTypeTag[A].tpe.members.collect { case x: MethodSymbol if x.isStable => x }
      val dstTpeMembers = srcMembers.map(x => q"""def ${x.name}: Rep[${x.returnType}]""")
      val dstMembers = srcMembers.map {x => q"""
        def ${x.name}: Rep[${x.returnType}] =
          __$$structField[${x.returnType}](rec,${x.name.toString})(
            ${tpeManifest(x.returnType)}.asInstanceOf[Manifest[${x.returnType}]])
      """}

      q"""new RecordAccessor[Rep[${weakTypeOf[A]}],{..$dstTpeMembers}]{
        def apply(rec: Rep[${weakTypeOf[A]}]): {..$dstTpeMembers} = new {
          ..$dstMembers
        }
      }"""
    }

    def materializeManifest[A <: Record : c.WeakTypeTag](ev: Tree): Tree ={
      val tp = c.weakTypeTag[A].tpe
      q"${refinedManifest(recordTypes(tp))}.asInstanceOf[Manifest[$tp]]"
    }


    private def checkDuplicate(schema: Seq[(String, c.Type)]): Unit = {
      val duplicateFields = schema.groupBy(_._1).filter(_._2.size > 1)
      if (duplicateFields.nonEmpty) {
        val fields = duplicateFields.keys.toList.sorted
        if (fields.size == 1)
          c.abort(NoPosition, s"Field ${fields.head} is defined more than once.")
        else
          c.abort(NoPosition, s"Fields ${fields.mkString(", ")} are defined more than once.")
      }
    }

    private def recordTypes(tpe: Type): Seq[(String, Type)] = for {
      mem <- tpe.members.toSeq
      if mem.asMethod.isStable
    } yield (mem.name.encoded, mem.asMethod.returnType)

    private def tpeManifest(tpe: Type): Tree =
      if(tpe <:< typeOf[Record]) refinedManifest(recordTypes(tpe)) else q"manifest[$tpe]"

    private def refinedManifest(schema: Seq[(String, Type)]): Tree = q"""
      new _root_.org.scala_lang.virtualized.RefinedManifest[Record] {
        val fields = List(..${schema.map(v => q"(${v._1}, ${tpeManifest(v._2)})")})
        def runtimeClass: Class[_] = classOf[Record]
      }
    """
  }

trait RecordOps extends StructOps {

  object Record extends Dynamic {

    /**
     * Create a "literal record" with field value pairs `v` using named
     * parameters:
     * {{{
     * Rec(name = "Hans", age = 7)
     * }}}
     */
    def applyDynamicNamed(method: String)(v: (String, Any)*): Any =
      macro RecordMacros.apply_impl[Rep[_]]
  }

  implicit def __$materializeRecordAccessor[A <: Record, B]: RecordAccessor[Rep[A], B] =
    macro RecordMacros.materializeImpl[A, B]

  implicit def __$convertRecord[A <: Record, B](rec: Rep[A])(implicit ev: RecordAccessor[Rep[A], B]): B =
    ev(rec)

  sealed trait RecordEvidence[+T]
  implicit def ev[T <: Record]: RecordEvidence[T] = new RecordEvidence[T]{}
  implicit def __$materializeManifest[T <: Record](implicit ev: RecordEvidence[T]): Manifest[T] =
    macro RecordMacros.materializeManifest[T]

  def __$newRecord[T: Manifest](v: (String, Rep[Any])*): Rep[T] =
    record_new[T](v.toSeq.map(v => (v._1, false, (_: Rep[T]) => v._2)))
  def __$structField[T: Manifest](rec: Rep[Record], field: String): Rep[T] =
    record_select(rec, field)

  trait RecordAccessor[From, To] {
    def apply(v: From): To
  }
}
